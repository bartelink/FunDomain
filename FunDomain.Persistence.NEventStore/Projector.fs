namespace FunDomain.Persistence.NEventStore

open System.Threading

type Projector( store:Store, sleepMs, projection ) = 
    let empty = new Event<unit>()
    let wakeEvent = new AutoResetEvent false
    let _ =
        MailboxProcessor.Start <|
            fun inbox ->
                let rec loop token = async {
                    let! nextToken = store.project projection token
                    match nextToken with
                    | Some token -> 
                        return! loop token
                    | _ -> 
                        empty.Trigger ()
                        let! _ = Async.AwaitWaitHandle (wakeEvent,sleepMs)
                        return! loop token }

                async {
                    return! loop CheckpointToken.initial }
    member this.sleeping = empty.Publish
    member this.Pulse = wakeEvent.Set >> ignore    