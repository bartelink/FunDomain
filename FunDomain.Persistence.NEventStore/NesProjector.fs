module FunDomain.Persistence.NEventStore.NesProjector

open NesGateway

let private empty = new Event<unit>()

let sleeping = empty.Publish

let start (store:Streamer) sleepMs projection = 
    MailboxProcessor.Start <|
        fun inbox ->
            let rec loop token = async {
                match token |> store.project projection with
                | Some token -> 
                    return! loop token
                | _ -> 
                    empty.Trigger ()
                    do! Async.Sleep sleepMs 
                    return! loop token }

            async {
                return! loop CheckpointToken.initial }
    |> ignore