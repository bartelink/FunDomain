module FunDomain.Persistence.EventStore.GesAgent

open FunDomain
open FSharp.Control
open System.Collections.Concurrent
open FunDomain.Persistence.EventStore

module Agent = 
    let receive<'msg> (inbox : MailboxProcessor<'msg>) = inbox.Receive()
    let tryReceive<'msg> (inbox : MailboxProcessor<'msg>) = inbox.TryReceive 0
    let start = MailboxProcessor.Start
    
    let foldReceive handle (initialState : 'state) inbox = 
        let rec loop state = 
            async { 
                let! evt = receive<'event> inbox
                let state = handle state evt
                return! loop state
            }
        loop initialState
    
    let foldTryReceive handle (initialState : 'state) inbox = 
        let rec loop state = 
            async { 
                let! event = tryReceive<'event> inbox
                let state' = handle state event
                return! loop state'
            }
        loop initialState
    
    let foldReceiveAsync handle initialState inbox = 
        let rec loop state = async { let! evt = receive inbox
                                     let! state = handle state evt
                                     return! loop state }
        loop initialState

let createBoundedStreamer (store : Store) stream bufferSize = 
    asyncSeq { 
        // 'inspired by' https://gist.github.com/eulerfx/a4a29502f673f13b6a23
        use buffer = new BlockingCollection<_>(bufferSize : int)
        let inline onEvent (batch : EventBatch) = batch.mapToUnion() |> Seq.iter buffer.Add
        use! subs = store.subscribeStream stream onEvent
        yield! buffer.GetConsumingEnumerable() |> AsyncSeq.ofSeq
    }

type Command = 
    | Stop

type Output<'e> = 
    | EventAppeared of 'e

let createEventStreamerAgent storeEndpoint topicName dispatch = 
    let body inbox = 
        let rec loop state = 
            async { 
                match state with
                | None -> 
                    let! store = GesGateway.create storeEndpoint
                    let ticksObservable = createBoundedStreamer store topicName 10000 |> AsyncSeq.toObservable
                    let sub = ticksObservable |> Observable.subscribe (dispatch << EventAppeared)
                    return! loop <| Some(store, sub)
                | Some(store, sub) -> 
                    let! cmd = Agent.receive inbox
                    match cmd with
                    | Stop -> 
                        sub.Dispose()
                        store.Dispose()
            }
        loop None
    Agent.start body

let inline createCommandHandlerAgent storeEndpoint topic (handle : 'state -> 'command -> 'event list) = 
    let body inbox = 
        let rec loop state = 
            async { 
                match state with
                | None -> 
                    let! store = GesGateway.create storeEndpoint
                    let handler = CommandHandler.create handle
                    let persistingHandler = handler store.read (store.appendIdempotent DetermisticGuid.ofBytes) topic
                    return! loop <| Some persistingHandler
                | Some persistingHandler -> let! cmd = Agent.receive inbox
                                            do! persistingHandler cmd
            }
        loop None
    Agent.start body
