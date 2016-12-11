module FunDomain.Persistence.EventStore.Acceptance.IdExample

open FunDomain
open FunDomain.Persistence.EventStore
open FunDomain.Persistence.Fixtures
open Swensen.Unquote
open Xunit

type Event = 
    | Updated of int

type State = { Current : int }
let initialState = { Current = -1 }
let evolve state = function
    | Updated x when state.Current = x - 1 -> { state with Current = x }
    | Updated _ as e -> failwithf "event %A not permitted in state %A" e state

type Command =
    | Update of int

let decide = function
    | { Current = current } -> 
        function 
        | Update x when current = x -> []
        | Update x when current + 1 = x -> [ Updated x ]
        | Update _ as c -> failwithf "Command %A not permitted in State %A" c current

let run = Seq.fold evolve initialState >> decide

[<Fact>]
let ``Can send 0 initially``() = 
    []
    |> run <| Update 0
    =! [ Updated 0 ]

[<Fact>]
let ``Can not handle non-zero initally``() = 
    <@ [] |> run <| Update 5 @>
    |> raisesWith <| fun e -> <@ (e : exn).Message = "Command Update 5 not permitted in State -1" @>

[<Fact>]
let ``Can handle sequenced updates``() = 
    [   Updated 0
        Updated 1 ]
    |> run <| Update 2
    =! [Updated 2]

module Agent = 
    let recieve<'msg> (inbox : MailboxProcessor<'msg>) = inbox.Receive()
    let start = MailboxProcessor.Start
    
    let fold handle (initialState : 'state) inbox = 
        let rec loop state = 
            async { 
                let! evt = recieve<'event> inbox
                let state = handle state evt
                return! loop state
            }
        loop initialState
    
    let foldAsync handle initialState inbox = 
        let rec loop state = async { let! evt = recieve inbox
                                     let! state = handle state evt
                                     return! loop state }
        loop initialState

open System
open System.Net

// Requires an EventStore 3.0 or later (with default parameters) instance to be running on the current machine 
let createStore() = GesGateway.create <| IPEndPoint(IPAddress.Loopback, 1113)

type CountMonitor() = 
    let mutable max = 0
    let handle _ = function 
        | Updated x -> max <- x
    let agent = Agent.start <| Agent.fold handle ()
    member __.Post = agent.Post
    member __.Max = max
let idAggregate = evolve, initialState, decide

module Dispatcher = 
    let forStreamProjector handler (batch : EventBatch) = batch.mapToUnion() |> Seq.iter handler
    let forGlobalProjector handler s (batch : EventBatch) = batch.mapToUnion() |> Seq.iter (fun e -> handler s e)

module Subscriptions = 
    [<Fact>]
    let ``Can manage updates with GES using global subscription``() = 
        async { 
            use! store = createStore()
            let topic = string <| Guid.NewGuid()
            let monitor = CountMonitor()
            let dispatcher = Dispatcher.forGlobalProjector (fun s e -> monitor.Post e)
            use! sub = store.subscribeAll ("admin", "changeit") dispatcher
            let handle = CommandHandler.ofGesStore store idAggregate topic
            do! handle <| Update 0
            do! handle <| Update 1
            (fun () -> monitor.Max =! 1) |> withRetryingAndDelaying 5 100 } |> Async.StartAsTask
    
    [<Fact>]
    let ``Can manage updates with GES using local subscription``() = 
        async { 
            use! store = createStore()
            let topic = string <| Guid.NewGuid()
            let monitor = CountMonitor()
            let dispatcher = Dispatcher.forStreamProjector monitor.Post
            use! sub = store.subscribeStream topic dispatcher
            let handle = CommandHandler.ofGesStore store idAggregate topic
            do! handle <| Update 0
            do! handle <| Update 1
            (fun () -> monitor.Max =! 1) |> withRetryingAndDelaying 5 100 } |> Async.StartAsTask

module ParallelUpdates = 
    // See also CommandHandler.create
    module CommandHandlerIdempotent = 
        open EventStore.ClientAPI.Exceptions
        
        let inline create
            (read : 'streamId -> int -> int -> Async<EncodedEvent seq * 'token * int option>)
            (appendIdempotent : (byte [] -> Guid) -> 'streamId -> 'token -> EncodedEvent seq -> Async<'token>)
            (determisticGuidOfBytes : byte [] -> Guid)
            (evolve : 'state -> 'event -> 'state)
            (initialState : 'state)
            (decide : 'state -> 'command -> 'event list) =
            let saveEvents streamId token (state : 'state) events = 
                async { 
                    try 
                        let! token' = events 
                                      |> CommandHandler.save (appendIdempotent determisticGuidOfBytes) streamId token
                        let state' = Seq.fold evolve state events
                        return Some(token', state')
                    with :? AggregateException as e when (e.Flatten().InnerException :? WrongExpectedVersionException) -> 
                        return None
                }
            fun streamId (interpret : 'state -> 'command) bookmark -> 
                async { 
                    let! (token, state) = match bookmark with
                                          | None -> CommandHandler.load read evolve initialState streamId
                                          | Some bookmark -> bookmark |> async.Return
                    let command = interpret state
                    let events = decide state command
                    if List.isEmpty events then return None
                    else return! events |> saveEvents streamId token state
                }
    
    let act (r : Random) topic (monitor : CountMonitor) = 
        async { 
            use! store = createStore()
            let dispatcher = Dispatcher.forStreamProjector monitor.Post
            use! sub = store.subscribeStream topic dispatcher
            let handle =
                CommandHandlerIdempotent.create store.read store.appendIdempotent DetermisticGuid.ofBytes evolve initialState decide topic
            let interpret { Current = current } = 
                let incrementOrMaybeNot = (current + r.Next(2))
                Update incrementOrMaybeNot
            
            let! b1 = handle interpret None
            let! b2 = handle interpret b1
            let! b3 = handle interpret b2
            let! b4 = handle interpret b3
            let! b5 = handle interpret b4
            let! b6 = handle interpret b5
            let maxSaved = 
                [ b1; b2; b3; b4; b5; b6 ]
                |> List.map (Option.map (fun (_, { Current = current }) -> current))
                |> List.max
            return maxSaved
        }
    
    [<Fact>]
    let ``Can manage parallel updates with GES``() =
        async { 
            let actRandom = act <| Random()
            let topic = string <| Guid.NewGuid()
            let actRandomOnTopic = actRandom topic
            let monitor1, monitor2 = CountMonitor(), CountMonitor()
            let! act1 = actRandomOnTopic monitor1 |> Async.StartChild
            let! current1 = actRandomOnTopic monitor2
            let! current2 = act1
            fun () -> 
                let max1, max2 = monitor1.Max, monitor2.Max
                defaultArg (max current1 current2) 42 =! max max1 max2
            |> withRetryingAndDelaying 10 100 } |> Async.StartAsTask