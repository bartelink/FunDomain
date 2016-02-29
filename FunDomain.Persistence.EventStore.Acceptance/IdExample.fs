module FunDomain.Persistence.EventStore.Acceptance.IdExample

open Dynamical
open FunDomain
open FunDomain.Persistence.EventStore
open FunDomain.Persistence.Fixtures
open Swensen.Unquote
open Xunit

type Event = 
    | Updated of int

type State = 
    { Current : int }
    static member InitialState = { Current = -1 }
    member this.Evolve = 
        function 
        | Updated x when this.Current = x - 1 -> { this with Current = x }
        | Updated _ as e -> failwithf "event %A not permitted in state %A" e this

type Command = 
    | Update of int

let handle = function 
    | { Current = current } -> 
        function 
        | Update x when current = x -> []
        | Update x when current + 1 = x -> [ Updated x ]
        | Update _ as c -> failwithf "Command %A not permitted in State %A" c current

[<AutoOpen>]
module TestHelpers = 
    let inline Given e = Dynamical.Evolution.replay e
    let inline When c s = handle s c
    let inline Then e r = r =! e

[<Fact>]
let ``Can send 0 initially``() = 
    Given []
    |> When(Update 0)
    |> Then [ Updated 0 ]

[<Fact>]
let ``Can not handle non-zero initally``() = 
    let state = Given []
    <@ handle state <| Update 5 @>
    |> raisesWith
    <| (fun e -> <@ (e : exn).Message = "Command Update 5 not permitted in State -1" @>)

[<Fact>]
let ``Can handle sequenced updates``() = 
    Given [ Updated 0
            Updated 1 ]
    |> When(Update 2)
    |> Then [ Updated 2 ]

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

/// Ensure we match the required signature dictated by xUnit.net
let toFact computation : System.Threading.Tasks.Task = Async.StartAsTask computation :> _

type CountMonitor() = 
    let mutable max = 0
    let handle _ = function 
        | Updated x -> max <- x
    let agent = Agent.start <| Agent.fold handle ()
    member this.Post = agent.Post
    member this.Max = max

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
            let handle = CommandHandler.create handle store.read store.append topic
            do! handle <| Update 0
            do! handle <| Update 1
            (fun () -> monitor.Max =! 1) |> withRetryingAndDelaying 5 100
        }
        |> toFact
    
    [<Fact>]
    let ``Can manage updates with GES using local subscription``() = 
        async { 
            use! store = createStore()
            let topic = string <| Guid.NewGuid()
            let monitor = CountMonitor()
            let dispatcher = Dispatcher.forStreamProjector monitor.Post
            use! sub = store.subscribeStream topic dispatcher
            let handle = CommandHandler.create handle store.read store.append topic
            do! handle <| Update 0
            do! handle <| Update 1
            (fun () -> monitor.Max =! 1) |> withRetryingAndDelaying 5 100
        }
        |> toFact

module ParallelUpdates = 
    // See also CommandHandler.create
    module CommandHandlerIdempotent = 
        open EventStore.ClientAPI.Exceptions
        
        let inline create (handle : 'state -> 'command -> 'event list) 
                   (read : 'streamId -> int -> int -> Async<EncodedEvent seq * 'token * int option>) 
                   (appendIdempotent : (byte [] -> Guid) -> 'streamId -> 'token -> EncodedEvent seq -> Async<'token>) 
                   (determisticGuidOfBytes : byte [] -> Guid) = 
            let saveEvents streamId token state events = 
                async { 
                    try 
                        let! token' = events 
                                      |> CommandHandler.save (appendIdempotent determisticGuidOfBytes) streamId token
                        let state' = Evolution.advance state events
                        return Some(token', state')
                    with :? AggregateException as e when (e.Flatten().InnerException :? WrongExpectedVersionException) -> 
                        return None
                }
            fun streamId (decide : 'state -> 'command) bookmark -> 
                async { 
                    let! (token, state) = match bookmark with
                                          | None -> CommandHandler.load read streamId
                                          | Some bookmark -> bookmark |> async.Return
                    let command = decide state
                    let events = handle state command
                    if List.isEmpty events then return None
                    else return! events |> saveEvents streamId token state
                }
    
    let act (r : Random) topic (monitor : CountMonitor) = 
        async { 
            use! store = createStore()
            let dispatcher = Dispatcher.forStreamProjector monitor.Post
            use! sub = store.subscribeStream topic dispatcher
            let decide ({ Current = current } as state) = 
                let incrementOrMaybeNot = (current + r.Next(2))
                Update incrementOrMaybeNot
            
            let handle = 
                CommandHandlerIdempotent.create handle store.read store.appendIdempotent DetermisticGuid.ofBytes topic
            let! b1 = handle decide None
            let! b2 = handle decide b1
            let! b3 = handle decide b2
            let! b4 = handle decide b3
            let! b5 = handle decide b4
            let! b6 = handle decide b5
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
            |> withRetryingAndDelaying 10 100
        }
        |> toFact
