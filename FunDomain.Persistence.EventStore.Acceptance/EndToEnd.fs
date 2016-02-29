module FunDomain.Persistence.EventStore.Acceptance.EndToEnd

open FunDomain.Persistence.Fixtures // fullGameCommands, gameTopicId, randomGameId, establishProjection, toFact
open Uno // Card Builders
open Uno.Game // Commands, handle
open FunDomain // CommandHandler
open FunDomain.Persistence.EventStore // Store
open Xunit
open Swensen.Unquote

let playCircuit (store : Store) = 
    async { 
        let domainHandler = CommandHandler.create handle
        let monitor = DirectionMonitor()
        let projection = establishProjection monitor
        let persistingHandler = domainHandler store.read store.append
        let gameId = randomGameId()
        let topicId = gameTopicId gameId
        use! sub = store.subscribeStream topicId projection
        for action in fullCircuitCommands gameId do
            printfn "Processing %A against Stream %A" action topicId
            do! action |> persistingHandler topicId
        return fun () -> monitor.CurrentDirectionOfGame gameId
    }

// Requires an EventStore 3.0 or later (with default parameters) instance to be running on the current machine 
let createStore() = GesGateway.create <| System.Net.IPEndPoint(System.Net.IPAddress.Loopback, 1113)

[<Fact>]
let ``Can play a circuit and consume projection using GetEventStore``() = 
    async { 
        use! store = createStore()
        let! checkResult = playCircuit store
        fun () -> 
            let finalDirection = checkResult()
            CounterClockWise =! finalDirection
        |> withRetryingAndDelaying 10 100
    }
    |> toFact
