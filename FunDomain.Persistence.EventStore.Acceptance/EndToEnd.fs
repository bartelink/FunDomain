module FunDomain.Persistence.EventStore.Acceptance.EndToEnd

open FunDomain.Persistence.EventStore // Store
open FunDomain.Persistence.Fixtures // fullGameCommands, gameTopicId, randomGameId, establishProjection
open Swensen.Unquote
open Uno // Card Builders
open Uno.Game // Commands, aggregate
open Xunit

let playCircuit (store : Store) = async { 
    let monitor = DirectionMonitor()
    let projection = establishProjection monitor
    let gameId = randomGameId()
    let topicId = gameTopicId gameId
    let handle = CommandHandler.ofGesStore store topicId evolve decide 
    use! sub = store.subscribeStream topicId projection
    for cmd in fullCircuitCommands gameId do
        printfn "Processing %A against Stream %A" cmd topicId
        do! handle cmd
    return fun () -> monitor.CurrentDirectionOfGame gameId }

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
        |> withRetryingAndDelaying 10 100 } |> Async.StartAsTask