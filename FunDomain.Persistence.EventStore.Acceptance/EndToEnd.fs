module FunDomain.Persistence.EventStore.Acceptance.EndToEnd

open FunDomain.Persistence.Fixtures // Logger, DirectionMonitor, fullGameCommands, gameTopicId, randomGameId

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.EventStore 

open Xunit
open Swensen.Unquote

let playCircuit (store:Store) =
    let domainHandler = CommandHandler.createAsyncSliced (initial'()) evolve' handle 

    let monitor = DirectionMonitor()
    let logger = Logger()
    let credentials = "admin", "changeit"
    store.subscribe credentials (fun evt ->
        monitor.Post evt
        logger.Post evt ) |> Async.Ignore |> Async.RunSynchronously
    let persistingHandler = domainHandler store.read store.append

    let gameId = randomGameId ()
    let topicId = gameTopicId gameId
    for action in fullCircuitCommands gameId do 
        printfn "Processing %A against Stream %A" action topicId
        action |> persistingHandler topicId |> Async.RunSynchronously

    Async.Sleep 2000 |> Async.RunSynchronously

    monitor.CurrentDirectionOfGame gameId

let [<Fact>] ``Can play a circuit and consume projection using GetEventStore`` () =
    let store = GesGateway.create <| System.Net.IPEndPoint(System.Net.IPAddress.Loopback, 1113) |> Async.RunSynchronously

    let finalDirection = playCircuit store

    test <@ CounterClockWise = finalDirection @>