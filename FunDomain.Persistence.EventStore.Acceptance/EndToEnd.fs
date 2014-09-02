module FunDomain.Persistence.EventStore.Acceptance.EndToEnd

open FunDomain.Persistence.Fixtures // Logger, DirectionMonitor, fullGameCommands, gameTopicId, randomGameId, createMonitorAndProjection

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.play
open FunDomain.Persistence.EventStore // Store

open Xunit
open Swensen.Unquote

let playCircuit (store:Store) = async {
    let domainHandler = CommandHandler.create play handle 

    let monitor, projection = createMonitorAndProjection ()
    let credentials = "admin", "changeit"
    let! _ = store.subscribe credentials projection

    let persistingHandler = domainHandler store.read store.append

    let gameId = randomGameId ()
    let topicId = gameTopicId gameId
    for action in fullCircuitCommands gameId do 
        printfn "Processing %A against Stream %A" action topicId
        do! action |> persistingHandler topicId 

    do! Async.Sleep 2000

    return monitor.CurrentDirectionOfGame gameId }

// Requires an EventStore 3.0 rc9 or later (with default parameters) instance to be running on the current machine 
let [<Fact>] ``Can play a circuit and consume projection using GetEventStore`` () = Async.StartAsTask <| async {
    let! store = GesGateway.create <| System.Net.IPEndPoint(System.Net.IPAddress.Loopback, 1113) 

    let! finalDirection = playCircuit store

    test <@ CounterClockWise = finalDirection @> }