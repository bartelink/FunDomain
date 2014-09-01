module FunDomain.Persistence.NEventStore.Acceptance.EndToEnd

open FunDomain.Persistence.Fixtures // Logger, DirectionMonitor, fullGameCommands, gameTopicId, randomGameId

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.NEventStore // Projector, StreamId

open Xunit
open Swensen.Unquote

// Shadow with explicit declaration of desired bucketing
let gameTopicId id = {Bucket=None; StreamId=gameTopicId id }

let playCircuit store =
    let domainHandler = CommandHandler.create replay handle 

    let monitor = DirectionMonitor()
    let logger = Logger()

    let projector = Projector( store, 10, (fun batch ->
        batch.chooseOfUnion () |> Seq.iter (fun evt ->
            monitor.Post evt
            logger.Post evt)))

    let persistingHandler = domainHandler store.read store.append

    let gameId = randomGameId ()
    let topicId = gameTopicId gameId
    for action in fullCircuitCommands gameId do 
        printfn "Processing %A against Stream %A" action topicId
        action |> persistingHandler topicId
        projector.Pulse ()

    Async.AwaitEvent projector.sleeping |> Async.RunSynchronously
    printfn "Projection queue empty"

    monitor.CurrentDirectionOfGame gameId

let [<Fact>] ``Can play a circuit and consume projection using NEventStore InMemory`` () =
    let store = NesGateway.createInMemory ()

    let finalDirection = playCircuit store

    test <@ CounterClockWise = finalDirection @>

// NB Requires a SQL Server Instance with a DB Created. Any SQL will do, but the app.config will be satisfied if you run ./CreateSqlLocalDb.ps1
let [<Fact>] ``Can play a circuit and consume projection using NEventStore SqlPersistence`` () =
    let connectionStringName = "UnoNes"
    let perfCounterInstanceName = "UnoNes"
    let store = NesGateway.createInMsSqlWithPerfCounters connectionStringName perfCounterInstanceName 

    let finalDirection = playCircuit store

    test <@ CounterClockWise = finalDirection @>