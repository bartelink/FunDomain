module FunDomain.Persistence.NEventStore.Acceptance.EndToEnd

open FunDomain.Persistence.Fixtures // fullGameCommands, gameTopicId, randomGameId, establishProjection
open Uno // Card Builders
open Uno.Game // Commands, handle
open FunDomain // CommandHandler
open FunDomain.Persistence.NEventStore // Projector, StreamId
open Xunit
open Swensen.Unquote

// Shadow with explicit declaration of desired bucketing
let gameTopicId id = 
    { Bucket = None
      StreamId = gameTopicId id }

let playCircuit store = 
    async { 
        let domainHandler = CommandHandler.create handle
        let monitor = DirectionMonitor()
        let projection = establishProjection monitor
        let projector = Projector(store, 10, projection)
        let persistingHandler = domainHandler store.read store.append
        let gameId = randomGameId()
        let topicId = gameTopicId gameId
        for action in fullCircuitCommands gameId do
            printfn "Processing %A against Stream %A" action topicId
            do! action |> persistingHandler topicId
            projector.Pulse()
        do! Async.AwaitEvent projector.sleeping
        printfn "Projection queue empty"
        return monitor.CurrentDirectionOfGame gameId
    }

[<Fact>]
let ``Can play a circuit and consume projection using NES InMemory``() = 
    async { 
        let store = NesGateway.createInMemory()
        let! finalDirection = playCircuit store
        test <@ CounterClockWise = finalDirection @>
    }
    |> toFact

// NB Requires a SQL Server Instance with a DB Created
// Any SQL server version will do, but the app.config OOTB will be satisfied if you run ./CreateSqlLocalDb.ps1
[<Fact>]
let ``Can play a circuit and consume projection using NES SqlPersistence``() = 
    async { 
        let connectionStringName = "UnoNes"
        let store = NesGateway.createInMsSql connectionStringName
        store.executeDdlIfNecessary()
        let! finalDirection = playCircuit store
        test <@ CounterClockWise = finalDirection @>
    }
    |> toFact
