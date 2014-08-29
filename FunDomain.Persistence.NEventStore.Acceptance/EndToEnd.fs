module FunDomain.Persistence.NEventStore.Acceptance.EndToEnd

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.NEventStore // Projector, StreamId

open Xunit
open Swensen.Unquote

type FlowEvents =
    | DirectionChanged of DirectionChanged
    | Started of GameStarted

type Logger() =
    let agent =
        MailboxProcessor.Start <| fun inbox -> async { 
            while true do
                let! evt = inbox.Receive () 
                evt |> function
                    | Started { GameId = GameId no } -> printfn "Started: %i" no
                    | DirectionChanged { GameId = GameId no; Direction = direction } -> printfn "Game %i direction is now: %A" no direction }
    member this.Post = agent.Post
        
type DirectionMonitor() = 
    let dirs = System.Collections.Generic.Dictionary<_,_> ()
    let agent = 
        MailboxProcessor.Start <| fun inbox -> async {
            while true do
                let! evt = inbox.Receive () 
                evt |> function
                    | Started e -> dirs.[e.GameId] <- ClockWise
                    | DirectionChanged e -> dirs.[e.GameId] <- e.Direction }
    member this.Post = agent.Post
    member this.CurrentDirectionOfGame gameId = dirs.[gameId]

let fullGameActions gameId = [
    StartGame { GameId=gameId; PlayerCount=4; FirstCard=red 3 }
    PlayCard  { GameId=gameId; Player=0; Card=blue 3 }
    PlayCard  { GameId=gameId; Player=1; Card=blue 8 }
    PlayCard  { GameId=gameId; Player=2; Card=yellow 8 }
    PlayCard  { GameId=gameId; Player=3; Card=yellow 4 }
    PlayCard  { GameId=gameId; Player=0; Card=green 4 } 
    PlayCard  { GameId=gameId; Player=1; Card=KickBack Green } ]

let gameStreamId (GameId no) = {Bucket=None; StreamId=string no }

let playCircuit store =
    let domainHandler = CommandHandler.create replay handle 

    let monitor = DirectionMonitor()
    let logger = Logger()

    let projector = Projector( store, 10, (fun batch ->
        batch.chooseOfUnion () |> Seq.iter (fun evt ->
            monitor.Post evt
            logger.Post evt)))

    let persistingHandler = domainHandler store.read store.append

    let gameId = GameId 42
    let streamId = gameStreamId gameId
    for action in fullGameActions gameId do 
        printfn "Processing %A against Stream %A" action streamId
        action |> persistingHandler streamId
        projector.Pulse ()

    Async.AwaitEvent projector.sleeping
    |> Async.RunSynchronously
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