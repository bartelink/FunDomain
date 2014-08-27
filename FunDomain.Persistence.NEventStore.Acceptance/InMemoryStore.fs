module FunDomain.Persistence.NEventStore.Acceptance.InMemoryStore

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.NEventStore // Projector, StreamId

open Xunit
open Swensen.Unquote

type FlowEvents =
    | DirectionChanged of DirectionChanged
    | Started of GameStarted

let logger = 
    MailboxProcessor.Start <| fun inbox -> async { 
        while true do
            let! evt = inbox.Receive () 
            evt |> function
                | Started { GameId = GameId no } -> printfn "Started: %i" no
                | DirectionChanged { GameId = GameId no; Direction = direction } -> printfn "Game %i direction is now: %A" no direction }
        
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

let [<Fact>] ``Can run a full round using NEventStore's InMemoryPersistence`` () =
    let domainHandler = CommandHandler.create replay handle 

    let monitor = DirectionMonitor()

    let store = NesGateway.createInMemory()
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

    test <@ CounterClockWise = monitor.CurrentDirectionOfGame gameId @>