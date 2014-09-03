module FunDomain.Persistence.Fixtures

open Uno // Card Builders
open Uno.Game // Commands, handle
open FunDomain // CachingEventBatch

type FlowEvents =
    | DirectionChanged of DirectionChanged
    | GameStarted of GameStarted

type Logger() =
    let agent =
        MailboxProcessor.Start <| fun inbox -> async { 
            while true do
                let! evt = inbox.Receive () 
                evt |> function
                    | GameStarted { GameId = GameId no } -> 
                        printfn "Started: %i" no
                    | DirectionChanged { GameId = GameId no; Direction = direction } -> 
                        printfn "Game %i direction is now: %A" no direction }
    member this.Post = agent.Post
        
type DirectionMonitor() = 
    // NB we can have multiple concurrent readers (+single writer) hence this needs to be a concurrency-safe collection
    let gameDirections = System.Collections.Concurrent.ConcurrentDictionary<_,_> () 
    let agent = 
        MailboxProcessor.Start <| fun inbox -> async {
            while true do
                let! evt = inbox.Receive () 
                evt |> function
                    | GameStarted e -> gameDirections.[e.GameId] <- ClockWise
                    | DirectionChanged e -> gameDirections.[e.GameId] <- e.Direction }
    member this.Post = agent.Post
    member this.CurrentDirectionOfGame gameId = gameDirections.[gameId]

let createMonitorAndProjection () =
    let monitor = DirectionMonitor()
    let logger = Logger()

    let projection (batch:EventBatch) =
        let dispatchFlowEvent evt =
            monitor.Post evt
            logger.Post evt 
        batch.mapToUnion () |> Seq.iter dispatchFlowEvent

    monitor,projection

let fullCircuitCommands gameId = [
    StartGame { GameId=gameId; PlayerCount=4; FirstCard=red 3 }
    PlayCard  { GameId=gameId; Player=0; Card=blue 3 }
    PlayCard  { GameId=gameId; Player=1; Card=blue 8 }
    PlayCard  { GameId=gameId; Player=2; Card=yellow 8 }
    PlayCard  { GameId=gameId; Player=3; Card=yellow 4 }
    PlayCard  { GameId=gameId; Player=0; Card=green 4 } 
    PlayCard  { GameId=gameId; Player=1; Card=KickBack Green } ]

let gameTopicId (GameId no) = sprintf "Game-%s" <| string no

let randomGameId () =
    let gameNo = System.Random().Next()
    GameId gameNo