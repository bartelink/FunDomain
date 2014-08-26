module Scenarios

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.NEventStore.NesGateway // createInMemory, StreamId
open FunDomain.Persistence.NEventStore // NesProjector

open Xunit
open Swensen.Unquote

let drain (store:Streamer) project =     
    let dispatch (batch:EncodedEventBatch) =
        batch.chooseOfUnion () |> Seq.iter project
    let rec loop token =
        match token |> store.project dispatch with
        | Some token -> loop token
        | _ -> () // Exit
    loop CheckpointToken.initial

let fullGameActions gameId = [
    StartGame { GameId=gameId; PlayerCount=4; FirstCard=red 3 }
    PlayCard  { GameId=gameId; Player=0; Card=blue 3 }
    PlayCard  { GameId=gameId; Player=1; Card=blue 8 }
    PlayCard  { GameId=gameId; Player=2; Card=yellow 8 }
    PlayCard  { GameId=gameId; Player=3; Card=yellow 4 }
    PlayCard  { GameId=gameId; Player=0; Card=green 4 } 
    PlayCard  { GameId=gameId; Player=1; Card=KickBack Green } ]

let gameStreamId gameNo = {Bucket=None; StreamId=string gameNo}

type FlowEvents =
    | DirectionChanged of DirectionChanged
    | Started of GameStarted

let [<Fact>] ``Can run a full round using NEventStore's InMemoryPersistence`` () =
    let domainHandler = CommandHandler.create replay handle 

    let store = createInMemory()
    let persistingHandler = domainHandler store.read store.append 
    
    let gameNo = 42
    let streamId = gameStreamId gameNo

    for action in fullGameActions <| GameId gameNo do 
        printfn "Processing %A against Stream %A" action streamId
        action |> persistingHandler streamId

    let dirs = System.Collections.Generic.Dictionary<_,_> ()

    NesProjector.start store 10 (fun batch ->
        batch.chooseOfUnion () |> Seq.iter (fun evt ->
            evt |> function
                | Started { GameId = GameId no } -> printfn "Started: %i" no
                | DirectionChanged { GameId = GameId no; Direction = direction } -> printfn "Game %i direction is now: %A" no direction

            evt |> function
                | Started { GameId = GameId no } -> dirs.[no] <- ClockWise
                | DirectionChanged { GameId = GameId no; Direction = direction } -> dirs.[no] <- direction ) )

    Async.AwaitEvent NesProjector.sleeping 
    |> Async.RunSynchronously

    test <@ CounterClockWise = dirs.[gameNo] @>