module Scenarios

open Uno // Card Builders
open Uno.Game // Commands, handle

open FunDomain // CommandHandler, Evolution.replay
open FunDomain.Persistence.NEventStore.NesGateway // createInMemory, StreamId

open Xunit

let fullGameActions gameId = [
    StartGame { GameId=gameId; PlayerCount=4; FirstCard=red 3 }
    PlayCard  { GameId=gameId; Player=0; Card=blue 3 }
    PlayCard  { GameId=gameId; Player=1; Card=blue 8 }
    PlayCard  { GameId=gameId; Player=2; Card=yellow 8 }
    PlayCard  { GameId=gameId; Player=3; Card=yellow 4 }
    PlayCard  { GameId=gameId; Player=0; Card=green 4 } 
    PlayCard  { GameId=gameId; Player=0; Card=KickBack Green} ]

let gameStreamId gameNo = {Bucket=None; StreamId=string gameNo}

let [<Fact>] ``Can run a full round using NEventStore's InMemoryPersistence`` () =
    let domainHandler = CommandHandler.create replay handle 

    let store = createInMemory()
    let persistingHandler = domainHandler store.read store.append 

    let gameNo = 42
    let streamId = gameStreamId gameNo

    for action in fullGameActions <| GameId gameNo do 
        printfn "Processing %A against Stream %A" action streamId
        action |> persistingHandler streamId