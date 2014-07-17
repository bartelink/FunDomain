module Scenarios

open FunUno.UnoGame // Commands, replay, handle
open FunUno.UnoGame.Events // Digit

open FunDomain // CommandHandler
open FunDomain.Persistence.NEventStore.NesGateway // createInMemory, StreamId

open Xunit

let fullGameActions gameId = [
    StartGame { GameId=gameId; PlayerCount=4; TopCard=Digit(3, Red) }
    PlayCard  { GameId=gameId; Player=0; Card=Digit(3, Blue) }
    PlayCard  { GameId=gameId; Player=1; Card=Digit(8, Blue) }
    PlayCard  { GameId=gameId; Player=2; Card=Digit(8, Yellow) }
    PlayCard  { GameId=gameId; Player=3; Card=Digit(4, Yellow) }
    PlayCard  { GameId=gameId; Player=0; Card=Digit(4, Green) } ]

let streamId gameId = {Bucket=None; StreamId=gameId |> string}

let [<Fact>] ``Can run a full round using NEventStore's InMemoryPersistence`` () =
    let domainHandler = CommandHandler.create replay handle 

    let store = createInMemory()
    let persistingHandler = domainHandler store.read store.append 

    let gameId = 42
    let stream = streamId gameId

    for action in fullGameActions gameId do 
        printfn "Processing %A against Stream %A" action stream
        action |> persistingHandler stream