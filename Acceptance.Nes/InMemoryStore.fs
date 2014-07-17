module Scenarios

open FunUno
open FunUno.UnoGame
open FunUno.UnoGame.Events

open FunDomain // CommandHandler
open FunDomain.Persistence.NEventStore // NesStore

open Xunit
open System

let fullGameActions gameId = [
    StartGame { GameId=gameId; PlayerCount=4; TopCard=Digit(3, Red) }
    PlayCard  { GameId=gameId; Player=0; Card=Digit(3, Blue) }
    PlayCard  { GameId=gameId; Player=1; Card=Digit(8, Blue) }
    PlayCard  { GameId=gameId; Player=2; Card=Digit(8, Yellow) }
    PlayCard  { GameId=gameId; Player=3; Card=Digit(4, Yellow) }
    PlayCard  { GameId=gameId; Player=0; Card=Digit(4, Green) } ]

let [<Fact>] ``Can run a full round using NEventStore's InMemoryPersistence`` () =
    let gameId = 42

    let unoGameHandler = CommandHandler.create UnoGame.replay UnoGame.handle 

    let store = NesGateway.createInMemory()

    let bucket = "default"
    let streamId = bucket,string gameId

    for action in fullGameActions gameId do 
        printfn "%A" action
        action |> unoGameHandler store.read store.append streamId