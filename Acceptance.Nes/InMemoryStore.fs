module Scenarios

open FunUno
open FunUno.UnoGame
open FunUno.UnoGame.Events

open FunDomain
open FunDomain.Persistence

open Xunit
open System

let [<Fact>] ``Full Game with NES using InMemoryPersistence`` () =
    let store = NesStore.createInMemory()

    let gameId = 42

    let unoGameHandler = CommandHandler.create UnoGame.replay UnoGame.handle 

    let actions = [
        StartGame { GameId=gameId; PlayerCount=4; TopCard=Digit(3, Red) }
        PlayCard  { GameId=gameId; Player=0; Card=Digit(3, Blue) }
        PlayCard  { GameId=gameId; Player=1; Card=Digit(8, Blue) }
        PlayCard  { GameId=gameId; Player=2; Card=Digit(8, Yellow) }
        PlayCard  { GameId=gameId; Player=3; Card=Digit(4, Yellow) }
        PlayCard  { GameId=gameId; Player=0; Card=Digit(4, Green) } ]

    let streamId = "_",string gameId

    for action in actions do 
        printfn "%A" action
        action |> unoGameHandler store.read store.append streamId