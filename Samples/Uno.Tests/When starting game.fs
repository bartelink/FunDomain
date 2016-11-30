module Uno.Tests.``When starting game``

open Uno // UL
open Game // direct access to our SUT
let replay = Dynamical.Evolution.replay // Unquote breaks if you lean on the AutoOpen

open Swensen.Unquote
open Xunit
open System

let gameId = GameId 1

let [<Fact>] ``Started game should be started``() =
    test <@ [] |> replay |> handle <| StartGame { GameId = gameId; PlayerCount = 4; FirstCard = red 3 }
        = [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = red 3; FirstPlayer = 0 } ] @> 

let [<Fact>] ``0 players should be rejected``() =
    <@  [] |> replay |> handle <| StartGame { GameId = gameId; PlayerCount = 0; FirstCard = red 3 } @>
    |> raises<ArgumentException>

let [<Fact>] ``Game should not be started twice``() =
    <@  [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = red 3; FirstPlayer = 0 } ]
        |> replay |> handle <| StartGame { GameId = gameId; PlayerCount = 4; FirstCard = red 2 } @>
    |> raises<InvalidOperationException>

let [<Fact>] ``Starting with a kickback should change direction``() =
    test <@ [] |> replay |> handle <| StartGame { GameId = gameId; PlayerCount = 4; FirstCard = KickBack Yellow }
        = [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = KickBack Yellow; FirstPlayer = 0}
            DirectionChanged { GameId = gameId; Direction = CounterClockWise } ] @> 

let [<Fact>] ``Starting with a skip should skip dealer's turn``() =
    test <@ [] |> replay |> handle <| StartGame { GameId = gameId; PlayerCount = 4; FirstCard = Skip Yellow }
        = [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = Skip Yellow; FirstPlayer = 1} ] @>