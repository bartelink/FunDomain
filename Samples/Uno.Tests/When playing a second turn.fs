module Uno.Tests.``When playing a second turn``

open Uno // UL
open Game // direct access to our SUT
let replay = Dynamical.Evolution.replay // Unquote breaks if you lean on the AutoOpen

open Xunit
open Swensen.Unquote

let gameId = GameId 1

let context = [   
    GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = red 3; FirstPlayer = 0 }
    CardPlayed { GameId = gameId; Player = 0; Card = red 9; NextPlayer = 1 } ] 

let [<Fact>] ``Same color should be accepted``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 1; Card = red 8 }
        = [ CardPlayed { GameId = gameId; Player = 1; Card = red 8; NextPlayer = 2 } ] @>

let [<Fact>] ``Same value should be accepted``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 1; Card = yellow 9 }
        = [ CardPlayed { GameId = gameId; Player = 1; Card = yellow 9; NextPlayer = 2 } ] @>

let [<Fact>] ``Different value and color should be rejected``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 1; Card = yellow 8 }
        = [ PlayedWrongCard { GameId = gameId; Player = 1; Card = yellow 8}] @>

let [<Fact>] ``Player should play at his turn``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 2; Card = green 9 }
        = [ PlayedAtWrongTurn { GameId = gameId; Player = 2; Card = green 9 }] @>

let [<Fact>]  ``After a full round it should be player 0 turn``() =
    test <@ [   GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = red 3; FirstPlayer = 0 }
                CardPlayed { GameId = gameId; Player = 0; Card = red 9; NextPlayer = 1 }
                CardPlayed { GameId = gameId; Player = 1; Card = red 8; NextPlayer = 2 }
                CardPlayed { GameId = gameId; Player = 2; Card = red 6; NextPlayer = 3 } ]
            |> replay |> handle <| PlayCard { GameId = gameId; Player = 3; Card = red 1 }
        = [ CardPlayed { GameId = gameId; Player = 3; Card = red 1; NextPlayer = 0 } ] @>