module Uno.Tests.``When playing card``

open Uno // UL
open Game // direct access to our SUT
let replay = Dynamical.Evolution.replay // Unquote breaks if you lean on the AutoOpen

open Swensen.Unquote
open Xunit

let gameId = GameId 1
let context = [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = red 3; FirstPlayer = 0 }]

let [<Fact>] ``Same color should be accepted``() = 
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 0; Card = red 9 }
        = [ CardPlayed { GameId = gameId; Player = 0; Card = red 9; NextPlayer = 1 } ] @>

let [<Fact>] ``Same value should be accepted``() =
    test <@ context |> replay|> handle <| PlayCard { GameId = gameId; Player = 0; Card = yellow 3 }
        = [ CardPlayed { GameId = gameId; Player = 0; Card = yellow 3; NextPlayer = 1 } ] @>

let [<Fact>] ``Different value and color should be rejected``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 0; Card = yellow 8 }
        = [ PlayedWrongCard { GameId = gameId; Player = 0; Card = yellow 8}] @> 

let [<Fact>] ``First player should play at his turn``() =
    test <@ context |> replay |> handle <| PlayCard { GameId = gameId; Player = 2; Card = green 3 }
        = [ PlayedAtWrongTurn { GameId = gameId; Player = 2; Card = green 3 } ] @>
    
let [<Fact>] ``First player player after starting with kickback should be the dealer, next one should be on the right`` () =
    test <@ [   GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = KickBack Green; FirstPlayer = 0 }
                DirectionChanged { GameId = gameId; Direction = CounterClockWise } ]
            |> replay |> handle <| PlayCard { GameId = gameId; Player = 0; Card = green 3 } 
        = [ CardPlayed { GameId = gameId; Player = 0; Card = green 3; NextPlayer = 3 }] @>

let [<Fact>] ``First player player after starting with skip should be the one after the dealer``() =
    test <@ [ GameStarted { GameId = gameId; PlayerCount = 4; FirstCard = Skip Green; FirstPlayer = 1 } ]
            |> replay |> handle <| PlayCard { GameId = gameId; Player = 1; Card = green 3 }
        = [ CardPlayed { GameId = gameId; Player = 1; Card = green 3; NextPlayer = 2 } ] @>