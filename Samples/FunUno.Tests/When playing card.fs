module FunUno.Tests.``When playing card``

open FunUno.UnoGame

open Swensen.Unquote
open Xunit
open System

let context = [ GameStarted { GameId=1; PlayerCount=4; FirstCard=Digit(3, Red) }]

let [<Fact>] ``Same color should be accepted`` () =
    <@ replay context
    |> handle( PlayCard { GameId=1; Player=0; Card=Digit(9,Red)})
    =[ CardPlayed { GameId=1; Player=0; Card=Digit(9, Red) } ] @> 
    |> test

let [<Fact>] ``Same value should be accepted`` () =
    <@ replay context
    |> handle ( PlayCard { GameId=1; Player=0; Card=Digit(3, Yellow) } )
    =[ CardPlayed { GameId=1; Player=0; Card=Digit(3, Yellow) } ] @> 
    |> test

let [<Fact>] ``Different value and color should be rejected`` () =
    <@ replay context
    |> handle ( PlayCard { GameId=1; Player=0; Card=Digit(8, Yellow) }) @>
    |> raises<InvalidOperationException>

let [<Fact>] ``First player should play at his turn``() =
    <@ replay context
    |> handle ( PlayCard { GameId=1; Player=2; Card=Digit(3, Green) } ) @>
    |> raises<InvalidOperationException>