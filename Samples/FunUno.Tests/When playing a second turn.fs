module FunUno.Tests.``When playing a second turn``

open FunUno.UnoGame

open Swensen.Unquote
open Xunit
open System

let context = [   
    GameStarted {GameId=1; PlayerCount=4; FirstCard=Digit(3, Red)}
    CardPlayed  {GameId=1; Player=0; Card=Digit(9, Red) } ]

let [<Fact>] ``Same color should be accepted`` () =
    <@ replay context
    |> handle ( PlayCard {GameId=1; Player=1; Card=Digit(8, Red) } ) 
    =[ CardPlayed {GameId=1; Player=1; Card=Digit(8, Red)} ] @> 
    |> test

let [<Fact>] ``Same value should be accepted`` () =
    <@ replay context
    |> handle ( PlayCard {GameId=1; Player=1; Card=Digit(9, Yellow) } ) 
    =[ CardPlayed {GameId=1; Player=1; Card=Digit(9, Yellow)} ] @> 
    |> test

let [<Fact>] ``Different value and color should be rejected`` () =
    <@ replay context
    |> handle ( PlayCard {GameId=1; Player=1; Card=Digit(8, Yellow) } ) @>
    |> raises<InvalidOperationException>

let [<Fact>] ``First player should play at his turn``() =
    <@ replay context
    |> handle ( PlayCard {GameId=1; Player=4; Card=Digit(9, Green) } ) @>
    |> raises<InvalidOperationException>

let [<Fact>] ``After a full round it should be player 0 turn``() =
    <@ replay [ GameStarted {GameId=1; PlayerCount=4; FirstCard= Digit(3, Red)} 
                CardPlayed {GameId=1; Player=0; Card=Digit(9, Red)}
                CardPlayed {GameId=1; Player=1; Card=Digit(8, Red)}
                CardPlayed {GameId=1; Player=2; Card=Digit(6, Red)}
                CardPlayed {GameId=1; Player=3; Card=Digit(6, Red)} ] 
    |> handle ( PlayCard {GameId=1; Player=0; Card=Digit(1, Red)} )                
    =[ CardPlayed {GameId=1; Player=0; Card=Digit(1, Red) } ] @> 
    |> test