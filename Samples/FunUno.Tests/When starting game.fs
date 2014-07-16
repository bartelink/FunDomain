module FunUno.Tests.``When starting game``

open FunUno.UnoGame
open FunUno.UnoGame.Events

open Swensen.Unquote
open Xunit
open System

let [<Fact>] ``Started game should be started`` () = 
    <@ replay [ ] 
    |> handle ( StartGame {GameId=1; PlayerCount=3; TopCard=Digit(3, Red)})
    =[ GameStarted {GameId=1; PlayerCount=3; TopCard=Digit(3, Red)} ] @> 
    |> test

let [<Fact>] ``0 players should be rejected`` () = 
    <@ replay [ ]
    |> handle ( StartGame {GameId=1; PlayerCount=0; TopCard=Digit(3, Red)}) @>
    |> raises<ArgumentException> 

let [<Fact>] ``Game should not be started twice`` () =
    <@ replay [ GameStarted {GameId=1; PlayerCount=4; TopCard=Digit(3, Red)} ]
    |> handle ( StartGame {GameId=1; PlayerCount=4; TopCard=Digit(2, Red)} ) @>
    |> raises<InvalidOperationException>