namespace Uno

type Digit = Digit of int

type Color = 
    | Red
    | Green
    | Blue
    | Yellow

type Card =
    | DigitCard of Value:Digit * Color:Color
    | KickBack of Color:Color
    | Skip of Color:Color

type GameId = GameId of int

// Commands
type StartGame = { GameId:GameId; PlayerCount:int; FirstCard:Card }
type PlayCard = { GameId:GameId; Player:int; Card:Card }