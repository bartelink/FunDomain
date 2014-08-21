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

type Direction =
    | ClockWise
    | CounterClockWise

type GameId = GameId of int

type GameStarted = { GameId: GameId; PlayerCount: int; FirstCard: Card; FirstPlayer: int }
type CardPlayed = { GameId: GameId; Player: int; Card: Card; NextPlayer: int }
type PlayedAtWrongTurn = { GameId: GameId; Player: int; Card: Card }
type PlayedWrongCard = { GameId: GameId; Player: int; Card: Card }
type DirectionChanged = { GameId: GameId; Direction: Direction }