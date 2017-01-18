module Uno.Game

////////////////////////////////////////////////////////////////////////////////
// Event Contracts

type Direction =
    | ClockWise
    | CounterClockWise

// Events
type GameStarted = { GameId:GameId; PlayerCount:int; FirstCard:Card; FirstPlayer:int }
type CardPlayed = { GameId:GameId; Player:int; Card:Card; NextPlayer:int }
type PlayedAtWrongTurn = { GameId:GameId; Player:int; Card:Card }
type PlayedWrongCard = { GameId:GameId; Player:int; Card:Card }
type DirectionChanged = { GameId:GameId; Direction:Direction }

type Event =
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | PlayedAtWrongTurn of PlayedAtWrongTurn
    | PlayedWrongCard of PlayedWrongCard
    | DirectionChanged of DirectionChanged

////////////////////////////////////////////////////////////////////////////////
// State / evolution function for folding events into same

type Turn =
    {   Player : int
        PlayerCount : int
        Direction : Direction }

/// Type representing current player turn; All operations should be encapsulated
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Turn =
    let start player count =
        {   Player = player
            PlayerCount = count
            Direction = ClockWise }

    let next turn =
        let selectNextPlayer turn =
            match turn.Direction with
            | ClockWise -> (turn.Player + 1) % turn.PlayerCount
            | CounterClockWise -> // the + count is here to avoid having negative result
                (turn.Player - 1 + turn.PlayerCount) % turn.PlayerCount
        { turn with Turn.Player = selectNextPlayer turn }

    let skip = next >> next

    let reverse turn = 
        match turn.Direction with
        | ClockWise -> { turn with Direction = CounterClockWise }
        | CounterClockWise -> { turn with Direction = ClockWise }

    let withPlayer player turn =
        if player < 0 || player >= turn.PlayerCount then 
            invalidArg "player" "The player value should be between 0 and player count"
        { turn with Player = player }

    let withDirection direction turn =
        { turn with Turn.Direction = direction }

type State =
    {   Turn : Turn
        TopCard : Card }

let evolve : State option -> Event -> State option =
    let beganWith = function
        | GameStarted e ->
            {   Turn = Turn.start e.FirstPlayer e.PlayerCount
                TopCard = e.FirstCard }
        | _ -> failwith "malformed event stream; expecting GameStarted"
    let continuedWith s = function
        | CardPlayed e ->
            {   Turn = s.Turn |> Turn.withPlayer e.NextPlayer
                TopCard = e.Card }
        | DirectionChanged e ->
            { s with Turn = s.Turn |> Turn.withDirection e.Direction }
        | PlayedAtWrongTurn _
        | PlayedWrongCard _ ->
            s
        | GameStarted _ -> failwith "illegal restart"
    function
    | None -> beganWith >> Some
    | Some state -> continuedWith state >> Some

////////////////////////////////////////////////////////////////////////////////
// Operations of the Game aggregate

type Command =
    | StartGame of StartGame
    | PlayCard of PlayCard

let (|Color|) = function
    | DigitCard (_,c) 
    | KickBack c 
    | Skip c -> c

let (|SameColor|_|) = function
    | Color c1c,Color c2c when c1c=c2c -> Some c1c
    | _ -> None

let (|SameValue|_|) = function
    | DigitCard(n1,_),DigitCard (n2,_) when n1 = n2 -> Some ()
    | KickBack _, KickBack _ -> Some ()
    | _ -> None

let decide : State option -> Command -> Event list = function
    | None -> function
        | StartGame c ->
            if c.PlayerCount <= 2 then invalidArg "playerCount" "There should be at least 3 players"

            let gameStartedWithPlayer firstPlayer = 
                GameStarted { GameId = c.GameId; PlayerCount = c.PlayerCount; FirstCard = c.FirstCard; FirstPlayer = firstPlayer }

            match c.FirstCard with
            | KickBack _ ->
                [   gameStartedWithPlayer 0 
                    DirectionChanged { GameId = c.GameId; Direction = CounterClockWise } ]
            | Skip _ -> [ gameStartedWithPlayer 1 ]
            | _ -> [ gameStartedWithPlayer 0]
        | _ -> invalidOp "The game needs to be started first"
    | Some state -> function
        | StartGame _ -> invalidOp "The game cannot be started more than once"
        | PlayCard c when state.Turn.Player <> c.Player ->
            [ PlayedAtWrongTurn { GameId = c.GameId; Player = c.Player; Card = c.Card } ]
        | PlayCard c ->
            match c.Card, state.TopCard with
            | SameColor _ 
            | SameValue ->
                let cardPlayed nextPlayer = 
                    CardPlayed { GameId = c.GameId; Player = c.Player; Card = c.Card; NextPlayer = nextPlayer }

                match c.Card with
                | KickBack _ ->
                    let nextTurn = state.Turn |> Turn.reverse |> Turn.next

                    [   cardPlayed nextTurn.Player
                        DirectionChanged { GameId = c.GameId; Direction = nextTurn.Direction } ]
                | Skip _ ->
                    let nextTurn = state.Turn |> Turn.skip

                    [ cardPlayed nextTurn.Player ]
                | _ ->
                    let nextTurn = state.Turn |> Turn.next
                    [ cardPlayed nextTurn.Player ]
            | _ -> [ PlayedWrongCard { GameId = c.GameId; Player = c.Player; Card = c.Card } ]