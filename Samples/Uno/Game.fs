module Uno.Game

type Command =
    | StartGame of StartGame
    | PlayCard of PlayCard

type Event =
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | PlayedAtWrongTurn of PlayedAtWrongTurn
    | PlayedWrongCard of PlayedWrongCard
    | DirectionChanged of DirectionChanged

/// Type representing current player turn; All operations should be encapsulated
type Turn =
    {   Player : int
        PlayerCount : int
        Direction : Direction }

    static member Start player count =
        {   Player = player
            PlayerCount = count
            Direction = ClockWise }

    member turn.Next = 
        let selectNextPlayer = function 
            | ClockWise -> (turn.Player + 1) % turn.PlayerCount
            | CounterClockWise -> // the + count is here to avoid having negative result
                (turn.Player - 1 + turn.PlayerCount) % turn.PlayerCount
        { turn with Player = selectNextPlayer turn.Direction }
    member turn.Skip =
        turn.Next.Next
    member turn.Reverse =
        { turn with Direction = turn.Direction.Reverse }

    member turn.WithDirection direction =
        { turn with Direction = direction }
    member turn.WithPlayer player =
        if player < 0 || player >= turn.PlayerCount then 
            invalidArg "player" "The player value should be between 0 and player count"
        { turn with Player = player }
and Direction with
    member this.Reverse =
        match this with
        | ClockWise -> CounterClockWise
        | CounterClockWise -> ClockWise 

////////////////////////////////////////////////////////////////////////////////
// State / evolution function for folding events into same

type State =
    {   Turn : Turn
        TopCard : Card }

let evolve : State option -> Event -> State option =
    let beganWith = function
        | GameStarted e ->
            {   Turn = Turn.Start e.FirstPlayer e.PlayerCount
                TopCard = e.FirstCard }
        | _ -> failwith "malformed event stream; expecting GameStarted"
    let continuedWith s = function
        | CardPlayed e ->
            {   Turn = s.Turn.WithPlayer e.NextPlayer
                TopCard = e.Card }
        | DirectionChanged e ->
            { s with Turn = s.Turn.WithDirection e.Direction }
        | PlayedAtWrongTurn _
        | PlayedWrongCard _ ->
            s
        | GameStarted _ -> failwith "illegal restart"
    function
    | None -> beganWith >> Some
    | Some state -> continuedWith state >> Some

////////////////////////////////////////////////////////////////////////////////
// Operations of the Game aggregate

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
                    let nextTurn = state.Turn.Reverse.Next

                    [   cardPlayed nextTurn.Player
                        DirectionChanged { GameId = c.GameId; Direction = nextTurn.Direction } ]
                | Skip _ ->
                    let nextTurn = state.Turn.Skip

                    [ cardPlayed nextTurn.Player ]
                | _ ->
                    let nextTurn = state.Turn.Next
                    [ cardPlayed nextTurn.Player ]
            | _ -> [ PlayedWrongCard { GameId = c.GameId; Player = c.Player; Card = c.Card } ]

let aggregate = evolve, None, decide
