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
type Turn = { Player:int; PlayerCount:int; Direction:Direction } with
    static member start player count = { Player = player; PlayerCount = count; Direction = ClockWise }
    static member empty = Turn.start 0 1

    member turn.next = 
        match turn.Direction with
        | ClockWise -> { turn with Player = (turn.Player + 1) % turn.PlayerCount }
        | CounterClockWise -> { turn with Player = (turn.Player + turn.PlayerCount - 1) % turn.PlayerCount } // the + count is here to avoid having negative result

    member turn.skip = turn.next.next
    
    member turn.setDirection direction = { turn with Turn.Direction = direction }

    member turn.reverse = 
        match turn.Direction with
        | ClockWise -> { turn with Direction = CounterClockWise }
        | CounterClockWise -> { turn with Direction = ClockWise }

    member turn.setPlayer player =
        if player < 0 || player >= turn.PlayerCount then 
            invalidArg "player" "The player value should be between 0 and player count"
        { turn with Player = player }

/// State + evolve function used by handle
type State = { GameAlreadyStarted:bool; Turn:Turn; TopCard:Card } with
    static member initial = { GameAlreadyStarted = false; Turn = Turn.empty; TopCard = DigitCard( Digit 0, Red) }

let evolve state = function
    | GameStarted e -> 
        { GameAlreadyStarted = true
          Turn = Turn.start e.FirstPlayer e.PlayerCount 
          TopCard = e.FirstCard }
    | CardPlayed e ->
        { state with
            Turn = state.Turn.setPlayer e.NextPlayer
            TopCard = e.Card }
    | DirectionChanged e ->
        { state with 
            Turn = state.Turn.setDirection e.Direction }
    | PlayedAtWrongTurn _ 
    | PlayedWrongCard _ -> 
        state

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

let handle state = function
    | StartGame c ->
        if c.PlayerCount <= 2 then invalidArg "playerCount" "There should be at least 3 players"
        if state.GameAlreadyStarted then invalidOp "The game cannot be started more than once"
    
        let gameStarted firstPlayer = 
            GameStarted { GameId = c.GameId; PlayerCount = c.PlayerCount; FirstCard = c.FirstCard; FirstPlayer = firstPlayer }
 
        match c.FirstCard with
        | KickBack _ ->
            [ gameStarted 0 
              DirectionChanged { GameId = c.GameId; Direction = CounterClockWise } ]
        | Skip _ -> [ gameStarted 1 ]
        | _ -> [ gameStarted 0]
    | PlayCard c ->
        if state.Turn.Player <> c.Player then 
            [ PlayedAtWrongTurn { GameId = c.GameId; Player = c.Player; Card = c.Card } ]
        else
            match c.Card, state.TopCard with
            | SameColor _ 
            | SameValue ->
                let cardPlayed nextPlayer = 
                    CardPlayed { GameId = c.GameId; Player = c.Player; Card = c.Card; NextPlayer = nextPlayer }
              
                match c.Card with
                | KickBack _ ->
                    let nextTurn = state.Turn.reverse.next

                    [ cardPlayed nextTurn.Player
                      DirectionChanged { GameId = c.GameId; Direction = nextTurn.Direction } ]
                | Skip _ ->
                    let nextTurn = state.Turn.skip

                    [ cardPlayed nextTurn.Player ]
                | _ -> 
                    let nextTurn = state.Turn.next
                    [ cardPlayed nextTurn.Player ]
            | _ -> [ PlayedWrongCard { GameId = c.GameId; Player = c.Player; Card = c.Card } ] 

let replay = List.fold evolve State.initial