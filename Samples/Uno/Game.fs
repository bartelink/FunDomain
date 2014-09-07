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
    member turn.withPlayer player = { turn with Player = player }

    member turn.next = 
        turn.withPlayer <| 
            match turn.Direction with
            | ClockWise -> (turn.Player + 1) % turn.PlayerCount 
            | CounterClockWise -> // the + count is here to avoid having negative result
                (turn.Player - 1 + turn.PlayerCount) % turn.PlayerCount 

    member turn.skip = turn.next.next
    
    member turn.setDirection direction = { turn with Turn.Direction = direction }

    member turn.reverse =
        let withDirection direction = { turn with Direction = direction }
        withDirection <|
            match turn.Direction with
            | ClockWise -> CounterClockWise
            | CounterClockWise -> ClockWise 

    member turn.setPlayer player =
        if player < 0 || player >= turn.PlayerCount then 
            invalidArg "player" "The player value should be between 0 and player count"
        turn.withPlayer player

/// State with initial / evolve as required by Evolution.replay
type PlayState = { Turn:Turn; TopCard:Card }
type State = 
    | Initial
    | Playing of PlayState with
    static member initial = Initial
    member this.evolve = 
        match this with
        | Initial -> function
            | GameStarted e -> Playing { 
                Turn = Turn.start e.FirstPlayer e.PlayerCount
                TopCard = e.FirstCard } 
            | _ -> failwith "need to start first"
        | Playing s -> function
            | CardPlayed e -> Playing { 
                Turn = s.Turn.setPlayer e.NextPlayer
                TopCard = e.Card }
            | DirectionChanged e -> Playing { 
                s with Turn = s.Turn.setDirection e.Direction }
            | PlayedAtWrongTurn _ 
            | PlayedWrongCard _ -> 
                this
            | GameStarted _ -> 
                failwith "illegal restart" 

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

let handle = function
    | Initial -> function
        | StartGame c ->
            if c.PlayerCount <= 2 then invalidArg "playerCount" "There should be at least 3 players"
    
            let gameStartedWithPlayer firstPlayer = 
                GameStarted { GameId = c.GameId; PlayerCount = c.PlayerCount; FirstCard = c.FirstCard; FirstPlayer = firstPlayer }
 
            match c.FirstCard with
            | KickBack _ ->
                [ gameStartedWithPlayer 0 
                  DirectionChanged { GameId = c.GameId; Direction = CounterClockWise } ]
            | Skip _ -> [ gameStartedWithPlayer 1 ]
            | _ -> [ gameStartedWithPlayer 0]
        | _ -> invalidOp  "The game needs to be started first"
    | Playing state -> function
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
        | StartGame _ -> invalidOp "The game cannot be started more than once"
