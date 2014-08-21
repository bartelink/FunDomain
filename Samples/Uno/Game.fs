module Uno.Game

type Command =
    | StartGame of StartGame
    | PlayCard of PlayCard
and StartGame = { GameId:GameId; PlayerCount:int; FirstCard:Card }
and PlayCard = { GameId:GameId; Player:int; Card:Card }

type Event =
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | PlayedAtWrongTurn of PlayedAtWrongTurn
    | PlayedWrongCard of PlayedWrongCard
    | DirectionChanged of DirectionChanged

/// Type representing current player turn; All operations should be encapsulated
type Turn = { Player: int; PlayerCount:int; Direction:Direction } with
    static member empty = { Player= 0; PlayerCount = 1; Direction = ClockWise }
    static member start player count = { Player = player; PlayerCount = count; Direction = ClockWise }

    member turn.next = 
        match turn.Direction with
        | ClockWise -> { turn with Player = (turn.Player + 1) % turn.PlayerCount }
        | CounterClockWise -> { turn with Player = (turn.Player + turn.PlayerCount - 1) % turn.PlayerCount } // the + count is here to avoid having negative result

    member turn.skip = turn.next.next

    member turn.reverse = 
        match turn.Direction with
        | ClockWise -> { turn with Direction = CounterClockWise }
        | CounterClockWise -> { turn with Direction = ClockWise }

    member turn.setPlayer player =
        if player < 0 || player >= turn.PlayerCount then 
            invalidArg "player" "The player value should be between 0 and player count"
        { turn with Player = player }

    member turn.setDirection direction =
        { turn with Turn.Direction = direction }

type State = { GameAlreadyStarted:bool; Turn:Turn; TopCard:Card } with
    static member initial = { GameAlreadyStarted = false; Turn = Turn.empty; TopCard = DigitCard( Digit 0, Red) }

// Operations of the Game aggregate

let (|Color|) = function
    | DigitCard (_,c) | KickBack c | Skip c -> c

let (|SameColor|_|) = function
    | Color c1c,Color c2c when c1c=c2c -> Some c1c
    | _ -> None

let (|SameValue|_|) = function
    | DigitCard(n1,_),DigitCard (n2,_) when n1 = n2 -> Some ()
    | KickBack _, KickBack _ -> Some ()
    | _ -> None

let startGame (command: StartGame) state =
    if command.PlayerCount <= 2 then invalidArg "playerCount" "There should be at least 3 players"
    if state.GameAlreadyStarted then invalidOp "The game cannot be started more than once"
    
    let gameStarted firstPlayer = 
        GameStarted { GameId = command.GameId; PlayerCount = command.PlayerCount; FirstCard = command.FirstCard; FirstPlayer = firstPlayer }
 
    match command.FirstCard with
    | KickBack _ ->
        [ gameStarted 0 
          DirectionChanged { GameId = command.GameId; Direction = CounterClockWise } ]
    | Skip _ -> [ gameStarted 1 ]
    | _ -> [ gameStarted 0]

let playCard (command: PlayCard) state =
    if state.Turn.Player <> command.Player then 
        [ PlayedAtWrongTurn { GameId = command.GameId; Player = command.Player; Card = command.Card } ]
    else
        match command.Card, state.TopCard with
        | SameColor _ | SameValue ->
            let cardPlayed nextPlayer = 
                CardPlayed { GameId = command.GameId; Player = command.Player; Card = command.Card; NextPlayer = nextPlayer }
              
            match command.Card with
            | KickBack _ ->
                let nextTurn = state.Turn.reverse.next

                [ cardPlayed nextTurn.Player
                  DirectionChanged { GameId = command.GameId; Direction = nextTurn.Direction } ]
            | Skip _ ->
                let nextTurn = state.Turn.skip

                [ cardPlayed nextTurn.Player ]
            | _ -> 
                let nextTurn = state.Turn.next
                [ cardPlayed nextTurn.Player ]
        | _ -> [ PlayedWrongCard { GameId = command.GameId; Player = command.Player; Card = command.Card} ] 

/// Map commands to aggregate operations
let handle = function
    | StartGame c -> startGame c
    | PlayCard c -> playCard c

type State with
    static member evolve state = function
        | GameStarted event -> 
            { GameAlreadyStarted = true
              Turn = Turn.start event.FirstPlayer event.PlayerCount 
              TopCard = event.FirstCard }
        | CardPlayed event ->
            { state with
                Turn = state.Turn.setPlayer event.NextPlayer
                TopCard = event.Card }
        | DirectionChanged event ->
            { state with 
                Turn = state.Turn.setDirection event.Direction }
        | PlayedAtWrongTurn _ 
        | PlayedWrongCard _ -> 
            state

let replay = List.fold State.evolve State.initial