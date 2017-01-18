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
// Inferences per Event

type Inference =
    | Started of firstPlayer:int * playerCount:int * card:Card
    | Played of Card
    | NextIs of int
    | DirectionBecomes of Direction

let infer : Event -> Inference list = function
    | GameStarted e ->      [Started (e.FirstPlayer, e.PlayerCount, e.FirstCard)]
    | CardPlayed e ->       [Played e.Card; NextIs e.NextPlayer]
    | PlayedAtWrongTurn _
    | PlayedWrongCard _ ->  []
    | DirectionChanged e -> [DirectionBecomes e.Direction]

////////////////////////////////////////////////////////////////////////////////
// State / Evolution function for folding inference into same

type Turn =
    {   Player : int
        PlayerCount : int
        Direction : Direction }

type State =
    {   Turn : Turn
        TopCard : Card }

let evolve': State option -> Inference -> State option = function
    | None ->
        function
        | Started (firstPlayer=firstPlayer; playerCount=playerCount; card=card) ->
            {   TopCard = card
                Turn = {    Turn.Player = firstPlayer
                            PlayerCount = playerCount
                            Direction = ClockWise } }
        | _ -> failwith "malformed event stream; expecting Start"
        >> Some
    | Some ({Turn = t} as s) ->
        function
        | Started _ -> failwith "illegal restart"
        | Played c ->
            { s with TopCard = c }
        | DirectionBecomes d ->
            { s with Turn = { t with Direction = d } }
        | NextIs p ->
            if p < 0 || p >= t.PlayerCount then
                invalidArg "player" "The player value should be between 0 and player count"
            { s with Turn = { t with Player = p } }
        >> Some
let evolve s = infer >> Seq.fold evolve' s

////////////////////////////////////////////////////////////////////////////////
// Decision type and decision function

type Context = Context of game:GameId * player:int
type Decision =
    | StartForward of ctx: Context * card: Card * playerCount:int
    | StartBackward of ctx: Context * card: Card * playerCount:int
    | Play of Context * card:Card * nextTurn: Turn
    | PlayAndReverse of Context * card:Card * nextTurn: Turn
    | WrongCard of Context * card: Card
    | OutOfTurn of Context * card: Card

let interpret: Decision -> Event list = function
    | StartForward (Context (game, player), card, playerCount) ->
        [   GameStarted { GameId = game; FirstPlayer = player; PlayerCount = playerCount; FirstCard = card } ]
    | StartBackward (Context (game, player), card, playerCount) ->
        [   GameStarted { GameId = game; FirstPlayer = player; PlayerCount = playerCount; FirstCard = card }
            DirectionChanged { GameId = game; Direction = Direction.CounterClockWise } ]
    | Play (Context (game, player), card, nextTurn) ->
        [   CardPlayed { GameId = game; Player = player; Card = card; NextPlayer = nextTurn.Player } ]
    | PlayAndReverse (Context (game, player), card, nextTurn) ->
        [   CardPlayed { GameId = game; Player = player; Card = card; NextPlayer = nextTurn.Player }
            DirectionChanged { GameId = game; Direction = nextTurn.Direction } ]
    | WrongCard (Context (game, player), card) ->
        [   PlayedWrongCard { GameId = game; Player = player; Card = card } ]
    | OutOfTurn (Context (game, player), card) ->
        [   PlayedAtWrongTurn { GameId = game; Player = player; Card = card } ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Turn =
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

type Command =
    | StartGame of StartGame
    | PlayCard of PlayCard

let decide' : State option -> Command -> Decision = function
    | None -> function
        | StartGame c ->
            if c.PlayerCount <= 2 then invalidArg "playerCount" "There should be at least 3 players"
            let ctx nextPlayer = Context (c.GameId, nextPlayer)
            match c.FirstCard with
            | KickBack _ ->     StartBackward (ctx 0, c.FirstCard, c.PlayerCount)
            | Skip _ ->         StartForward (ctx 1, c.FirstCard, c.PlayerCount)
            | _ ->              StartForward (ctx 0, c.FirstCard, c.PlayerCount)
        | _ -> invalidOp "The game needs to be started first"
    | Some state -> function
        | StartGame _ -> invalidOp "The game cannot be started more than once"
        | PlayCard c when state.Turn.Player <> c.Player ->
            OutOfTurn (Context (c.GameId, c.Player), c.Card)
        | PlayCard c ->
            let ctx = Context (c.GameId, c.Player)
            match c.Card, state.TopCard with
            | SameColor _ 
            | SameValue ->
                match c.Card with
                | KickBack _ -> PlayAndReverse (ctx, c.Card, state.Turn |> Turn.reverse |> Turn.next)
                | Skip _ ->     Play (ctx, c.Card, state.Turn |> Turn.skip)
                | _ ->          Play (ctx, c.Card, state.Turn |> Turn.next)
            | _ ->              WrongCard (ctx, c.Card)
let decide s = decide' s >> interpret