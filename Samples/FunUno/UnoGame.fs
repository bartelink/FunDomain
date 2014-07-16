namespace FunUno

module UnoGame =
    module Events =
        type Color = 
        | Red
        | Green
        | Blue
        | Yellow

        type Card =
        | Digit of int * Color

        type GameStartedEvent = {GameId: int; PlayerCount:int; TopCard: Card}
        type CardPlayedEvent = {GameId: int; Player:int; Card: Card}

    type Event =
    | GameStarted   of Events.GameStartedEvent
    | CardPlayed    of Events.CardPlayedEvent

    type Turn = { Player:int; Of:int } with
        member this.next () = {this with Player = (this.Player + 1) % this.Of}
        member this.isNot player = this.Player <> player 
        static member empty = {Player=0; Of=0}
        static member start count = {Player=0; Of=count}

    type State = {
        GameInProgress: bool
        Turn: Turn
        TopCard: Events.Card }

    let replay = 
        let empty = {
            State.GameInProgress = false
            Turn = Turn.empty
            TopCard = Events.Digit(0, Events.Red) }

        let apply state = function
            | GameStarted {PlayerCount=playerCount; TopCard=topCard} -> 
                { GameInProgress = true
                  Turn = Turn.start playerCount
                  TopCard = topCard }
            | CardPlayed {Card=card} ->
                { state with
                    Turn = state.Turn.next()
                    TopCard = card }

        List.fold apply empty

    module Commands =
        type StartGameCommand   = { GameId: int; PlayerCount: int; TopCard: Events.Card} 
        type PlayCardCommand    = { GameId: int; Player: int; Card: Events.Card}

    type Command =
    | StartGame of Commands.StartGameCommand
    | PlayCard  of Commands.PlayCardCommand

    let handle command state = 
        match command with
        | StartGame { GameId=gameId; PlayerCount=players; TopCard=top}  -> 
            if players <= 2 then invalidArg "playerCount" "You should be at least 3 players"
            if state.GameInProgress then invalidOp "You cannot start game twice"

            [ GameStarted {GameId=gameId; PlayerCount=players; TopCard=top} ]

        | PlayCard { GameId=gameId; Player=player; Card=card} ->
            if state.Turn.isNot player then invalidOp "Player should play at his turn"

            match card, state.TopCard with
            | Events.Digit(n1, color1), Events.Digit(n2, color2) when n1 = n2 || color1 = color2 ->
                [ CardPlayed { GameId = gameId; Player=player; Card=card }]
            | _ -> invalidOp "Play same color or same value !"