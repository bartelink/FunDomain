namespace FunUno

module UnoGame =
    type Color = 
        | Red
        | Green
        | Blue
        | Yellow

    type Card =
        | Digit of int * Color
        | KickBack of Color

    type Direction =
        | ClockWise
        | CounterClockWise

    type Turn = (*player*)int * (*playerCount*)int

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Turn =
        let empty = (0,1)
        let start count = (0, count)
        let next (player, count) = (player + 1) % count, count
        let isNot p (current, _) = p <> current

    type State = {
        GameAlreadyStarted: bool
        Player: Turn
        TopCard: Card }

    let empty = {
        State.GameAlreadyStarted = false
        Player = Turn.empty
        TopCard = Digit(0,Red) }

    type GameStartedEvent = {GameId: int; PlayerCount:int; FirstCard: Card}
    type CardPlayedEvent = {GameId: int; Player:int; Card: Card}
    type Event =
        | GameStarted   of GameStartedEvent
        | CardPlayed    of CardPlayedEvent

    let apply state = function
        | GameStarted {PlayerCount=playerCount; FirstCard=firstCard} -> 
            { GameAlreadyStarted = true
              Player = Turn.start playerCount
              TopCard = firstCard }
        | CardPlayed {Card=card} ->
            { state with
                Player = state.Player |> Turn.next 
                TopCard = card }

    let replay events = List.fold apply empty events

    type StartGameCommand = { GameId: int; PlayerCount: int; FirstCard: Card}
    let startGame command state =
        if command.PlayerCount <= 2 then invalidArg "playerCount" "You should be at least 3 players"
        if state.GameAlreadyStarted then invalidOp "You cannot start game twice"

        [ GameStarted {GameId=command.GameId; PlayerCount=command.PlayerCount; FirstCard=command.FirstCard} ]

    type PlayCardCommand = { GameId: int; Player: int; Card: Card}
    let playCard command (state:State) =
        if state.Player |> Turn.isNot (command.Player) then invalidOp "Player should play at his turn"

        match command.Card, state.TopCard with
        | Digit(n1, color1), Digit(n2, color2) when n1 = n2 || color1 = color2 ->
            [ CardPlayed { GameId = command.GameId; Player=command.Player; Card=command.Card }]
        | _ -> invalidOp "Play same color or same value !"

    type Command =
        | StartGame         of StartGameCommand
        | PlayCard          of PlayCardCommand

    let handle =
        function
        | StartGame(cmd)    -> startGame cmd
        | PlayCard(cmd)     -> playCard cmd