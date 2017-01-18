module FunDomain.CommandHandler

// Reading

type StreamReader<'token> = int -> int -> Async<EncodedEvent seq * 'token * int option>

let inline load
        (read               : StreamReader<'token>)
        (evolve             : 'state -> 'event -> 'state)
        (initialState       : 'state) : Async<'token*'state> =
    let rec fold version currentState = 
        async { 
            let sliceSize = 500
            let! encodedEvents, token, nextVersion = read version sliceSize
            let updatedState = 
                encodedEvents
                |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'event>
                |> Seq.fold evolve currentState
            match nextVersion with
            | Some minVersion -> return! fold minVersion updatedState
            | None -> return token, updatedState }
    fold 0 initialState

// Appending

type StreamAppender<'token> = 'token -> EncodedEvent seq -> Async<'token>

let inline save (append : StreamAppender<'token>) (token : 'token) =
    List.map EncodedEvent.serializeUnionByCaseItemTypeName
    >> append token

// Deciding

type Streamer<'token> =
    {   read                : StreamReader<'token>
        append              : StreamAppender<'token> }

let inline create
        { read = read; append = append }
        (evolve         : 'state option -> 'event -> 'state option)
        (decide         : 'state option -> 'command -> 'event list)
        command = async {
    let! token, initialState = load read evolve None
    let decision = decide initialState command
    do! save append token decision |> Async.Ignore }