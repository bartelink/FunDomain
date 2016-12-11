module FunDomain.CommandHandler

// Reading

type StreamReader<'streamId,'token> = 'streamId -> int -> int -> Async<EncodedEvent seq * 'token * int option>

let inline load 
        (read               : StreamReader<'streamId, 'token>)
        (evolve             : 'state -> 'event -> 'state)
        (initialState       : 'state)
        topicId             : Async<'token*'state> = 
    let rec fold version currentState = 
        async { 
            let sliceSize = 500
            let! encodedEvents, token, nextVersion = read topicId version sliceSize
            let updatedState = 
                encodedEvents
                |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'event>
                |> Seq.fold evolve currentState
            match nextVersion with
            | Some minVersion -> return! fold minVersion updatedState
            | None -> return token, updatedState }
    fold 0 initialState

// Appending

type StreamAppender<'streamId,'token> = 'streamId -> 'token -> EncodedEvent seq -> Async<'token>

let inline save (append : StreamAppender<'streamId,'token>) (streamId : 'streamId) (token : 'token) =
    List.map EncodedEvent.serializeUnionByCaseItemTypeName
    >> append streamId token

// Deciding

type Streamer<'streamId,'token> =
    {   read                : StreamReader<'streamId,'token>
        append              : StreamAppender<'streamId,'token> }

let inline create
        { read = read; append = append }
        (   (evolve         : 'state -> 'event -> 'state),
            (initialState   : 'state),
            (decide         : 'state -> 'command -> 'event list))
        streamId command = async {
    let! token, initialState = load read evolve initialState streamId
    let decision = decide initialState command
    do! save append streamId token decision |> Async.Ignore }