module FunDomain.CommandHandler

let inline create
        (handle:'state->'command->('event list))
        (read:'streamId->int->int->Async<EncodedEvent seq*'token*int option>)
        (append:'streamId->'token->EncodedEvent list->Async<'r>) =
    let load stream =
        let rec fold state version = async {
            let sliceSize = 500
            let! events, token, nextVersion = read stream version sliceSize
            let updatedPosition = 
                events 
                |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'event>
                |> List.ofSeq
                |> List.fold evolve' state
            match nextVersion with
            | None -> return token, updatedPosition
            | Some minVersion -> return! fold updatedPosition minVersion }
        let initialState = initial' ()
        fold initialState 0

    fun topicId command -> async {
        let! initialVersion, state = load topicId 
        do! handle state command
            |> List.map EncodedEvent.serializeUnionByCaseItemTypeName
            |> append topicId initialVersion
            |> Async.Ignore }