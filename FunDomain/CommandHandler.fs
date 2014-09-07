module FunDomain.CommandHandler

let inline create//<'state,'event,'command,'token,'streamId,'r
//        when 'state : (static member evolve: 'state->('event->'state))
//        and 'state : (static member initial: 'state)>
        (handle:'state->'command->('event list))
        (read:'streamId->int->int->Async<EncodedEvent seq*'token*int option>)
        (append:'streamId->'token->EncodedEvent list->Async<'r>) =
    let load stream =
        let rec fold state version = async {
            let sliceSize = 500
            let! events, sliceLastToken, nextMinEventNumber = read stream version sliceSize
            let updatedPosition = 
                events 
                |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'event>
                |> List.ofSeq
                |> List.fold evolve' state
            match nextMinEventNumber with
            | None -> return sliceLastToken, updatedPosition
            | Some minStreamIndex -> return! fold updatedPosition minStreamIndex }
        let initialState = initial' ()
        fold initialState 0

    fun topicId command -> async {
        let! initialVersion, state = load topicId 
        do! handle state command
            |> List.map EncodedEvent.serializeUnionByCaseItemTypeName
            |> append topicId initialVersion
            |> Async.Ignore }