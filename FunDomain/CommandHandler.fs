module FunDomain.CommandHandler

let create<'sid,'token,'e,'s,'c,'r> (initial:unit->'s) (evolve':'s->'e->'s) (handle:'s->'c->('e list)) (read:'sid->int->int->Async<StorableEvent seq*'token*int option>) (append:'sid->'token->StorableEvent list->Async<'r>) =
    let load stream =
        let rec fold state version = async {
            let sliceSize = 500
            let! events, sliceLastToken, nextMinEventNumber = read stream version sliceSize
            let newState = 
                events 
                |> EventBatch.toUnionList<'e>
                |> List.fold evolve' state
            match nextMinEventNumber with
            | None -> return sliceLastToken, newState
            | Some minStreamIndex -> return! fold newState minStreamIndex }
        fold (initial()) 0

    fun topicId command -> async {
        let! initialVersion, state = load topicId 
        do! handle state command
            |> EventBatch.fromUnionList
            |> append topicId initialVersion
            |> Async.Ignore }