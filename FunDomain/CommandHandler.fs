module FunDomain.CommandHandler

let createAsyncSliced<'sid,'token,'e,'s,'c,'r> (initial':'s) (evolve':'s->'e list->'s) (handle:'s->'c->('e list)) (read:'sid->int->int->Async<'e list*'token*int option>) (append:'sid->'token->'e list->Async<'r>) =
    let load stream =
        let rec fold state version = async {
            let sliceSize = 500
            let! events, sliceLastToken, nextMinEventNumber = read stream version sliceSize
            let state = events |> evolve' state 
            match nextMinEventNumber with
            | None -> return sliceLastToken, state
            | Some minStreamIndex -> return! fold state minStreamIndex }
        fold initial' 0

    fun topicId command -> async {
        let! initialVersion, state = load topicId 
        do! handle state command
            |> append topicId initialVersion
            |> Async.Ignore }