module FunDomain.CommandHandler

let create replay handle read append =

    fun id command ->
        let initialVersion,existingEvents = read id 
        let state = replay existingEvents
        
        handle state command
        |> append id initialVersion 

let createAsyncSliced<'e,'s,'c,'r> (initial':'s) (evolve':'s->'e list->'s) (handle:'s->'c->('e list)) (read:string->int->int->Async<'e list*int*int option>) (append:string->int->'e list->Async<'r>) =
    let load stream =
        let rec fold state version = async {
            let! events, lastEvent, nextEvent = read stream version 500
            let state = events |> evolve' state 
            match nextEvent with
            | None -> return lastEvent, state
            | Some n -> return! fold state n }
        fold initial' 0

    fun stream command -> async {
        let! initialVersion, state = load stream 
        do! handle state command
            |> append stream initialVersion
            |> Async.Ignore }