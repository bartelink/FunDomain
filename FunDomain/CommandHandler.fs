module FunDomain.CommandHandler

open Dynamical

let inline load 
        (read : 'streamId -> int -> int -> Async<EncodedEvent seq * 'token * int option>) 
        topicId : Async<'token*'state> = 
    let rec fold version (currentState:'state) = 
        async { 
            let sliceSize = 500
            let! encodedEvents, token, nextVersion = read topicId version sliceSize
            let updatedState = 
                encodedEvents
                |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'event>
                |> Evolution.advance currentState
            match nextVersion with
            | Some minVersion -> return! fold minVersion updatedState
            | None -> return token, updatedState }
    fold 0 (Evolution.initialState())

let inline save
        (append : 'streamId -> 'token -> EncodedEvent seq -> Async<'token>) =
    fun streamId token ->
        List.map EncodedEvent.serializeUnionByCaseItemTypeName 
        >> append streamId token

let inline create
        (handle:'state -> 'command -> 'event list)
        (read:'streamId -> int -> int -> Async<EncodedEvent seq*'token*int option>)
        (append:'streamId -> 'token -> EncodedEvent seq -> Async<'token>) =
    fun streamId command ->
        async {
            let! token, initialState = load read streamId 
            do! handle initialState command
                |> save append streamId token 
                |> Async.Ignore }