module FunUno.CommandHandler

let create readStream appendToStream =

    let load id apply empty =
        let rec fold state version =
            let events, lastEvent, nextEvent = readStream id version 500
            let state = List.fold apply state events
            match nextEvent with
            | None -> lastEvent, state
            | Some n -> fold state n
        fold empty 0

    let inline (/|>) (v,s) f = v, f s    

    fun command id handle apply empty ->
        load id apply empty
        /|> handle command
        ||> appendToStream id