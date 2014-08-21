module FunDomain.CommandHandler

let create replay handle read append =

    fun id command ->
        let initialVersion,existingEvents = read id 
        let state = replay existingEvents
        
        handle state command
        |> append id initialVersion 