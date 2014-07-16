module FunDomain.CommandHandler

let create replay handle read append =

    fun id command ->
        let initialVersion,existingEvents = read id 
        replay existingEvents
        |> handle command
        |> append id initialVersion 