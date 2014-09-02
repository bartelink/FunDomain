namespace FunDomain

[<AutoOpen>]
module Evolution =
    // Evolves the State a single step along a trajectory using the convention of an evolve function on the State type
    let inline private evolve' state =
        let evolve state = (^S : (static member evolve: ^S option -> (^E -> ^S)) state)
        evolve state
    
    // Folds a collection of events into a state using a supplied fold function
    let inline play (fold:('s option->'e->'s option)->'s option->'es->'s option) state =
        fold (fun s e -> Some <| evolve' s e) state
        
    // Evolves State with the supplied events as the evolution parameter per step
    let inline replay events =
        play List.fold None events