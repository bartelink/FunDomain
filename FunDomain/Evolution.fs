namespace FunDomain

[<AutoOpen>]
module Evolution =
    // A generic replay function that can be used on any aggregate
    let inline replay events =
        let initial = (^S: (static member initial: ^S) ()) 
        let evolve s = (^S: (static member evolve: ^S -> (^E -> ^S)) s)
        List.fold evolve initial events