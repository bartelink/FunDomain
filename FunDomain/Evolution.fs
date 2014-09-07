namespace FunDomain

[<AutoOpen>]
module Evolution =
    // Obtains the initial state of the trajectory for a State build-up
    // requires an 'initial' static member function on the State type
    let inline initial' (): ^state =
        (^state: (static member initial: ^state) ()) 

    // Evolves the State one step with 'event' as the 'evolution parameter'
    // requires an 'evolve' static member function on the State type
    let inline evolve' (state:^state) (event:'event) =
        (^state: (member evolve: (^event-> ^state)) state) event

    // Builds State with nodes of 'events' as trajectory's evolution parameters
    // requires 'inital' and 'evolve' static member function on the State type
    let inline replay events =
        let initialState = initial'()
        List.fold evolve' initialState events