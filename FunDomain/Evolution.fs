namespace Dynamical

/// See http://thinkbeforecoding.github.io/FsUno.Prod/Dynamical%20Systems.html
module Evolution =
    // Obtains the initial state of the trajectory for a State build-up
    // requires an 'initial' static member function on the State type
    let inline initialState (): ^state =
        (^state: (static member InitialState: ^state) ()) 

    // Evolves the State one step with 'event' as the 'evolution parameter'
    // requires an 'evolve' member function on the State type
    let inline step (state:^state) (event:^event) =
        (^state: (member Evolve: (^event-> ^state)) state) event

    // Advance the state multiple 'step's with the supplied events as the trajectory's evolution parameters
    let inline advance (state:'state) = 
        Seq.fold step state

    // Builds State with elements of 'events' as the trajectory's evolution parameters
    // requires 'Initial' static member function and 'Evolve' member function on the State type
    let inline replay events =
        let initialState = initialState()
        advance initialState events
