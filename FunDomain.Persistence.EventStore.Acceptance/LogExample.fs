module FunDomain.Persistence.EventStore.Acceptance.LogExample

open FunDomain.Persistence.EventStore.GesAgent
open FunDomain.Persistence.Fixtures
open Swensen.Unquote
open System
open System.Net
open Xunit

type Event = 
    | Started of started : DateTime

type LogState = 
    | Initial
    | Running of started : DateTime
    static member InitialState = Initial
    member this.Evolve = 
        match this with
        | Initial -> function 
            | Started date -> Running date
        | Running s -> function 
            | Started e -> e |> failwithf "%A but got %A" this

type Command = 
    | Start of DateTime

let handle = 
    function 
    | Initial as s -> function 
        | Start date -> [ Started date ]
    | Running s -> function 
        | Start date -> date |> failwithf "%A but got %A" s

[<Fact>]
let CanRountrip() = 
    let storeEndpoint = IPEndPoint(IPAddress.Loopback, 1113)
    let topic = string <| Guid.NewGuid()
    let mutable success = false
    let inputDate = DateTime.Today
    
    let dispatch = 
        function 
        | EventAppeared(Started date) -> success <- date = inputDate
    
    let _ = createEventStreamerAgent storeEndpoint topic dispatch
    let handler = createCommandHandlerAgent storeEndpoint topic handle
    handler.Post <| Start inputDate
    (fun () -> success =! true) |> withRetryingAndDelaying 50 100
