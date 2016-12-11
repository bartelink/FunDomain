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
let initialState = Initial
let evolve = function
    | Initial -> function 
        | Started date -> Running date
    | (Running s) as self -> function 
        | Started e -> e |> failwithf "%A but got %A" self

type Command = 
    | Start of DateTime

let decide = function
    | Initial -> function
        | Start date -> [ Started date ]
    | Running s -> function 
        | Start date -> date |> failwithf "%A but got %A" s

let aggregate = evolve, initialState, decide

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
    let handle = createCommandHandlerAgent storeEndpoint topic aggregate
    handle.Post <| Start inputDate
    (fun () -> success =! true) |> withRetryingAndDelaying 50 100