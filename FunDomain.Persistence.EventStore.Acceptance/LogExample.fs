module FunDomain.Persistence.EventStore.Acceptance.LogExample

open FunDomain.Persistence.EventStore.GesAgent
open FunDomain.Persistence.Fixtures
open Swensen.Unquote
open System
open System.Net
open Xunit

type Event = 
    | Started of started : DateTime

let evolve = function
    | None -> function 
        | Started date -> Some date
    | Some s -> function 
        | Started e -> e |> failwithf "%A but got %A" s

type Command = 
    | Start of DateTime

let decide = function
    | None -> function
        | Start date -> [ Started date ]
    | Some s -> function 
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
    let handle = createCommandHandlerAgent storeEndpoint topic evolve decide
    handle.Post <| Start inputDate
    (fun () -> success =! true) |> withRetryingAndDelaying 50 100