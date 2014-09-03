﻿namespace FunDomain.Persistence.EventStore

open FunDomain

/// This module implements AwaitTask for non generic Task
/// <remarks>Should be unnecessary in F# 4 since as scheduled to be implemented in FSharp.Core</remarks>
[<AutoOpen>]
module private AsyncExtensions =
    open System
    open System.Threading.Tasks
    type Microsoft.FSharp.Control.Async with
        static member Raise ex = Async.FromContinuations(fun (_,econt,_) -> econt ex)

        static member AwaitTask (t:Task) =
            let tcs = new TaskCompletionSource<unit>(TaskContinuationOptions.None)
            let propagateToTcs _ =
                if t.IsFaulted      then    tcs.SetException t.Exception
                elif t.IsCanceled   then    tcs.SetCanceled ()
                else tcs.SetResult          () 
            t.ContinueWith(propagateToTcs, TaskContinuationOptions.ExecuteSynchronously) |> ignore
            async {
                try
                    do! Async.AwaitTask tcs.Task
                with
                | :? AggregateException as ex -> 
                    do! Async.Raise (ex.Flatten().InnerExceptions |> Seq.head) }

open EventStore.ClientAPI

/// Expose GES 3+'s Task Async (only) operations as F# functions yielding Async<'T>
[<AutoOpen>]
module private EventStoreExtensions =
    type EventStore.ClientAPI.IEventStoreConnection with
        member this.AsyncConnect () = 
            Async.AwaitTask( this.ConnectAsync())
        member this.AsyncAppendToStream stream expectedVersion events =
            Async.AwaitTask <| this.AppendToStreamAsync(stream, expectedVersion, events)
        member this.AsyncReadStreamEventsForward stream start count resolveLinkTos =
            Async.AwaitTask <| this.ReadStreamEventsForwardAsync(stream, start, count, resolveLinkTos)
        member this.AsyncSubscribeToAll resolveLinkTos eventAppeared credentials =
            Async.AwaitTask <| this.SubscribeToAllAsync(resolveLinkTos, System.Action<_,_>(eventAppeared), null, credentials)

/// Wrapper yielded by create* functions with create/append functions matching FunDomain.CommandHandler requirements
type Store private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an EventStore.Client package reference on all downstream users
    let inner : IEventStoreConnection = unbox inner'

    static member internal wrap connection = Store( box connection)

    member this.append streamId expectedVersion newEncodedEvents = 
        inner.AsyncAppendToStream streamId expectedVersion 
            [| for e in newEncodedEvents -> EventData(System.Guid.NewGuid(), e.EventType, (*isJson*)true, e.Data, (*metadata*)null) |]

    member this.read streamId version count = async {
        let! slice = inner.AsyncReadStreamEventsForward streamId version count (*resolveLinkTos*)true
        let nextSliceToken = if slice.IsEndOfStream then None else Some slice.NextEventNumber

        let events = seq { for e in slice.Events -> { EventType = e.Event.EventType; Data = e.Event.Data } }
        return events, slice.LastEventNumber, nextSliceToken }

    member this.subscribe (username, password) projection =
        inner.AsyncSubscribeToAll (*resolveLinkTos*)true 
            (fun _ e -> projection <| EventBatch [| { EventType = e.Event.EventType; Data = e.Event.Data } |]) 
            (SystemData.UserCredentials(username, password))

module GesGateway =
    let create tcpEndpoint = async {
        let storeConn = EventStoreConnection.Create tcpEndpoint
        do! storeConn.AsyncConnect()
        return storeConn |> Store.wrap }