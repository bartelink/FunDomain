namespace FunDomain.Persistence.EventStore

/// This module implements AwaitTask for non generic Task
/// <remarks>Should be unnecessary in F# 4 since as scheduled to be implemented in FSharp.Core</remarks>
[<AutoOpen>]
module AsyncExtensions =
    open System
    open System.Threading.Tasks
    type Microsoft.FSharp.Control.Async with
        static member Raise(ex) = Async.FromContinuations(fun (_,econt,_) -> econt ex)

        static member AwaitTask (t:Task) =
            let tcs = new TaskCompletionSource<unit>(TaskContinuationOptions.None)
            t.ContinueWith((fun _ -> 
                if t.IsFaulted then tcs.SetException t.Exception
                elif t.IsCanceled then tcs.SetCanceled ()
                else tcs.SetResult () ), TaskContinuationOptions.ExecuteSynchronously) |> ignore
            async {
                try
                    do! Async.AwaitTask tcs.Task
                with
                | :? AggregateException as ex -> 
                    do! Async.Raise (ex.Flatten().InnerExceptions |> Seq.head) }

open EventStore.ClientAPI

/// Expose GES 3+'s Task Async (only) operations as F# functions yielding Async<'T>
[<AutoOpen>]
module EventStoreExtensions =
    type EventStore.ClientAPI.IEventStoreConnection with
        member this.AsyncConnect () = 
            Async.AwaitTask( this.ConnectAsync())
        member this.AsyncAppendToStream stream expectedVersion events =
            Async.AwaitTask <| this.AppendToStreamAsync(stream, expectedVersion, events)
        member this.AsyncReadStreamEventsForward stream start count resolveLinkTos =
            Async.AwaitTask <| this.ReadStreamEventsForwardAsync(stream, start, count, resolveLinkTos)
        member this.AsyncSubscribeToAll resolveLinkTos eventAppeared credentials =
            Async.AwaitTask <| this.SubscribeToAllAsync(resolveLinkTos, System.Action<_,_>(eventAppeared), null, credentials)

open FunDomain.Persistence.Serialization

/// Wrapper yielded by create* functions with create/append functions matching FunDomain.CommandHandler requirements
type Store private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an EventStore.Client package reference on all downstream users
    let inner : IEventStoreConnection = unbox inner'

    let serialize event = 
        let encoded = EncodedEvent.serializeUnionByCaseItemType event
        EventData(System.Guid.NewGuid(), encoded.EventType, (*isJson*)true, encoded.Data, (*metadata*)null)

    let deserialize (event:ResolvedEvent) = 
        let encoded = { EventType = event.Event.EventType; Data = event.Event.Data }
        encoded.deserializeUnionByCaseItemType ()

    static member internal wrap connection = Store( box connection)

    member this.append streamId expectedVersion newEvents = async {
        let serializedEvents = newEvents |> Seq.map serialize |> Array.ofSeq
        return! inner.AsyncAppendToStream streamId expectedVersion serializedEvents }

    member this.read streamId version count = async {
        let! slice = inner.AsyncReadStreamEventsForward streamId version count (*resolveLinkTos*)true
        let nextSliceToken = if slice.IsEndOfStream then None else Some slice.NextEventNumber

        let events = 
            slice.Events 
            |> Seq.choose deserialize
            |> Seq.toList
        
        return events, slice.LastEventNumber, nextSliceToken }

    member this.subscribe<'a> (username,password) (projection:'a -> unit) =
        inner.AsyncSubscribeToAll 
            (*resolveLinkTos*)true 
            (fun _ e -> e |> deserialize |> Option.iter projection) 
            (SystemData.UserCredentials(username, password))

module GesGateway =
    let create tcpEndpoint = async {
        let storeConn = EventStoreConnection.Create tcpEndpoint
        do! storeConn.AsyncConnect()
        return storeConn |> Store.wrap }