namespace FunDomain.Persistence.EventStore

open FunDomain
open System
open System.Net
open EventStore.ClientAPI

// http://stackoverflow.com/a/23444419/11635
module Async = 
    let AwaitTaskVoid : Threading.Tasks.Task -> Async<unit> = Async.AwaitIAsyncResult >> Async.Ignore

/// Expose GES 3+'s Task Async (only) operations as F# functions yielding Async<'T>
[<AutoOpen>]
module private EventStoreExtensions = 
    type EventStore.ClientAPI.IEventStoreConnection with
        member this.AsyncConnect() = Async.AwaitTaskVoid <| this.ConnectAsync()
        member this.AsyncAppendToStream stream expectedVersion events = 
            Async.AwaitTask <| this.AppendToStreamAsync(stream, expectedVersion, events)
        member this.AsyncReadStreamEventsForward stream start count resolveLinkTos = 
            Async.AwaitTask <| this.ReadStreamEventsForwardAsync(stream, start, count, resolveLinkTos)
        member this.AsyncSubscribeToAll resolveLinkTos eventAppeared credentials = 
            Async.AwaitTask <| this.SubscribeToAllAsync(resolveLinkTos, Action<_, _>(eventAppeared), null, credentials)
        member this.AsyncSubscribeToStream stream resolveLinkTos onEvent onDrop = 
            Async.AwaitTask 
            <| this.SubscribeToStreamAsync(stream, resolveLinkTos, Action<_, _>(onEvent), Action<_, _, _>(onDrop), null)

// TODO port https://github.com/LogosBible/Logos.Utility/blob/master/src/Logos.Utility/GuidUtility.cs
module DetermisticGuid = 
    let ofBytes (bytes : byte []) = 
        use provider = new System.Security.Cryptography.MD5CryptoServiceProvider()
        provider.ComputeHash bytes |> Guid

/// Wrapper yielded by create* functions with create/append functions matching FunDomain.CommandHandler requirements
[<Sealed>]
type Store private (inner') = 
    // Hoop jumping a la C++ pimpl pattern to avoid foisting an EventStore.Client package reference on consumers
    let inner : IEventStoreConnection = unbox inner'
    
    let toEncodedEvent (e : ResolvedEvent) = 
        { EventType = e.Event.EventType
          Data = e.Event.Data }
    
    let batchEvent e = EventBatch [| toEncodedEvent e |]
    let dispatchTo projection = (fun _ e -> projection <| batchEvent e)
    let dispatchWithStreamIdTo projection = (fun s e -> projection s <| batchEvent e)
    
    let appendWithIdGenerator generator streamId expectedVersion newEncodedEvents = 
        async { 
            let isJson, metadata = true, null
            let! wr = [| for e in newEncodedEvents -> EventData(generator e.Data, e.EventType, isJson, e.Data, metadata) |]
                      |> inner.AsyncAppendToStream streamId expectedVersion
            return wr.NextExpectedVersion
        }
    
    member this.Dispose() = (this :> IDisposable).Dispose()
    
    member this.Dispose disposing = 
        if disposing then inner.Dispose()
    
    interface IDisposable with
        member this.Dispose() = 
            this.Dispose true
            GC.SuppressFinalize this
    
    static member internal wrap connection = new Store(box connection)
    
    member this.append = 
        let ignoreTheData _ = Guid.NewGuid()
        appendWithIdGenerator ignoreTheData
    
    member this.appendIdempotent = appendWithIdGenerator
    
    member this.read streamId version count = 
        async { 
            let! slice = inner.AsyncReadStreamEventsForward streamId version count (*resolveLinkTos*) true
            let nextSliceToken = 
                if slice.IsEndOfStream then None
                else Some slice.NextEventNumber
            
            let events = slice.Events |> Seq.map toEncodedEvent
            return events, slice.LastEventNumber, nextSliceToken
        }
    
    member this.subscribeAll (username, password) projection = 
        inner.AsyncSubscribeToAll (*resolveLinkTos*) true (dispatchWithStreamIdTo projection) 
            (SystemData.UserCredentials(username, password))
    member this.subscribeStream stream projection = 
        // 'inspired by' https://gist.github.com/eulerfx/a4a29502f673f13b6a23
        let inline onDrop (subs : EventStoreSubscription) (reason : SubscriptionDropReason) (ex : exn) = 
            printfn "SUBSCRIPTION DROPPED! last position=%O reason=%O ex=%O" subs.LastEventNumber reason ex
        inner.AsyncSubscribeToStream stream false (dispatchTo projection) onDrop

module GesGateway = 
    let create (tcpEndpoint : IPEndPoint) = 
        async { 
            let storeConn = EventStoreConnection.Create tcpEndpoint
            do! storeConn.AsyncConnect()
            return storeConn |> Store.wrap
        }
