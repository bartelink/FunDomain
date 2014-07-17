module FunDomain.Persistence.NEventStore.NesGateway

open FunDomain.Persistence.Serialization

open NEventStore
open NEventStore.Persistence
open NEventStore.Persistence.Sql.SqlDialects

open System
open System.Collections.Generic

/// Opaque token yielded by NesStreamer.read and consumed by NesStreamer.append
type NesToken = { CommitSequence : int; StreamRevision : int}

/// Wrapper class yielded by NesStore.create* functions with create/append functions matching FunDomain.CommandHandler requirements
type NesStreamer private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an NEventStore package reference on all downstream users
    let inner : IPersistStreams = unbox inner'
    let load (bucketId,streamId) minRevision maxRevision =
        inner.GetFrom(bucketId, streamId, minRevision, maxRevision) 
    let commit = inner.Commit >> ignore
    let readStream streamId startIndex count =
        let minRevision,maxRevision = startIndex,startIndex+count-1
        async {
            let commits = 
                load streamId minRevision maxRevision
                |> Array.ofSeq
            let events =
                commits
                |> Seq.collect (fun ev -> ev.Events)
                |> Seq.map (fun em -> 
                    let caseName = em.Headers.["type"] :?> string
                    let blob = em.Body |> unbox
                    deserializeUnion caseName blob)
                |> Seq.choose id
                |> List.ofSeq
            let tokenOption = 
                if commits.Length = 0 then 
                    None 
                else 
                    let lastCommit = commits |> Seq.last
                    Some {CommitSequence=lastCommit.CommitSequence; StreamRevision=lastCommit.StreamRevision}

            return events, tokenOption, None }

    let appendToStream (bucketId,streamId) streamMeta token events =
        let commitId,commitStamp,commitHeaders = streamMeta
        async {
            let eventMessages = 
                events |> Seq.map (fun event ->
                    let caseName,blob = event |> serializeUnion
                    let headers = Dictionary<_,_>(capacity=1)
                    headers.["type"] <- box caseName
                    let body = box blob
                    EventMessage(Headers=headers, Body=body))
            let updatedStreamRevision=token |> Option.map (fun token -> token.StreamRevision+1)
            let updatedCommitSequence=token |> Option.map (fun token -> token.CommitSequence+1) 
            let attempt = 
                CommitAttempt(
                    bucketId, streamId, 
                    updatedStreamRevision |> defaultArg <| 1, 
                    commitId, 
                    updatedCommitSequence |> defaultArg <| 1, 
                    commitStamp, 
                    commitHeaders, 
                    eventMessages)
            commit attempt}

    static member internal wrap persister = NesStreamer( box persister)

    member this.read streamId = 
        let events,version,_ = 
            readStream streamId 0 Int32.MaxValue 
            |> Async.RunSynchronously
        version,events

    member this.append streamId token events = 
        let commitMetadata() =
            let commitId = Guid.NewGuid() 
            let commitDateTime = DateTime.UtcNow
            let commitHeaders = null
            commitId,commitDateTime,commitHeaders
        let metadata = commitMetadata() 
        appendToStream streamId metadata token events
        |> Async.RunSynchronously

let createFromStore (inner:IStoreEvents) = 
    inner.Advanced |> NesStreamer.wrap

let createInMemory () = 
    Wireup.Init()
        .LogToOutputWindow()
        .UsingInMemoryPersistence()
        .UsingJsonSerialization()
        .Build()
    |> createFromStore

let createInMsSqlWithPerfCounters (connectionString:string) perfCounterSetName = 
    Wireup.Init()
        .UsingSqlPersistence(connectionString)
        .WithDialect(new MsSqlDialect())
        .InitializeStorageEngine()
        .TrackPerformanceInstance(perfCounterSetName)
        .UsingJsonSerialization()
            .Compress()
        .Build()
    |> createFromStore