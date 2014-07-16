module FunDomain.Persistence.NesStore

open FunDomain.Persistence.Serialization

open NEventStore
open NEventStore.Persistence
open NEventStore.Persistence.Sql.SqlDialects

open System
open System.Collections.Generic

type FunStreamer private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an NEventStore package reference on all downstream users
    let inner : IStoreEvents = unbox inner'
    let get (bucketId,streamId) minRevision maxRevision =
        inner.Advanced.GetFrom(bucketId, streamId, minRevision, maxRevision) 
    let commit = inner.Advanced.Commit >> ignore

    let readStream streamId startIndex count =
        let minRevision,maxRevision = startIndex,startIndex+count-1
        async {
            let commits = 
                get streamId minRevision maxRevision
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
            let version = 
                if commits.Length <> 0 then
                    let lastCommit = commits |> Seq.last
                    lastCommit.CommitSequence,lastCommit.StreamRevision
                else
                    1,1 // TODO verify

            return events, version, None }

    let appendToStream (bucketId,streamId) streamMeta expectedVersion events =
        let commitId,commitStamp,commitHeaders = streamMeta
        let commitSequence,streamRevision = expectedVersion
        async {
            let eventMessages = 
                events |> Seq.map (fun event ->
                    let caseName,blob = event |> serializeUnion
                    let headers = Dictionary<_,_>(capacity=1)
                    headers.["type"] <- box caseName
                    let body = box blob
                    EventMessage(Headers=headers, Body=body))
            let attempt = CommitAttempt(bucketId,streamId,streamRevision+1,commitId,commitSequence+1,commitStamp,commitHeaders,eventMessages)
            commit attempt}

    static member internal wrap inner = FunStreamer( box inner)

    member this.read streamId = 
        let events,version,_ = 
            readStream streamId 0 Int32.MaxValue 
            |> Async.RunSynchronously
        version,events

    member this.append streamId expectedVersion events = 
        let commitMetadata() =
            let commitId = Guid.NewGuid() 
            let commitDateTime = DateTime.UtcNow
            let commitHeaders = null
            commitId,commitDateTime,commitHeaders
        let metadata = commitMetadata() 
        appendToStream streamId metadata expectedVersion events
        |> Async.RunSynchronously

let createInMemory () = 
    FunStreamer.wrap <| Wireup.Init()
        .LogToOutputWindow()
        .UsingInMemoryPersistence()
        .UsingJsonSerialization()
        .Build()

let createInMsSqlWithPerfCounters (connectionString:string) perfCounterSetName = 
    FunStreamer.wrap <| Wireup.Init()
        .UsingSqlPersistence(connectionString)
        .WithDialect(new MsSqlDialect())
        .InitializeStorageEngine()
        .TrackPerformanceInstance(perfCounterSetName)
        .UsingJsonSerialization()
            .Compress()
        .Build()