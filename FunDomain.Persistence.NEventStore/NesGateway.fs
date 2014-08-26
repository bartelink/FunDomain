module FunDomain.Persistence.NEventStore.NesGateway

open FunDomain.Persistence.Serialization

open NEventStore
open NEventStore.Persistence
open NEventStore.Persistence.Sql.SqlDialects

open System
open System.Collections.Generic

/// Opaque token yielded by Streamer.read and consumed by Streamer.append
type Token = { CommitSequence:int; StreamRevision:int }

/// Identifier of a stream in NEventStore
type StreamId = { Bucket:string option; StreamId:string }

type CheckpointToken = { Token:string option } with 
    static member initial = { Token = None }

type EncodedEventBatch (events) =
    let cached = Seq.cache events
    member this.chooseOfUnion<'a> () =
        cached |> Seq.choose (fun (ee:EncodedEvent) -> ee.deserializeUnionByCaseItemType<'a> ())

/// Wrapper yielded by create* functions with create/append functions matching FunDomain.CommandHandler requirements
type Streamer private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an NEventStore package reference on all downstream users
    let inner : IPersistStreams = unbox inner'
    let defaultBucket bucketId = defaultArg bucketId "default"
    let load { Bucket=bucketId; StreamId=streamId } minRevision maxRevision =
        inner.GetFrom(bucketId |> defaultBucket, streamId, minRevision, maxRevision) 
    let poll { Token=token } =
        inner.GetFrom( defaultArg token null)

    let commit = inner.Commit >> ignore

    let readStream streamId startIndex count =
        let minRevision,maxRevision = startIndex,startIndex+count-1
        async {
            let commits = load streamId minRevision maxRevision |> Array.ofSeq
            let tokenOption = 
                if commits.Length = 0 then 
                    None 
                else 
                    let lastCommit = commits |> Seq.last
                    Some {CommitSequence=lastCommit.CommitSequence; StreamRevision=lastCommit.StreamRevision}

            return commits,tokenOption,None }

    let generateEventMessage (encoded:EncodedEvent) =
        let headers = Dictionary<_,_>(capacity=1)
        headers.["type"] <- box encoded.EventTypeName
        let body = box encoded.Encoded
        EventMessage(Headers=headers, Body=body)

    let extractEncodedEvents (commit:ICommit) : EncodedEvent seq =
        let extractEncoded (em:EventMessage) = { EventTypeName = em.Headers.["type"] :?> string; Encoded = em.Body |> unbox }
        commit.Events |> Seq.map extractEncoded 

    let appendToStream {Bucket=bucketId; StreamId=streamId} streamMeta token encodedEvents =
        let commitId,commitStamp,commitHeaders = streamMeta
        async {
            let eventMessages = encodedEvents |> Seq.map generateEventMessage
            let updatedStreamRevision=token |> Option.map (fun token -> token.StreamRevision+1)
            let updatedCommitSequence=token |> Option.map (fun token -> token.CommitSequence+1) 
            let attempt = 
                CommitAttempt(
                    bucketId |> defaultBucket, streamId, 
                    updatedStreamRevision |> defaultArg <| 1, 
                    commitId, 
                    updatedCommitSequence |> defaultArg <| 1, 
                    commitStamp, 
                    commitHeaders, 
                    eventMessages)
            commit attempt}

    let fetch token = 
        poll token
        |> Seq.map (fun commit -> 
            let token = { Token = Some commit.CheckpointToken }
            let encodedEvents = commit |> extractEncodedEvents
            token,EncodedEventBatch(encodedEvents))

    member this.project projection checkpointToken =
        let batch = fetch checkpointToken
        let dispatchElements _ (checkpoint,elements) =
            elements |> projection 
            Some checkpoint
        batch |> Seq.fold dispatchElements None

    static member internal wrap persister = Streamer( box persister)

    member this.read<'a> stream = 
        let commits,version,_ = 
            readStream stream 0 Int32.MaxValue 
            |> Async.RunSynchronously
        let encodedEvents = commits |> Seq.collect extractEncodedEvents
        let inUnion = EncodedEventBatch(encodedEvents).chooseOfUnion<'a> () |> Seq.toList
        version,inUnion

    member this.append stream token events = 
        let commitMetadata() =
            let commitId = Guid.NewGuid() 
            let commitDateTime = DateTime.UtcNow
            let commitHeaders = null
            commitId,commitDateTime,commitHeaders
        let metadata = commitMetadata() 
        let encodedEvents = events |> Seq.map EncodedEvent.serializeUnionByCaseItemType 
        appendToStream stream metadata token encodedEvents
        |> Async.RunSynchronously

let createFromStore (inner:IStoreEvents) = 
    inner.Advanced |> Streamer.wrap

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