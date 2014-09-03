namespace FunDomain.Persistence.NEventStore

open FunDomain

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

// NB Tuple signature used here is shared with CommandHandler (and GesGateway)
[<AutoOpen>]
module private EncodedEventMapping =
    let toEventMessage { EventType = eventType; Data = data } =
        let headers = Dictionary<_,_>(capacity=1)
        headers.["type"] <- box eventType
        let body = box data
        EventMessage(Headers=headers, Body=body)

    let toGatewayEventTypeAndData (commit:ICommit) =
        seq { for em in commit.Events -> { EventType = em.Headers.["type"] :?> string; Data = em.Body |> unbox } }

/// Wrapper yielded by create* functions with create/append functions matching FunDomain.CommandHandler requirements
type Store private (inner') = 
    // Hoop jumping a la C++ pimpl pattern - if we don't do this, we're foisting an NEventStore package reference on all downstream users
    let inner : IPersistStreams = unbox inner'
    
    let defaultBucket bucketId = defaultArg bucketId "default"
    
    let load { Bucket=bucketId; StreamId=streamId } minRevision maxRevision =
        inner.GetFrom(bucketId |> defaultBucket, streamId, minRevision, maxRevision) 
    let poll { Token=token } =
        inner.GetFrom(defaultArg token null)
    let commit = inner.Commit >> ignore

    let (|LastCommit|_|) (commits:ICommit array) = 
        if commits.Length = 0 then None
        else Some <| Seq.last commits 

    let (|LastCommitToken|) = function
        | LastCommit last -> { CommitSequence = last.CommitSequence; StreamRevision = last.StreamRevision }
        | _ -> { CommitSequence = 0; StreamRevision = 0 }

    let readStream streamId minRevision sliceSize = async {
        let maxRevision = minRevision + sliceSize - 1 
        let commits = load streamId minRevision maxRevision |> Array.ofSeq

        return commits |> function
        | LastCommitToken token when commits.Length = sliceSize -> 
            commits, token, Some <| token.StreamRevision + 1
        | LastCommitToken token -> 
            commits, token, None }

    let appendToStream {Bucket=bucketId; StreamId=streamId} streamMeta token encodedEvents = async {
        let commitId, commitStamp, commitHeaders = streamMeta
        let eventMessages = encodedEvents |> Seq.map toEventMessage
        let attempt = 
            CommitAttempt(
                bucketId |> defaultBucket, streamId, 
                token.StreamRevision + 1, 
                commitId, 
                token.CommitSequence + 1, 
                commitStamp, 
                commitHeaders, 
                eventMessages)
        commit attempt }

    let fetch token = async {
        return poll token
        |> Seq.map (fun commit -> 
            let token = { Token = Some commit.CheckpointToken }
            let encodedEvents = commit |> toGatewayEventTypeAndData
            token, encodedEvents) }

    static member internal wrap persister = Store( box persister)

    member this.executeDdlIfNecessary () = inner.Initialize()

    member this.append stream token events = async {
        let commitMetadata() =
            let commitId = Guid.NewGuid() 
            let commitDateTime = DateTime.UtcNow
            let commitHeaders = null
            commitId, commitDateTime, commitHeaders
        let metadata = commitMetadata() 
        do! appendToStream stream metadata token events }

    member this.read stream minRevision sliceSize = async {
        let! commits, sliceLastToken, nextMinRevision = readStream stream minRevision sliceSize
        let events = commits |> Seq.collect toGatewayEventTypeAndData
        return events, sliceLastToken, nextMinRevision }

    member this.project checkpointToken projection = async {
        let! batch = fetch checkpointToken
        let dispatchElements _ (checkpoint, elements) =
            elements |> projection 
            Some checkpoint
        return batch |> Seq.fold dispatchElements None }

module NesGateway =
    let createFromStore (inner:IStoreEvents) = 
        inner.Advanced |> Store.wrap

    let createInMemory () = 
        Wireup.Init()
            .LogToOutputWindow()
            .UsingInMemoryPersistence()
            .UsingJsonSerialization()
            .Build()
        |> createFromStore

    let createInMsSql (connectionName:string) = 
        Wireup.Init()
            .UsingSqlPersistence(connectionName)
            .WithDialect(new MsSqlDialect())
            .UsingJsonSerialization()
                .Compress()
            .Build()
        |> createFromStore