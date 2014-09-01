namespace FunDomain

open FunDomain.Serialization

open Newtonsoft.Json
open System
open Microsoft.FSharp.Reflection

type EncodedEvent = { EventType:string; Data:byte[] } with
    static member serializeUnionByCaseItemType (o:'a) =
        let case,fields = FSharpValue.GetUnionFields(o, typeof<'a>)
        let serializer = createUnionSerializer<'a> case
        use stream = new IO.MemoryStream()
        use writer = new IO.StreamWriter(stream)
        let item = fields |> Seq.exactlyOne
        serializer.Serialize(writer, o)
        writer.Flush()
        { EventType = item.GetType().Name; Data = stream.ToArray() }

    member this.deserializeUnionByCaseItemType<'a> () = 
        let isItemOfEventType (case:UnionCaseInfo) =
            let item = case.GetFields() |> Seq.exactlyOne 
            item.PropertyType.Name = this.EventType

        FSharpType.GetUnionCases(typeof<'a>)
        |> Array.tryFind isItemOfEventType 
        |> Option.map (fun case ->  
            let serializer = createUnionSerializer<'a> case
            use stream = new IO.MemoryStream(this.Data)
            use reader = new JsonTextReader(new IO.StreamReader(stream))
            serializer.Deserialize<'a>(reader))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EncodedEvent =
    let serializeFromUnion = EncodedEvent.serializeUnionByCaseItemType

    let deserializeToUnion<'e> event = 
        let encoded = { EventType = event.EventType; Data = event.Data }
        encoded.deserializeUnionByCaseItemType<'e> ()

type StorableEvent = (*eventType*)string*(*data*)byte[]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StorableEvent =
    let toEncodedEvent (eventType, data) = 
        { EventType = eventType; Data = data }

    let ofEncodedEvent ({ EventType = eventType; Data = data }) = 
        eventType, data

module EventBatch =
    let toUnionList<'e> =
        Seq.map StorableEvent.toEncodedEvent
        >> Seq.choose EncodedEvent.deserializeToUnion<'e>
        >> List.ofSeq
    let fromUnionList events =
        events 
        |> List.map EncodedEvent.serializeFromUnion
        |> List.map StorableEvent.ofEncodedEvent

type CachingEventBatch(events) =
    let cached = Seq.cache events
    member this.mapToUnion<'e> () =
        cached 
        |> Seq.map StorableEvent.toEncodedEvent
        |> Seq.choose EncodedEvent.deserializeToUnion<'e>