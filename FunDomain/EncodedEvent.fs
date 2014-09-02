namespace FunDomain

open FunDomain.Serialization

open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open System.IO

type EncodedEvent = { EventType:string; Data:byte[] } with
    static member serializeUnionByCaseItemType (o:'a) =
        let case,fields = FSharpValue.GetUnionFields(o, typeof<'a>)
        let serializer = createUnionSerializer<'a> case
        use stream = new MemoryStream()
        use writer = new StreamWriter(stream)
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
            use stream = new MemoryStream(this.Data)
            use reader = new JsonTextReader(new StreamReader(stream))
            serializer.Deserialize<'a>(reader))

// NB Tuple signature here is shared with CommandHandler (and gateways such as GesGateway and NesGateway)
type GatewayEventTypeAndData = (*eventType*)string*(*data*)byte[]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EncodedEvent =
    let serializeFromUnion = EncodedEvent.serializeUnionByCaseItemType

    let deserializeToUnion<'e> event = 
        let encoded = { EventType = event.EventType; Data = event.Data }
        encoded.deserializeUnionByCaseItemType<'e> ()

    let ofGatewayEventTypeAndData (eventType, data) = 
        { EventType = eventType; Data = data }

    let toGatewayEventTypeAndData ({ EventType = eventType; Data = data }) = 
        eventType, data