namespace FunDomain

type EventBatch(gatewayEventTypeAndDatas) =
    let cached = Seq.cache gatewayEventTypeAndDatas
    member this.mapToUnion<'e> () =
        cached 
        |> Seq.map EncodedEvent.ofGatewayEventTypeAndData
        |> Seq.choose EncodedEvent.deserializeToUnion<'e>