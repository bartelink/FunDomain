namespace FunDomain

type EventBatch(encodedEvents) =
    let cached = Seq.cache encodedEvents
    member this.mapToUnion<'e> () =
        cached |> Seq.choose EncodedEvent.deserializeUnionByCaseItemTypeName<'e>