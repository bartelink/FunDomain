module FunDomain.Tests.EventBatchFacts

open FunDomain

open Xunit
open Swensen.Unquote

module ``Event types with unique names enlisted into overlapping DUs`` =
    type SignificantThingOccurred = { Id:int }
    type LoggedIn = { Name:string; Value:string }
    type LoggedOut = { Name:string }

    type ProducerEvents =
        | ET1 of SignificantThingOccurred
        | ET2 of LoggedIn
        | ET3 of LoggedOut

    let [<Fact>] ``Can serialize, emitting item typeName together with a neutral DU body`` () =
        let input = ET1 { Id = 5 }
        let encoded = EncodedEvent.serializeUnionByCaseItemType input
        printfn "%s" <| System.Text.Encoding.Default.GetString encoded.Data
        test <@ "SignificantThingOccurred" = encoded.EventType @>

    type UninterestedProjection =
        | ET2 of LoggedOut
        | ET3 of LoggedIn

    let [<Fact>] ``deserialize into incompatible DU yields None`` () =
        let input = ET1 { Id = 5 }
        let encoded = EncodedEvent.serializeUnionByCaseItemType input
        test <@ None = encoded.deserializeUnionByCaseItemType<UninterestedProjection> () @>
        
    type CompatibleProjection =
        | ET2 of LoggedIn
        | ET3 of SignificantThingOccurred

    let [<Fact>] ``deserialize into compatible DU yields compatible case`` () =
        let input = ET1 { Id = 5 }
        let encoded = EncodedEvent.serializeUnionByCaseItemType input
        let compatible = ET3 { Id = 5 }
        test <@ Some compatible = encoded.deserializeUnionByCaseItemType () @>