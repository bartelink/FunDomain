module FunDomain.Tests.SerializationFacts

open FunDomain.Persistence.Serialization

open Newtonsoft.Json
open System.IO
open Xunit
open Swensen.Unquote

let createSerializer converters =
    let serializer = JsonSerializer()
    converters |> List.iter serializer.Converters.Add
    serializer

let serialize converters o = 
    let serializer = createSerializer converters
    use w = new StringWriter()
    serializer.Serialize(w, o)
    w.ToString()

let deserialize<'a> converters s =
    let serializer = createSerializer converters
    use r = new StringReader(s)
    serializer.Deserialize<'a>(new JsonTextReader(r))

module structs = 
    type Digit = struct val value:int; new value = { value = value } end

    let [<Fact>] ``a value should be serialized as its content`` () =
        let serialize = serialize [ valueConverter typeof<Digit> ]

        test <@ "7" = (Digit 7 |> serialize) @>

    let [<Fact>] ``a value should be deserialized from its content`` () =
        let deserialize = deserialize [ valueConverter typeof<Digit> ]
    
        test <@ Digit 7 = ("7" |> deserialize) @>

module ``Single case DUs`` =
    type GameId = GameId of int

    let [<Fact>] ``a single case union should be serialized as its content`` () =
        let serialize = serialize [ unionConverter ]

        test <@ "1234" = (GameId 1234 |> serialize) @>

    let [<Fact>] ``a single case union should be deserialized from its content`` () =
        let deserialize = deserialize [ unionConverter ]
    
        test <@ GameId 1234 = ("1234" |> deserialize)  @>

module ``Event types with unique names enlisted into overlapping DUs`` =
    type E1 = { Id:int }
    type E2 = { Name:string; Value:string }

    type Event =
        | ET1 of E1
        | ET2 of E2

    let [<Fact>] ``Can serialize, emitting item typeName together with a neutral DU body`` () =
        let input = ET1 { Id = 5 }
        let eventType,body = serializeBody input
        printfn "%s" <| System.Text.Encoding.Default.GetString body
        test <@ "E1" = eventType @>

    type Event2 =
        | ET3 of int
        | ET4 of E2

    let [<Fact>] ``deserialize into incompatible DU yields None`` () =
        let input = ET1 { Id = 5 }
        let eventType,body = serializeBody input
        test <@ None = deserializeBody<Event2> eventType body @>
        
    type Event3 =
        | ET4 of E2
        | ET5 of E1

    let [<Fact>] ``deserialize into compatible DU yields compatible case`` () =
        let input = ET1 { Id = 5 }
        let eventType,body = serializeBody input
        let compatible = ET5 { Id = 5 }
        test <@ Some compatible = deserializeBody eventType body @>