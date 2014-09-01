module FunDomain.Tests.SerializationFacts

open FunDomain.Serialization

open Newtonsoft.Json
open System.IO
open Xunit
open Swensen.Unquote

module ``Fixups for JsonNet intrinsic behavior`` =
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