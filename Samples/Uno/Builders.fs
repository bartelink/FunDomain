namespace Uno

[<AutoOpen>]
module Builders = 
    let digit value color =
        if value < 0 || value > 9 then invalidArg "value" "A digit value should be from 0 to 9" 
        DigitCard( value |> Digit,color)

    let red n = digit n Red
    let green n = digit n Green
    let blue n = digit n Blue
    let yellow n = digit n Yellow