namespace Rotem.Framework

module Common =
    open System

    let (|Integer|_|) value =
        match Int32.TryParse(value) with
        | (true, number) -> Some number
        | _ -> None