namespace Rotem.Framework.Logging

[<RequireQualifiedAccess>]
module ConsoleLogger =
    open System

    let Log (message:string) =
        Console.WriteLine(message)