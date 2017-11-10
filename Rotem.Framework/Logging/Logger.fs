namespace Rotem.Framework.Logging

[<RequireQualifiedAccess>]
module Logger =
    type logFunction = string -> unit

    let create (loggers : logFunction seq) =
        (fun message -> Seq.iter (fun logger -> logger message) loggers)

