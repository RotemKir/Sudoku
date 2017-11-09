namespace Rotem.Framework.Logging

[<RequireQualifiedAccess>]
module FileLogger =
    open System.IO

    let private DeleteFileIfExists fileName =
        if File.Exists(fileName) then File.Delete(fileName)
    
    let private NormalizeMessage message =
        String.collect 
            (fun c ->  
                match c with  
                | '\n' -> "\r\n"  
                | _ -> c.ToString()) 
            message    
        
    let private AppendLineToFile fileName (message:string) =
        use streamWriter = File.AppendText(fileName)
        streamWriter.WriteLine(message)

    let private LogToFile fileName message =
        NormalizeMessage message |> AppendLineToFile fileName

    let Create fileName =
        DeleteFileIfExists fileName
        LogToFile fileName