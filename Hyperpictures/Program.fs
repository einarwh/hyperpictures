open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting

let getHandler (path : string) : string = 
    sprintf "GET to %s!" path

let postHandler (path : string) : string = 
    sprintf "POST to %s" path

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()

    app.MapGet("/{**path}", Func<string, string>(getHandler)) |> ignore
    app.MapPost("/{**path}", Func<string, string>(postHandler)) |> ignore

    app.Run()

    0 // Exit code
