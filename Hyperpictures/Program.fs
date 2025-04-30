open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Handler
open Stack

let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    let pathElements = nonNullPath.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
    try 
        let result = handleRequest pathElements
        ctx.Response.WriteAsync(result)
    with 
    | StackUnderflowException -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync("Stack underflow exception!")
    | TypeException msg -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync(msg)
    | ex -> 
        ctx.Response.StatusCode <- 500
        ctx.Response.WriteAsync(ex.Message)

let postHandler (ctx : HttpContext) : Task = 
    match ctx.Request.HasFormContentType with
    | false -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync("Bad request - where is the form?")
    | true ->
        match ctx.Request.Form.TryGetValue("number") with 
        | (true, formStringValues) ->
            let numberStr = formStringValues.[0]
            let pathStr = ctx.Request.Path.Value 
            let location = 
                if pathStr.EndsWith("/") then sprintf "%s%s" pathStr numberStr 
                else sprintf "%s/%s" pathStr numberStr
            printfn "Redirect to location '%s'" location
            ctx.Response.Redirect(location, false)
            Task.CompletedTask;
        | _ ->
            ctx.Response.StatusCode <- 400
            ctx.Response.WriteAsync("Bad request - where is the number?")

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.MapGet("/{**path}", Func<HttpContext, Task>(getHandler)) |> ignore
    app.MapPost("/{**path}", Func<HttpContext, Task>(postHandler)) |> ignore
    app.Run()
    0 
