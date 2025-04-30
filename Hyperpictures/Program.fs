open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http

let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    ctx.Response.WriteAsync(sprintf "Hello %s" nonNullPath)
    
[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.MapGet("/{**path}", Func<HttpContext, Task>(getHandler)) |> ignore
    app.Run()
    0 
