namespace Wox.Plugin.Github

open Octokit
open Wox.Plugin
open System.Collections.Generic

type GithubPlugin() = 
    
    let mutable PluginContext = new PluginInitContext()

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query q =
            List<Result> [ new Result() ]