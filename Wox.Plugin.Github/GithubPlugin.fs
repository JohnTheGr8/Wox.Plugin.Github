namespace Wox.Plugin.Github

open Octokit
open Wox.Plugin
open System.Collections.Generic

type GithubPlugin() = 
    
    let client = new GitHubClient(new ProductHeaderValue("Octokit"))

    let mutable PluginContext = new PluginInitContext()

    let getRepositories (r:string) =
        async {
            let task = client.Search.SearchRepo(new SearchRepositoriesRequest(r))
            let! result = Async.AwaitTask task
            return result
        }
    
    member this.ProcessQuery x =
        match x with
        | ["repos"; search] ->
            let result  = Async.RunSynchronously (getRepositories search)
            result.Items
                |> Seq.map (fun r -> r.FullName, r.Description )
        | _ ->
            Seq.empty

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
            let query = List.ofArray q.Terms |> List.skip 1
            
            this.ProcessQuery query 
            |> Seq.map (fun (t,s) -> 
                new Result(Title = t, SubTitle = s) )
            |> List<Result>