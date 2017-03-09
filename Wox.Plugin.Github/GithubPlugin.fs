﻿namespace Wox.Plugin.Github

open Octokit
open Wox.Plugin
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics

type GithubPlugin() = 
    
    let client = new GitHubClient(new ProductHeaderValue("Octokit"))

    let mutable PluginContext = new PluginInitContext()

    let (|UserRepoFormat|OtherFormat|) (name:string) =
        let m = Regex.Match(name, "^(?<user>(.+))(\/)(?<repo>(.+))$")
        if m.Success then
            let user,group = m.Groups.["user"].Value, m.Groups.["repo"].Value
            UserRepoFormat(user, group)
        else OtherFormat

    let getRepositories (r:string) =
        async {
            let task = client.Search.SearchRepo(new SearchRepositoriesRequest(r))
            let! result = Async.AwaitTask task
            return result
        }
    
    let getUsers (u:string) =
        async {
            let task = client.Search.SearchUsers(new SearchUsersRequest(u))
            let! result = Async.AwaitTask task
            return result
        }
    
    let getIssues (u:string) (r:string) =
        async {
            let task = client.Issue.GetAllForRepository(u, r)
            let! result = Async.AwaitTask task
            return result
        }

    let getRepo (u:string) (r:string) =
        async {
            let task = client.Repository.Get(u,r)
            let! result = Async.AwaitTask task
            return result
        }

    member this.ProcessQuery x =
        match x with
        | ["repos"; search] ->
            let result = Async.RunSynchronously (getRepositories search)
            
            result.Items
                |> Seq.map (fun r -> 
                    new Result(
                        Title = r.FullName,
                        SubTitle = sprintf "(★%d | %s) %s" r.StargazersCount r.Language r.Description,
                        Action = fun _ -> 
                            PluginContext.API.ChangeQuery <| sprintf "%s repo %s" PluginContext.CurrentPluginMetadata.ActionKeyword r.FullName
                            false
                        ))
        | ["users"; search] ->
            let result = Async.RunSynchronously (getUsers search)
            result.Items
                |> Seq.map (fun u -> 
                    new Result(
                        Title = u.Login,
                        SubTitle = u.HtmlUrl,
                        Action = fun _ ->
                            Process.Start u.HtmlUrl |> ignore
                            true
                        ))
        | ["issues"; UserRepoFormat(u,r)] ->
            let res  = Async.RunSynchronously (getIssues u r)
            res
                |> Seq.filter (fun i -> isNull i.PullRequest)
                |> Seq.map (fun i -> 
                    new Result(
                        Title = i.Title,
                        SubTitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login),
                        Action = fun _ -> 
                            Process.Start (i.HtmlUrl.ToString()) |> ignore
                            true
                    ))
        | ["pr"; UserRepoFormat(u,r)] ->
            let res  = Async.RunSynchronously (getIssues u r)
            res
                |> Seq.filter (fun i -> not (isNull i.PullRequest) )
                |> Seq.map (fun i -> 
                    new Result(
                        Title = i.Title,
                        SubTitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login),
                        Action = fun _ -> 
                            Process.Start (i.HtmlUrl.ToString()) |> ignore
                            true
                    ))
        | ["repo"; UserRepoFormat(u, r)] ->
            let res  = Async.RunSynchronously (getRepo u r)
            let issues = Async.RunSynchronously (getIssues u r)

            let issueCount,prCount = issues |> Seq.fold (fun (i,pr) x -> if isNull x.PullRequest then (i+1,pr) else (i,pr+1)) (0, 0)

            seq [
                new Result(
                    Title = res.FullName, 
                    SubTitle = sprintf "(★%d | %s) %s" res.StargazersCount res.Language res.Description,
                    Action = fun _ -> 
                        Process.Start res.HtmlUrl |> ignore
                        true 
                    );
                new Result(
                    Title = "Issues", 
                    SubTitle = (sprintf "%d issues open" issueCount),
                    Action = fun _ -> 
                        PluginContext.API.ChangeQuery <| sprintf "%s issues %s/%s" PluginContext.CurrentPluginMetadata.ActionKeyword u r
                        false
                    );
                new Result(
                    Title = "Pull Requests", 
                    SubTitle = (sprintf "%d pull requests open" prCount),
                    Action = fun _ -> 
                        PluginContext.API.ChangeQuery <| sprintf "%s pr %s/%s" PluginContext.CurrentPluginMetadata.ActionKeyword u r
                        false 
                    );
            ]
        | _ ->
            Seq.empty

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
            let query = List.ofArray q.Terms |> List.skip 1
            
            this.ProcessQuery query
            |> List<Result>