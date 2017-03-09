namespace Wox.Plugin.Github

open Octokit
open Wox.Plugin
open System.Collections.Generic
open System.Text.RegularExpressions

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
            let result  = Async.RunSynchronously (getRepositories search)
            result.Items
                |> Seq.map (fun r -> r.FullName, r.Description )
        | ["users"; search] ->
            let result = Async.RunSynchronously (getUsers search)
            result.Items
                |> Seq.map (fun u -> u.Login, u.HtmlUrl)
        | ["issues"; UserRepoFormat(u,r)] ->
            let res  = Async.RunSynchronously (getIssues u r)
            res
                |> Seq.filter (fun i -> isNull i.PullRequest)
                |> Seq.map (fun i -> i.Title, i.User.Login)
        | ["pr"; UserRepoFormat(u,r)] ->
            let res  = Async.RunSynchronously (getIssues u r)
            res
                |> Seq.filter (fun i -> not (isNull i.PullRequest) )
                |> Seq.map (fun i -> i.Title, i.User.Login )
        | ["repo"; UserRepoFormat(u, r)] ->
            let res  = Async.RunSynchronously (getRepo u r)
            let issues = Async.RunSynchronously (getIssues u r)

            let issueCount,prCount = issues |> Seq.fold (fun (i,pr) x -> if isNull x.PullRequest then (i+1,pr) else (i,pr+1)) (0, 0)

            seq [
                res.FullName, res.Description;
                "Issues", (sprintf "%d issues open" issueCount);
                "Pull Requests", (sprintf "%d pull requests open" prCount);
            ]
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