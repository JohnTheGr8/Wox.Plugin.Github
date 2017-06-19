namespace Wox.Plugin.Github

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

    let openUrl (url:string) = 
        Process.Start url |> ignore
        true

    let changeQuery (newQuery:string) (newParam:string) =
        PluginContext.API.ChangeQuery <| sprintf "%s %s %s" PluginContext.CurrentPluginMetadata.ActionKeyword newQuery newParam
        false

    let errorResult (e:exn) = 
        match e.InnerException with
        | :? RateLimitExceededException -> 
            seq [ new Result(Title = "Rate limit exceeded", SubTitle = "please try again later", IcoPath = "icon.png" ) ]
        | :? NotFoundException -> 
            seq [ new Result(Title = "Search failed", SubTitle = "The repository could not be found", IcoPath = "icon.png") ]
        | _ -> 
            seq [ new Result(Title = "Search failed", SubTitle = e.Message, IcoPath = "icon.png") ]

    let getRepositories (r:string) = async {
        let task = client.Search.SearchRepo(new SearchRepositoriesRequest(r))
        return! Async.AwaitTask task
    }
    
    let getUsers (u:string) = async {
        let task = client.Search.SearchUsers(new SearchUsersRequest(u))
        return! Async.AwaitTask task
    }
    
    let getIssues (u:string) (r:string) = async {
        let task = client.Issue.GetAllForRepository(u, r)
        return! Async.AwaitTask task
    }

    let getRepo (u:string) (r:string) = async {
        let task = client.Repository.Get(u,r)
        return! Async.AwaitTask task
    }

    member this.ProcessQuery x =
        let queryResults = match x with
        | ["repos"; search] ->
            getRepositories search
            |> Async.Catch
            |> Async.RunSynchronously
            |> function
                | Choice1Of2 result ->
                    result.Items
                    |> Seq.map (fun r -> 
                        new Result(
                            Title = r.FullName,
                            SubTitle = sprintf "(★%d | %s) %s" r.StargazersCount r.Language r.Description,
                            IcoPath = "icon.png",
                            Action = fun _ -> changeQuery "repo" r.FullName
                            ))
                | Choice2Of2 err -> errorResult err
        | ["users"; search] ->
            getUsers search
            |> Async.Catch
            |> Async.RunSynchronously
            |> function
                | Choice1Of2 result -> 
                    result.Items
                    |> Seq.map (fun u -> 
                        new Result(
                            Title = u.Login,
                            SubTitle = u.HtmlUrl,
                            IcoPath = "icon.png",
                            Action = fun _-> openUrl u.HtmlUrl
                        ))
                | Choice2Of2 err -> errorResult err
        | ["issues"; UserRepoFormat(u,r)] ->
            getIssues u r
            |> Async.Catch
            |> Async.RunSynchronously
            |> function
                | Choice1Of2 result -> 
                    result
                    |> Seq.filter (fun i -> isNull i.PullRequest)
                    |> Seq.map (fun i -> 
                        new Result(
                            Title = i.Title,
                            SubTitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login),
                            IcoPath = "icon.png",
                            Action = fun _-> openUrl (i.HtmlUrl.ToString())
                        ))
                | Choice2Of2 err -> errorResult err
        | ["pr"; UserRepoFormat(u,r)] ->
            getIssues u r
            |> Async.Catch
            |> Async.RunSynchronously
            |> function
                | Choice1Of2 result -> 
                    result
                    |> Seq.filter (fun i -> not (isNull i.PullRequest) )
                    |> Seq.map (fun i -> 
                        new Result(
                            Title = i.Title,
                            SubTitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login),
                            IcoPath = "icon.png",
                            Action = fun _-> openUrl (i.HtmlUrl.ToString())
                        ))
                | Choice2Of2 err -> errorResult err
        | ["repo"; UserRepoFormat(u, r)] ->
            let repoResult = getRepo u r |> Async.Catch |> Async.RunSynchronously
            let issuesResult = getIssues u r |> Async.Catch |> Async.RunSynchronously

            match repoResult, issuesResult with
            | Choice1Of2 res, Choice1Of2 issues ->
                let issueCount,prCount = issues |> Seq.fold (fun (i,pr) x -> if isNull x.PullRequest then (i+1,pr) else (i,pr+1)) (0, 0)

                seq [
                    new Result(
                        Title = res.FullName, 
                        SubTitle = sprintf "(★%d | %s) %s" res.StargazersCount res.Language res.Description,
                        IcoPath = "icon.png",
                        Action = fun _-> openUrl res.HtmlUrl
                        );
                    new Result(
                        Title = "Issues", 
                        SubTitle = (sprintf "%d issues open" issueCount),
                        IcoPath = "icon.png",
                        Action = fun _ -> changeQuery "issues" res.FullName
                        );
                    new Result(
                        Title = "Pull Requests", 
                        SubTitle = (sprintf "%d pull requests open" prCount),
                        IcoPath = "icon.png",
                        Action = fun _ -> changeQuery "pr" res.FullName
                        );
                ]
            | Choice2Of2 err, _ | _, Choice2Of2 err -> errorResult err
        | [search] ->
            seq [
                new Result(
                    Title = "Search repositories",
                    SubTitle = sprintf "Search for repositories matching \"%s\"" search,
                    IcoPath = "icon.png",
                    Action = fun _ -> changeQuery "repos" search
                    );
                new Result(
                    Title = "Search users",
                    SubTitle = sprintf "Search for users matching \"%s\"" search,
                    IcoPath = "icon.png",
                    Action = fun _ -> changeQuery "users" search
                    );
            ]
        | [] -> 
            seq [ 
                new Result(
                    Title = "Search repositories",
                    SubTitle = "Search Github repositories with \"gh repos {repo-search-term}\"",
                    IcoPath = "icon.png",
                    Action = fun _ -> changeQuery "repos" ""
                    );
                new Result(
                    Title = "Search users",
                    SubTitle = "Search Github users with \"gh users {user-search-term}\"",
                    IcoPath = "icon.png",
                    Action = fun _ -> changeQuery "users" ""
                    );
            ]

        if Seq.isEmpty queryResults then
            seq [ new Result(Title = "No results found", SubTitle = "please try a different query", IcoPath = "icon.png") ]
        else
            queryResults

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
            let query = List.ofArray q.Terms |> List.skip 1
            
            this.ProcessQuery query
            |> List<Result>