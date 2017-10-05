namespace Wox.Plugin.Github

open Octokit
open Wox.Plugin
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics

type GithubPlugin() = 
    
    let client = GitHubClient(ProductHeaderValue("Octokit"))

    let mutable PluginContext = PluginInitContext()

    let (|UserRepoFormat|_|) (name:string) =
        let m = Regex.Match(name, "^(?<user>(.+))(\/)(?<repo>(.+))$")
        if m.Success 
        then Some (m.Groups.["user"].Value, m.Groups.["repo"].Value)
        else None

    let isPullRequest (i:Issue) = not (isNull i.PullRequest)     

    let openUrl (url:string) = 
        Process.Start url |> ignore
        true

    let changeQuery (newQuery:string) (newParam:string) =
        PluginContext.API.ChangeQuery <| sprintf "%s %s %s" PluginContext.CurrentPluginMetadata.ActionKeyword newQuery newParam
        false

    let errorResult (e:exn) = 
        match e.InnerException with
        | :? RateLimitExceededException -> 
            seq [ { title = "Rate limit exceeded"; subtitle = "please try again later"; action = fun _ -> false } ]
        | :? NotFoundException -> 
            seq [ { title = "Search failed"; subtitle = "The repository could not be found"; action = fun _ -> false } ]
        | _ -> 
            seq [ { title = "Search failed"; subtitle = e.Message; action = fun _ -> false } ]

    let getRepositories (r:string) = async {
        let task = client.Search.SearchRepo(SearchRepositoriesRequest(r))
        return! Async.AwaitTask task
    }
    
    let getUsers (u:string) = async {
        let task = client.Search.SearchUsers(SearchUsersRequest(u))
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

    let getRepoInfo (u:string) (r:string) = async { 
        let! repo = getRepo u r 
        let! issues = getIssues u r
        let issueCount, prCount = 
            issues |> Seq.fold (fun (i,pr) x -> if isPullRequest x then (i,pr+1) else (i+1,pr)) (0, 0)
        return repo, issueCount, prCount
    }

    let continueWith f = 
        Async.Catch >> Async.RunSynchronously >> function
        | Choice1Of2 result -> f result
        | Choice2Of2 error  -> errorResult error

    member this.ProcessQuery query =
        let queryResults = match query with
        | ["repos"; search] ->
            getRepositories search
            |> continueWith (fun result ->
                result.Items
                |> Seq.map (fun r -> 
                    { title    = r.FullName
                      subtitle = sprintf "(★%d | %s) %s" r.StargazersCount r.Language r.Description
                      action   = fun _ -> changeQuery "repo" r.FullName } ))

        | ["users"; search] ->
            getUsers search
            |> continueWith (fun result ->
                result.Items
                |> Seq.map (fun u -> 
                    { title    = u.Login
                      subtitle = u.HtmlUrl
                      action   = fun _ -> openUrl u.HtmlUrl } ))

        | ["issues"; UserRepoFormat(u,r)] ->
            getIssues u r
            |> continueWith (fun result ->
                result
                |> Seq.filter (isPullRequest >> not)
                |> Seq.map (fun i -> 
                    { title    = i.Title
                      subtitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login)
                      action   = fun _ -> openUrl (i.HtmlUrl.ToString()) } ))

        | ["pr"; UserRepoFormat(u,r)] ->
            getIssues u r
            |> continueWith (fun result ->
                result
                |> Seq.filter isPullRequest
                |> Seq.map (fun i -> 
                    { title    = i.Title
                      subtitle = (sprintf "#%d | opened %s by %s" i.Number (i.CreatedAt.ToString("dd/mm/yy")) i.User.Login)
                      action   = fun _ -> openUrl (i.HtmlUrl.ToString()) } ))

        | ["repo"; UserRepoFormat(u, r)] ->
            getRepoInfo u r 
            |> continueWith (fun (res, issueCount, prCount) ->
                seq [ 
                    { title    = res.FullName
                      subtitle = sprintf "(★%d | %s) %s" res.StargazersCount res.Language res.Description
                      action   = fun _ -> openUrl res.HtmlUrl };
                    { title    = "Issues"
                      subtitle = (sprintf "%d issues open" issueCount)
                      action   = fun _ -> changeQuery "issues" res.FullName };
                    { title    = "Pull Requests"
                      subtitle = (sprintf "%d pull requests open" prCount)
                      action   = fun _ -> changeQuery "pr" res.FullName } ] )

        | [search] -> 
            seq [   { title    = "Search repositories"
                      subtitle = sprintf "Search for repositories matching \"%s\"" search
                      action   = fun _ -> changeQuery "repos" search };
                    { title    = "Search users"
                      subtitle = sprintf "Search for users matching \"%s\"" search
                      action   = fun _ -> changeQuery "users" search } ]

        | [] -> 
            seq [   { title    = "Search repositories"
                      subtitle = "Search Github repositories with \"gh repos {repo-search-term}\""
                      action   = fun _ -> changeQuery "repos" "" };
                    { title    = "Search users"
                      subtitle = "Search Github users with \"gh users {user-search-term}\""
                      action   = fun _ -> changeQuery "users" "" } ]

        if Seq.isEmpty queryResults then
            seq [ Result(Title = "No results found", SubTitle = "please try a different query", IcoPath = "icon.png") ]
        else
            queryResults 
            |> Seq.map (fun (r:SearchResult) -> Result(Title = r.title, SubTitle = r.subtitle, IcoPath = "icon.png", Action = fun x -> r.action x ))

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
            let query = List.ofArray q.Terms |> List.skip 1
            
            this.ProcessQuery query
            |> List<Result>

and 
    SearchResult = { title : string ; subtitle : string; action : ActionContext -> bool }