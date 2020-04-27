namespace Wox.Plugin.Github

open Wox.Plugin
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics
open Humanizer

type ActionForQuery =
    | RunApiSearch of Async<ApiSearchResult>
    | SuggestQuery of QuerySuggestion

and QuerySuggestion =
    | SearchRepos of string
    | DefaultSuggestion

type SearchResult = { title : string ; subtitle : string; action : ActionContext -> bool }

type GithubPlugin() =

    let (|UserRepoFormat|_|) (name:string) =
        let m = Regex.Match(name, "^(?<user>(.+))(\/)(?<repo>(.+))$")
        if m.Success
        then Some (m.Groups.["user"].Value, m.Groups.["repo"].Value)
        else None

    let (|IssueFormat|_|) (value: string) =
        if value.StartsWith "#" && value.Length > 1 then
            match System.Int32.TryParse (value.Substring 1) with
            | true, x when x > 0 -> Some x
            | _ -> None
        else None

    let parseQuery = function
        | [ "repos"; search ]                      -> RunApiSearch (GithubApi.getRepositories search)
        | [ "users"; search ]                      -> RunApiSearch (GithubApi.getUsers search)
        | [ "issues"; UserRepoFormat (user,repo) ] -> RunApiSearch (GithubApi.getRepoIssues user repo)
        | [ "pr";     UserRepoFormat (user,repo) ] -> RunApiSearch (GithubApi.getRepoPRs user repo)
        | [ "pull";   UserRepoFormat (user,repo) ] -> RunApiSearch (GithubApi.getRepoPRs user repo)
        | [ "repo";   UserRepoFormat (user,repo) ] -> RunApiSearch (GithubApi.getRepoInfo user repo)
        | [ UserRepoFormat (user, repo)          ] -> RunApiSearch (GithubApi.getRepoInfo user repo)
        | [ UserRepoFormat (user,repo); "issues" ] -> RunApiSearch (GithubApi.getRepoIssues user repo)
        | [ UserRepoFormat (user,repo); "pr"     ] -> RunApiSearch (GithubApi.getRepoPRs user repo)
        | [ UserRepoFormat (user,repo); "pull"   ] -> RunApiSearch (GithubApi.getRepoPRs user repo)
        | [ UserRepoFormat (user,repo); "repo"   ] -> RunApiSearch (GithubApi.getRepoInfo user repo)
        | [ UserRepoFormat (u,r); IssueFormat i  ] -> RunApiSearch (GithubApi.getSpecificIssue u r i)
        | [ search ]                               -> SuggestQuery (SearchRepos search)
        | _                                        -> SuggestQuery DefaultSuggestion

    let mutable pluginContext = PluginInitContext()

    let openUrl (url:string) =
        Process.Start url |> ignore
        true

    let changeQuery (newQuery:string) (newParam:string) =
        pluginContext.API.ChangeQuery <| sprintf "%s %s %s" pluginContext.CurrentPluginMetadata.ActionKeyword newQuery newParam
        false

    /// ApiSearchResult -> SearchResult list
    let presentApiSearchResult = function
        | Repos [] | RepoIssues [] | RepoPRs [] | Users [] ->
            [   { title    = "No results found"
                  subtitle = "please try a different query"
                  action   = fun _ -> false } ]
        | Repos repos ->
            [ for r in repos ->
                { title    = r.FullName
                  subtitle = sprintf "(★%d | %s) %s" r.StargazersCount r.Language r.Description
                  action   = fun _ -> changeQuery "repo" r.FullName } ]
        | RepoIssues issues ->
            [ for i in issues ->
                { title    = i.Title
                  subtitle = sprintf "issue #%d | created %s by %s" i.Number (i.CreatedAt.Humanize()) i.User.Login
                  action   = fun _ -> openUrl (string i.HtmlUrl) } ]
        | RepoIssue issue ->
            [   { title    = sprintf "#%d - %s" issue.Number issue.Title
                  subtitle = sprintf "%A | created by %s | last updated %s" issue.State issue.User.Login (issue.UpdatedAt.Humanize())
                  action   = fun _ -> openUrl (string issue.HtmlUrl) } ]
        | RepoPRs issues ->
            [ for i in issues ->
                { title    = i.Title
                  subtitle = sprintf "PR #%d | created %s by %s" i.Number (i.CreatedAt.Humanize()) i.User.Login
                  action   = fun _ -> openUrl (string i.HtmlUrl) } ]
        | Users users ->
            [ for u in users ->
                { title    = u.Login
                  subtitle = u.HtmlUrl
                  action   = fun _ -> openUrl (string u.HtmlUrl) } ]
        | RepoDetails (res, issues, prs) ->
            [   { title    = res.FullName
                  subtitle = sprintf "(★%d | %s) %s" res.StargazersCount res.Language res.Description
                  action   = fun _ -> openUrl res.HtmlUrl };
                { title    = "Issues"
                  subtitle = sprintf "%d issues open" (List.length issues)
                  action   = fun _ -> changeQuery "issues" res.FullName };
                { title    = "Pull Requests"
                  subtitle = sprintf "%d pull requests open" (List.length prs)
                  action   = fun _ -> changeQuery "pr" res.FullName } ]

    /// QuerySuggestion -> SearchResult list
    let presentSuggestion = function
        | SearchRepos search ->
            [   { title    = "Search repositories"
                  subtitle = sprintf "Search for repositories matching \"%s\"" search
                  action   = fun _ -> changeQuery "repos" search };
                { title    = "Search users"
                  subtitle = sprintf "Search for users matching \"%s\"" search
                  action   = fun _ -> changeQuery "users" search } ]
        | DefaultSuggestion ->
            [   { title    = "Search repositories"
                  subtitle = "Search Github repositories with \"gh repos {repo-search-term}\""
                  action   = fun _ -> changeQuery "repos" "" };
                { title    = "Search users"
                  subtitle = "Search Github users with \"gh users {user-search-term}\""
                  action   = fun _ -> changeQuery "users" "" } ]

    /// exn -> SearchResult list
    let presentApiSearchExn (e: exn) =
        let defaultResult = { title = "Search failed"; subtitle = e.Message; action = fun _ -> false }
        match e.InnerException with
        | null ->
            [ defaultResult ]
        | :? Octokit.RateLimitExceededException ->
            [ { defaultResult with
                    title = "Rate limit exceeded"
                    subtitle = "please try again later" } ]
        | :? Octokit.NotFoundException ->
            [ { defaultResult with
                    subtitle = "The repository could not be found" } ]
        | _ ->
            [ defaultResult ]

    let tryRunApiSearch =
           Async.Catch
        >> Async.RunSynchronously
        >> function
            | Choice1Of2 result -> presentApiSearchResult result
            | Choice2Of2 exn -> presentApiSearchExn exn

    member this.ProcessQuery terms =
        match parseQuery terms with
        | RunApiSearch fSearch -> tryRunApiSearch fSearch
        | SuggestQuery suggestion -> presentSuggestion suggestion

    interface IPlugin with
        member this.Init (context:PluginInitContext) =
            pluginContext <- context

        member this.Query (query:Query) =
            query.Terms
            |> List.ofArray
            |> List.skip 1
            |> this.ProcessQuery
            |> List.map (fun r -> Result( Title = r.title, SubTitle = r.subtitle, IcoPath = "icon.png", Action = fun x -> r.action x ))
            |> List<Result>
