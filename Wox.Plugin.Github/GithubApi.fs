namespace Wox.Plugin.Github

open Octokit

type ApiSearchRequest =
    | FindRepos of string
    | FindUsers of string
    | FindIssues of string * string
    | FindPRs of string * string
    | FindIssue of string * string * int
    | FindRepo of string * string
    | FindUserRepos of string

type ApiSearchResult =
    | Repos of Repository list
    | RepoIssues of Issue list
    | RepoIssue of Issue
    | RepoPRs of PullRequest list
    | Users of User list
    | RepoDetails of Repository * Issue list * Issue list

module Cache =
    open System
    open System.Collections.Concurrent

    let private resultCache =
        ConcurrentDictionary<ApiSearchRequest, ApiSearchResult * DateTime>()

    let private cacheAge = TimeSpan.FromHours 2.0

    let private addToCache (key, value) =
        let valueWithAge = (value, DateTime.Now.Add cacheAge)
        resultCache.TryAdd (key, valueWithAge) |> ignore

    let memoize fCompute key = async {
        match resultCache.TryGetValue key with
        | true, (res,exp) when exp > DateTime.Now ->
            return res
        | _ ->
            let! result = fCompute key

            addToCache (key, result)

            return result
    }

module GithubApi =

    let private getClient () = 
        let productHeader = 
            ProductHeaderValue "Wox.Plugin.Github"

        match tryLoadGithubToken () with
        | Some token -> GitHubClient(productHeader, Credentials = Credentials token)
        | None -> GitHubClient productHeader

    let private client =
        getClient ()

    let getRepositories (search: string) = async {
        let! results = client.Search.SearchRepo (SearchRepositoriesRequest search) |> Async.AwaitTask
        return List.ofSeq results.Items |> Repos
    }

    let getUsers (search: string) = async {
        let! results = client.Search.SearchUsers(SearchUsersRequest search) |> Async.AwaitTask
        return List.ofSeq results.Items |> Users
    }

    let getUserRepos (owner: string) = async {
        let! results = client.Repository.GetAllForUser owner |> Async.AwaitTask
        return List.ofSeq results |> List.sortByDescending (fun repo -> repo.UpdatedAt) |> Repos
    }

    let getIssuesAndPRs (user: string) (repo: string) = async {
        let! results = client.Issue.GetAllForRepository(user, repo) |> Async.AwaitTask
        return List.ofSeq results
    }

    let isNotPullRequest (i:Issue) = isNull i.PullRequest

    let getRepoIssues user repo = async {
        let! data = getIssuesAndPRs user repo
        return data |> List.filter isNotPullRequest |> RepoIssues
    }

    let getRepoPRs (user: string) (repo: string) = async {
        let! data = client.PullRequest.GetAllForRepository(user, repo) |> Async.AwaitTask
        return List.ofSeq data |> RepoPRs
    }

    let getRepoInfo (user: string) (repo: string) = async {
        let! repository = client.Repository.Get(user,repo) |> Async.AwaitTask
        let! issuesAndPRs = getIssuesAndPRs user repo
        let issues, prs = issuesAndPRs |> List.partition isNotPullRequest

        return RepoDetails (repository, issues, prs)
    }

    let getSpecificIssue (user: string) (repo: string) (issue: int) = async {
        let! data = client.Issue.Get(user, repo, issue) |> Async.AwaitTask
        return RepoIssue data
    }

module Gh =

    let runSearch = function
        | FindRepos search              -> GithubApi.getRepositories search
        | FindUsers search              -> GithubApi.getUsers search
        | FindIssues (user, repo)       -> GithubApi.getRepoIssues user repo
        | FindPRs (user, repo)          -> GithubApi.getRepoPRs user repo
        | FindIssue (user, repo, issue) -> GithubApi.getSpecificIssue user repo issue
        | FindRepo (user, repo)         -> GithubApi.getRepoInfo user repo
        | FindUserRepos user            -> GithubApi.getUserRepos user

    let runSearchCached search =
        Cache.memoize runSearch search
