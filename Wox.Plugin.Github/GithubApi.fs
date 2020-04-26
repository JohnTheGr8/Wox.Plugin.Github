namespace Wox.Plugin.Github

open Octokit

type ApiSearchResult =
    | Repos of Repository list
    | RepoIssues of Issue list
    | RepoPRs of PullRequest list
    | Users of User list
    | RepoDetails of Repository * Issue list * Issue list

module GithubApi =

    let private client =
        GitHubClient(ProductHeaderValue "Wox.Plugin.Github")

    let getRepositories (search: string) = async {
        let! results = client.Search.SearchRepo (SearchRepositoriesRequest search) |> Async.AwaitTask
        return List.ofSeq results.Items |> Repos
    }

    let getUsers (search: string) = async {
        let! results = client.Search.SearchUsers(SearchUsersRequest search) |> Async.AwaitTask
        return List.ofSeq results.Items |> Users
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
