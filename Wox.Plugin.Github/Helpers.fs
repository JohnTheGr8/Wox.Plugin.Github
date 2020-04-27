[<AutoOpen>]
module Wox.Plugin.Github.Helpers

open System
open System.IO
open System.Text.RegularExpressions

let (|UserRepoFormat|_|) (name:string) =
    let m = Regex.Match(name, "^(?<user>(.+))(\/)(?<repo>(.+))$")
    if m.Success
    then Some (m.Groups.["user"].Value, m.Groups.["repo"].Value)
    else None

let (|IssueFormat|_|) (value: string) =
    if value.StartsWith "#" && value.Length > 1 then
        match Int32.TryParse (value.Substring 1) with
        | true, x when x > 0 -> Some x
        | _ -> None
    else None

let tryEnvVar var =
    match Environment.GetEnvironmentVariable var with
    | null -> None
    | value -> Some value

let tryReadFile path = 
    if File.Exists path then File.ReadAllText path |> Some else None

let tryLoadGithubToken () = 
    tryEnvVar "GITHUB_API_TOKEN"
    |> Option.orElse (tryReadFile "github_token.txt")
