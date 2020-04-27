[<AutoOpen>]
module Wox.Plugin.Github.Helpers

open System
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
