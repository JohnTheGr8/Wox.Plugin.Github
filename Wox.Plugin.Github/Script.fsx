#I "../packages/Octokit/lib/net45"
#I "../packages/Wox.Plugin/lib/net452"

#r "Octokit.dll"
#r "Wox.Plugin.dll"

#load "GithubPlugin.fs"
open Wox.Plugin
open Wox.Plugin.Github

let printResult res =
    printfn "Title: \t\t %s \nSubTitle: \t %s " <|| res

let plugin = new GithubPlugin()

Seq.iter printResult <| plugin.ProcessQuery [ "repos"; "wox" ]

Seq.iter printResult <| plugin.ProcessQuery [ "users"; "john" ]