#I "../packages/Octokit/lib/net45"
#I "../packages/Wox.Plugin/lib/net452"

#r "Octokit.dll"
#r "Wox.Plugin.dll"

#load "GithubPlugin.fs"
open Wox.Plugin
open Wox.Plugin.Github

let printResult (r:Result) =
    printfn "Title: \t\t %s \nSubTitle: \t %s " r.Title r.SubTitle

let plugin = new GithubPlugin()

Seq.iter printResult <| plugin.ProcessQuery [ "repos"; "wox" ]

Seq.iter printResult <| plugin.ProcessQuery [ "users"; "john" ]

Seq.iter printResult <| plugin.ProcessQuery [ "issues"; "wox-launcher/wox" ]

Seq.iter printResult <| plugin.ProcessQuery [ "pr"; "wox-launcher/wox" ]

Seq.iter printResult <| plugin.ProcessQuery [ "repo"; "wox-launcher/wox" ]

Seq.iter printResult <| plugin.ProcessQuery [ "search term" ]

Seq.iter printResult <| plugin.ProcessQuery [ ]