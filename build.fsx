#if FAKE
#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Testing.XUnit2
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"
#endif

#if !FAKE
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.DotNet.Testing.XUnit2"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Cli"

let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif

open Fake
open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing
open Fake.DotNet
open Fake.Core
open Fake.Core.TargetOperators
open System
open Fake.DotNet.Testing
open Fake.IO.FileSystemOperators
open Fake.DotNet.NuGet

let runDotNet cmd workingDir args =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd args
    if result.ExitCode <> 0 then failwithf $"'dotnet %s{cmd}' failed in %s{workingDir}"

let dotNetBuild = runDotNet "build"

let build config outputDir (projects:IGlobbingPattern) = 
  projects |> Seq.iter (fun p -> dotNetBuild "." $"%s{p} --output %s{outputDir} --configuration %s{config}")

let buildRelease = build "release"

let dotnetTest (projects:IGlobbingPattern) =
    projects |> Seq.iter (fun p -> runDotNet "test" "." p)

type Project = 
    { name:string
      summary:string
      description:string
      authors:string list
      tags:string }

let project = "Outatime"
let summary = "Temporality category to map function and values changing over the time"
let description = "Temporality category to map function and values changing over the time"
let authors = ["@cboudereau"]
let tags = "F# fsharp temporality category applicative functor monads map"

let projects = [ { name=project; summary=summary; authors=authors; tags=tags; description=description } ]

let nugetDir = "./nuget/"
let outDir = "./bin/"
let testDir = "./test/"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [outDir; testDir; nugetDir]
)

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
    for project in projects do
        let fileName = "src" @@ project.name @@ "/AssemblyInfo.fs"
        printfn "%s" fileName
        AssemblyInfoFile.createFSharp fileName
            [ AssemblyInfo.Title project.name
              AssemblyInfo.Product project.name
              AssemblyInfo.Description project.summary
              AssemblyInfo.Version release.AssemblyVersion
              AssemblyInfo.FileVersion release.AssemblyVersion ] 
)

Target.create "Build" (fun _ ->
    projects 
    |> List.iter(fun p ->
         !! ( "src" @@ p.name @@ p.name + ".*proj")
           |> buildRelease (outDir @@ p.name)
       )
)

Target.create "Tests" (fun _ ->
    !! "**/*Test.*proj"
    |> dotnetTest
)

Target.create "NuGet" (fun _ ->
    let description project = project.description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")
    let toLines (lines:string seq) = String.Join(Environment.NewLine, lines)
    
    let fsproj = __SOURCE_DIRECTORY__ @@ "src" @@ "Outatime" @@ "Outatime.fsproj"

    let releaseNotes = release.Notes |> toLines
    runDotNet "pack" "." $"""{fsproj} --no-build -p:PackageVersion={release.NugetVersion} -p:PackageReleaseNotes="{releaseNotes}" --output {nugetDir}"""
)

Target.create "All" ignore

"Clean"
==> "AssemblyInfo"
==> "Build"
==> "Tests"
==> "NuGet"
==> "All"

Target.runOrDefault "All"
