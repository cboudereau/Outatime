#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System

type Project = 
    { name:string
      summary:string
      description:string
      authors:string list
      tags:string
      framework:string }

let project = "Outatime"
let summary = "Temporality category to map function and values changing over the time"
let description = "Temporality category to map function and values changing over the time"
let authors = ["@cboudereau"]
let tags = "F# fsharp temporality category applicative functor monads map"

let projects = [ { name=project; summary=summary; authors=authors; tags=tags; description=description; framework="net452" } ]

let nugetDir = "./nuget/"
let outDir = "./bin/"
let testDir = "./test/"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

Target "Clean" (fun _ ->
    CleanDirs [outDir; testDir; nugetDir]
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    for project in projects do
        let fileName = "src" @@ project.name @@ "/AssemblyInfo.fs"
        printfn "%s" fileName
        CreateFSharpAssemblyInfo fileName
            [ Attribute.Title project.name
              Attribute.Product project.name
              Attribute.Description project.summary
              Attribute.Version release.AssemblyVersion
              Attribute.FileVersion release.AssemblyVersion ] 
)

Target "Build" (fun _ ->
    projects 
    |> List.iter(fun p ->
        !! ( "src" @@ p.name @@ p.name + ".*proj")
        |> MSBuildRelease (outDir @@ p.name) "Build"
        |> ignore)
)

open Fake.Testing.XUnit2
Target "Tests" (fun _ ->
    !! ("**/*Test.*proj")
    |> MSBuildRelease testDir "Build"
    |> ignore

    !! (testDir @@ "*Test.dll")
    |> xUnit2 (fun p -> { p with ShadowCopy=true; ForceAppVeyor=true; Parallel=ParallelMode.All })
)

Target "NuGet" (fun _ ->
    let description project = project.description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")

    projects
    |> Seq.iter(fun project ->
        CopyDir (nugetDir @@ "/lib/" @@ project.framework) (outDir @@ project.name) (fun file -> file.Contains "FSharp.Core." |> not)
        NuGet (fun p -> 
            { p with
                Authors = project.authors
                Project = project.name
                Summary = project.summary
                Description = project |> description
                Version = release.NugetVersion
                ReleaseNotes = release.Notes |> toLines
                Tags = tags
                OutputPath = nugetDir
                AccessKey = getBuildParamOrDefault "nugetkey" ""
                Publish = hasBuildParam "nugetkey" && (release.Date |> Option.isSome)
                Dependencies = [] })
            ("template.nuspec"))
)

Target "All" DoNothing

"Clean"
==> "AssemblyInfo"
==> "Build"
==> "Tests"
==> "NuGet"
==> "All"

RunTargetOrDefault "All"
