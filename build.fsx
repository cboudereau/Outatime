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
      tags:string }

let project = "FSharp.Temporality"
let summary = "An applicative functor for temporaries (ie: value over the time) to apply function on intersection"
let description = "An applicative functor for temporaries (ie: value over the time) to apply function on intersection"
let authors = ["@cboudereau"]
let tags = "F# fsharp temporality applicative functor"

let projects = [ { name=project; summary=summary; authors=authors; tags=tags; description=description } ]

let cloneUrl = "https://github.com/cboudereau/FSharp.Temporality.git"
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
    let fileName = project.name + "/AssemblyInfo.fs"
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
        !! ( p.name + "/" + p.name + ".fsproj")
        |> MSBuildRelease outDir "Rebuild"
        |> ignore)

    !! ("**/*Test.fsproj")
    |> MSBuildRelease testDir "Rebuild"
    |> ignore
)

open System.Diagnostics

Target "Tests" (fun _ ->
    let runner = findToolInSubPath "xunit.console.clr4.exe" (currentDirectory @@ "tools" @@ "xUnit")
    let xunit t =
        let xunitInfo t (info:ProcessStartInfo) = 
            info.FileName <- runner
            info.Arguments <- sprintf "'%s' /appveyor" t
        ExecProcess (xunitInfo t) (TimeSpan.FromMinutes(30.)) = 0
    
    !! (testDir + "*Test.dll")
    |> Seq.iter (xunit >> ignore)
)

Target "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let description project = project.description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")

    projects
    |> Seq.iter(fun project ->
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
                Publish = hasBuildParam "nugetkey"
                Dependencies = [] })
            (project.name + ".nuspec"))
)

Target "All" DoNothing

"Clean"
==> "AssemblyInfo"
==> "Build"
==> "Tests"
//  ==> "NuGet"
==> "All"

RunTargetOrDefault "All"
