// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let buildDir = "./.artifacts/"

// Targets
Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "Build" (fun _ ->
    !! "*.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "Build-Output: "
)

// start build
RunTargetOrDefault "Build"