open System
open System.IO

type Fsx = string
type Content = string 
type DepTree =
  { Dependencies : ( Fsx*Content ) list }

let loadDep dep = File.ReadAllLines dep

// Strip out #loads
// Indent each module with 2 spaces
// Fully scan the #loads into a list. E.g ["#load i.fsx; #load d.fsx"] |> Set.ofSeq
// Load in order the segments of each #load, prepending "module <name> =" whilst
// removing references


let getLoadsAndContentForSingleFsx entrypointFile =
  let moduleName = Path.GetFileNameWithoutExtension entrypointFile
  let lines = loadDep entrypointFile
  let loads = 
    lines 
    |> Seq.filter ( fun line -> line.StartsWith "#load" )
  let reg = System.Text.RegularExpressions.Regex("^#load\s?\"+([^\"]*)")
  let moduleNames = loads |> Seq.map reg.Match |> Seq.map (fun m -> m.Value)
  let content = 
    lines 
    |> Seq.filter ( fun line -> line.StartsWith "#load" |> not )
    |> Seq.map ( fun line -> "  " + line )
    |> Seq.toList
    
    
  loads, moduleNames, content
  

let ``Contains load expressions`` = 
  loadDep "Main.fsx" 
  |> Seq.exists ( fun line -> line.Contains "#load" )

let PrependAModuleName =
  let _,lines = getLoadsAndContentForSingleFsx "Main.fsx"
  lines.Head = "module Main ="