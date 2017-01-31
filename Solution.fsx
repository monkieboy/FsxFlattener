open System
open System.IO

type Fsx = string
type Line = string
type Load = string
type Lines = Line []
type Loads = Load []
type Code = Lines
type Module = string

let cons x y =
  y :: x

let loadFsx ( fsx : Fsx) : Lines = File.ReadAllLines fsx

let getLoads ( lines : Lines ) : Loads = 
  lines |> Array.filter ( fun line -> line.StartsWith "#load" )

let getCode ( lines : Lines ) : Code =
  lines |> Array.filter ( fun line -> line.StartsWith "#load" |> not )

let getFsxFromLoad ( load : Load ) : Fsx =
  let trimHashLoad = load.Replace ( "#load ", "" )
  let fsxPath = trimHashLoad.Replace ( "\"", "" )
  fsxPath

let rec buildModuleFromLoad ( load : Load ) : Module =
  let getModuleNameFromFsx ( fsx : Fsx ) : string =
    Path.GetFileNameWithoutExtension fsx

  let fsx = getFsxFromLoad load
  let lines = fsx |> loadFsx

  let innerFsxs = 
    lines 
    |> getLoads
    |> Seq.map ( getFsxFromLoad >> loadFsx )

  let innerLoads =
    innerFsxs
    |> Seq.map getLoads
    
  let innerCode =
    innerFsxs
    |> Seq.map getCode

  let result = 
    lines 
    |> getCode 
    |> Array.map ( (+) "  " ) 
    |> Seq.filter ( fun line -> line.StartsWith "#load" |> not )
    |> Seq.toList

  let folder i s =
    fst s 
    |> Seq.map ( getFsxFromLoad >> getModuleNameFromFsx >> buildModuleFromLoad >> ( sprintf "module %s =" ) )
    |> Seq.filter ( fun line -> line.StartsWith "#load" |> not )
    |> Seq.map ( (+) "\n\n" ) 
    |> String.concat "\n"
    |> cons ( snd s |> Array.map ( (+) "  " ) |> Seq.toList ) 
    |> String.concat "\n" 
    |> cons i

  let innerCode = 
    Seq.zip innerLoads innerCode 
    |> Seq.fold folder result
    |> String.concat "\n"

  innerCode :: ( getModuleNameFromFsx fsx |> sprintf "module %s =" ) :: result 
  |> Seq.toArray
  |> String.concat "\n"


let run fsx =
  let lines = fsx |> loadFsx 
  let loads = getLoads lines
  let code = getCode lines |> String.concat "\n"

  let built =
    loads 
    |> Array.map buildModuleFromLoad
    |> String.concat "\n\n"
    |> cons [ code ]
    |> String.concat "\n\n"

  built

run "MainOneLevel.fsx"
run "MainTwoLevels.fsx"
