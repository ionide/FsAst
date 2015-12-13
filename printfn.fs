[<AutoOpen>]
module FsAst.printfn

let printfn format = Printf.ksprintf System.Diagnostics.Debug.WriteLine format