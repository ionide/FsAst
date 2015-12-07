[<AutoOpen>]
module FsAst.FormatFs

open System.IO
open Fantomas

let formatAst ast =
    let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true } // do not format comments
    CodeFormatter.FormatAST(ast, None, cfg)

let formatFs() =
    let fn =  @"..\..\Hello.fs"
    let s = File.ReadAllText @"..\..\Hello.fs"
    let ast = CodeFormatter.Parse(fn, s)
    let txt = formatAst ast
    printfn "%s" txt