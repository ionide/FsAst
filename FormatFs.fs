[<AutoOpen>]
module FsAst.FormatFs

open System.IO
open Fantomas

let formatAst ast =
    let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true } // do not format comments
    CodeFormatter.FormatAST(ast, None, cfg)

let formatFs fs =
    let s = File.ReadAllText fs
    let ast = CodeFormatter.Parse(fs, s)
    let txt = formatAst ast
    printfn "%s" txt