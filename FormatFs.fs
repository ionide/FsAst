[<AutoOpen>]
module ConsoleApp.FormatFs

open System.IO
open Fantomas

let formatAst ast =
    let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true } // no comments
//    let noOriginalSourceCode = "//"
//    CodeFormatter.formatAST ast noOriginalSourceCode cfg
    CodeFormatter.FormatAST(ast, None, cfg)

let formatFs() =
    let s = File.ReadAllText @"..\..\Hello.fs"
    let isFsi = false
    let ast = CodeFormatter.parse isFsi s
    let txt = formatAst ast
    printfn "%s" txt