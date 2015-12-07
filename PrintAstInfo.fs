[<AutoOpen>]
module FsAst.PrintAstInfo

open System.IO
open Fantomas
open Microsoft.FSharp.Compiler.Ast

let printAstInfo filename =
    let s = File.ReadAllText filename
    let ast = CodeFormatter.Parse(filename, s)
    match ast with
    | ParsedInput.ImplFile f ->
        let fr = f.ToRcd
        let m = fr.Modules.Head.ToRcd
        printfn "module: %s" m.Id.Head.idText
        match m.Modules.Head with
        | SynModuleDecl.Types(types, _) ->
            let t = types.Head.ToRcd
            let info = t.Info.ToRcd
            printfn "type: %s" info.Id.Head.idText
            for xd in info.XmlDoc.Lines do
                printfn "  xmldoc: %s" xd
            match t.Repr.ToRcd with
            | SynTypeDefnReprRcd.ObjectModel om ->
                let m1 = om.Members.[1] // [[0] is ImplicitCtor
                match om.Members.[1] with
                | SynMemberDefn.Member(b, _) ->
                    let br = b.ToRcd
                    match br.Pattern with
                    | SynPat.LongIdent(id,_,_,_,_,_) ->
                        printfn "member: %s %s" id.Lid.[0].idText id.Lid.[1].idText
                    | _ -> ()
                    match br.Expr with
                    | SynExpr.Const(c,_) ->
                        match c with
                        | SynConst.Int32 i -> printfn "member value: %d" i
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            | SynTypeDefnReprRcd.Simple simple ->
                match simple.Repr.ToRcd with
                | SynTypeDefnSimpleReprRcd.Enum enm ->
                    for c in enm.Cases do
                        let cr = c.ToRcd
                        printfn "  case: %s: %A" cr.Id.idText cr.Constant
                | _ -> ()
                
        | _ -> ()
    | _ -> ()