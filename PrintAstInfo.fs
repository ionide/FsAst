[<AutoOpen>]
module FsAst.PrintAstInfo

open System.IO
open Fantomas
open Microsoft.FSharp.Compiler.Ast

let rec printType indent typ =
    match typ with
    | SynType.App(typ, _, typs, _, _, _, _) ->
        printfn "%*sType App, types %d" indent "" typs.Length
        printType (indent+2) typ
        for typ in typs do
            printType (indent+2) typ
    | SynType.LongIdent id ->
        printfn "%*sType LongIdent %s" indent "" id.AsString
    | _ -> printfn "%*sType not matched: %A" indent "" typ

let rec printExpr indent expr =
    match expr with
    | SynExpr.Const(c,_) ->
        match c with
        | SynConst.Int32 i -> printfn "%*sConst Int32: %d" indent "" i
        | SynConst.String (s,_) -> printfn "%*sConst String: %s" indent "" s
        | cnst -> printfn "%*scnst not matched: %A" indent "" cnst
    | SynExpr.Paren(innerExpr, _, _, _) ->
        printfn "%*sParen expr" indent ""
        printExpr (indent+2) innerExpr
    | SynExpr.Tuple(exprs, _, _) ->
        printfn "%*sTuple expr %d" indent "" exprs.Length
        for exp in exprs do
            printExpr (indent+2) exp
    | SynExpr.App(atomicFlag, _, funcExpr, argExpr, _) ->
        printfn "%*sApp expr: atomic: %A" indent "" atomicFlag
        printExpr (indent+2) funcExpr
        printExpr (indent+2) argExpr
    | SynExpr.Ident id -> printfn "%*sIdent expr: %s" indent "" id.idText
    | SynExpr.Typed(exp, typ, _) ->
        printfn "%*sExpr Typed" indent ""
        printType (indent+2) typ
        printExpr (indent+2) exp
    | expr -> printfn "%*sExpr not matched: %A" indent "" expr

let rec printPattern indent pat =
    match pat with
    | SynPat.LongIdent(id, id2, decls, cnstrArgs, access, _) ->
        printfn "%*sPat LongIdent: %s" indent "" id.AsString
//        printfn "%*s  decls: %A" indent "" decls
        match cnstrArgs with
        | SynConstructorArgs.Pats pats ->
            printfn "%*s  cnstrArgs, %d pats" indent "" pats.Length
            for p in pats do
                printPattern (indent+4) p
        | SynConstructorArgs.NamePatPairs (namedPats, _) ->
            printfn "%*s  cnstrArgs, %d namedPats" indent "" namedPats.Length
            for id, p in namedPats do
                printfn "%*s  id: %s" indent "" id.idText
                printPattern (indent+4) p
    | SynPat.Tuple (pats, _) ->
        printfn "%*sPat Tuple %d" indent "" pats.Length
        for p in pats do
            printPattern (indent+2) p
    | SynPat.Attrib(p, attributes, _) ->
        printfn "%*sPat Atrrib" indent ""
        printPattern (indent+2) p
    | SynPat.Typed(p, typ, _) ->
        printfn "%*sPat Typed" indent ""
        printType (indent+2) typ
        printPattern (indent+2) p
    | SynPat.Named(p, id, _, _, _) ->
        printfn "%*sPat Named %s" indent "" id.idText
        printPattern (indent+2) p
    | SynPat.Wild _ ->
        printfn "%*sPat Wild" indent ""
    | pat -> printfn "%*sPat not matched: %A" indent "" pat

let printAstInfo filename =
    let s = File.ReadAllText filename
    let ast = CodeFormatter.Parse(filename, s)
    match ast with
    | ParsedInput.ImplFile f ->
        let fr = f.ToRcd
        let m = fr.Modules.Head.ToRcd
        printfn "module: %s" m.Id.Head.idText
//        printfn "module: %A" m
        for decl in m.Declarations do
            match decl with
            | SynModuleDecl.Types(types, _) ->
                let t = types.Head.ToRcd
                let info = t.Info
                printfn "type: %s" info.Id.Head.idText
                for xd in info.XmlDoc.Lines do
                    printfn "  xmldoc: %s" xd
                match t.Repr.ToRcd with
                | SynTypeDefnReprRcd.ObjectModel om ->
                    let m1 = om.Members.[1] // [[0] is ImplicitCtor
                    match om.Members.[1] with
                    | SynMemberDefn.Member(b, _) ->
                        let br = b.ToRcd
                        printPattern 2 br.Pattern.FromRcd
                        printExpr 2 br.Expr
                    | mbr -> printfn "mbr not matched: %A" mbr
                | SynTypeDefnReprRcd.Simple simple ->
                    match simple.Repr.ToRcd with
                    | SynTypeDefnSimpleReprRcd.Enum enm ->
                        for c in enm.Cases do
                            let cr = c.ToRcd
                            printfn "  case: %s: %A" cr.Id.idText cr.Constant
                            printfn "    attributes: %A" cr.Attributes
                    | repr -> printfn "not matched: %A" repr
            | SynModuleDecl.Open(id, _) ->
                printfn "open: %s" id.AsString
            | SynModuleDecl.Let(a, bindings, _) ->
                printfn "%d let bindings, %b" bindings.Length a
                for binding in bindings |> List.map (fun b -> b.ToRcd) do
//                    printfn "binding: %A" binding
                    printfn "  binding kind: %A" binding.Kind
                    printExpr 2 binding.Expr
                    
                    match binding.ReturnInfo with
                    | Some ri ->
                        match ri with
                        | SynBindingReturnInfo.SynBindingReturnInfo(tp, _, attrs) ->
                            printfn "  returnInfo: attrs: %A" attrs
                            printType 4 tp
                    | None -> ()

                    printfn "binding, %d attributes" binding.Attributes.Length
                    for attr in binding.Attributes do
                        printfn "  attribute: %s" attr.TypeName.AsString
                        printExpr 4 attr.ArgExpr
                    match binding.ValData with
                    | SynValData.SynValData(memberFlags, valInfo, id) ->
                        printfn "  ValData: id %A, %d args" id valInfo.ArgInfos.Length
                        for args in valInfo.ArgInfos do
                            printfn "    %d sub args" args.Length
                            for arg in args do
                                match arg with
                                | SynArgInfo.SynArgInfo(attrs, _, id) ->
                                    printfn "    arg: %A" id
                    printPattern 2 binding.Pattern.FromRcd
            | decl -> printfn "decl not matched: %A" decl
    | input -> printfn "input not matched: %A" input