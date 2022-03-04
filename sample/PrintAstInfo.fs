[<AutoOpen>]
module FsAst.PrintAstInfo

open System.IO
open Fantomas
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis

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
    | SynExpr.Const(c, _) ->
        match c with
        | SynConst.Int32 i -> printfn "%*sConst Int32: %d" indent "" i
        | SynConst.String (s, _, _) -> printfn "%*sConst String: %s" indent "" s
        | cnst -> printfn "%*scnst not matched: %A" indent "" cnst
    | SynExpr.Paren(innerExpr, _, _, _) ->
        printfn "%*sParen expr" indent ""
        printExpr (indent+2) innerExpr
    | SynExpr.Tuple(_,exprs, _, _) ->
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
    | SynPat.LongIdent(id, propertyKeyword, id2, decls, cnstrArgs, access, _) ->
        printfn "%*sPat LongIdent: %s" indent "" id.AsString
//        printfn "%*s  decls: %A" indent "" decls
        match cnstrArgs with
        | SynArgPats.Pats pats ->
            printfn "%*s  cnstrArgs, %d pats" indent "" pats.Length
            for p in pats do
                printPattern (indent+4) p
        | SynArgPats.NamePatPairs (namedPats, _) ->
            printfn "%*s  cnstrArgs, %d namedPats" indent "" namedPats.Length
            for id, range, p in namedPats do
                printfn "%*s  id: %s" indent "" id.idText
                printPattern (indent+4) p
    | SynPat.Tuple (_,pats, _) ->
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
    | SynPat.Named(id, _, _, _) ->
        printfn "%*sPat Named %s" indent "" id.idText
    | SynPat.Wild _ ->
        printfn "%*sPat Wild" indent ""
    | pat -> printfn "%*sPat not matched: %A" indent "" pat

let printAstInfo filename checker =
    async {
    let s = File.ReadAllText filename
    let parsOpt = {FSharpParsingOptions.Default with SourceFiles = [|filename|]}
    let! res = CodeFormatter.ParseAsync(filename, SourceOrigin.SourceString s, parsOpt, checker)
    for (ast,_) in res do
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
                        | SynTypeDefnSimpleReprRcd.Record record ->
                            for field in record.Fields do
                                match field with
                                | SynField(attribs, isstatic, ident, typ, e, prexmldoc, access, range) ->
                                    printfn "%s:"
                                        (match ident with
                                        | Some i -> i.idText
                                        | None -> "")
                                    printType 4 typ
                        | repr -> printfn "not matched: %A" repr
                    printfn "Members: %A" t.Members
                | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace (id,_), _) ->
                    printfn "open: %s" id.Head.idText
                | SynModuleDecl.Let(a, bindings, _) ->
                    printfn "%d let bindings, %b" bindings.Length a
                    for binding in bindings |> List.map (fun b -> b.ToRcd) do
    //                    printfn "binding: %A" binding
                        printfn "  binding kind: %A" binding.Kind
                        printExpr 2 binding.Expr

                        match binding.ReturnInfo with
                        | Some ri ->
                            printfn "  returnInfo:"
                            printType 4 ri.Type
                        | None -> ()

                        printfn "binding, %d attributes" binding.Attributes.Length
                        for attr in binding.Attributes do
                            for a in attr.Attributes do
                                printfn "  attribute: %s" a.TypeName.AsString
                                printExpr 4 a.ArgExpr
                        match binding.ValData with
                        | SynValData.SynValData(memberFlags, (SynValInfo (argsInfo, retInfo)), id) ->
                            printfn "  ValData: id %A, %d args" id argsInfo.Length
                            for args in argsInfo do
                                printfn "    %d sub args" args.Length
                                for arg in args do
                                    match arg with
                                    | SynArgInfo.SynArgInfo(attrs, _, id) ->
                                        printfn "    arg: %A" id
                        printPattern 2 binding.Pattern.FromRcd
                | decl -> printfn "decl not matched: %A" decl
        | input -> printfn "input not matched: %A" input
    }