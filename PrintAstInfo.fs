[<AutoOpen>]
module ConsoleApp.PrintAstInfo

open System.IO
open Fantomas
open Microsoft.FSharp.Compiler.Ast

let printAstInfo() =
    let filename =  @"..\..\Hello.fs"
    let s = File.ReadAllText filename
    let ast = CodeFormatter.Parse(filename, s)
    match ast with
    | ParsedInput.ImplFile f ->
        let fr = f.ToRcd
        let m = fr.Modules.Head.ToRcd
        printfn "module: %s" m.Id.Head.idText
        match m.Decls.Head with
        | SynModuleDecl.Types(types, _) ->
            let t = types.Head.ToRcd
            printfn "type: %s" t.Info.ToRcd.Id.Head.idText
            let om = t.Repr.ToObjectModelRcd
            let m1 = om.Members.[1] // [[0] is ImplicitCtor
            match om.Members.[1] with
            | SynMemberDefn.Member(b, _) ->
                let br = b.ToRcd
                match br.Pat with
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
        | _ -> ()
    | _ -> ()