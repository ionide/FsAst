[<AutoOpen>]
module FsAst.CreateAst

open Fantomas
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let createBasicClass() =

    // create Type
    let triangle =
        SynModuleDecl.CreateType (
            SynComponentInfoRcd.Create (Ident.Create1 "Triangle"),
            [   SynMemberDefn.CreateImplicitCtor()
                SynMemberDefn.CreateMember
                    { SynBindingRcd.Null with
                        Pattern = SynPat.CreateLongIdent2 "x" "Points"
                        Expr = SynExpr.CreateConst(SynConst.Int32 3)
                    }
            ]
        )

    // create file
    ParsedInput.CreateImplFile(
        ParsedImplFileInputRcd.CreateFs("Hello")
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.Create1 "Hello")
                    .AddDeclaration(triangle)
            )
    )
    |> formatAst
    |> printfn "%s"