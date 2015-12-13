[<AutoOpen>]
module FsAst.CreateAst

open Fantomas
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let createBasicClass() =
    let mdl = "BasicClass"

    // create Type
    let typ =
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
        ParsedImplFileInputRcd.CreateFs(mdl)
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.Create1 mdl)
                    .AddDeclaration(typ)
            )
    )
    |> formatAst
    |> printfn "%s"

let createBasicEnums() =
    let mdl = "BasicEnums"

    // create Type
    let typ =
        SynModuleDecl.CreateSimpleType (
            { SynComponentInfoRcd.Create (Ident.Create1 "CXErrorCode") with
                XmlDoc = PreXmlDoc.Create [ " enum uint32" ]
            },
            SynTypeDefnSimpleReprEnumRcd.Create(
                [   SynEnumCaseRcd.Create(Ident.Create "CXError_Success", SynConst.UInt32 0u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_Failure", SynConst.UInt32 1u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_Crashed", SynConst.UInt32 2u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_InvalidArguments", SynConst.UInt32 3u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_ASTReadError", SynConst.UInt32 4u)
                ]
            )
            |> SynTypeDefnSimpleReprRcd.Enum
        )

    // create file
    ParsedInput.CreateImplFile(
        ParsedImplFileInputRcd.CreateFs(mdl)
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.Create1 mdl)
                    .AddDeclaration(typ)
            )
    )
    |> formatAst
    |> printfn "%s"