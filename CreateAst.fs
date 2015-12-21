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

let createBasicPInvoke() =
    let mdl = "BasicPInvoke"

    let opn = SynModuleDecl.CreateOpen (LongIdentWithDots.Create ["System"; "Runtime"; "InteropServices"])
        
    let unitExpr = SynExpr.CreateIdent(Ident.Create "unit")
    let unitType = SynType.CreateLongIdent(LongIdentWithDots.Create1 "unit")

    let dgemm = SynModuleDecl.CreateLet([
        { SynBindingRcd.Null with
//            Kind = SynBindingKind.StandaloneExpression
            Pattern = SynPat.CreateLongIdent1 "dgemm_"
            Expr = 
                SynExpr.CreateTyped(
                    SynExpr.CreateApp(ExprAtomicFlag.NonAtomic, false, unitExpr, SynExpr.CreateConst (SynConst.CreateString "extern was not given a DllImport attribute")),
                    SynType.CreateApp(unitType, [], false)
                )
            ReturnInfo = SynBindingReturnInfo.SynBindingReturnInfo(SynType.CreateApp(unitType, [], false), range.Zero, []) |> Some
            ValData = SynValData(None, SynValInfo([], SynArgInfo(SynAttributes.Empty, false, None)), None)
        }
    ])

    // create file
    ParsedInput.CreateImplFile(
        ParsedImplFileInputRcd.CreateFs(mdl)
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.Create1 mdl)
                    .AddDeclaration(opn)
                    .AddDeclaration(dgemm)
            )
    )
    |> formatAst
    |> printfn "%s"