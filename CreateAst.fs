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
            SynComponentInfoRcd.Create (Ident.CreateLong "Triangle"),
            [   SynMemberDefn.CreateImplicitCtor()
                SynMemberDefn.CreateMember
                    { SynBindingRcd.Null with
                        Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "x.Points", [])
                        Expr = SynExpr.CreateConst(SynConst.Int32 3)
                    }
            ]
        )

    // create file
    ParsedInput.CreateImplFile(
        ParsedImplFileInputRcd.CreateFs(mdl)
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
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
            { SynComponentInfoRcd.Create (Ident.CreateLong "CXErrorCode") with
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
                SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                    .AddDeclaration(typ)
            )
    )
    |> formatAst
    |> printfn "%s"

let createBasicPInvoke() =
    let mdl = "BasicPInvoke"

    let opn = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System.Runtime.InteropServices")
        
    let at : SynAttribute = 
        {   TypeName = LongIdentWithDots.CreateString "DllImport"
            ArgExpr =
                SynExpr.CreateParen(
                    SynExpr.CreateTuple(
                        [   SynExpr.CreateConstString "blas.dll"
                            SynExpr.CreateApp(
                                SynExpr.CreateAppInfix(
                                    SynExpr.CreateIdentString "op_Equality",
                                    SynExpr.CreateIdentString "EntryPoint"),
                                SynExpr.CreateConstString "dgemm_"
                            )
                        ]
                    )
                )
            Target = None
            AppliesToGetterAndSetter = false
            Range = range.Zero
        }

    let pats =
        [   
            SynPatRcd.CreateTuple(
                [   SynPatRcd.CreateAttrib(
                        SynPatRcd.CreateTyped(
                            SynPatRcd.CreateNamed(
                                Ident.Create "transa",
                                SynPatRcd.CreateWild
                            ),
                            SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString "nativeptr"),
                                [SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString "char"), [])])
                        ),
                        []
                    )
                ]
            )
        ]

    let unitExpr = SynExpr.CreateIdentString "unit"
    let unitType = SynType.CreateLongIdent(LongIdentWithDots.CreateString "unit")

    let dgemm = SynModuleDecl.CreateLet([
        { SynBindingRcd.Let with
            Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "dgemm_", pats)
            Expr = 
                SynExpr.CreateTyped(
                    SynExpr.CreateApp(unitExpr, SynExpr.CreateConstString "extern was not given a DllImport attribute"),
                    SynType.CreateApp(unitType, [])
                )
            ReturnInfo = SynBindingReturnInfo.SynBindingReturnInfo(SynType.CreateApp(unitType, []), range.Zero, []) |> Some
            Attributes = [at]
        }
    ])

    // create file
    ParsedInput.CreateImplFile(
        ParsedImplFileInputRcd.CreateFs(mdl)
            .AddModule(
                SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                    .AddDeclaration(opn)
                    .AddDeclaration(dgemm)
            )
    )
    |> formatAst
    |> printfn "%s"