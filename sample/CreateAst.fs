[<AutoOpen>]
module FsAst.CreateAst

open System
open Fantomas
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open Microsoft.FSharp.Quotations
open FSharp.Compiler.XmlDoc

let createBasicClass() =
    async {
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
        let! res =
            ParsedInput.CreateImplFile(
                ParsedImplFileInputRcd.CreateFs(mdl)
                    .AddModule(
                        SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                            .AddDeclaration(typ)
                    )
            )
            |> formatAst
        printfn "%s" res
    }

let createBasicEnums() =
    async {
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
        let! res =
            // create file
            ParsedInput.CreateImplFile(
                ParsedImplFileInputRcd.CreateFs(mdl)
                    .AddModule(
                        SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                            .AddDeclaration(typ)
                    )
            )
            |> formatAst
        printfn "%s" res
    }

let createBasicPInvoke() =
    async {
        let mdl = "BasicPInvoke"

        let opn = SynModuleDecl.CreateOpen "System.Runtime.InteropServices"

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

        let args =
            [   SynType.Char(), "transa"
                SynType.Char(), "transb"
                SynType.Int(), "m"
                SynType.Int(), "n"
                SynType.Int(), "k"
                SynType.Double(), "alpha"
                SynType.Double(), "a"
                SynType.Int(), "lda"
                SynType.Double(), "b"
                SynType.Int(), "ldb"
                SynType.Double(), "beta"
                SynType.Double(), "c"
                SynType.Int(), "ldc"
            ]
            |> List.map (fun (typ, name) ->
                SynPatRcd.CreateAttrib(
                    SynPatRcd.CreateTyped(
                        SynPatRcd.CreateNamed(Ident.Create name, SynPatRcd.CreateWild),
                        SynType.NativePointer(typ)
                    ),
                    []
                )
            )

        let dgemm =
            SynModuleDecl.CreateLet(
                { SynBindingRcd.Let with
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "dgemm_", [SynPatRcd.CreateTuple args])
                    ReturnInfo = SynBindingReturnInfoRcd.Create(SynType.CreateApp(SynType.CreateUnit, [])) |> Some
                    Attributes =  [{ SynAttributeList.Attributes = [at]; Range = range.Zero }]
                } |> List.singleton
        )
        let! res =
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
        printfn "%s" res
    }
