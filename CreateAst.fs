[<AutoOpen>]
module FsAst.CreateAst

open System
open Fantomas
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Quotations

//todo: move to resting place
[<RequireQualifiedAccess>]
module Ensure =
    let argNotNull name (value: obj) =
        if isNull value
        then raise <| ArgumentNullException(name)
        
[<RequireQualifiedAccess>]
module Naming =
    
    let private mapFirstChar f (input: string) =
        if String.IsNullOrEmpty input then input
        else
            let first = input.[0] |> f |> string
            if input.Length > 1 then first + input.[1..]
            else first
    
    /// Converts "name_like_that" to "NameLikeThat"
    let snakeToPascal (identifier: string) =
        identifier.Split('_')
        |> Seq.map (mapFirstChar Char.ToUpper)
        |> String.concat String.Empty
    
    /// Converts "NameLikeThat" to "nameLikeThat"
    let pascalToCamel = mapFirstChar Char.ToLower
    
    /// Converts "nameLikeThat" to "NameLikeThat" 
    let camelToPascal = mapFirstChar Char.ToUpper
    
    /// Converts "name_like_that" to "nameLikeThat"
    let snakeToCamel = snakeToPascal >> pascalToCamel
    
    /// Converts "NAME_LIKE_THAT" to "nameLikeThat"
    let upperSnakeToPascal (identifier: string) =
        snakeToPascal <| identifier.ToLower()


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

    let args =
        [   "char", "transa"
            "char", "transb"
            "int", "m"
            "int", "n"
            "int", "k"
            "double", "alpha"
            "double", "a"
            "int", "lda"
            "double", "b"
            "int", "ldb"
            "double", "beta"
            "double", "c"
            "int", "ldc"
        ]
        |> List.map (fun (typ, name) ->
            SynPatRcd.CreateAttrib(
                SynPatRcd.CreateTyped(
                    SynPatRcd.CreateNamed(Ident.Create name, SynPatRcd.CreateWild),
                    SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString "nativeptr"),
                        [SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString typ), [])])
                ),
                []
            )
        )

    let dgemm =
        SynModuleDecl.CreateLet(
            { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "dgemm_", [SynPatRcd.CreateTuple args])
                ReturnInfo = SynBindingReturnInfoRcd.Create(SynType.CreateApp(SynType.CreateUnit, [])) |> Some
                Attributes = [at]
            } |> List.singleton
     )

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