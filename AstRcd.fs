[<AutoOpen>]
module Fantomas.AstRcd

open System
open Fantomas
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

type ParsedImplFileInputRcd = { 
    File: string
    IsScript: bool
    QualName: QualifiedNameOfFile
    Pragmas: ScopedPragma list
    HashDirectives: ParsedHashDirective list
    Modules: SynModuleOrNamespace list
    IsLastCompiland: bool }

type ParsedImplFileInput with
    member x.ToRcd
        with get() =
            let (ParsedImplFileInput(file, isScript, qualName, pragmas, hashDirectives, modules, isLastCompiland)) = x
            { File = file; IsScript = isScript; QualName = qualName; Pragmas = pragmas; HashDirectives = hashDirectives; Modules = modules; IsLastCompiland = isLastCompiland }

type ParsedImplFileInputRcd with
    member x.FromRcd
        with get() = ParsedImplFileInput(x.File, x.IsScript, x.QualName, x.Pragmas, x.HashDirectives, x.Modules, x.IsLastCompiland)

type SynModuleOrNamespaceRcd = {
    Id: LongIdent 
    IsModule: bool
    Decls: SynModuleDecls
    XmlDoc: PreXmlDoc
    Attribs: SynAttributes
    Access: SynAccess option
    Range: range }

type SynModuleOrNamespace with
    member x.ToRcd
        with get() =
            let (SynModuleOrNamespace(id, isModule, decls, xmlDoc, attribs, access, range)) = x
            { Id = id; IsModule = isModule; Decls = decls; XmlDoc = xmlDoc; Attribs = attribs; Access = access; Range = range }

type SynModuleOrNamespaceRcd with
    member x.FromRcd
        with get() = SynModuleOrNamespace(x.Id, x.IsModule, x.Decls, x.XmlDoc, x.Attribs, x.Access, x.Range)

type SynTypeDefnRcd = {
    Info: SynComponentInfo
    Repr: SynTypeDefnRepr
    Members: SynMemberDefns
    Range: range }

type SynTypeDefn with
    member x.ToRcd
        with get() =
            let (TypeDefn(info, repr, members, range)) = x
            { Info = info; Repr = repr; Members = members; Range = range }

type SynTypeDefnRcd with
    member x.FromRcd
        with get() = TypeDefn(x.Info, x.Repr, x.Members, x.Range)

type SynComponentInfoRcd = {
    Attribs: SynAttributes
    TyParams: SynTyparDecl list
    Constraints: SynTypeConstraint list
    Id: LongIdent
    XmlDoc: PreXmlDoc
    PreferPostfix: bool
    Access: SynAccess option
    Range: range }

type SynComponentInfo with
    member x.ToRcd
        with get() =
            let (ComponentInfo(attribs, typarams, constraints, id, xmldoc, preferPostfix, access, range)) = x
            { Attribs = attribs; TyParams = typarams; Constraints = constraints; Id = id; XmlDoc = xmldoc; PreferPostfix = preferPostfix; Access = access; Range = range }

type SynComponentInfoRcd with
    member x.FromRcd
        with get() = ComponentInfo(x.Attribs, x.TyParams, x.Constraints, x.Id, x.XmlDoc, x.PreferPostfix, x.Access, x.Range)

type SynTypeDefnReprObjectModelRcd = {
    Kind: SynTypeDefnKind
    Members: SynMemberDefns
    Range: range }

type SynTypeDefnReprSimpleRcd = {
    Repr: SynTypeDefnSimpleRepr
    Range: range }

let internal failcase (x:Object) = failwithf "not expecting the disciminated union case of type %s" (x.GetType().FullName)

type SynTypeDefnRepr with
    member x.ToObjectModelRcd
        with get() = 
            match x with 
            | ObjectModel(kind, members, range) -> { Kind = kind; Members = members; Range = range }
            | _ -> failcase x
    member x.ToSimpleRcd
        with get() = 
            match x with 
            | Simple(repr, range) -> { Repr = repr; Range = range }
            | _ -> failcase x

type SynTypeDefnReprObjectModelRcd with
    member x.FromRcd
        with get() = ObjectModel(x.Kind, x.Members, x.Range)

type SynTypeDefnReprSimpleRcd with
    member x.FromRcd
        with get() = Simple(x.Repr, x.Range)

type SynBindingRcd = {
    Access: SynAccess option
    Kind: SynBindingKind
    IsInline: bool
    IsMutable: bool
    Attribs: SynAttributes
    XmlDoc: PreXmlDoc
    ValData: SynValData
    Pat: SynPat
    ReturnInfo: SynBindingReturnInfo option
    Expr: SynExpr
    Range: range
    Bind: SequencePointInfoForBinding }

type SynBinding with
    member x.ToRcd
        with get() =
            let (Binding(access, kind, isInline, isMutable, attrs, xmlDoc, info, headPat, retTyOpt, rhsExpr, mBind, spBind)) = x
            { Access = access; Kind = kind; IsInline = isInline; IsMutable = isMutable; Attribs = attrs; XmlDoc = xmlDoc; ValData = info; Pat = headPat; ReturnInfo = retTyOpt; Expr = rhsExpr; Range = mBind; Bind = spBind }

type SynBindingRcd  with
    member x.FromRcd
        with get() = Binding(x.Access, x.Kind, x.IsInline, x.IsMutable, x.Attribs, x.XmlDoc, x.ValData, x.Pat, x.ReturnInfo, x.Expr, x.Range, x.Bind)

let mkId name = Ident(name, range.Zero)
let mkQualifiedNameOfFile name = QualifiedNameOfFile(mkId name)