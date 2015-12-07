[<AutoOpen>]
module FsAst.AstRcd

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
    Modules: SynModuleDecls
    XmlDoc: PreXmlDoc
    Attributes: SynAttributes
    Access: SynAccess option
    Range: range }

type SynModuleOrNamespace with
    member x.ToRcd
        with get() =
            let (SynModuleOrNamespace(id, isModule, modules, xmlDoc, attributes, access, range)) = x
            { Id = id; IsModule = isModule; Modules = modules; XmlDoc = xmlDoc; Attributes = attributes; Access = access; Range = range }

type SynModuleOrNamespaceRcd with
    member x.FromRcd
        with get() = SynModuleOrNamespace(x.Id, x.IsModule, x.Modules, x.XmlDoc, x.Attributes, x.Access, x.Range)

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
    Attributes: SynAttributes
    Parameters: SynTyparDecl list
    Constraints: SynTypeConstraint list
    Id: LongIdent
    XmlDoc: PreXmlDoc
    PreferPostfix: bool
    Access: SynAccess option
    Range: range }

type SynComponentInfo with
    member x.ToRcd
        with get() =
            let (ComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)) = x
            { Attributes = attributes; Parameters = parameters; Constraints = constraints; Id = id; XmlDoc = xmldoc; PreferPostfix = preferPostfix; Access = access; Range = range }

type SynComponentInfoRcd with
    member x.FromRcd
        with get() = ComponentInfo(x.Attributes, x.Parameters, x.Constraints, x.Id, x.XmlDoc, x.PreferPostfix, x.Access, x.Range)

type SynTypeDefnReprObjectModelRcd = {
    Kind: SynTypeDefnKind
    Members: SynMemberDefns
    Range: range }

type SynTypeDefnReprSimpleRcd = {
    Repr: SynTypeDefnSimpleRepr
    Range: range }

[<RequireQualifiedAccess>]
type SynTypeDefnReprRcd =
    | ObjectModel of SynTypeDefnReprObjectModelRcd
    | Simple of SynTypeDefnReprSimpleRcd

type SynTypeDefnRepr with
    member x.ToRcd
        with get() =
            match x with
            | ObjectModel(kind, members, range) ->
                { Kind = kind; Members = members; Range = range }
                |> SynTypeDefnReprRcd.ObjectModel
            | Simple(repr, range) ->
                { Repr = repr; Range = range }
                |> SynTypeDefnReprRcd.Simple

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
    Attributes: SynAttributes
    XmlDoc: PreXmlDoc
    ValData: SynValData
    Pattern: SynPat
    ReturnInfo: SynBindingReturnInfo option
    Expr: SynExpr
    Range: range
    Bind: SequencePointInfoForBinding }

type SynBinding with
    member x.ToRcd
        with get() =
            let (Binding(access, kind, isInline, isMutable, attrs, xmlDoc, info, pattern, returnInfo, rhsExpr, mBind, spBind)) = x
            { Access = access; Kind = kind; IsInline = isInline; IsMutable = isMutable; Attributes = attrs; XmlDoc = xmlDoc; ValData = info; Pattern = pattern; ReturnInfo = returnInfo; Expr = rhsExpr; Range = mBind; Bind = spBind }

type SynBindingRcd  with
    member x.FromRcd
        with get() = Binding(x.Access, x.Kind, x.IsInline, x.IsMutable, x.Attributes, x.XmlDoc, x.ValData, x.Pattern, x.ReturnInfo, x.Expr, x.Range, x.Bind)

let mkId name = Ident(name, range.Zero)
let mkQualifiedNameOfFile name = QualifiedNameOfFile(mkId name)

type SynTypeDefnSimpleReprUnionRcd = {
    Access: SynAccess option
    Cases: SynUnionCases
    Range: range }

type SynTypeDefnSimpleReprEnumRcd = {
    Cases: SynEnumCases
    Range: range }

type SynTypeDefnSimpleReprRecordRcd = {
    Access: SynAccess option
    Fields: SynFields
    Range: range }

type SynTypeDefnSimpleReprGeneralRcd = {
    Kind: SynTypeDefnKind
    // TODO incomplete
    // (SynType * range * Ident option) list
    // (SynValSig * MemberFlags) list
    // SynField list
    // bool
    // bool
    // SynSimplePat list option
    Range: range }

type SynTypeDefnSimpleReprLibraryOnlyILAssemblyRcd = {
    ILType: Microsoft.FSharp.Compiler.AbstractIL.IL.ILType
    Range: range }

type SynTypeDefnSimpleReprTypeAbbrevRcd = {
    ParseDetail: Microsoft.FSharp.Compiler.Ast.ParserDetail
    Type: SynType
    Range: range }

type SynTypeDefnSimpleReprNoneRcd = {
    Range: range }

[<RequireQualifiedAccess>]
type SynTypeDefnSimpleReprRcd =
    | Union of SynTypeDefnSimpleReprUnionRcd
    | Enum of SynTypeDefnSimpleReprEnumRcd
    | Record of SynTypeDefnSimpleReprRecordRcd
    | General of SynTypeDefnSimpleReprGeneralRcd
    | LibraryOnlyILAssembly of SynTypeDefnSimpleReprLibraryOnlyILAssemblyRcd
    | TypeAbbrev of SynTypeDefnSimpleReprTypeAbbrevRcd
    | None of SynTypeDefnSimpleReprNoneRcd

type SynTypeDefnSimpleRepr with
    member x.ToRcd
        with get() =
            match x with
            | SynTypeDefnSimpleRepr.Union(access, cases, range) ->
                { Access = access; Cases =  cases; Range = range }
                |> SynTypeDefnSimpleReprRcd.Union
            | SynTypeDefnSimpleRepr.Enum(cases, range) ->
                { Cases = cases; Range = range }
                |> SynTypeDefnSimpleReprRcd.Enum
            | SynTypeDefnSimpleRepr.Record(access, fields, range) ->
                { Access = access; Fields = fields; Range = range }
                |> SynTypeDefnSimpleReprRcd.Record
            | SynTypeDefnSimpleRepr.General(kind, _, _, _, _ , _, _, range) ->
                { Kind = kind; Range = range }
                |> SynTypeDefnSimpleReprRcd.General
            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(iltype, range) ->
                { ILType = iltype; Range = range }
                |> SynTypeDefnSimpleReprRcd.LibraryOnlyILAssembly
            | SynTypeDefnSimpleRepr.TypeAbbrev(parseDetail, typ, range) ->
                { ParseDetail = parseDetail; Type = typ; Range = range }
                |> SynTypeDefnSimpleReprRcd.TypeAbbrev
            | SynTypeDefnSimpleRepr.None(range) ->
                { Range = range }
                |> SynTypeDefnSimpleReprRcd.None

type SynEnumCaseRcd = {
    Attributes: SynAttributes
    Id: Ident 
    Constant: SynConst
    XmlDoc: PreXmlDoc
    Range: range }

type SynEnumCase with
    member x.ToRcd
        with get() =
            match x with
            | EnumCase(attributes, id, constant, xmlDoc, range) ->
                { Attributes = attributes; Id = id; Constant = constant; XmlDoc = xmlDoc; Range = range }
    
type XmlDoc with
    member x.Lines
        with get() =
            match x with
            | XmlDoc lines -> lines

type PreXmlDoc with
    member x.Lines
        with get() =
            x.ToXmlDoc().Lines