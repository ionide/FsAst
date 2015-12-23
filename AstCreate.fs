[<AutoOpen>]
module FsAst.AstCreate

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

type Ident with
    static member Create text =
        Ident(text, range.Zero)
    static member CreateLong (text: string) =
        text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

type LongIdentWithDots with
    static member Create texts =
        LongIdentWithDots(texts |> List.map Ident.Create, [])
    static member CreateString (text: string) =
        LongIdentWithDots(Ident.CreateLong text, [])

    member x.AsString =
        let sb = System.Text.StringBuilder()
        for i in 0 .. x.Lid.Length - 2 do
            sb.Append x.Lid.[i].idText |> ignore
            sb.Append '.' |> ignore
        sb.Append x.Lid.[x.Lid.Length-1].idText |> ignore
        sb.ToString()

type SynPatLongIdentRcd with
    static member Create (id, args) =
        { Id = id; Args = args; Access = None; Range = range.Zero }

type SynConstructorArgs with
    static member Empty =
        SynConstructorArgs.Pats[]

type SynPatRcd with
    static member CreateLongIdent (id, args: SynPatRcd list) =
        SynPatRcd.LongIdent (SynPatLongIdentRcd.Create(id, args |> List.map (fun a -> a.FromRcd) |> SynConstructorArgs.Pats ))
    static member CreateTuple patterns =
        SynPatRcd.Tuple { Patterns = patterns; Range = range.Zero }
    static member CreateAttrib (pattern, attributes) =
        SynPatRcd.Attrib { Pattern = pattern; Attributes = attributes; Range = range.Zero }
    static member CreateTyped (pattern, typ) =
        SynPatRcd.Typed { Pattern = pattern; Type = typ; Range = range.Zero }
    static member CreateNamed (id, pattern) =
        SynPatRcd.Named { Pattern = pattern; Id = id; IsThis = false; Access = None; Range = range.Zero }
    static member CreateWild =
        SynPatRcd.Wild { Range = range.Zero }

type QualifiedNameOfFile with
    static member Create name =
        QualifiedNameOfFile(Ident.Create name)

type MemberFlags with
    static member InstanceMember =
        { IsInstance = true; MemberKind = MemberKind.Member; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false }
    static member StaticMember =
        { MemberFlags.InstanceMember with IsInstance = false }

type SynConst with
    static member CreateString s =
        SynConst.String(s, range.Zero)

type SynExpr with
    static member CreateConst cnst =
        SynExpr.Const(cnst, range.Zero)
    static member CreateConstString s =
        SynExpr.CreateConst (SynConst.CreateString s)
    static member CreateTyped (expr, typ) =
        SynExpr.Typed(expr, typ, range.Zero)
    static member CreateApp (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range.Zero)
    static member CreateAppInfix (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range.Zero)
    static member CreateIdent id =
        SynExpr.Ident(id)
    static member CreateIdentString id =
        SynExpr.Ident(Ident.Create id)
    static member CreateLongIdent (isOptional, id, altNameRefCell) =
        SynExpr.LongIdent(isOptional, id, altNameRefCell, range.Zero)
    static member CreateParen expr =
        SynExpr.Paren(expr, range.Zero, None, range.Zero)
    static member CreateTuple list =
        SynExpr.Tuple(list, [], range.Zero)
    static member CreateNull =
        SynExpr.Null(range.Zero)

type SynType with
    static member CreateApp (typ, args) =
        SynType.App(typ, None, args, [], None, false, range.Zero)
    static member CreateLongIdent id =
        SynType.LongIdent(id)
    static member CreateUnit =
        SynType.CreateLongIdent(LongIdentWithDots.CreateString "unit")

type SynArgInfo with
    static member Empty =
        SynArgInfo(SynAttributes.Empty, false, None)
    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)
    static member CreateIdString id =
        SynArgInfo.CreateId(Ident.Create id)

type SynPatRcd with
    static member CreateNull =
        SynPatRcd.Null { Range = range.Zero }

type SynValInfo with
    static member Empty =
        SynValInfo([], SynArgInfo.Empty)
        

type SynBindingReturnInfoRcd with
    static member Create typ =
        { Type = typ; Range = range.Zero; Attributes = [] }

type SynBindingRcd with
    static member Null =
        {   Access = None
            Kind = SynBindingKind.NormalBinding
            IsInline = false
            IsMutable = false
            Attributes = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some MemberFlags.InstanceMember, SynValInfo.Empty, None)
            Pattern = SynPatRcd.CreateNull
            ReturnInfo = None
            Expr = SynExpr.Null range.Zero
            Range = range.Zero
            Bind = SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding
        }
    static member Let =
        { SynBindingRcd.Null with
            ValData = SynValData(None, SynValInfo([], SynArgInfo.Empty), None)
            Expr = SynExpr.CreateTyped(SynExpr.CreateNull, SynType.CreateUnit)
        }

type SynComponentInfoRcd with
    static member Create id =
        {   Attributes = SynAttributes.Empty
            Parameters = []
            Constraints = []
            Id = id
            XmlDoc = PreXmlDoc.Empty
            PreferPostfix = false
            Access = None
            Range = range.Zero
        }

type SynMemberDefn with
    static member CreateImplicitCtor() =
        SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, [], None, range.Zero)
    static member CreateMember (binding:SynBindingRcd) =
        SynMemberDefn.Member(binding.FromRcd, range.Zero)

type SynTypeDefnReprObjectModelRcd with
    static member Create members =
        {   //Kind = SynTypeDefnKind.TyconClass
            Kind = SynTypeDefnKind.TyconUnspecified
            Members = members
            Range = range.Zero
        }

type SynTypeDefnRcd with
    static member Create (info: SynComponentInfoRcd, members) =
        {   Info = info
            Repr = SynTypeDefnReprObjectModelRcd.Create(members).FromRcd
            Members = []
            Range = range.Zero
        }
    static member CreateSimple (info: SynComponentInfoRcd, simple: SynTypeDefnSimpleRepr) =
        {   Info = info
            Repr =  SynTypeDefnRepr.Simple(simple, range.Zero)
            Members = []
            Range = range.Zero
        }

type SynModuleDecl with
    static member CreateType (info, members) =
        SynModuleDecl.Types([SynTypeDefnRcd.Create(info,members).FromRcd], range.Zero)
    static member CreateSimpleType (info, simple: SynTypeDefnSimpleReprRcd) =
        SynModuleDecl.Types([SynTypeDefnRcd.CreateSimple(info, simple.FromRcd).FromRcd], range.Zero)
    static member CreateOpen id =
        SynModuleDecl.Open(id, range.Zero)
    static member CreateLet (bindings: SynBindingRcd list) =
        SynModuleDecl.Let(false, bindings |> List.map(fun b -> b.FromRcd), range.Zero)

type SynModuleOrNamespaceRcd with
    static member CreateModule id =
        {   Id = id
            IsModule = true
            Declarations = []
            XmlDoc = PreXmlDoc.Empty
            Attributes = SynAttributes.Empty
            Access = None
            Range = range.Zero
        }
    static member CreateNamespace id =
        { SynModuleOrNamespaceRcd.CreateModule id with
            IsModule = false
        }
    member x.AddDeclarations decls =
        { x with
            Declarations = List.append x.Declarations decls
        }
    member x.AddDeclaration decl =
        x.AddDeclarations [decl]

type ParsedImplFileInputRcd with
    static member CreateFs name =
        {   File = sprintf "%s.fs" name
            IsScript = false
            QualName = QualifiedNameOfFile.Create name
            Pragmas = []
            HashDirectives = []
            Modules = []
            IsLastCompiland = true
        }
    member x.AddModules (modules: SynModuleOrNamespaceRcd list) =
        { x with
            Modules = List.append x.Modules (modules |> List.map (fun m -> m.FromRcd))
        }
    member x.AddModule mdl =
        x.AddModules [mdl]

type ParsedInput with
    static member CreateImplFile (implFile: ParsedImplFileInputRcd) =
        ParsedInput.ImplFile implFile.FromRcd

type SynTypeDefnSimpleReprUnionRcd with
    static member Create cases = // TODO SynUnionCaseRcd list
        { Access = None; Cases = cases; Range = range.Zero }

type SynTypeDefnSimpleReprEnumRcd with
    static member Create (cases: SynEnumCaseRcd list) =
        { Cases = (cases |> List.map (fun c -> c.FromRcd)); Range = range.Zero }

type SynTypeDefnSimpleReprRecordRcd with
    static member Create fields = // TODO SynFieldRcd list
        { Access = None; Fields = fields; Range = range.Zero }

//type SynUnionCase with // TODO

type SynEnumCaseRcd with
    static member Create (id, cnst) =
        {   Attributes = SynAttributes.Empty
            Id = id
            Constant = cnst
            XmlDoc = PreXmlDoc.Empty
            Range = range.Zero
        }

type PreXmlDoc with
    static member Create lines =
        let dc = XmlDocCollector()
        let mutable i = 0
        for line in lines do
            let p = mkPos i 0
            dc.AddXmlDocLine(line, p)
            i <- i + 1
        PreXmlDoc.CreateFromGrabPoint(dc, mkPos i 0)