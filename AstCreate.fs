[<AutoOpen>]
module FsAst.AstCreate

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

type Ident with
    static member Create text =
        Ident(text, range.Zero)
    /// create a LongIdent with 1 ID
    static member Create1 a =
        [Ident.Create a]
    /// create a LongIdent with 2 IDs
    static member Create2 a b =
        [Ident.Create a; Ident.Create a]

type LongIdentWithDots with
    static member Create texts =
        LongIdentWithDots(texts |> List.map Ident.Create, [range.Zero])
    /// create with 1 ID
    static member Create1 a =
        LongIdentWithDots([Ident.Create a], [range.Zero])
    /// create with 2 IDs
    static member Create2 a b =
        LongIdentWithDots([Ident.Create a; Ident.Create b], [range.Zero])

type SynPat with
    static member CreateLongIdent texts =
        SynPat.LongIdent(LongIdentWithDots.Create texts, None, None, SynConstructorArgs.Pats[], None, range.Zero)
    static member CreateLongIdent1 a =
        SynPat.LongIdent(LongIdentWithDots.Create1 a, None, None, SynConstructorArgs.Pats[], None, range.Zero)
    static member CreateLongIdent2 a b =
        SynPat.LongIdent(LongIdentWithDots.Create2 a b, None, None, SynConstructorArgs.Pats[], None, range.Zero)

type QualifiedNameOfFile with
    static member Create name =
        QualifiedNameOfFile(Ident.Create name)

type MemberFlags with
    static member InstanceMember =
        { IsInstance = true; MemberKind = MemberKind.Member; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false }
    static member StaticMember =
        { MemberFlags.InstanceMember with IsInstance = false }

type SynExpr with
    static member CreateConst cnst =
        SynExpr.Const(cnst, range.Zero)

type SynBindingRcd with
    static member Null =
        {   Access = None
            Kind = SynBindingKind.NormalBinding
            IsInline = false
            IsMutable = false
            Attributes = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some MemberFlags.InstanceMember, SynValInfo([], SynArgInfo(SynAttributes.Empty, false, None)), None)
            Pattern = SynPat.Null range.Zero
            ReturnInfo = None
            Expr = SynExpr.Null range.Zero
            Range = range.Zero
            Bind = SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding
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
    static member Create (info:SynComponentInfoRcd, members) =
        {   Info = info.FromRcd
            Repr = SynTypeDefnReprObjectModelRcd.Create(members).FromRcd
            Members = []
            Range = range.Zero
        }

type SynModuleDecl with
    static member CreateType (info, members) =
        SynModuleDecl.Types([SynTypeDefnRcd.Create(info,members).FromRcd], range.Zero)

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