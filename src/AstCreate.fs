[<AutoOpen>]
module FsAst.AstCreate
open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Xml
open FSharp.Compiler.SyntaxTrivia

type Ident with
    static member Create text =
        Ident(text, range0)
    static member CreateLong (text: string) =
        text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

type LongIdentWithDots with
    static member Create texts =
        LongIdentWithDots(texts |> List.map Ident.Create, [])
    static member CreateString (text: string) =
        LongIdentWithDots(Ident.CreateLong text, [])
    static member CreateFromLongIdent (longIdent: LongIdent) =
        LongIdentWithDots(longIdent, [])

    member x.AsString =
        let sb = Text.StringBuilder()
        for i in 0 .. x.Lid.Length - 2 do
            sb.Append x.Lid.[i].idText |> ignore
            sb.Append '.' |> ignore
        sb.Append x.Lid.[x.Lid.Length-1].idText |> ignore
        sb.ToString()

type SynPatLongIdentRcd with
    static member Create (id, args, ?propertyKeyword, ?extraId, ?typarDecls, ?access) =
        { Id = id; PropertyKeyword = propertyKeyword; ExtraId = extraId; TyparDecls = typarDecls ; Args = args; Access = access; Range = range0 }

type SynArgPats with
    static member Empty =
        SynArgPats.Pats[]

type SynPatRcd with
    static member CreateLongIdent (id, args: SynPatRcd list, ?access) =
        SynPatRcd.LongIdent ( {SynPatLongIdentRcd.Create(id, args |> List.map (fun a -> a.FromRcd) |> SynArgPats.Pats ) with Access = access } )
    static member CreateTuple patterns =
        SynPatRcd.Tuple { Patterns = patterns; Range = range0 }
    static member CreateParen pattern =
        SynPatRcd.Paren { Pattern = pattern; Range = range0 }
    static member CreateAttrib (pattern, attributes) =
        SynPatRcd.Attrib { Pattern = pattern; Attributes = attributes; Range = range0 }
    static member CreateTyped (pattern, typ) =
        SynPatRcd.Typed { Pattern = pattern; Type = typ; Range = range0 }
    static member CreateNamed id =
        SynPatRcd.Named { Id = id; IsThis = false; Access = None; Range = range0 }
    static member CreateWild =
        SynPatRcd.Wild { Range = range0 }

type QualifiedNameOfFile with
    static member Create name =
        QualifiedNameOfFile(Ident.Create name)

type SynMemberFlags with
    static member InstanceMember : SynMemberFlags =
        { IsInstance = true; MemberKind = SynMemberKind.Member; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false; Trivia = SynMemberFlagsTrivia.Zero }
    static member StaticMember =
        { SynMemberFlags.InstanceMember with IsInstance = false }

type SynConst with
    /// Creates a <see href="SynStringKind.Regular">Regular</see> string
    static member CreateString s =
        SynConst.String(s, SynStringKind.Regular, range0)

type SynExpr with
    static member CreateConst cnst =
        SynExpr.Const(cnst, range0)
    static member CreateConstString s =
        SynExpr.CreateConst (SynConst.CreateString s)
    static member CreateTyped (expr, typ) =
        SynExpr.Typed(expr, typ, range0)
    static member CreateApp (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range0)
    static member CreateAppInfix (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range0)
    static member CreateIdent id =
        SynExpr.Ident(id)
    static member CreateIdentString id =
        SynExpr.Ident(Ident.Create id)
    static member CreateLongIdent (isOptional, id, altNameRefCell) =
        SynExpr.LongIdent(isOptional, id, altNameRefCell, range0)
    static member CreateLongIdent id =
        SynExpr.CreateLongIdent(false, id, None)
    static member CreateParen expr =
        SynExpr.Paren(expr, range0, None, range0)
    static member CreateTuple list =
        SynExpr.Tuple(false, list, [], range0)
    static member CreateParenedTuple list =
        SynExpr.CreateTuple list
        |> SynExpr.CreateParen
    static member CreateUnit =
        SynExpr.CreateConst SynConst.Unit
    static member CreateNull =
        SynExpr.Null(range0)
    static member CreateRecord (fields: list<RecordFieldName * option<SynExpr>>) =
        let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range0)
    static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates) =
        let fields = fieldUpdates |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range0)
    static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates ) =
        let blockSep = (range0, None) : BlockSeparator
        let copyInfo = Some (copyInfo, blockSep)
        SynExpr.Record (None, copyInfo, fieldUpdates, range0)
    /// Creates:
    ///
    /// ```
    /// match matchExpr with
    /// | clause1
    /// | clause2
    /// ...
    /// | clauseN
    /// ```
    static member CreateMatch(matchExpr, clauses) =
        SynExpr.Match(range0, DebugPointAtBinding.Yes range0, matchExpr, range0, clauses, range0)
    /// Creates : `instanceAndMethod(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        SynExpr.CreateApp(valueExpr, args)
    /// Creates : `instanceAndMethod()`
    static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots) =
        SynExpr.CreateInstanceMethodCall(instanceAndMethod, SynExpr.CreateUnit)
    /// Creates : `instanceAndMethod<type1, type2,... type}>(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots, instanceMethodsGenericTypes, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, instanceMethodsGenericTypes, [], None, range0, range0)
        SynExpr.CreateApp(valueExprWithType, args)
    /// Creates: expr1; expr2; ... exprN
    static member CreateSequential exprs =
        let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.SuppressBoth, false, expr1, expr2, range0)
        let rec inner exprs state =
            match state, exprs with
            | None, [] -> SynExpr.CreateConst SynConst.Unit
            | Some expr, [] -> expr
            | None, [single] -> single
            | None, [one;two] -> seqExpr one two
            | Some exp, [single] -> seqExpr exp single
            | None, head::shoulders::tail ->
                seqExpr head shoulders
                |> Some
                |> inner tail
            | Some expr, head::tail ->
                seqExpr expr head
                |> Some
                |> inner tail
        inner exprs None


type SynType with
    static member CreateApp (typ, args, ?isPostfix) =
        SynType.App(typ, None, args, [], None, (defaultArg isPostfix false), range0)
    static member CreateLongIdent id =
        SynType.LongIdent(id)
    static member CreateLongIdent s =
        SynType.CreateLongIdent(LongIdentWithDots.CreateString s)
    static member CreateUnit =
        SynType.CreateLongIdent("unit")
    static member CreateFun (fieldTypeIn, fieldTypeOut) =
        SynType.Fun (fieldTypeIn, fieldTypeOut, range0)

    static member Create(name: string) = SynType.CreateLongIdent name

    static member Option(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member ResizeArray(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "ResizeArray",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Set(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Set",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member NativePointer(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "nativeptr",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Option(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Dictionary(key, value) =
        SynType.App(
            typeName=SynType.LongIdent(LongIdentWithDots.Create [ "System"; "Collections"; "Generic"; "Dictionary" ]),
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Map(key, value) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Map",
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member List(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Array(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "array",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member List(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member DateTimeOffset() =
        SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTimeOffset" ])

    static member DateTime() =
        SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTime" ])

    static member Guid() =
        SynType.LongIdent(LongIdentWithDots.Create [ "System"; "Guid" ])

    static member Int() =
        SynType.Create "int"

    static member UInt() =
        SynType.Create "uint"

    static member Int8() =
        SynType.Create "int8"

    static member UInt8() =
        SynType.Create "uint8"

    static member Int16() =
        SynType.Create "int16"

    static member UInt16() =
        SynType.Create "uint16"

    static member Int64() =
        SynType.Create "int64"

    static member UInt64() =
        SynType.Create "uint64"

    static member String() =
        SynType.Create "string"

    static member Bool() =
        SynType.Create "bool"

    static member Float() =
        SynType.Create "float"

    static member Float32() =
        SynType.Create "float32"

    static member Double() =
        SynType.Create "float"

    static member Decimal() =
        SynType.Create "decimal"

    static member Unit() =
        SynType.Create "unit"

    static member BigInt() =
        SynType.Create "bigint"

    static member Byte() =
        SynType.Create "byte"

    static member Char() =
        SynType.Create "char"

type SynArgInfo with
    static member Empty =
        SynArgInfo(SynAttributes.Empty, false, None)
    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)
    static member CreateIdString id =
        SynArgInfo.CreateId(Ident.Create id)

type SynPatRcd with
    static member CreateNull =
        SynPatRcd.Null { Range = range0 }

type SynValInfo with
    static member Empty =
        SynValInfo([], SynArgInfo.Empty)

type SynBindingReturnInfoRcd with
    static member Create typ =
        { Type = typ; Range = range0; Attributes = [] }

type SynBindingRcd with
    static member Null =
        {   Access = None
            Kind = SynBindingKind.Normal
            IsInline = false
            IsMutable = false
            Attributes = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some SynMemberFlags.InstanceMember, SynValInfo.Empty, None)
            Pattern = SynPatRcd.CreateNull
            ReturnInfo = None
            Expr = SynExpr.Null range0
            Range = range0
            Bind = DebugPointAtBinding.NoneAtInvisible
        }
    static member Let =
        { SynBindingRcd.Null with
            ValData = SynValData(None, SynValInfo([], SynArgInfo.Empty), None)
            Expr = SynExpr.CreateTyped(SynExpr.CreateNull, SynType.CreateUnit)
        }

type SynComponentInfoRcd with
    static member Create id =
        {   Attributes = SynAttributes.Empty
            Parameters = None
            Constraints = []
            Id = id
            XmlDoc = PreXmlDoc.Empty
            PreferPostfix = false
            Access = None
            Range = range0
        }

type SynMemberDefn with
    static member CreateImplicitCtor (ctorArgs) =
        SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, SynSimplePats.SimplePats(ctorArgs, range0), None, PreXmlDoc.Empty, range0 )
    static member CreateImplicitCtor() =
        SynMemberDefn.CreateImplicitCtor []

    /// <summary>
    /// Creates an instance member from a binding definition: [member {binding} = {expr}]
    /// where {binding} = {this.pattern args} and {expr} is the body of the binding
    /// </summary>
    static member CreateMember (binding:SynBindingRcd) =
        SynMemberDefn.Member(binding.FromRcd, range0)

    /// <summary>
    /// Creates a member from a binding definition: [static member {binding} = {expr}]
    /// where {binding} = {pattern args} and {expr} is the body of the static binding
    /// </summary>
    static member CreateStaticMember(binding:SynBindingRcd) =
        let (SynValData(usedMemberFlags, valInfo, identifier)) = binding.ValData
        let staticMemberFlags: SynMemberFlags option = Some {
            // this means the member is static
            IsInstance = false;
            IsOverrideOrExplicitImpl = false
            IsDispatchSlot = false;
            IsFinal = false
            MemberKind = SynMemberKind.Member
            Trivia = SynMemberFlagsTrivia.Zero
        }
        let staticBinding = { binding with ValData = SynValData.SynValData(staticMemberFlags, valInfo, identifier) }
        SynMemberDefn.Member(staticBinding.FromRcd, range0)

    /// <summary>
    /// Creates an instance member from a binding definition: [override {binding} = {expr}]
    /// where {binding} = {this.pattern args} and {expr} is the body of the static binding
    /// </summary>
    static member CreateOverrideMember(binding:SynBindingRcd) =
        let (SynValData(usedMemberFlags, valInfo, identifier)) = binding.ValData
        let overrideMemberFlags: SynMemberFlags option = Some {
            IsInstance = true;
            IsOverrideOrExplicitImpl = true
            IsDispatchSlot = false;
            IsFinal = false
            MemberKind = SynMemberKind.Member
            Trivia = SynMemberFlagsTrivia.Zero
        }
        let overrideBinding = { binding with ValData = SynValData.SynValData(overrideMemberFlags, valInfo, identifier) }
        SynMemberDefn.Member(overrideBinding.FromRcd, range0)

    static member CreateInterface(interfaceType, members) =
        SynMemberDefn.Interface(interfaceType, None, members, range0)

type SynTypeDefnReprObjectModelRcd with
    static member Create members =
        {   //Kind = SynTypeDefnKind.TyconClass
            Kind = SynTypeDefnKind.Unspecified
            Members = members
            Range = range0
        }

type SynTypeDefnRcd with
    static member Create (info: SynComponentInfoRcd, members) =
        {   Info = info
            Repr = SynTypeDefnReprObjectModelRcd.Create(members).FromRcd
            Members = []
            ImplicitConstructor = None
            Range = range0
        }
    static member CreateSimple (info: SynComponentInfoRcd, simple: SynTypeDefnSimpleRepr, ?members) =
        {   Info = info
            Repr =  SynTypeDefnRepr.Simple(simple, range0)
            Members = Option.defaultValue [] members
            ImplicitConstructor = None
            Range = range0
        }

type SynModuleDecl with
    static member CreateType (info, members) =
        SynModuleDecl.Types([SynTypeDefnRcd.Create(info, members).FromRcd], range0)
    static member CreateSimpleType (info, simple: SynTypeDefnSimpleReprRcd, ?members) =
        SynModuleDecl.Types( [SynTypeDefnRcd.CreateSimple(info, simple.FromRcd, members = Option.defaultValue [] members).FromRcd], range0)
    static member CreateOpen id =
        SynModuleDecl.Open(id, range0)
    static member CreateOpen (fullNamespaceOrModuleName: string) =
        SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(Ident.CreateLong fullNamespaceOrModuleName, range0), range0)
    static member CreateHashDirective (directive, values) =
        SynModuleDecl.HashDirective (ParsedHashDirective (directive, values, range0), range0)
    static member CreateLet (bindings: SynBindingRcd list) =
        SynModuleDecl.Let(false, bindings |> List.map(fun b -> b.FromRcd), range0)
    static member CreateAttribute(ident, expr, isProp, ?target) =
            { SynAttribute.TypeName = ident
              SynAttribute.ArgExpr = expr
              SynAttribute.Target = target
              SynAttribute.AppliesToGetterAndSetter = isProp
              SynAttribute.Range = range0 }
    static member CreateAttributes(attributes) =
        SynModuleDecl.Attributes(attributes, range0)
    static member CreateNestedModule(info : SynComponentInfoRcd, members) =
        SynModuleDecl.NestedModule(info.FromRcd, false, members, false, range0, SynModuleDeclNestedModuleTrivia.Zero)
    static member CreateTypes (types: SynTypeDefnRcd list) =
        SynModuleDecl.Types(types |> List.map (fun t -> t.FromRcd), range0)

type SynModuleOrNamespaceRcd with
    static member CreateModule id =
        {   Id = id
            IsRecursive = false
            Kind = SynModuleOrNamespaceKind.NamedModule
            Declarations = []
            XmlDoc = PreXmlDoc.Empty
            Attributes = SynAttributes.Empty
            Access = None
            Range = range0
        }
    static member CreateNamespace id =
        { SynModuleOrNamespaceRcd.CreateModule id with
            Kind = SynModuleOrNamespaceKind.DeclaredNamespace
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
            IsExe = false
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

type SynTypeDefnSimpleReprEnumRcd with
    static member Create (cases: SynEnumCaseRcd list) =
        { Cases = cases |> List.map (fun c -> c.FromRcd)
          Range = range0 }

type SynTypeDefnSimpleReprRecordRcd with
    static member Create (fields: SynFieldRcd list) =
        { Access = None
          Fields = (fields |> List.map (fun f -> f.FromRcd))
          Range = range0 }

type SynTypeDefnSimpleReprUnionRcd with
    static member Create cases =
        { Access = None; Cases = cases; Range = range0 }

    static member Create (fields: SynUnionCaseRcd list) : SynTypeDefnSimpleReprUnionRcd=
        { Access = None
          Cases = fields |> List.map (fun f -> f.FromRcd)
          Range = range0 }

type SynUnionCaseRcd with
    static member Create(id, typ) : SynUnionCaseRcd =
        { Attributes = SynAttributes.Empty
          Id = id
          Type = typ
          XmlDoc = PreXmlDoc.Empty
          Access = None
          Range = range0 }

type SynUnionCaseKind with
    static member Create(synFieldList : SynFieldRcd list) =
        SynUnionCaseKind.Fields(synFieldList |> List.map (fun sf -> sf.FromRcd))

type SynEnumCaseRcd with
    static member Create (id, cnst) =
        {   Attributes = SynAttributes.Empty
            Id = id
            Constant = cnst
            ValueRange = Range.Zero
            XmlDoc = PreXmlDoc.Empty
            Range = range0
        }

type SynFieldRcd with
    static member Create(id, typ, ?isMutable) : SynFieldRcd =
        let isMutable = defaultArg isMutable false
        {   Attributes = SynAttributes.Empty
            IsStatic = false
            Id = Some id
            Type = typ
            IsMutable = isMutable
            XmlDoc = PreXmlDoc.Empty
            Access = None
            Range = range0
        }
    static member Create(id, typ) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent typ)
    static member CreateInt(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "int")
    static member CreateIntOption(id) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "int" ]) ]
    static member CreateString(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "string")
    static member CreateStringOption(id) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "string" ]) ]
    static member CreateFloat(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "float")
    static member CreateFloatOption(id) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "float" ]) ]
    static member CreateBool(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "bool")
    static member CreateBoolOption(id) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "bool" ]) ]
    static member CreateDecimal(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "decimal")
    static member CreateDecimalOption(id) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "decimal" ]) ]
    static member CreateOption(id, optional) =
        SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ optional ]) ]
    static member CreateApp id typ args =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateApp(SynType.CreateLongIdent typ, args |> List.map (SynType.CreateLongIdent)))

type SynAttributeList with
    static member Create(attrs): SynAttributeList =
        {
            Attributes = attrs
            Range = range0
        }

    static member Create(attr): SynAttributeList =
        {
            Attributes = [ attr ]
            Range = range0
        }

    static member Create([<ParamArray>] attrs): SynAttributeList =
        {
            Attributes = List.ofArray attrs
            Range = range0
        }

type SynAttribute with
    static member Create(name: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Unit, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
        }

    static member Create(name: string, argument: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
        }

    static member Create(name: string, argument: bool) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
        }

    static member Create(name: string, argument: int) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
        }

    static member Create(name: string, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
        }

    static member Create(name: Ident, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots([ name ], [ ])
        }

    static member Create(name: Ident list, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots(name, [ ])
        }

    static member RequireQualifiedAccess() =
        SynAttribute.Create("RequireQualifiedAccess")

    static member CompiledName(valueArg: string) =
        SynAttribute.Create("CompiledName", valueArg)

type PreXmlDoc with
    static member Create (lines: string list) =
        let lines = List.toArray lines
        let lineMaxIndex = Array.length lines - 1
        let s = mkPos 0 0
        let e = mkPos lineMaxIndex 0
        let containingRange = mkRange "" s e
        PreXmlDoc.Create(lines, containingRange)

    static member Create (docs: string option) =
        PreXmlDoc.Create [
            if docs.IsSome
            then docs.Value
        ]

    static member Create(docs: string) =
        PreXmlDoc.Create [
            if not (String.IsNullOrWhiteSpace docs)
            then docs
        ]

type SynSimplePat with
    static member CreateTyped(ident, ``type``) =
        let ssp = SynSimplePat.Id(ident, None, false, false, false, range0)
        SynSimplePat.Typed(ssp, ``type``, range0 )

    static member CreateId(ident, ?altNameRefCell, ?isCompilerGenerated, ?isThis, ?isOptional) =
        SynSimplePat.Id(ident, altNameRefCell,
                        Option.defaultValue false isCompilerGenerated,
                        Option.defaultValue false isThis,
                        Option.defaultValue false isOptional,
                        range0)

type SynSimplePats with
    static member Create(patterns) =
        SynSimplePats.SimplePats(patterns, range0)
