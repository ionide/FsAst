[<AutoOpen>]
module ConsoleApp.CreateAst

open Fantomas
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let createAst() =

    // create member
    let memberFlags : MemberFlags = {IsInstance = true; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false; MemberKind = MemberKind.Member}
    let b : SynBindingRcd = 
        {   Access = None
            Kind = SynBindingKind.NormalBinding
            IsInline = false
            IsMutable = false
            Attribs = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some memberFlags, SynValInfo([], SynArgInfo(SynAttributes.Empty, false, None)), None)
            Pat = SynPat.LongIdent(LongIdentWithDots([mkId "x"; mkId "Points"], [range.Zero]), None, None, SynConstructorArgs.Pats[], None, range.Zero)
            ReturnInfo = None
            Expr = SynExpr.Const(SynConst.Int32 3, range.Zero)
            Range = range.Zero
            Bind = SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding
        }

    // create Type
    let ti : SynComponentInfoRcd = 
        {   Attribs = SynAttributes.Empty
            TyParams = []
            Constraints = []
            Id = [mkId "Triangle"]
            XmlDoc = PreXmlDoc.Empty
            PreferPostfix = false
            Access = None
            Range = range.Zero
        }

    let ms : SynMemberDefns = 
        [
            SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, [], None, range.Zero)
            SynMemberDefn.Member(b.FromRcd, range.Zero)
        ]

    let r : SynTypeDefnReprObjectModelRcd = 
        {   //Kind = SynTypeDefnKind.TyconClass
            Kind = SynTypeDefnKind.TyconUnspecified
            Members = ms
            Range = range.Zero
        }

    let t : SynTypeDefnRcd = 
        {   Info = ti.FromRcd
            Repr = r.FromRcd
            Members = []
            Range = range.Zero
        }

    // create module
    let m : SynModuleOrNamespaceRcd = 
        {   Id = [mkId "Hello"]
            IsModule = true
            Decls = [SynModuleDecl.Types([t.FromRcd], range.Zero)]
            XmlDoc = PreXmlDoc.Empty
            Attribs = SynAttributes.Empty
            Access = None
            Range = range.Zero
        }

    // create file
    let pi : ParsedImplFileInputRcd = 
        {   File = "Hello.fs"
            IsScript = false
            QualName = QualifiedNameOfFile(mkId "Hello")
            Pragmas = []
            HashDirectives = []
            Modules = [m.FromRcd]
            IsLastCompiland = true
        }

    let txt = formatAst (ParsedInput.ImplFile pi.FromRcd)
    printfn "%s" txt