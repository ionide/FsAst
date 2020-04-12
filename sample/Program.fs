module FsAst.Program

[<EntryPoint>]
let main argv =

    let fs =
       __SOURCE_DIRECTORY__ +
        @"/Hello.fs"
//        @"/Enum.fs"
//        @"/PInvoke.fs"

    printfn "### printAstInfo\n"
    PrintAstInfo.printAstInfo fs

    printfn "\n### formatFs:\n"
    FormatFs.formatFs fs

    printfn "\n### create AST:\n"
    CreateAst.createBasicClass()
//    CreateAst.createBasicEnums()
//    CreateAst.createBasicPInvoke()
    0
