module FsAst.Program

[<EntryPoint>]
let main argv =

    let fs =
       __SOURCE_DIRECTORY__ +
        @"/Hello.fs"
//        @"/Enum.fs"
//        @"/PInvoke.fs"

    let checker = Fantomas.FakeHelpers.sharedChecker.Value

    printfn "### printAstInfo\n"
    PrintAstInfo.printAstInfo fs checker
    |> Async.RunSynchronously

    printfn "\n### formatFs:\n"
    FormatFs.formatFs fs checker
    |> Async.RunSynchronously

    printfn "\n### create AST:\n"
    CreateAst.createBasicClass()
    |> Async.RunSynchronously
//    CreateAst.createBasicEnums()
//    CreateAst.createBasicPInvoke()
    0
