module ConsoleApp.Program

[<EntryPoint>]
let main argv =
    printfn "### printAstInfo\n"
    PrintAstInfo.printAstInfo()

    printfn "\n### formatFs:\n"
    FormatFs.formatFs()

    printfn "\n### createAst:\n"
    CreateAst.createAst()
    0
