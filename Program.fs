module FsAst.Program

[<EntryPoint>]
let main argv =
    
    let filename =
//        @"..\..\Hello.fs"
        @"..\..\Enum.fs"

    printfn "### printAstInfo\n"
    PrintAstInfo.printAstInfo filename

//    printfn "\n### formatFs:\n"
//    FormatFs.formatFs()
//
//    printfn "\n### createAst:\n"
//    CreateAst.createAst()
    0
