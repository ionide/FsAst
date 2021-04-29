module FsAst.Program

[<EntryPoint>]
let main argv =

    printfn "\n### create AST:\n"
    [
        CreateAst.createBasicClass
        CreateAst.createBasicEnums
        CreateAst.createBasicPInvoke
    ]
    |> List.iter (fun task -> Async.RunSynchronously(task()))
    0