# FsAst

[![NuGet](https://img.shields.io/nuget/v/FsAst.svg)](https://www.nuget.org/packages/FsAst/)

Set of helpers for creating and manipulating F# AST.

FsAst is part of Ionide tooling suite - You can support its development on [Open Collective](https://opencollective.com/ionide).

[![open collective backers](https://img.shields.io/opencollective/backers/ionide.svg?color=blue)](https://opencollective.com/ionide)
[![open collective sponsors](https://img.shields.io/opencollective/sponsors/ionide.svg?color=blue)](https://opencollective.com/ionide)

[![Open Collective](https://opencollective.com/ionide/donate/button.png?color=blue)](https://opencollective.com/ionide)

## Useful links

* Original blog post about the project by [@ctaggart](https://github.com/ctaggart) - http://blog.ctaggart.com/2014/09/generating-f-code-using-its-ast.html
* Applied Meta-Programming with Myriad and Falanax by [@7sharp9](https://github.com/7sharp9)  - https://7sharp9.dev/2019/04/24/2019-04-24-applied-metaprogramming-with-myriad/  **(Although Myriad no longer uses FsAst)**
* Sample for printing out generated code with Fantomas - https://github.com/ionide/FsAst/tree/master/sample

## Build process

 * You need [.NET Core SDK](https://dotnet.microsoft.com/download/dotnet-core/) - check the exact version in [global.json](global.json)
 * Run `dotnet tool restore` to restore the .NET Core 3 local tools defined at .config/dotnet-tools.json
 * To build the project run `dotnet build`

## Special thanks

* [@ctaggart](https://github.com/ctaggart) - Original creator and maintainer of the FsAst.


## How to contribute

*Imposter syndrome disclaimer*: I want your help. No really, I do.

There might be a little voice inside that tells you you're not ready; that you need to do one more tutorial, or learn another framework, or write a few more blog posts before you can help me with this project.

I assure you, that's not the case.

This project has some clear Contribution Guidelines and expectations that you can [read here](https://github.com/Ionide/FsAst/blob/master/CONTRIBUTING.md).

The contribution guidelines outline the process that you'll need to follow to get a patch merged. By making expectations and process explicit, I hope it will make it easier for you to contribute.

And you don't just have to write code. You can help out by writing documentation, tests, or even by giving feedback about this work. (And yes, that includes giving feedback about the contribution guidelines.)

Thank you for contributing!


## Contributing and copyright

The project is hosted on [GitHub](https://github.com/Ionide/FsAst) where you can [report issues](https://github.com/Ionide/FsAst/issues), fork
the project and submit pull requests. Please read [Contribution Guide](https://github.com/Ionide/FsAst/blob/master/CONTRIBUTING.md)

The library is available under [MIT license](https://github.com/Ionide/FsAst/blob/master/LICENSE.md), which allows modification and redistribution for both commercial and non-commercial purposes.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
