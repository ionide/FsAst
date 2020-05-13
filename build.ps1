$version = '0.2.1' # the version under development, update after a release

dotnet tool restore
dotnet build -c Release FsAst.sln /p:Version=$version
# dotnet test --no-build -c Release FsAst.Test.fsproj
dotnet pack --no-build -c Release src/FsAst.fsproj /p:Version=$version -o $psscriptroot/bin