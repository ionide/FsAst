$version = '0.2.0' # the version under development, update after a release
$versionSuffix = '-build.0' # manually incremented for local builds

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

if ($env:appveyor){
    $versionSuffix = '-build.' + $env:appveyor_build_number
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
        $versionSuffix = ''
    }
    Update-AppveyorBuild -Version "$version$versionSuffix"
}

dotnet build -c Release FsAst.sln /p:Version=$version$versionSuffix
# dotnet test --no-build -c Release FsAst.Test.fsproj
dotnet pack --no-build -c Release FsAst.fsproj /p:Version=$version$versionSuffix -o $psscriptroot/bin