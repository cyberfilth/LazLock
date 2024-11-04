#!/usr/bin/env pwsh
##############################################################################################################

Function PrivClipper {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function PrivWget {
    ForEach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @params | Out-Null
        Return $params.OutFile
    }
}

Function PrivMsiexec {
    While ($Input.MoveNext()) {
        Start-Process -PassThru -Wait -FilePath $Input.Current -ArgumentList '/SP-', '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART'
        Remove-Item $Input.Current
    }
}

Function PrivPrepare {
    $VAR = @(
        @{
            Cmd = 'lazbuild'
            Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
            Path = "C:\Lazarus"
        }
    )
    ForEach ($REPLY in $VAR) {
        If (-not (Get-Command $REPLY.Cmd -ea 'continue')) {
            PrivWget $REPLY.Url | PrivMsiexec
            $env:PATH+=";$($REPLY.Path)"
            Get-Command $REPLY.Cmd
        }
    }
}

Function PrivPackages {
    If ( Test-Path -Path 'use' ) {
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--init'
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--remote'
    } Else {
        New-Item -ItemType Directory -Name 'use'
    }
    If ($args.count -gt 0) {
        ForEach ($REPLY in $args) {
            PrivWget "https://packages.lazarus-ide.org/$($REPLY).zip" | Expand-Archive -DestinationPath "use/$($REPLY).zip" -Force
            Remove-Item "$($REPLY).zip"
        }
    }
    Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
        Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--add-package-link', $_.Name
    }
}

Function PrivMain {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        PrivPrepare
        Switch ($args[0]) {
            'build' {
                PrivPackages 'PoweredBy' 'EyeCandyControls' 'splashabout'
                Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
                    Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--no-write-project', '--recursive', '--no-write-project', '--build-mode=release', $_.Name
                }
            }
            Default {
                PrivClipper
            }
        }
    } Else {
        PrivClipper
    }
}

##############################################################################################################
PrivMain @args
