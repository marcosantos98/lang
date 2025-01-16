function fail_on_err {
	param(
		[string] $command,
		[string] $label,
		[bool] $newline = $true
	)
	Write-Host "> " -ForegroundColor Green -NoNewline
	Write-Host $command
	if ( $newline -eq "true" ) {
	    Write-Host ""
	}
	Invoke-Expression $command
	if ($LastExitCode -ne 0) {
		Write-Host $label -ForegroundColor Red
		exit 1
	}
}

if ( $args[0] -eq "rec") {
    Get-ChildItem -Path "./tests/" -Filter "*.lang" | ForEach-Object {
	./lang.exe build $($_.FullName)
    }
    Get-ChildItem -Path "./tests/" -Filter "*.cpp" | ForEach-Object {
	mv -Force $($_.FullName) "$($_.FullName).out"	
    }
} elseif ( $args[0] -eq "test") {
    Write-Host "Transpile all tests" -ForegroundColor Red
    Get-ChildItem -Path "./tests/" -Filter "*.lang" | ForEach-Object {
	fail_on_err "./lang.exe build $($_.FullName)" "Failed to compile $($_.FullName)" -newline $false
    }
} else {
    $release = $false
    $show_timings = $true
    $trace = $false
    #$trace = $true
    $flags = "-warnings-as-errors -vet-unused -vet-shadowing -vet-packages:main"
    if ($show_timings -eq $true) {
    	$flags += " -show-timings"
    }
    if ($release -eq $false) {
    	$flags += " -debug"
    }
    if ($trace -eq $true) {
	$flags += " -define=trace=true"
    }
    
    Write-Host "Options:" -ForegroundColor Magenta
    Write-Host "Release: $release" 
    Write-Host "Args:" -ForegroundColor Magenta
    Write-Host "$($args)"
    
    Write-Host "Building:" -ForegroundColor Magenta
    fail_on_err "odin build . $flags" "Failed to build."
    Write-Host "===================================================="    
    ./lang.exe $($args)
}

