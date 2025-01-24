@echo off

if "%1" == "test" (
    for /r %%f in (tests\*.lang) do (
	.\lang.exe build %%f || exit /b
    )
) else (
    set trace=false
    set debug=true
    set flags=-show-timings
    
    if "%trace%" == "true" set flags=%flags% -define:trace=true
    if "%debug%" == "true" set flags=%flags% -debug
    
    echo Flags: %flags%
    
    odin build . %flags%
    if %errorlevel% neq 0 (
        echo Failed compilation.
        exit /b
    )
    
    .\lang.exe %*
)
