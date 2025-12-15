@echo off
REM Run unit tests for dired-video-thumbnail on Windows

setlocal

set SCRIPT_DIR=%~dp0
set PROJECT_DIR=%SCRIPT_DIR%..

echo Running dired-video-thumbnail unit tests...
echo Project directory: %PROJECT_DIR%
echo Test directory: %SCRIPT_DIR%
echo.

REM Run Emacs in batch mode with the tests
emacs -Q --batch ^
    --eval "(add-to-list 'load-path \"%PROJECT_DIR:\=/%\")" ^
    --eval "(add-to-list 'load-path \"%SCRIPT_DIR:\=/%\")" ^
    -l "%PROJECT_DIR:\=/%/dired-video-thumbnail.el" ^
    -l "%SCRIPT_DIR:\=/%/dired-video-thumbnail-test.el" ^
    -f ert-run-tests-batch-and-exit

if %ERRORLEVEL% equ 0 (
    echo.
    echo All tests passed!
) else (
    echo.
    echo Some tests failed (exit code: %ERRORLEVEL%^)
)

exit /b %ERRORLEVEL%
