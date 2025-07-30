@echo off
REM Setup MSVC environment with 20022
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"

REM Run cmake command - adjust args as needed
REM cmake -B build -G Ninja -DCMAKE_C_COMPILER=clang-cl.exe -DCMAKE_CXX_COMPILER=clang-cl.exe -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .

cmake -B build -G Ninja -DCMAKE_C_COMPILER=cl.exe -DCMAKE_CXX_COMPILER=cl.exe -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DAC_API_DEVKIT_DIR="./acapi28/support" -DAC_VERSION=28 .

REM Optionally, build the project immediately
cmake --build build

REM Pause so you can see output before window closes (optional)
pause
