@echo off
rem JWasm used to create HimemX.exe & HimemX2.exe
jwasm.exe -nologo -mz -Sg -Sn -Fl=Release\HimemX.LST -Fo=Release\HimemX.exe HimemX.asm
jwasm.exe -nologo -mz -Sg -Sn -D?ALTSTRAT=1 -Fl=Release\HimemX2.LST -Fo=Release\HimemX2.exe HimemX.asm
