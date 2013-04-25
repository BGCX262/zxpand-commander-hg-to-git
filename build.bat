REM you may need to change this
set TASMDIR=c:\bin

REM name of the source
set ASM=CMDR

setlocal
set TASMTABS=%TASMDIR%
%TASMDIR%\tasm -80 -b %ASM%.asm %ASM%.p

pause
