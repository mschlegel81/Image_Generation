..\lazarus64\fpc\3.0.2\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64
..\lazarus64\lazbuild.exe --bm=Release im\imageExplorer.lpi
move im\imageExplorer.exe ..\art.bin\
..\lazarus64\lazbuild.exe --bm=Release display\display.lpi
move display\imig.exe ..\art.bin\
..\lazarus64\fpc\3.0.2\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64