..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc > build.log 2>&1
..\lazarus64\lazbuild.exe --bm=Release im\imageExplorer.lpi >> build.log 2>&1
move im\imageExplorer.exe ..\art.bin\ 
..\lazarus64\lazbuild.exe --bm=Release display\display.lpi >> build.log 2>&1
copy display\imig.exe c:\bin\
move display\imig.exe ..\art.bin\
..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc >> build.log 2>&1
type build.log