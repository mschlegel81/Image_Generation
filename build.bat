..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe imig\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc > build.log 2>&1
@rem ..\lazarus64\lazbuild.exe --bm=Release im\imageExplorer.lpi >> build.log 2>&1
@rem move im\imageExplorer.exe ..\art.bin\ 
..\lazarus64\lazbuild.exe --bm=Release imig\imig.lpi >> build.log 2>&1
move imig\imig.exe ..\art.bin\
..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe imig\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc >> build.log 2>&1
type build.log