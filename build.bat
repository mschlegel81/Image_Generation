..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc
..\lazarus64\lazbuild.exe --bm=Release im\imageExplorer.lpi
move im\imageExplorer.exe ..\art.bin\
..\lazarus64\lazbuild.exe --bm=Release display\display.lpi
copy display\imig.exe c:\bin\
move display\imig.exe ..\art.bin\
..\lazarus64\fpc\3.0.4\bin\x86_64-win64\delp.exe display\lib\x86_64-win64 im\lib\x86_64-win64 common igSrc