@set fpc=d:\dev\lazarus32\fpc\2.6.4\bin\i386-win32\fpc
@set fpc64=d:\dev\lazarus64\fpc\2.6.4\bin\x86_64-win64\fpc
@set delp=d:\dev\lazarus32\fpc\2.6.4\bin\i386-win32\delp
@set unitPaths=-dLCL -dLCLwin32 -FuigSrc -FuauxSrc -FuD:\dev\lazarus32\lcl\units\i386-win32\win32 -FuD:\dev\lazarus32\lcl\units\i386-win32 -FuD:\dev\lazarus32\components\lazutils\lib\i386-win32 -FuD:\dev\lazarus32\packager\units\i386-win32 
@if not exist ..\binary\im.exe          %fpc% %unitPaths% -S2 -O3 -Si -XX -CX im.pas -duseExtensions -o..\binary\im.exe
@if not exist ..\binary\im64.exe      %fpc64% %unitPaths% -S2 -O3 -Si -XX -CX im.pas -duseExtensions -dnaked -o..\binary\im64.exe
@if not exist ..\binary\display.exe     %fpc% %unitPaths% -S2 -O3 -Si -XX -CX -Sh display.pas -dexpandFileNames -o..\binary\display.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\expoClouds.exe  %fpc% %unitPaths% -S2 -O3 -Si -XX -CX expoClouds.pas -ddoubleAccuracy -o..\binary\expoClouds.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@%delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\ifs.exe         %fpc% %unitPaths% -S2 -O3 -Si -XX -CX ifs4.pas -o..\binary\ifs.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\ifsjobber.exe   %fpc% %unitPaths% -S2 -O3 -Si -XX -CX ifs4.pas -djobberMode -o..\binary\ifsjobber.exe
@if not exist ..\binary\funcTrees.exe   %fpc% %unitPaths% -S2 -O3 -Si -XX -CX funcTrees.pas -duseExtensions  -o..\binary\funcTrees.exe
@if not exist ..\binary\epicycles.exe   %fpc% %unitPaths% -S2 -O3 -Si -XX -CX epicycles2.pas -o..\binary\epicycles.exe
@if not exist ..\binary\reliefs.exe     %fpc% %unitPaths% -S2 -O3 -Si -XX -CX reliefs.pas -duseExtensions -ddoubleAccuracy -o..\binary\reliefs.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\bif_typ0.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp0 -o..\binary\bif_typ0.exe
@if not exist ..\binary\bif_typ0a.exe   %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp0a -o..\binary\bif_typ0a.exe
@if not exist ..\binary\bif_typ1.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp1 -o..\binary\bif_typ1.exe
@if not exist ..\binary\bif_typ2.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp2 -o..\binary\bif_typ2.exe
@if not exist ..\binary\bif_typ3.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp3 -o..\binary\bif_typ3.exe
@if not exist ..\binary\bif_typ4.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp4 -o..\binary\bif_typ4.exe
@if not exist ..\binary\bif_typ5.exe    %fpc% %unitPaths% -S2 -O3 -Si -XX -CX bifurcation.pas -ddoubleAccuracy -dtyp5 -o..\binary\bif_typ5.exe
@%delp% . ..\binary auxSrc guiSrc igSrc
@For /f "tokens=1 delims=." %%a in ('dir /B frac_incs\frac_*.inc') do @if not exist ..\binary\%%a.exe copy frac_incs\%%a.inc frac.inc & %fpc% %unitPaths% -S2 -O3 -Si -XX -CX fractals.pas -ddoubleAccuracy -o..\binary\%%a.exe
@%delp% . ..\binary auxSrc guiSrc igSrc


