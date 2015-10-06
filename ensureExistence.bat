@set fpc=c:\lazarus32\fpc\2.6.4\bin\i386-win32\fpc
@set strip=c:\lazarus32\fpc\2.6.4\bin\i386-win32\strip
@set delp=c:\lazarus32\fpc\2.6.4\bin\i386-win32\delp
@if "%1"=="catalogue" goto ensureCatalogue
@if not exist ..\binary\im.exe          %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc im.pas -duseExtensions -o..\binary\im.exe
@if not exist ..\binary\display.exe     %fpc% -S2 -O3 -Si -XX -CX -Sh display.pas -dexpandFileNames -FuigSrc -FuauxSrc  -o..\binary\display.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\expoClouds.exe  %fpc% -S2 -O3 -Si -XX -CX expoClouds.pas -ddoubleAccuracy -duseExtensions -FuigSrc -FuauxSrc -o..\binary\expoClouds.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@%delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\ifs.exe         %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc ifs4.pas -duseExtensions -o..\binary\ifs.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\ifsjobber.exe   %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc ifs4.pas -duseExtensions -djobberMode -o..\binary\ifsjobber.exe
@if not exist ..\binary\funcTrees.exe   %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc funcTrees.pas -duseExtensions  -o..\binary\funcTrees.exe
@if not exist ..\binary\epicycles.exe   %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc epicycles2.pas -o..\binary\epicycles.exe
@if not exist ..\binary\reliefs.exe     %fpc% -S2 -O3 -Si -XX -CX -FuigSrc -FuauxSrc reliefs.pas -duseExtensions -ddoubleAccuracy -o..\binary\reliefs.exe & %delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\bif_typ0.exe    %fpc% -S2 -O3 -Si -XX -CX bifurcation.pas -FuigSrc -FuauxSrc -duseExtensions -ddoubleAccuracy -dtyp0 -o..\binary\bif_typ0.exe
@if not exist ..\binary\bif_typ1.exe    %fpc% -S2 -O3 -Si -XX -CX bifurcation.pas -FuigSrc -FuauxSrc -duseExtensions -ddoubleAccuracy -dtyp1 -o..\binary\bif_typ1.exe
@if not exist ..\binary\bif_typ2.exe    %fpc% -S2 -O3 -Si -XX -CX bifurcation.pas -FuigSrc -FuauxSrc -duseExtensions -ddoubleAccuracy -dtyp2 -o..\binary\bif_typ2.exe
@%delp% . ..\binary auxSrc guiSrc igSrc
@For /f "tokens=1 delims=." %%a in ('dir /B frac_incs\frac_*.inc') do @if not exist ..\binary\%%a.exe copy frac_incs\%%a.inc frac.inc & %fpc% -S2 -O3 -Si -XX -CX fractals.pas -FuigSrc -FuauxSrc -ddoubleAccuracy -o..\binary\%%a.exe
@%delp% . ..\binary auxSrc guiSrc igSrc
@if not exist ..\binary\jpgSizeLimiter.exe %fpc% jpgSizeLimiter.pas -S2 -O3 -S2 -XX -CX -FuigSrc -FuauxSrc -FuC:\lazarus32\lcl\units\i386-win32 -FuC:\lazarus32\components\lazutils\lib\i386-win32 -o..\binary\jpgSizeLimiter.exe & %delp% . ..\binary auxSrc guiSrc igSrc
:ensureCatalogue
@if not exist ..\binary\catalogue.exe %fpc% guiSrc\catalogue2.lpr -S2 -Sh -CX -XX -gl -FuigSrc -FuauxSrc -FuguiSrc -FuC:\lazarus32\lcl\units\i386-win32\win32 -FuC:\lazarus32\lcl\units\i386-win32 -FuC:\lazarus32\components\lazutils\lib\i386-win32 -FuC:\lazarus32\packager\units\i386-win32 -Fu. -FuC:\lazarus32\components\synedit\units\i386-win32\win32 -dLCL -dLCLwin32 -o..\binary\catalogue.exe -WG & %delp% . ..\binary auxSrc guiSrc igSrc & del ..\binary\*.lfm
@%delp% . ..\binary auxSrc guiSrc igSrc



