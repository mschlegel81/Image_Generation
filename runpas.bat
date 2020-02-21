@if "%1"=="clean" @goto cleanup
@goto run

:cleanup
  @c:\lazarus32\fpc\2.6.4\bin\i386-win32\delp . auxSrc igSrc
  @del *.exe
  @goto end

:run
  @For /f "tokens=1 delims=\." %%a in ('echo %1') do @del %%a.exe
  @rem @For /f "tokens=1 delims=." %%a in ('echo %1') do c:\lazarus32\fpc\2.6.4\bin\i386-win32\fpc %%a.pas -S2 -Si -O3 -Sh -CX -XX -FuigSrc -FuauxSrc -duseExtensions -ddoubleAccuracy
  @For /f "tokens=1 delims=." %%a in ('echo %1') do c:\lazarus32\fpc\2.6.4\bin\i386-win32\fpc %%a.pas -S2 -gl -FuigSrc -FuauxSrc -duseExtensions -ddoubleAccuracy
  @For /f "tokens=1 delims=\." %%a in ('echo %1') do %%a.exe %2 %3 %4 %5 %6 %7 %8 %9
:end