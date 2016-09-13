PROGRAM imageExplorer;

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, imageExplorerMain, imExModifyDialog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.initialize;
  Application.CreateForm(TMainForm, mainForm);
  Application.CreateForm(TmodifyForm, modifyForm);
  Application.run;
end.

