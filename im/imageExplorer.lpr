PROGRAM imageExplorer;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, imageExplorerMain, imExModifyDialog, myParams
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.initialize;
  Application.CreateForm(TMainForm, mainForm);
  Application.CreateForm(TmodifyForm, modifyForm);
  Application.run;
end.

