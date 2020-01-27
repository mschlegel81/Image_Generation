PROGRAM display;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}cthreads,cmem,{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, displayMain, imageGeneration, imageContexts,
  jobberUnit, editHelper;

{$R *.res}

begin
  Application.Scaled:=true;
  Application.title:='IM_IG';
  RequireDerivedFormResource := true;
  Application.initialize;
  Application.CreateForm(TDisplayMainForm, DisplayMainForm);
  Application.run;
end.

