PROGRAM display;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}cthreads,cmem,{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, displayMain, mypics, myColors, mySys, myGenerics, imageGeneration,
  jobberUnit, editHelper
  { you can add units after this };

{$R *.res}

begin
  Application.title:='IM_IG';
  RequireDerivedFormResource := true;
  Application.initialize;
  Application.CreateForm(TDisplayMainForm, DisplayMainForm);
  Application.CreateForm(TjobberForm, jobberForm);
  Application.CreateForm(TEditHelperForm, EditHelperForm);
  Application.run;
end.

