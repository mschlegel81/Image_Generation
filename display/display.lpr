program display;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, displayMain, mypics, myColors, mySys, myGenerics, imageGeneration,
  jobberUnit, editHelper
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='IM_IG';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDisplayMainForm, DisplayMainForm);
  Application.CreateForm(TjobberForm, jobberForm);
  Application.CreateForm(TEditHelperForm, EditHelperForm);
  Application.Run;
end.

