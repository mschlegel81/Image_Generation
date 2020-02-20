PROGRAM imig;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}cthreads,cmem,{$endif}
  Forms, Interfaces, displayMain;

{$R *.res}

begin
  Application.Scaled:=true;
  Application.Title:='IM/IG';
  RequireDerivedFormResource := true;
  Application.initialize;
  Application.CreateForm(TDisplayMainForm, DisplayMainForm);
  Application.run;
end.

