PROGRAM imig;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}cthreads,cmem,{$endif}
  Forms, Interfaces, displayMain, ig_buddhaBrot;

{$R *.res}

begin
  Application.Scaled:=true;
  Application.title:='IM/IG';
  RequireDerivedFormResource := true;
  Application.initialize;
  Application.CreateForm(TDisplayMainForm, DisplayMainForm);
  Application.run;
end.

