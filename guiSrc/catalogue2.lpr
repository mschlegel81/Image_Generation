program catalogue2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, cat2Main, entryForm, formWithADropDownUnit, dbFiles, dbEntries, dbTags
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCatMainForm, CatMainForm);
  Application.CreateForm(TEntryDialog, EntryDialog);
  Application.CreateForm(TformWithADropDown, formWithADropDown);
  Application.Run;
end.

