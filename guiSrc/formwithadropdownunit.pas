UNIT formWithADropDownUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,dbTags;

TYPE

  { TformWithADropDown }

  TformWithADropDown = class(TForm)
    ComboBox: TComboBox;
    PROCEDURE ComboBoxKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState
      );
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
  private
    { private declarations }
    history:TStrings;
  public
    PROCEDURE initWithTags(title:string);
    PROCEDURE init(title:string);
    PROCEDURE init(title:string; dropDownList:TStrings);
    { public declarations }
  end;

VAR
  formWithADropDown: TformWithADropDown;

IMPLEMENTATION

{$R *.lfm}

{ TformWithADropDown }

PROCEDURE TformWithADropDown.ComboBoxKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
begin
  if key=13 then begin
    history.Append(ComboBox.Text);
    ModalResult:=mrOK;
  end else if key=27 then ModalResult:=mrCancel;
end;

PROCEDURE TformWithADropDown.FormCreate(Sender: TObject);
  begin
    history:=TStringList.create;
  end;

PROCEDURE TformWithADropDown.FormDestroy(Sender: TObject);
  begin
    history.Free;
  end;

PROCEDURE TformWithADropDown.initWithTags(title: string);
  begin
    Caption:=title;
    getTagsForDropDown(ComboBox.Items);
    ComboBox.Sorted:=true;
  end;

PROCEDURE TformWithADropDown.init(title: string);
  begin
    Caption:=title;
    ComboBox.Items.assign(history);
    ComboBox.Text:='';
  end;

PROCEDURE TformWithADropDown.init(title: string; dropDownList: TStrings);
  begin
    Caption:=title;
    ComboBox.Items.assign(dropDownList);
    ComboBox.Text:='';
  end;

end.

