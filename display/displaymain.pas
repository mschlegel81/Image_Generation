UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls;

TYPE

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox2: TComboBox;
    ComboBox20: TComboBox;
    ComboBox21: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    resetTypeComboBox: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Image1: TImage;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    manipulationPanel: TPanel;
    imageGenerationPanel: TPanel;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE renderImage(VAR img:T_rawImage);
  end;

VAR
  Form1: TForm1;
  img:T_rawImage;

IMPLEMENTATION

{$R *.lfm}

{ TForm1 }

PROCEDURE TForm1.FormCreate(Sender: TObject);
  begin
    img.create(paramStr(1));
  end;

PROCEDURE TForm1.FormResize(Sender: TObject);
  begin
    renderImage(img);
  end;


PROCEDURE TForm1.renderImage(VAR img: T_rawImage);
  begin
    img.copyToImage(Image1);
    Image1.width:=Image1.Picture.width;
    Image1.height:=Image1.Picture.height;
  end;

end.

