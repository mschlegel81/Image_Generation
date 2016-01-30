UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus;

TYPE

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
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

