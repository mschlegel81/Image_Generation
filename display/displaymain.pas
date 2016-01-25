UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics;

TYPE

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE renderImage(VAR img:T_FloatMap);
  end;

VAR
  Form1: TForm1;
  img:T_FloatMap;

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

PROCEDURE TForm1.renderImage(VAR img: T_FloatMap);
  FUNCTION transformCol(CONST c:T_floatColor):longint;
    VAR p:T_24Bit;
    begin
      p:=projectedColor(c);
      result:=p[2] or p[1] shl 8 or p[0] shl 16;
    end;

  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      x,y,x1,y1:longint;
      pc:T_24Bit;
      pix:PByte;
  begin
    if img.width>width then x1:=width else x1:=img.width;
    if img.height>height then y1:=height else y1:=img.height;


    ScanLineImage:=TLazIntfImage.create(x1,y1);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(x1,y1);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    for y:=0 to y1-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to x1-1 do begin
        pc:=projectedColor(img[x,y]);
        move(pc,(pix+3*x)^,3);
      end;
    end;
    Image1.Picture.Bitmap.width:=x1;
    Image1.Picture.Bitmap.height:=y1;
    tempIntfImage:=Image1.Picture.Bitmap.CreateIntfImage;
    tempIntfImage.CopyPixels(ScanLineImage);
    Image1.Picture.Bitmap.LoadFromIntfImage(tempIntfImage);
    tempIntfImage.free;
    ScanLineImage.free;
  end;

end.

