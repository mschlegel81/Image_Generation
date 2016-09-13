UNIT imageExplorerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ExtDlgs, StdCtrls, mypics, workflows, myGenerics, myParams, math, imExModifyDialog;

TYPE

  { TMainForm }

  TMainForm = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    miGamma: TMenuItem;
    miMultRGB: TMenuItem;
    miAddRGB: TMenuItem;
    miMultHSV: TMenuItem;
    miAddHSV: TMenuItem;
    miShine: TMenuItem;
    miCompress: TMenuItem;
    miCrop: TMenuItem;
    MenuItem3: TMenuItem;
    miRotLeft: TMenuItem;
    miRotRight: TMenuItem;
    miFlip: TMenuItem;
    miFlop: TMenuItem;
    miNormalize: TMenuItem;
    miGrey: TMenuItem;
    miSepia: TMenuItem;
    miBlur: TMenuItem;
    MenuItem2: TMenuItem;
    miSharpen: TMenuItem;
    miFixup: TMenuItem;
    miMedian: TMenuItem;
    miPseudomedian: TMenuItem;
    miDenoise: TMenuItem;
    miLagrange: TMenuItem;
    miSketch: TMenuItem;
    miEncircle: TMenuItem;
    miInvert: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    MenuItem6: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    MenuItem9: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    timer: TTimer;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAddHSVClick(Sender: TObject);
    PROCEDURE miAddRGBClick(Sender: TObject);
    PROCEDURE miBlurClick(Sender: TObject);
    PROCEDURE miCompressClick(Sender: TObject);
    PROCEDURE miCropClick(Sender: TObject);
    PROCEDURE miDenoiseClick(Sender: TObject);
    PROCEDURE miEncircleClick(Sender: TObject);
    PROCEDURE miFlipClick(Sender: TObject);
    PROCEDURE miFlopClick(Sender: TObject);
    PROCEDURE miGammaClick(Sender: TObject);
    PROCEDURE miGreyClick(Sender: TObject);
    PROCEDURE miInvertClick(Sender: TObject);
    PROCEDURE miLagrangeClick(Sender: TObject);
    PROCEDURE miMedianClick(Sender: TObject);
    PROCEDURE miMultHSVClick(Sender: TObject);
    PROCEDURE miMultRGBClick(Sender: TObject);
    PROCEDURE miNormalizeClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miPseudomedianClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miRotLeftClick(Sender: TObject);
    PROCEDURE miRotRightClick(Sender: TObject);
    PROCEDURE miSepiaClick(Sender: TObject);
    PROCEDURE miSharpenClick(Sender: TObject);
    PROCEDURE miSketchClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    currentFile:string;
    needRepaint:boolean;
    imageToDisplay:longint;
    PROCEDURE openFile(CONST fileName:string);
    PROCEDURE enableMenuItems;
    PROCEDURE addParameterlessStep(CONST imt:T_imageManipulationType);
    PROCEDURE addParameterizedStep(CONST imt:T_imageManipulationType);
    PROCEDURE updatePreview;
  public
  end;

VAR
  mainForm: TMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TMainForm }

PROCEDURE TMainForm.FormCreate(Sender: TObject);
  begin
    needRepaint:=false;
    currentFile:='';
    enableMenuItems;
    miSave.enabled:=false;
    if (paramCount=1) and fileExists(paramStr(1)) then openFile(paramStr(1));
  end;

PROCEDURE TMainForm.FormResize(Sender: TObject);
  begin
    if (currentFile='') or not fileExists(currentFile) then exit;
    image1.Align:=alClient;
    image1.stretch:=true;
    image1.Proportional:=true;
    image1.picture.loadFromFile(currentFile);
    if inputImage=nil then new(inputImage,create(1,1));
    inputImage^.copyFromImage(Image1);
    inputImage^.resize(max(image1.width,Image1.height),max(image1.width,Image1.height),res_fit);
    workflow.execute(true,true,true,ClientWidth,ClientHeight,ClientWidth,ClientHeight);
  end;

PROCEDURE TMainForm.miAddHSVClick      (Sender: TObject); begin addParameterizedStep(imt_addHSV); end;
PROCEDURE TMainForm.miAddRGBClick      (Sender: TObject); begin addParameterizedStep(imt_addRGB); end;
PROCEDURE TMainForm.miBlurClick        (Sender: TObject); begin addParameterizedStep(imt_blur); end;
PROCEDURE TMainForm.miCompressClick    (Sender: TObject); begin addParameterlessStep(imt_compress); end;
PROCEDURE TMainForm.miEncircleClick    (Sender: TObject); begin addParameterizedStep(imt_encircle); end;
PROCEDURE TMainForm.miFlipClick        (Sender: TObject); begin addParameterlessStep(imt_flip); end;
PROCEDURE TMainForm.miFlopClick        (Sender: TObject); begin addParameterlessStep(imt_flop); end;
PROCEDURE TMainForm.miGammaClick       (Sender: TObject); begin addParameterizedStep(imt_gammaRGB); end;
PROCEDURE TMainForm.miGreyClick        (Sender: TObject); begin addParameterlessStep(imt_grey); end;
PROCEDURE TMainForm.miInvertClick      (Sender: TObject); begin addParameterlessStep(imt_invert); end;
PROCEDURE TMainForm.miLagrangeClick    (Sender: TObject); begin addParameterizedStep(imt_lagrangeDiff); end;
PROCEDURE TMainForm.miMedianClick      (Sender: TObject); begin addParameterizedStep(imt_median); end;
PROCEDURE TMainForm.miMultHSVClick     (Sender: TObject); begin addParameterizedStep(imt_multiplyHSV); end;
PROCEDURE TMainForm.miMultRGBClick     (Sender: TObject); begin addParameterizedStep(imt_multiplyRGB); end;
PROCEDURE TMainForm.miNormalizeClick   (Sender: TObject); begin addParameterlessStep(imt_normalizeFull); end;
PROCEDURE TMainForm.miPseudomedianClick(Sender: TObject); begin addParameterizedStep(imt_pseudomedian); end;
PROCEDURE TMainForm.miRotLeftClick     (Sender: TObject); begin addParameterlessStep(imt_rotLeft); end;
PROCEDURE TMainForm.miRotRightClick    (Sender: TObject); begin addParameterlessStep(imt_rotRight); end;
PROCEDURE TMainForm.miSepiaClick       (Sender: TObject); begin addParameterlessStep(imt_sepia); end;
PROCEDURE TMainForm.miSharpenClick     (Sender: TObject); begin addParameterizedStep(imt_sharpen); end;
PROCEDURE TMainForm.miSketchClick      (Sender: TObject); begin addParameterizedStep(imt_sketch); end;

PROCEDURE TMainForm.miCropClick (Sender: TObject);
begin

end;

PROCEDURE TMainForm.miDenoiseClick(Sender: TObject);
  begin

  end;

PROCEDURE TMainForm.miOpenClick(Sender: TObject);
  begin
  if OpenPictureDialog1.execute then openFile(OpenPictureDialog1.fileName);
end;


PROCEDURE TMainForm.miRedoClick(Sender: TObject);
  begin
    if imageToDisplay>=workflow.stepCount-1 then exit;
    inc(imageToDisplay);
    workflow.renderIntermediate(imageToDisplay,Image1);
    enableMenuItems;
  end;

PROCEDURE TMainForm.miUndoClick(Sender: TObject);
  begin
    if workflow.stepCount<=imageToDisplay then exit;
    dec(imageToDisplay);
    if imageToDisplay<0 then inputImage^.copyToImage(Image1)
                        else workflow.renderIntermediate(imageToDisplay,Image1);
    enableMenuItems;
  end;

PROCEDURE TMainForm.TimerTimer(Sender: TObject);
  begin
    if not(needRepaint) then exit;
    if progressQueue.calculating then exit;
    workflow.renderIntermediate(imageToDisplay,Image1);
    image1.Align:=alClient;
    image1.stretch:=false;
    needRepaint:=false;
  end;

PROCEDURE TMainForm.openFile(CONST fileName: string);
  begin
    if not(fileExists(fileName)) then exit;
    currentFile:=fileName;
    workflow.clear;
    imageToDisplay:=workflow.stepCount-1;
    enableMenuItems;
    FormResize(nil);
  end;

PROCEDURE TMainForm.enableMenuItems;
  begin
    miSave.enabled:=true;
    miUndo.enabled:=imageToDisplay>=0;
    miRedo.enabled:=workflow.stepCount>imageToDisplay+1;
    miFixup.enabled:=(workflow.stepCount>imageToDisplay+1) and (workflow.step[imageToDisplay+1].descriptor^.typ<>pt_none);
  end;

PROCEDURE TMainForm.addParameterlessStep(CONST imt: T_imageManipulationType);
  begin
    while (workflow.stepCount-1>imageToDisplay) and (workflow.stepCount>0) do workflow.remStep(workflow.stepCount-1);
    if (workflow.addStep(stepParamDescription[imt]^.getDefaultParameterString)) then begin
      updatePreview;
      imageToDisplay:=workflow.stepCount-1;
      enableMenuItems;
    end;
  end;

PROCEDURE TMainForm.addParameterizedStep(CONST imt: T_imageManipulationType);
  begin
    if modifyForm.showModalFor(imt,imageToDisplay+1) then begin
      updatePreview;
      imageToDisplay:=workflow.stepCount-1;
      enableMenuItems;
    end;
  end;

PROCEDURE TMainForm.updatePreview;
  begin
    workflow.execute(true,true,true,Image1.width,Image1.height,Image1.width,Image1.height);
    needRepaint:=true;
  end;

end.

