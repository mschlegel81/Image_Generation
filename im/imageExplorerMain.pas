UNIT imageExplorerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ExtDlgs, StdCtrls, mypics, workflows, myParams, math, imExModifyDialog;

TYPE

  { TMainForm }

  TMainForm = class(TForm)
    Image1: TImage;
    busyLabel: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    miFit: TMenuItem;
    miFill: TMenuItem;
    miResize: TMenuItem;
    miResizeSub: TMenuItem;
    miDrip: TMenuItem;
    miMode: TMenuItem;
    miMono: TMenuItem;
    miQuantize: TMenuItem;
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
    miSharpen: TMenuItem;
    miMedian: TMenuItem;
    miPseudomedian: TMenuItem;
    miLagrange: TMenuItem;
    miSketch: TMenuItem;
    miEncircle: TMenuItem;
    miInvert: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    MenuItem6: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    cropShape: TShape;
    timer: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE Image1MouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE Image1MouseUp(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE miAddHSVClick(Sender: TObject);
    PROCEDURE miAddRGBClick(Sender: TObject);
    PROCEDURE miBlurClick(Sender: TObject);
    PROCEDURE miCompressClick(Sender: TObject);
    PROCEDURE miCropClick(Sender: TObject);
    PROCEDURE miDripClick(Sender: TObject);
    PROCEDURE miEncircleClick(Sender: TObject);
    PROCEDURE miFillClick(Sender: TObject);
    PROCEDURE miFitClick(Sender: TObject);
    PROCEDURE miFlipClick(Sender: TObject);
    PROCEDURE miFlopClick(Sender: TObject);
    PROCEDURE miGammaClick(Sender: TObject);
    PROCEDURE miGreyClick(Sender: TObject);
    PROCEDURE miInvertClick(Sender: TObject);
    PROCEDURE miLagrangeClick(Sender: TObject);
    PROCEDURE miMedianClick(Sender: TObject);
    PROCEDURE miModeClick(Sender: TObject);
    PROCEDURE miMonoClick(Sender: TObject);
    PROCEDURE miMultHSVClick(Sender: TObject);
    PROCEDURE miMultRGBClick(Sender: TObject);
    PROCEDURE miNormalizeClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miPseudomedianClick(Sender: TObject);
    PROCEDURE miQuantizeClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miResizeClick(Sender: TObject);
    PROCEDURE miRotLeftClick(Sender: TObject);
    PROCEDURE miRotRightClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSepiaClick(Sender: TObject);
    PROCEDURE miSharpenClick(Sender: TObject);
    PROCEDURE miShineClick(Sender: TObject);
    PROCEDURE miSketchClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    currentFile:string;
    loadedFile:string;
    selectingCropRegion:boolean;
    needRepaint:boolean;
    inputImageIsResized:boolean;
    imageToDisplay:longint;
    renderingOutput:boolean;
    PROCEDURE openFile(CONST fileName:string);
    PROCEDURE enableMenuItems;
    PROCEDURE addParameterlessStep(CONST imt:T_imageManipulationType);
    PROCEDURE addParameterizedStep(CONST imt:T_imageManipulationType);
    PROCEDURE updatePreview;
    PROCEDURE waitForRendering;
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
    loadedFile:='';
    inputImageIsResized:=true;
    renderingOutput:=false;
    selectingCropRegion:=false;
    enableMenuItems;
    miSave.enabled:=false;
    if (paramCount=1) and fileExists(paramStr(1)) then openFile(paramStr(1));
  end;

PROCEDURE TMainForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if renderingOutput then begin
      Hide;
      timer.enabled:=false;
    end;
    waitForRendering;
  end;

PROCEDURE TMainForm.FormResize(Sender: TObject);
  begin
    waitForRendering;
    if (currentFile='') or not fileExists(currentFile) then exit;
    if (currentFile=loadedFile) and not(inputImageIsResized) then exit;
    Image1.Align:=alClient;
    Image1.stretch:=true;
    Image1.Proportional:=true;
    Image1.picture.loadFromFile(currentFile);
    if inputImage=nil then new(inputImage,create(1,1));
    inputImage^.copyFromImage(Image1);
    inputImageIsResized:=false;
    loadedFile:=currentFile;
    if workflow.stepCount>0 then begin
      inputImage^.resize(max(Image1.width,Image1.height),max(Image1.width,Image1.height),res_fit);
      inputImageIsResized:=true;
      workflow.execute(true,true,true,inputImage^.width,inputImage^.height,inputImage^.width,inputImage^.height);
      needRepaint:=true;
    end;
  end;

PROCEDURE TMainForm.Image1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if button=mbRight then begin
      cropShape.visible:=false;
      miCrop.enabled:=false;
      exit;
    end;
    cropShape.top:=y;
    cropShape.Left:=x;
    cropShape.width:=1;
    cropShape.height:=1;
    cropShape.visible:=true;
    selectingCropRegion:=true;
    miCrop.enabled:=true;
    {$ifdef DEBUG}
    writeln('Crop region (new): ',x,'..',x+1,' x ',y,'..',y+1);
    {$endif}
  end;

PROCEDURE TMainForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR x0,x1,y0,y1:longint;
  begin
    if not(selectingCropRegion) then exit;
    x0:=cropShape.Left;
    x1:=cropShape.width+x0;
    y0:=cropShape.top;
    y1:=cropShape.height+y0;
    if x<x0 then x0:=x;
    if x>x1 then x1:=x;
    if y<y0 then y0:=y;
    if y>y1 then y1:=y;
    {$ifdef DEBUG}
    writeln('Crop region (mod): ',x0,'..',x1,' x ',y0,'..',y1);
    {$endif}
    cropShape.Left:=x0;
    cropShape.width:=x1-x0;
    cropShape.top:=y0;
    cropShape.height:=y1-y0;
  end;

PROCEDURE TMainForm.Image1MouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    selectingCropRegion:=false;
    if button=mbRight then begin
      cropShape.visible:=false;
      miCrop.enabled:=false;
      exit;
    end;
  end;

PROCEDURE TMainForm.miAddHSVClick(Sender: TObject); begin addParameterizedStep(imt_addHSV); end;
PROCEDURE TMainForm.miAddRGBClick(Sender: TObject); begin addParameterizedStep(imt_addRGB); end;
PROCEDURE TMainForm.miBlurClick(Sender: TObject); begin addParameterizedStep(imt_blur); end;
PROCEDURE TMainForm.miCompressClick(Sender: TObject); begin addParameterlessStep(imt_compress); end;
PROCEDURE TMainForm.miEncircleClick(Sender: TObject); begin addParameterizedStep(imt_encircle); end;

PROCEDURE TMainForm.miFlipClick(Sender: TObject); begin addParameterlessStep(imt_flip); end;
PROCEDURE TMainForm.miFlopClick(Sender: TObject); begin addParameterlessStep(imt_flop); end;
PROCEDURE TMainForm.miGammaClick(Sender: TObject); begin addParameterizedStep(imt_gammaRGB); end;
PROCEDURE TMainForm.miGreyClick(Sender: TObject); begin addParameterlessStep(imt_grey); end;
PROCEDURE TMainForm.miInvertClick(Sender: TObject); begin addParameterlessStep(imt_invert); end;
PROCEDURE TMainForm.miLagrangeClick(Sender: TObject); begin addParameterizedStep(imt_lagrangeDiff); end;
PROCEDURE TMainForm.miMedianClick(Sender: TObject); begin addParameterizedStep(imt_median); end;
PROCEDURE TMainForm.miModeClick(Sender: TObject); begin addParameterizedStep(imt_mode); end;
PROCEDURE TMainForm.miMonoClick(Sender: TObject); begin addParameterizedStep(imt_mono); end;
PROCEDURE TMainForm.miMultHSVClick(Sender: TObject); begin addParameterizedStep(imt_multiplyHSV); end;
PROCEDURE TMainForm.miMultRGBClick(Sender: TObject); begin addParameterizedStep(imt_multiplyRGB); end;
PROCEDURE TMainForm.miNormalizeClick(Sender: TObject); begin addParameterlessStep(imt_normalizeFull); end;
PROCEDURE TMainForm.miPseudomedianClick(Sender: TObject); begin addParameterizedStep(imt_pseudomedian); end;
PROCEDURE TMainForm.miQuantizeClick(Sender: TObject); begin addParameterizedStep(imt_quantize); end;
PROCEDURE TMainForm.miRotLeftClick(Sender: TObject); begin addParameterlessStep(imt_rotLeft); end;
PROCEDURE TMainForm.miRotRightClick(Sender: TObject); begin addParameterlessStep(imt_rotRight); end;
PROCEDURE TMainForm.miSepiaClick(Sender: TObject); begin addParameterlessStep(imt_sepia); end;
PROCEDURE TMainForm.miSharpenClick(Sender: TObject); begin addParameterizedStep(imt_sharpen); end;
PROCEDURE TMainForm.miShineClick       (Sender: TObject); begin addParameterlessStep(imt_shine); end;
PROCEDURE TMainForm.miSketchClick(Sender: TObject); begin addParameterizedStep(imt_sketch); end;
PROCEDURE TMainForm.miDripClick(Sender: TObject); begin addParameterizedStep(imt_drip); end;
PROCEDURE TMainForm.miResizeClick(Sender: TObject); begin addParameterizedStep(imt_resize); end;
PROCEDURE TMainForm.miFillClick(Sender: TObject); begin addParameterizedStep(imt_fill); end;
PROCEDURE TMainForm.miFitClick(Sender: TObject); begin addParameterizedStep(imt_fit); end;

PROCEDURE TMainForm.miSaveClick(Sender: TObject);
  begin
    if SaveDialog1.execute then begin
      waitForRendering;
      while (workflow.stepCount-1>imageToDisplay) and (workflow.stepCount>0) do workflow.remStep(workflow.stepCount-1);
      workflow.executeForTarget(currentFile,0,SaveDialog1.fileName);
      renderingOutput:=true;
      inputImageIsResized:=false;
    end;
  end;

PROCEDURE TMainForm.miCropClick(Sender: TObject);
  VAR param:T_parameterValue;
  begin
    if not(cropShape.visible) then exit;
    param.createFromValue(stepParamDescription[imt_crop],
                          cropShape.Left                 /Image1.destRect.Right,
                         (cropShape.Left+cropShape.width)/Image1.destRect.Right,
                          cropShape.top                  /Image1.destRect.Bottom,
                         (cropShape.top+cropShape.height)/Image1.destRect.Bottom);
    waitForRendering;
    while (workflow.stepCount-1>imageToDisplay) and (workflow.stepCount>0) do workflow.remStep(workflow.stepCount-1);
    if (workflow.addStep(param.toString(tsm_forSerialization))) then begin
      updatePreview;
      imageToDisplay:=workflow.stepCount-1;
      enableMenuItems;
    end;
    cropShape.visible:=false;
    miCrop.enabled:=false;
  end;

PROCEDURE TMainForm.miOpenClick(Sender: TObject);
  begin
    waitForRendering;
    if OpenPictureDialog1.execute then openFile(OpenPictureDialog1.fileName);
  end;


PROCEDURE TMainForm.miRedoClick(Sender: TObject);
  begin
    waitForRendering;
    if imageToDisplay>=workflow.stepCount-1 then exit;
    inc(imageToDisplay);
    workflow.renderIntermediate(imageToDisplay,Image1);
    enableMenuItems;
  end;

PROCEDURE TMainForm.miUndoClick(Sender: TObject);
  begin
    waitForRendering;
    if workflow.stepCount<=imageToDisplay then exit;
    dec(imageToDisplay);
    if imageToDisplay<0 then inputImage^.copyToImage(Image1)
                        else workflow.renderIntermediate(imageToDisplay,Image1);
    enableMenuItems;
  end;

PROCEDURE TMainForm.TimerTimer(Sender: TObject);
  begin
    if progressQueue.calculating then begin
      Image1.Cursor:=crHourGlass;
      busyLabel.visible:=true;
      busyLabel.caption:=progressQueue.getProgressString;
      exit;
    end else begin
      Image1.Cursor:=crDefault;
      busyLabel.visible:=false;
      if renderingOutput then begin
        renderingOutput:=false;
        Image1.picture.loadFromFile(currentFile);
        if inputImage=nil then new(inputImage,create(1,1));
        inputImage^.copyFromImage(Image1);
        inputImageIsResized:=false;
        updatePreview;
      end;
    end;
    if not(needRepaint) then exit;
    workflow.renderIntermediate(imageToDisplay,Image1);
    Image1.Align:=alClient;
    Image1.stretch:=false;
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
  end;

PROCEDURE TMainForm.addParameterlessStep(CONST imt: T_imageManipulationType);
  begin
    waitForRendering;
    while (workflow.stepCount-1>imageToDisplay) and (workflow.stepCount>0) do workflow.remStep(workflow.stepCount-1);
    if (workflow.addStep(stepParamDescription[imt]^.getDefaultParameterString)) then begin
      updatePreview;
      imageToDisplay:=workflow.stepCount-1;
      enableMenuItems;
    end;
  end;

PROCEDURE TMainForm.addParameterizedStep(CONST imt: T_imageManipulationType);
  begin
    waitForRendering;
    if modifyForm.showModalFor(imt,imageToDisplay+1) then begin
      updatePreview;
      imageToDisplay:=workflow.stepCount-1;
      enableMenuItems;
    end;
  end;

PROCEDURE TMainForm.updatePreview;
  begin
    if renderingOutput then exit;
    if not(inputImageIsResized) then begin
      inputImage^.resize(max(Image1.width,Image1.height),max(Image1.width,Image1.height),res_fit);
      inputImageIsResized:=true;
    end;
    workflow.execute(true,true,true,inputImage^.width,inputImage^.height,inputImage^.width,inputImage^.height);
    needRepaint:=true;
  end;

PROCEDURE TMainForm.waitForRendering;
  begin
    while renderingOutput and progressQueue.calculating do begin
      ThreadSwitch;
      sleep(100);
    end;
  end;

end.

