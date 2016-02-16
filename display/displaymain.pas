UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls, ValEdit, ComCtrls,math,myStringUtil,
  complex,myColors,
  LCLTranslator,
  imageGeneration,
  ig_gradient,
  ig_perlin,
  ig_fractals,
  myGenerics,myParams,workflows;

TYPE

  { TDisplayMainForm }

  TDisplayMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    pickLightHelperShape: TShape;
    zoomOutButton: TButton;
    pickLightButton: TButton;
    pickJuliaButton: TButton;
    ColorDialog: TColorDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mi_scale_original: TMenuItem;
    mi_scale_fit: TMenuItem;
    mi_scale_16_9: TMenuItem;
    mi_scale_16_10: TMenuItem;
    mi_scale_4_3: TMenuItem;
    mi_Scale_3_4: TMenuItem;
    mi_renderQualityHigh: TMenuItem;
    mi_renderQualityPreview: TMenuItem;
    resetButton: TButton;
    ComboBox1: TComboBox;
    manipulationStepComboBox: TComboBox;
    manipulationParameterComboBox: TComboBox;
    algorithmComboBox: TComboBox;
    resetTypeComboBox: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    image: TImage;
    selectionRect0: TShape;
    selectionRect1: TShape;
    selectionRect2: TShape;
    StepsListBox: TListBox;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    manipulationPanel: TPanel;
    imageGenerationPanel: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    timer: TTimer;
    ValueListEditor: TValueListEditor;
    PROCEDURE algorithmComboBoxSelect(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE ImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseLeave(Sender: TObject);
    PROCEDURE ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mi_renderQualityHighClick(Sender: TObject);
    PROCEDURE mi_renderQualityPreviewClick(Sender: TObject);
    PROCEDURE pickJuliaButtonClick(Sender: TObject);
    PROCEDURE pickLightButtonClick(Sender: TObject);
    PROCEDURE pickLightHelperShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE pickLightHelperShapeMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE ValueListEditorEditButtonClick(Sender: TObject);
    PROCEDURE ValueListEditorEditingDone(Sender: TObject);
    PROCEDURE evaluationFinished;
    PROCEDURE zoomOutButtonClick(Sender: TObject);
  private
    mouseSelection:record
      mouseHoversOverImage:boolean;
      lastX,lastY:longint;
      downX,downY:longint;
      selType:(none,for_zoom,for_cropPending,for_crop,for_pan,for_light);
      zoomOnMouseDown:double;
    end;
    statusBarParts:record
      errorMessage,
      progressMessage,
      crosshairMessage:ansistring;
    end;

    previousAlgorithmParameters:array of ansistring;
    subTimerCounter:longint;
    currentAlgoMeta:T_algorithmMeta;
    renderToImageNeeded:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE calculateImage;
    PROCEDURE renderImage(VAR img:T_rawImage);
    PROCEDURE updateStatusBar;
    PROCEDURE updateAlgoScaler(CONST finalize:boolean=false);
    PROCEDURE updateLight(CONST finalize:boolean=false);
    PROCEDURE updatePan(CONST finalize:boolean=false);
  end;

VAR
  DisplayMainForm: TDisplayMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TDisplayMainForm }

PROCEDURE TDisplayMainForm.FormCreate(Sender: TObject);
  PROCEDURE prepareAlgorithms;
    VAR i:longint;
    begin
      algorithmComboBox.Items.clear;
      for i:=0 to length(algorithms)-1 do algorithmComboBox.Items.append(algorithms[i].name);
      if length(algorithms)>0 then algorithmComboBox.ItemIndex:=0;
      algorithmComboBoxSelect(Sender);
    end;

  begin
    mouseSelection.selType:=none;


    subTimerCounter:=0;
    renderToImageNeeded:=false;
    prepareAlgorithms;
    progressQueue.registerOnEndCallback(@evaluationFinished);
  end;

PROCEDURE TDisplayMainForm.algorithmComboBoxSelect(Sender: TObject);
  VAR i,j:longint;
      resetStyles:T_arrayOfString;
      parDesc:P_parameterDescription;
  begin
    if (algorithmComboBox.ItemIndex<0) or (algorithmComboBox.ItemIndex>=length(algorithms)) then exit;
    currentAlgoMeta:=algorithms[algorithmComboBox.ItemIndex];

    zoomOutButton.Visible:=currentAlgoMeta.hasScaler;
    pickLightButton.Visible:=currentAlgoMeta.hasLight;
    pickLightButton.Enabled:=false;
    pickJuliaButton.Visible:=currentAlgoMeta.hasJuliaP;
    pickJuliaButton.Enabled:=false;

    resetTypeComboBox.Items.clear;
    resetStyles:=currentAlgoMeta.prototype^.parameterResetStyles;
    for i:=0 to length(resetStyles)-1 do resetTypeComboBox.Items.append(resetStyles[i]);
    if length(resetStyles)>0 then resetTypeComboBox.ItemIndex:=0;

    ValueListEditor.RowCount:=currentAlgoMeta.prototype^.numberOfParameters+1;

    setLength(previousAlgorithmParameters,currentAlgoMeta.prototype^.numberOfParameters);
    for i:=0 to currentAlgoMeta.prototype^.numberOfParameters-1 do begin
      parDesc:=currentAlgoMeta.prototype^.parameterDescription(i);
      ValueListEditor.Cells[0,i+1]:=currentAlgoMeta.prototype^.parameterDescription(i)^.name;
      ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString;
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
      if parDesc^.typ=pt_enum then with ValueListEditor.ItemProps[i] do begin
        EditStyle:=esPickList;
        readonly:=true;
        PickList.clear;
        for j:=0 to length(parDesc^.enumValues)-1 do PickList.add(parDesc^.enumValues[j]);
      end else if parDesc^.typ=pt_color then ValueListEditor.ItemProps[i].EditStyle:=esEllipsis
                                        else ValueListEditor.ItemProps[i].EditStyle:=esSimple;
    end;
    calculateImage;
  end;

PROCEDURE TDisplayMainForm.FormResize(Sender: TObject);
  VAR destRect:TRect;
  begin
    progressQueue.cancelCalculation(true);
    destRect:=Rect(0,0,ScrollBox1.width,ScrollBox1.height);
    if mi_scale_4_3  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,4/3);
    if mi_Scale_3_4  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,3/4);
    if mi_scale_16_10.Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/10);
    if mi_scale_16_9 .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/9);
    imageGeneration.renderImage.resize(destRect.Right,destRect.Bottom,res_dataResize);
    image.Left:=(ScrollBox1.width-destRect.Right) shr 1;
    image.Top :=(ScrollBox1.height-destRect.Bottom) shr 1;

    pickLightHelperShape.width:=min(destRect.Right,destRect.Bottom);
    pickLightHelperShape.height:=pickLightHelperShape.width;
    pickLightHelperShape.Top:=image.Top+(destRect.Bottom-pickLightHelperShape.height) shr 1;
    pickLightHelperShape.Left:=image.Left+(destRect.Right-pickLightHelperShape.width) shr 1;
    calculateImage;
  end;

PROCEDURE TDisplayMainForm.ImageMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    with mouseSelection do begin
      downX:=x;
      downY:=y;
      lastX:=x;
      lastY:=y;
      if selType=for_light then begin
        updateLight(true);
        exit;
      end;
      if ssRight in Shift then selType:=for_pan
      else if ssLeft in Shift then begin
        if selType=for_cropPending        then selType:=for_crop
        else if currentAlgoMeta.hasScaler then begin
          selType:=for_zoom;
          zoomOnMouseDown:=P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scaler.getZoom;
        end;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.ImageMouseLeave(Sender: TObject);
  VAR tmpX,tmpY:longint;
  begin
    with mouseSelection do if selType in [for_zoom] then begin
      tmpX:=lastX;
      tmpY:=lastY;
      lastX:=downX;
      lastY:=downY;
      updateAlgoScaler(true);
      mouseHoversOverImage:=false;
      lastX:=tmpX;
      lastY:=tmpY;
    end;
    selectionRect0.Visible:=false;
    selectionRect1.Visible:=false;
    selectionRect2.Visible:=false;
    if mouseSelection.selType<>for_light
    then mouseSelection.selType:=none;
  end;

PROCEDURE TDisplayMainForm.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
  PROCEDURE drawSelectionRect;
    VAR x0,x1,y0,y1:longint;
        factor:double;
    begin
      with mouseSelection do begin
        if selType=for_crop then begin
          x0:=min(lastX,downX);
          x1:=max(lastX,downX);
          y0:=min(lastY,downY);
          y1:=max(lastY,downY);
        end else if selType=for_zoom then begin
          factor:=system.sqrt((system.sqr(lastX-downX)+system.sqr(lastY-downY))/(system.sqr(image.width)+system.sqr(image.height)));
          x1:=round(factor*image.width);
          y1:=round(factor*image.height);
          x0:=downX-x1;
          y0:=downY-y1;
          x1:=downX+x1;
          y1:=downY+y1;
        end else begin
          selectionRect0.Visible:=false;
          selectionRect1.Visible:=false;
          selectionRect2.Visible:=false;
          exit;
        end;
      end;
      selectionRect0.Visible:=true;
      selectionRect1.Visible:=true;
      selectionRect2.Visible:=true;
      selectionRect0.Left  :=image.Left+x0;
      selectionRect1.Left  :=image.Left+round((x0+x1)/2);
      selectionRect2.Left  :=image.Left+x0;
      selectionRect0.Top   :=image.Top+y0;
      selectionRect1.Top   :=image.Top+y0;
      selectionRect2.Top   :=image.Top+round((y0+y1)/2);
      selectionRect0.width :=x1-x0;
      selectionRect1.width :=1;
      selectionRect2.width :=x1-x0;
      selectionRect0.height:=y1-y0;
      selectionRect1.height:=y1-y0;
      selectionRect2.height:=1;
    end;

  begin
    with mouseSelection do begin
      lastX:=x;
      lastY:=y;
      mouseHoversOverImage:=true;
    end;
    drawSelectionRect;

    if currentAlgoMeta.hasScaler
    then statusBarParts.crosshairMessage:=P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scaler.getPositionString(x,y,', ')
    else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
    updateStatusBar;

    updateLight;
    updateAlgoScaler;
    updatePan;
  end;

PROCEDURE TDisplayMainForm.ImageMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    with mouseSelection do begin
      lastX:=x;
      lastY:=y;
    end;
    selectionRect0.Visible:=false;
    selectionRect1.Visible:=false;
    selectionRect2.Visible:=false;
    updateAlgoScaler(true);
    updatePan(true);
    mouseSelection.selType:=none;
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityHighClick(Sender: TObject);
  begin
    mi_renderQualityHigh.Checked:=true;
    mi_renderQualityPreview.Checked:=false;
    calculateImage;
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityPreviewClick(Sender: TObject);
  begin
    mi_renderQualityHigh.Checked:=false;
    mi_renderQualityPreview.Checked:=true;
  end;

PROCEDURE TDisplayMainForm.pickJuliaButtonClick(Sender: TObject);
begin

end;

PROCEDURE TDisplayMainForm.pickLightButtonClick(Sender: TObject);
  begin
    mouseSelection.selType:=for_light;
    pickLightHelperShape.Visible:=true;
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseDown(Sender,button,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.Top-image.Top);
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseMove(Sender,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.Top-image.Top);
  end;

PROCEDURE TDisplayMainForm.resetButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    currentAlgoMeta.prototype^.resetParameters(resetTypeComboBox.ItemIndex);
    for i:=0 to currentAlgoMeta.prototype^.numberOfParameters-1 do begin
      ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString;
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
    end;
    calculateImage;
  end;

PROCEDURE TDisplayMainForm.TimerTimer(Sender: TObject);
  VAR currentlyCalculating:boolean=false;
  begin
    if progressQueue.calculating then begin
      statusBarParts.progressMessage:=progressQueue.getProgressString;
      renderToImageNeeded:=true;
      currentlyCalculating:=true;
    end;
    inc(subTimerCounter);

    if renderToImageNeeded and (subTimerCounter and 7=0) then begin
      renderImage(imageGeneration.renderImage);
      renderToImageNeeded:=currentlyCalculating;
    end;

    updateStatusBar;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorEditButtonClick(Sender: TObject);
  begin
    ColorDialog.execute;
  end;

PROCEDURE TDisplayMainForm.evaluationFinished;
  begin
    renderToImageNeeded:=true;
    statusBarParts.progressMessage:=progressQueue.getProgressString;
  end;

PROCEDURE TDisplayMainForm.zoomOutButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    if currentAlgoMeta.hasScaler then begin
      P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.zoomOnPoint(image,2);
      for i:=0 to SCALER_PARAMETER_COUNT-1 do begin
        ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString;
        previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
      end;
      calculateImage;
    end;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorEditingDone(Sender: TObject);
  VAR i:longint;
      value:T_parameterValue;
  begin
    if (currentAlgoMeta.prototype^.numberOfParameters<>length(previousAlgorithmParameters)) or
       (currentAlgoMeta.prototype^.numberOfParameters+1<>ValueListEditor.RowCount) then algorithmComboBoxSelect(Sender);

    for i:=0 to currentAlgoMeta.prototype^.numberOfParameters-1 do
    if (ValueListEditor.Cells[1,i+1]<>previousAlgorithmParameters[i]) then begin
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
      value.createToParse(currentAlgoMeta.prototype^.parameterDescription(i),ValueListEditor.Cells[1,i+1]);
      if value.isValid
      then begin
        currentAlgoMeta.prototype^.setParameter(i,value);
        if (currentAlgoMeta.prototype^.parameterDescription(i)^.typ=pt_enum) or (i=LIGHT_NORMAL_INDEX) then
          ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString();
      end else begin
        statusBarParts.errorMessage:='Malformed parameter: '+currentAlgoMeta.prototype^.parameterDescription(i)^.describe;
        exit;
      end;
    end;
    pickLightButton.Enabled:=currentAlgoMeta.hasLight and (P_functionPerPixelViaRawDataAlgorithm(currentAlgoMeta.prototype)^.lightIsRelevant);
    statusBarParts.errorMessage:='';
    calculateImage;
  end;

PROCEDURE TDisplayMainForm.calculateImage;
  begin
    if currentAlgoMeta.prototype^.prepareImage(mi_renderQualityPreview.Checked)
    then begin
      renderImage(imageGeneration.renderImage);
      renderToImageNeeded:=false;
    end else renderToImageNeeded:=true;
  end;

PROCEDURE TDisplayMainForm.renderImage(VAR img: T_rawImage);
  begin
    img.copyToImage(image);
    image.width:=image.Picture.width;
    image.height:=image.Picture.height;
  end;

PROCEDURE TDisplayMainForm.updateStatusBar;
  begin
    with statusBarParts do begin
      if errorMessage=''
      then StatusBar.SimpleText:=progressMessage+C_tabChar+crosshairMessage
      else StatusBar.SimpleText:=progressMessage+C_tabChar+errorMessage;
    end;
  end;

PROCEDURE TDisplayMainForm.updateAlgoScaler(CONST finalize: boolean);
  VAR i:longint;
  begin
    with mouseSelection do if (selType=for_zoom) and currentAlgoMeta.hasScaler and finalize then begin
      if (system.sqr(lastX-downX)+system.sqr(lastY-downY)>900) then begin
        with P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scaler do begin
          recenter(transform(downX,downY));
          setZoom(zoomOnMouseDown*0.5*system.sqrt((system.sqr(image.width)+system.sqr(image.height))/(system.sqr(lastX-downX)+system.sqr(lastY-downY))));
        end;
        for i:=0 to SCALER_PARAMETER_COUNT-1 do begin
          ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString();
          if finalize then previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
        end;
        P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage;
      end else with P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scaler do begin
        setZoom(zoomOnMouseDown);
        for i:=0 to SCALER_PARAMETER_COUNT-1 do ValueListEditor.Cells[1,i+1]:=previousAlgorithmParameters[i];
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.updateLight(CONST finalize: boolean);
  VAR c:T_Complex;
  begin
    with mouseSelection do if (selType=for_light) and currentAlgoMeta.hasLight then begin
      c.re:=  (lastX-imageGeneration.renderImage.width /2);
      c.im:=1-(lastY-imageGeneration.renderImage.height/2);
      c:=c*(4/pickLightHelperShape.width);
      P_functionPerPixelViaRawDataAlgorithm(currentAlgoMeta.prototype)^.lightNormal:=toSphere(c)-blue;
      calculateImage;
      ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1]:=currentAlgoMeta.prototype^.getParameter(LIGHT_NORMAL_INDEX).toString;
      if finalize then begin
        previousAlgorithmParameters[LIGHT_NORMAL_INDEX]:=ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1];
        mouseSelection.selType:=none;
        pickLightHelperShape.Visible:=false;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.updatePan(CONST finalize: boolean);
  VAR i:longint;
  begin
    with mouseSelection do if (selType=for_pan) and (currentAlgoMeta.hasScaler) then begin
      P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.panByPixels(image,lastX-downX,lastY-downY);
      downX:=lastX;
      downY:=lastY;
      if finalize then begin
        for i:=0 to SCALER_PARAMETER_COUNT-1 do begin
          ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString();
          if finalize then previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
        end;
        P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage;
      end;
    end;
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

end.

