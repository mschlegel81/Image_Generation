UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls, ValEdit, ComCtrls,math,myStringUtil,
  complex,myColors,jobberUnit,
  LCLTranslator,
  workflows,
  imageGeneration,
  ig_gradient,
  ig_perlin,
  ig_simples,
  ig_julia_fractals,
  ig_fractals,
  ig_epicycles,
  myGenerics,myParams;

TYPE

  { TDisplayMainForm }

  TDisplayMainForm = class(TForm)
    backToWorkflowButton: TButton;
    editAlgorithmButton: TButton;
    pmi_switchModes: TMenuItem;
    SwitchPopupMenu: TPopupMenu;
    StepsMemo: TMemo;
    MenuItem3: TMenuItem;
    mi_renderToFile: TMenuItem;
    mi_load: TMenuItem;
    mi_save: TMenuItem;
    newStepEdit: TComboBox;
    pickLightHelperShape: TShape;
    SaveDialog: TSaveDialog;
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
    algorithmComboBox: TComboBox;
    resetTypeComboBox: TComboBox;
    GroupBox2: TGroupBox;
    newOrEditStepBox: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    image: TImage;
    selectionRect0: TShape;
    selectionRect1: TShape;
    selectionRect2: TShape;
    StepsListBox: TListBox;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    workflowPanel: TPanel;
    imageGenerationPanel: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    timer: TTimer;
    ValueListEditor: TValueListEditor;
    PROCEDURE algorithmComboBoxSelect(Sender: TObject);
    PROCEDURE backToWorkflowButtonClick(Sender: TObject);
    PROCEDURE editAlgorithmButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE ImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseLeave(Sender: TObject);
    PROCEDURE ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mi_loadClick(Sender: TObject);
    PROCEDURE mi_renderQualityHighClick(Sender: TObject);
    PROCEDURE mi_renderQualityPreviewClick(Sender: TObject);
    PROCEDURE mi_renderToFileClick(Sender: TObject);
    PROCEDURE mi_saveClick(Sender: TObject);
    PROCEDURE newStepEditEditingDone(Sender: TObject);
    PROCEDURE newStepEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE pickJuliaButtonClick(Sender: TObject);
    PROCEDURE pickLightButtonClick(Sender: TObject);
    PROCEDURE pickLightHelperShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE pickLightHelperShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE pmi_switchModesClick(Sender: TObject);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE StepsListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE StepsListBoxSelectionChange(Sender: TObject; User: boolean);
    PROCEDURE StepsMemoEditingDone(Sender: TObject);
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
      selType:(none,for_zoom,for_cropPending,for_crop,for_pan,for_light,for_julia);
      zoomOnMouseDown:double;
    end;
    statusBarParts:record
      errorMessage,
      progressMessage,
      crosshairMessage:ansistring;
    end;

    editingWorkflow:boolean;
    previousAlgorithmParameters:array of ansistring;
    subTimerCounter:longint;
    currentAlgoMeta:T_algorithmMeta;
    renderToImageNeeded:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE calculateImage(CONST manuallyTriggered:boolean);
    PROCEDURE renderImage(VAR img:T_rawImage);
    PROCEDURE updateStatusBar;
    PROCEDURE updateAlgoScaler(CONST finalize:boolean=false);
    PROCEDURE updateLight(CONST finalize:boolean=false);
    PROCEDURE updateJulia;
    PROCEDURE updatePan(CONST finalize:boolean=false);

    PROCEDURE redisplayWorkflow;
    PROCEDURE switchModes;
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

  PROCEDURE prepareWorkflowParts;
    VAR imt:T_imageManipulationType;
    begin
      newStepEdit.Items.clear;
      for imt:=Low(T_imageManipulationType) to high(T_imageManipulationType) do
      if imt<>imt_generateImage then newStepEdit.Items.add(stepParamDescription[imt]^.name+':');
      newStepEdit.Sorted:=true;
            newStepEdit.Sorted:=false;
      newStepEdit.Items.Insert(0,'<GENERATE>');
      newStepEdit.ItemIndex:=0;
      editAlgorithmButton.Enabled:=true;
    end;

  begin
    mouseSelection.selType:=none;
    subTimerCounter:=0;
    renderToImageNeeded:=false;
    prepareAlgorithms;
    prepareWorkflowParts;
    redisplayWorkflow;
    imageGenerationPanel.width:=0;
    imageGenerationPanel.Enabled:=false;
    Splitter2.Enabled:=false;
    workflows.progressQueue.registerOnEndCallback(@evaluationFinished);
    imageGeneration.progressQueue.registerOnEndCallback(nil);
    editingWorkflow:=true;
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
    pickJuliaButton.Enabled:=true;

    resetTypeComboBox.Items.clear;
    resetStyles:=currentAlgoMeta.prototype^.parameterResetStyles;
    for i:=0 to length(resetStyles)-1 do resetTypeComboBox.Items.append(resetStyles[i]);
    if length(resetStyles)>0 then resetTypeComboBox.ItemIndex:=0;

    ValueListEditor.clear;
    ValueListEditor.ClearSelections;
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
    calculateImage(false);
  end;

PROCEDURE TDisplayMainForm.backToWorkflowButtonClick(Sender: TObject);
  begin
    switchModes;
  end;

PROCEDURE TDisplayMainForm.editAlgorithmButtonClick(Sender: TObject);
  VAR idx:longint;
  begin
    idx:=isPlausibleSpecification(newStepEdit.text,true);
    switchModes;
    if idx>=0 then begin
      algorithmComboBox.ItemIndex:=idx;
      algorithmComboBoxSelect(Sender);
    end;
  end;

PROCEDURE TDisplayMainForm.FormResize(Sender: TObject);
  VAR destRect:TRect;
  begin
    workflows.progressQueue.ensureStop;
    if editingWorkflow and (inputImage<>nil) then begin
      if mi_scale_original.Checked then begin
        destRect:=Rect(0,0,inputImage^.width,inputImage^.height);
        if (workflowImage.width<>inputImage^.width) or
           (workflowImage.height<>inputImage^.height)
        then workflowImage.copyFromImage(inputImage^);
      end else begin
        destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,inputImage^.width/inputImage^.height);
        if (workflowImage.width<>destRect.Right) or
           (workflowImage.height<>destRect.Bottom)
        then begin
          workflowImage.copyFromImage(inputImage^);
          workflowImage.resize(destRect.Right,destRect.Bottom,res_fit);
        end;
      end;
    end else begin
      destRect:=Rect(0,0,ScrollBox1.width,ScrollBox1.height);
      if mi_scale_4_3  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height, 4/3);
      if mi_Scale_3_4  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height, 3/4);
      if mi_scale_16_10.Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/10);
      if mi_scale_16_9 .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/9);
      workflowImage.resize(destRect.Right,destRect.Bottom,res_dataResize);
    end;
    generationImage^.resize(destRect.Right,destRect.Bottom,res_dataResize);

    if mi_scale_original.Checked then begin
      image.Left:=0;
      image.Top:=0;
    end else begin
      image.Left:=(ScrollBox1.width-destRect.Right) shr 1;
      image.Top :=(ScrollBox1.height-destRect.Bottom) shr 1;
    end;
    pickLightHelperShape.width:=min(destRect.Right,destRect.Bottom);
    pickLightHelperShape.height:=pickLightHelperShape.width;
    pickLightHelperShape.Top:=image.Top+(destRect.Bottom-pickLightHelperShape.height) shr 1;
    pickLightHelperShape.Left:=image.Left+(destRect.Right-pickLightHelperShape.width) shr 1;


    if editingWorkflow then renderImage(workflowImage)
                       else calculateImage(false);
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
      end else if selType=for_julia then begin
        updateJulia;
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

    if editingWorkflow then begin
      if inputImage<>nil
      then statusBarParts.crosshairMessage:=intToStr(round(x/workflowImage.width*inputImage^.width))+', '+intToStr(round(y/workflowImage.height*inputImage^.height))
      else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
    end else begin
      if currentAlgoMeta.hasScaler
      then statusBarParts.crosshairMessage:=P_scaledImageGenerationAlgorithm(currentAlgoMeta.prototype)^.scaler.getPositionString(x,y,', ')
      else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
    end;
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



PROCEDURE TDisplayMainForm.mi_loadClick(Sender: TObject);
  begin
    if (OpenDialog.execute) then begin
      if uppercase(extractFileExt(OpenDialog.fileName))='.WF' then begin
        workflows.progressQueue.ensureStop;
        workflow.loadFromFile(OpenDialog.fileName);
        redisplayWorkflow;
      end else begin
        if inputImage=nil then new(inputImage,create(OpenDialog.fileName))
                          else inputImage^.loadFromFile(OpenDialog.fileName);
        mi_scale_original.Enabled:=true;
        mi_scale_16_10.Enabled:=false;
        mi_scale_16_9.Enabled:=false;
        mi_Scale_3_4.Enabled:=false;
        mi_scale_4_3.Enabled:=false;
        if not mi_scale_fit.Checked then mi_scale_original.Checked:=true;
      end;
      if not(editingWorkflow)
      then switchModes
      else FormResize(Sender);
    end;
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityHighClick(Sender: TObject);
  begin
    mi_renderQualityHigh.Checked:=true;
    mi_renderQualityPreview.Checked:=false;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityPreviewClick(Sender: TObject);
  begin
    mi_renderQualityHigh.Checked:=false;
    mi_renderQualityPreview.Checked:=true;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.mi_renderToFileClick(Sender: TObject);
  begin
    timer.Enabled:=false;
    Hide;
    jobberForm.init;
    jobberForm.ShowModal;
    show;
    timer.Enabled:=true;
  end;

PROCEDURE TDisplayMainForm.mi_saveClick(Sender: TObject);
  begin
    if SaveDialog.execute then begin
      if uppercase(extractFileExt(SaveDialog.fileName))='.WF'
      then workflow.saveToFile(SaveDialog.fileName)
      else workflowImage.saveToFile(SaveDialog.fileName);
    end;
  end;

PROCEDURE TDisplayMainForm.newStepEditEditingDone(Sender: TObject);
  begin
    editAlgorithmButton.Enabled:=(newStepEdit.ItemIndex=0) or (newStepEdit.text=newStepEdit.Items[0]);
  end;

PROCEDURE TDisplayMainForm.newStepEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and (ssShift in Shift) then begin
      workflow.addStep(newStepEdit.text);
      redisplayWorkflow;
    end;
  end;

PROCEDURE TDisplayMainForm.pickJuliaButtonClick(Sender: TObject);
  begin
    mouseSelection.selType:=for_julia;
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

PROCEDURE TDisplayMainForm.pmi_switchModesClick(Sender: TObject);
begin
  StepsMemo.Visible:=not(StepsMemo.Visible);
  StepsMemo.Enabled:=not(StepsMemo.Enabled);
  StepsListBox.Visible:=not(StepsListBox.Visible);
  StepsListBox.Enabled:=not(StepsListBox.Enabled);
  if StepsListBox.Visible then redisplayWorkflow;
end;

PROCEDURE TDisplayMainForm.resetButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    currentAlgoMeta.prototype^.resetParameters(resetTypeComboBox.ItemIndex);
    for i:=0 to currentAlgoMeta.prototype^.numberOfParameters-1 do begin
      ValueListEditor.Cells[1,i+1]:=currentAlgoMeta.prototype^.getParameter(i).toString;
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
    end;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.StepsListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  CONST KEY_UP=38;
        KEY_DOWN=40;
        KEY_DEL=46;
        KEY_BACKSPACE=8;
  begin
    if (key=KEY_UP) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsListBox.ItemIndex>0) then begin
      workflow.swapStepDown(StepsListBox.ItemIndex-1);
      StepsListBox.ItemIndex:=StepsListBox.ItemIndex-1;
      redisplayWorkflow;
      exit;
    end;
    if (key=KEY_DOWN) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsListBox.ItemIndex<workflow.stepCount-1) then begin
      workflow.swapStepDown(StepsListBox.ItemIndex);
      StepsListBox.ItemIndex:=StepsListBox.ItemIndex+1;
      redisplayWorkflow;
      exit;
    end;
    if (key=KEY_DEL) or (key=KEY_BACKSPACE) then begin
      workflow.remStep(StepsListBox.ItemIndex);
      redisplayWorkflow;
      exit;
    end;
  end;

PROCEDURE TDisplayMainForm.StepsListBoxSelectionChange(Sender: TObject; User: boolean);
  begin
    if User then begin
      workflow.renderIntermediate(StepsListBox.ItemIndex,image);
      newStepEdit.text:=workflow.stepText(StepsListBox.ItemIndex);
      editAlgorithmButton.Enabled:=isPlausibleSpecification(newStepEdit.text,false)>=0;
    end;
  end;

PROCEDURE TDisplayMainForm.StepsMemoEditingDone(Sender: TObject);
  VAR i:longint;
  begin
    workflow.clear;
    for i:=0 to StepsMemo.lines.count-1 do if not(startsWith(StepsMemo.lines[i],'//')) then begin
      if not(workflow.addStep(StepsMemo.lines[i])) then StepsMemo.lines[i]:='//'+StepsMemo.lines[i];
    end;
  end;

PROCEDURE TDisplayMainForm.TimerTimer(Sender: TObject);
  VAR currentlyCalculating:boolean=false;
  begin
    if not(editingWorkflow) and imageGeneration.progressQueue.calculating then begin
      statusBarParts.progressMessage:=imageGeneration.progressQueue.getProgressString;
      renderToImageNeeded:=true;
      currentlyCalculating:=true;
    end;
    if editingWorkflow and workflows.progressQueue.calculating then begin
      statusBarParts.progressMessage:=workflows.progressQueue.getProgressString;
      renderToImageNeeded:=true;
      currentlyCalculating:=true;
    end;
    inc(subTimerCounter);

    if renderToImageNeeded and (subTimerCounter and 7=0) then begin
      if editingWorkflow then renderImage(workflowImage)
                         else renderImage(generationImage^);
      renderToImageNeeded:=currentlyCalculating;
    end;

    updateStatusBar;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorEditButtonClick(Sender: TObject);
  VAR c24:longint;
  begin
    if ColorDialog.execute then begin
      c24:=ColorDialog.color;
      ValueListEditor.Cells[1,ValueListEditor.Selection.Top]:=
        formatFloat('0.000',((c24       ) and 255)/255)+','+
        formatFloat('0.000',((c24 shr  8) and 255)/255)+','+
        formatFloat('0.000',((c24 shr 16) and 255)/255);
      ValueListEditor.EditingDone;
    end;
  end;

PROCEDURE TDisplayMainForm.evaluationFinished;
  begin
    renderToImageNeeded:=true;
    if editingWorkflow then statusBarParts.progressMessage:=workflows      .progressQueue.getProgressString
                       else statusBarParts.progressMessage:=imageGeneration.progressQueue.getProgressString;
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
      calculateImage(false);
    end;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorEditingDone(Sender: TObject);
  VAR i:longint;
      value:T_parameterValue;
      changes:boolean=false;
  begin
    if (currentAlgoMeta.prototype^.numberOfParameters<>length(previousAlgorithmParameters)) or
       (currentAlgoMeta.prototype^.numberOfParameters+1<>ValueListEditor.RowCount) then begin
      algorithmComboBoxSelect(Sender);
      changes:=true;
    end;

    for i:=0 to currentAlgoMeta.prototype^.numberOfParameters-1 do
    if (ValueListEditor.Cells[1,i+1]<>previousAlgorithmParameters[i]) then begin
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
      value.createToParse(currentAlgoMeta.prototype^.parameterDescription(i),ValueListEditor.Cells[1,i+1]);
      if value.isValid
      then begin
        changes:=true;
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
    if changes then calculateImage(false);
  end;

PROCEDURE TDisplayMainForm.calculateImage(CONST manuallyTriggered:boolean);
  begin
    if editingWorkflow then begin
      workflow.execute(mi_renderQualityPreview.Checked,true,workflowImage.width,workflowImage.height);
      renderToImageNeeded:=true;
    end else begin
      if not(manuallyTriggered or mi_renderQualityPreview.Checked) then exit;
      if currentAlgoMeta.prototype^.prepareImage(mi_renderQualityPreview.Checked)
      then begin
        renderImage(generationImage^);
        renderToImageNeeded:=false;
      end else renderToImageNeeded:=true;
    end;
  end;

PROCEDURE TDisplayMainForm.renderImage(VAR img: T_rawImage);
  VAR retried:longint=0;
      isOkay:boolean=false;
  begin
    repeat
      try
        img.copyToImage(image);
        isOkay:=true;
      except
        isOkay:=false;
      end;
      inc(retried);
    until isOkay or (retried>=3);
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
        calculateImage(false);
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
      c.re:=  (lastX-generationImage^.width /2);
      c.im:=1-(lastY-generationImage^.height/2);
      c:=c*(4/pickLightHelperShape.width);
      P_functionPerPixelViaRawDataAlgorithm(currentAlgoMeta.prototype)^.lightNormal:=toSphere(c)-blue;
      calculateImage(false);
      ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1]:=currentAlgoMeta.prototype^.getParameter(LIGHT_NORMAL_INDEX).toString;
      if finalize then begin
        previousAlgorithmParameters[LIGHT_NORMAL_INDEX]:=ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1];
        mouseSelection.selType:=none;
        pickLightHelperShape.Visible:=false;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.updateJulia;
  begin
    with mouseSelection do if (selType=for_julia) and currentAlgoMeta.hasJuliaP then begin
      with P_functionPerPixelViaRawDataJuliaAlgorithm(currentAlgoMeta.prototype)^ do juliaParam:=scaler.transform(lastX,lastY);
      ValueListEditor.Cells[1,JULIA_COORD_INDEX+1]:=currentAlgoMeta.prototype^.getParameter(JULIA_COORD_INDEX).toString;
      previousAlgorithmParameters[JULIA_COORD_INDEX]:=ValueListEditor.Cells[1,JULIA_COORD_INDEX+1];
      mouseSelection.selType:=none;
      calculateImage(false);
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
        calculateImage(false);
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.redisplayWorkflow;
  VAR lastIdx,i:longint;
  begin
    lastIdx:=StepsListBox.ItemIndex;
    StepsListBox.Items.clear;
    StepsMemo.lines.clear;
    for i:=0 to workflow.stepCount-1 do begin
      StepsListBox.Items.add(workflow.stepText(i));
      StepsMemo.lines.append(workflow.stepText(i));
    end;
    if lastIdx< 0                  then lastIdx:=0;
    if lastIdx>=StepsListBox.count then lastIdx:=StepsListBox.count-1;
    StepsListBox.ItemIndex:=lastIdx;
  end;

PROCEDURE TDisplayMainForm.switchModes;
  begin
    workflows.progressQueue.ensureStop;
    editingWorkflow:=not(editingWorkflow);
    if editingWorkflow then begin
      workflowPanel.width:=imageGenerationPanel.width;
      imageGenerationPanel.width:=0;
      imageGeneration.progressQueue.registerOnEndCallback(nil);

      newStepEdit.Caption:=currentAlgoMeta.prototype^.toString;

      mi_scale_original.Enabled:=(inputImage<>nil);
      mi_scale_16_10.Enabled:=(inputImage=nil);
      mi_scale_16_9 .Enabled:=(inputImage=nil);
      mi_Scale_3_4  .Enabled:=(inputImage=nil);
      mi_scale_4_3  .Enabled:=(inputImage=nil);
    end else begin
      imageGenerationPanel.width:=workflowPanel.width;
      workflowPanel.width:=0;
      imageGeneration.progressQueue.registerOnEndCallback(@evaluationFinished);

      mi_scale_original.Enabled:=false;
      mi_scale_16_10.Enabled:=true;
      mi_scale_16_9 .Enabled:=true;
      mi_Scale_3_4  .Enabled:=true;
      mi_scale_4_3  .Enabled:=true;
    end;
    workflowPanel       .Enabled:=editingWorkflow;
    Splitter1           .Enabled:=editingWorkflow;
    Splitter2           .Enabled:=not(editingWorkflow);
    imageGenerationPanel.Enabled:=not(editingWorkflow);
    FormResize(nil);
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

end.

