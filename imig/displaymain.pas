UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, workflows,editHelper,sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics, Menus, StdCtrls, ValEdit, ComCtrls,math,myStringUtil,
  complex,myColors,jobberUnit,fileHistories,
  EditBtn,imageGeneration,generationBasics,
  myGenerics,myParams,imageContexts;

TYPE

  { TDisplayMainForm }

  TDisplayMainForm = class(TForm)
    backToWorkflowButton: TButton;
    geneticsButton: TButton;
    cbRotateOnZoom: TCheckBox;
    editAlgorithmButton: TButton;
    mi_clearImage: TMenuItem;
    mi_scale_1_1: TMenuItem;
    mi_clear: TMenuItem;
    mi_hist2: TMenuItem;
    mi_hist9: TMenuItem;
    mi_hist4: TMenuItem;
    mi_hist5: TMenuItem;
    mi_hist6: TMenuItem;
    mi_hist7: TMenuItem;
    mi_hist8: TMenuItem;
    mi_hist3: TMenuItem;
    mi_hist1: TMenuItem;
    mi_hist0: TMenuItem;
    pmi_switchModes: TMenuItem;
    StepsValueListEditor: TValueListEditor;
    SwitchPopupMenu: TPopupMenu;
    StepsMemo: TMemo;
    submenuFile: TMenuItem;
    mi_renderToFile: TMenuItem;
    mi_load: TMenuItem;
    mi_save: TMenuItem;
    newStepEdit: TComboBox;
    pickLightHelperShape: TShape;
    SaveDialog: TSaveDialog;
    zoomOutButton: TButton;
    pickLightButton: TButton;
    pickJuliaButton: TButton;
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
    WorkFlowGroupBox: TGroupBox;
    newOrEditStepBox: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    image: TImage;
    selectionRect0: TShape;
    selectionRect1: TShape;
    selectionRect2: TShape;
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
    GroupBox1: TGroupBox;
    WorkingDirectoryEdit: TDirectoryEdit;
    miDuplicateStep: TMenuItem;
    CancelButton: TButton;
    submenuAddStep: TMenuItem;
    mis_generateImage: TMenuItem;
    miAccessImageRoot: TMenuItem;
    miGeometryRoot: TMenuItem;
    miColorsRoot: TMenuItem;
    miCombineRoot: TMenuItem;
    miStatisticOpRoot: TMenuItem;
    miFiltersRoot: TMenuItem;
    miMiscRoot: TMenuItem;
    PROCEDURE algorithmComboBoxSelect(Sender: TObject);
    PROCEDURE backToWorkflowButtonClick(Sender: TObject);
    PROCEDURE editAlgorithmButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE geneticsButtonClick(Sender: TObject);
    PROCEDURE ImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseLeave(Sender: TObject);
    PROCEDURE ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mi_clearClick(Sender: TObject);
    PROCEDURE mi_clearImageClick(Sender: TObject);
    PROCEDURE mi_hist0Click(Sender: TObject);
    PROCEDURE mi_hist1Click(Sender: TObject);
    PROCEDURE mi_hist2Click(Sender: TObject);
    PROCEDURE mi_hist3Click(Sender: TObject);
    PROCEDURE mi_hist4Click(Sender: TObject);
    PROCEDURE mi_hist5Click(Sender: TObject);
    PROCEDURE mi_hist6Click(Sender: TObject);
    PROCEDURE mi_hist7Click(Sender: TObject);
    PROCEDURE mi_hist8Click(Sender: TObject);
    PROCEDURE mi_hist9Click(Sender: TObject);
    PROCEDURE mi_loadClick(Sender: TObject);
    PROCEDURE mi_renderQualityHighClick(Sender: TObject);
    PROCEDURE mi_renderQualityPreviewClick(Sender: TObject);
    PROCEDURE mi_renderToFileClick(Sender: TObject);
    PROCEDURE mi_saveClick(Sender: TObject);
    PROCEDURE miAddCustomStepClick(Sender: TObject);
    PROCEDURE newStepEditEditingDone(Sender: TObject);
    PROCEDURE newStepEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE pickJuliaButtonClick(Sender: TObject);
    PROCEDURE pickLightButtonClick(Sender: TObject);
    PROCEDURE pickLightHelperShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE pickLightHelperShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE pmi_switchModesClick(Sender: TObject);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE StepsListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE StepsMemoEditingDone(Sender: TObject);
    PROCEDURE StepsValueListEditorButtonClick(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StepsValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
    PROCEDURE StepsValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE ValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
    PROCEDURE ValueListEditorValidateEntry(Sender: TObject; aCol,  aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE zoomOutButtonClick(Sender: TObject);
    PROCEDURE WorkingDirectoryEditEditingDone(Sender: TObject);
    PROCEDURE miDuplicateStepClick(Sender: TObject);
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE mis_generateImageClick(Sender: TObject);
  private
    mainWorkflow      :T_editorWorkflow;
    genPreviewWorkflow:T_generateImageWorkflow;
    messageQueue      :T_structuredMessageQueue;
    messageLog        :T_arrayOfString;

    mouseSelection:record
      mouseHoversOverImage:boolean;
      lastX,lastY:longint;
      downX,downY:longint;
      selType:(none,for_zoom,for_cropPending,for_crop,for_pan,for_light,for_julia);
      zoomOnMouseDown:double;
    end;
    statusBarParts:record
      logMessage,
      crosshairMessage:ansistring;
    end;

    stepGridSelectedRow:longint;
    algoGridSelectedRow:longint;
    editingGeneration:boolean;
    subTimer:record
      counter:longint;
      interval:longint;
    end;
    renderToImageNeeded:boolean;
    calculationPending :boolean;
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
    PROCEDURE switchToGenerationView;
    PROCEDURE switchToWorkflowView(CONST confirmModifications:boolean);
    PROCEDURE updateFileHistory;
    PROCEDURE openFromHistory(CONST idx:byte);
    PROCEDURE openFile(CONST nameUtf8:ansistring; CONST afterRecall:boolean);
    PROCEDURE enableDynamicItems;
  end;

VAR
  DisplayMainForm: TDisplayMainForm;
IMPLEMENTATION
USES strutils,pixMaps,ig_fractals,im_geometry,imageManipulation,geneticCreation;
{$R *.lfm}

PROCEDURE TDisplayMainForm.FormCreate(Sender: TObject);
  PROCEDURE prepareAlgorithms;
    VAR op:P_algorithmMeta;
    begin
      algorithmComboBox.items.clear;
      for op in imageGenerationAlgorithms do algorithmComboBox.items.append(op^.getName);
      if algorithmComboBox.items.count>0 then algorithmComboBox.ItemIndex:=0;
      algorithmComboBoxSelect(Sender);
    end;

  PROCEDURE prepareWorkflowParts;
    VAR operationIndex:longint;
        op:P_imageOperationMeta;
        parentItem,
        newItem:TMenuItem;
    begin
      newStepEdit.items.clear;
      for operationIndex:=0 to length(allImageOperations)-1 do begin
        op:=allImageOperations[operationIndex];
        if op^.category<>imc_generation then begin
          case op^.category of
            imc_imageAccess: parentItem:=miAccessImageRoot;
            imc_geometry   : parentItem:=miGeometryRoot;
            imc_colors     : parentItem:=miColorsRoot;
            imc_combination: parentItem:=miCombineRoot;
            imc_statistic  : parentItem:=miStatisticOpRoot;
            imc_filter     : parentItem:=miFiltersRoot;
            else             parentItem:=miMiscRoot;
          end;
          newItem:=TMenuItem.create(MainMenu);
          newItem.caption:=op^.getName;
          newItem.Tag:=operationIndex;
          newItem.OnClick:=@miAddCustomStepClick;
          parentItem.add(newItem);
          newStepEdit.items.add(op^.getDefaultParameterString);
        end;
      end;
      newStepEdit.sorted:=true;
      newStepEdit.ItemIndex:=0;
      editAlgorithmButton.enabled:=true;
    end;

  begin
    {$ifdef CPU32}caption:=caption+' (32bit)';{$endif}
    calculationPending:=false;
    messageQueue.create;
    messageQueue.messageStringLengthLimit:=200;
    messageQueue.Post('Initializing',false,-1,0);
    setLength(messageLog,0);
    mainWorkflow      .createEditorWorkflow(@messageQueue);
    genPreviewWorkflow.createOneStepWorkflow(@messageQueue,@mainWorkflow);

    WorkingDirectoryEdit.text:=GetCurrentDir;
    mouseSelection.selType:=none;
    subTimer.counter:=0;
    subTimer.interval:=1;
    renderToImageNeeded:=false;
    prepareAlgorithms;
    prepareWorkflowParts;
    redisplayWorkflow;
    imageGenerationPanel.width:=0;
    imageGenerationPanel.enabled:=false;
    Splitter2.enabled:=false;
    editingGeneration:=false;
    updateFileHistory;
    if (paramCount=1) and fileExists(expandFileName(paramStr(1))) then openFile(expandFileName(paramStr(1)),false);
    messageQueue.Post('Initialization done',false,-1,0);
  end;

PROCEDURE TDisplayMainForm.FormDestroy(Sender: TObject);
  begin
    timer.enabled:=false;
    {$ifdef debugMode}writeln(stdErr,'DEBUG FormDestroy: Destroying genPreviewWorkflow');{$endif}
    genPreviewWorkflow.destroy;
    {$ifdef debugMode}writeln(stdErr,'DEBUG FormDestroy: Destroying mainWorkflow');{$endif}
    mainWorkflow.destroy;
    {$ifdef debugMode}writeln(stdErr,'DEBUG FormDestroy: Destroying messageQueue');{$endif}
    messageQueue.destroy;
    {$ifdef debugMode}writeln(stdErr,'DEBUG FormDestroy: done');{$endif}
  end;

PROCEDURE TDisplayMainForm.algorithmComboBoxSelect(Sender: TObject);
  begin
    genPreviewWorkflow.algorithmIndex:=algorithmComboBox.ItemIndex;
    algorithmComboBox.ItemIndex:=genPreviewWorkflow.algorithmIndex;
    enableDynamicItems;
    calculateImage(false);
  end;

PROCEDURE TDisplayMainForm.backToWorkflowButtonClick(Sender: TObject);
  begin
    switchToWorkflowView(true);
  end;

PROCEDURE TDisplayMainForm.editAlgorithmButtonClick(Sender: TObject);
  begin
    if startsWith(newStepEdit.text,cropMeta^.getName+':') then begin
      mouseSelection.selType:=for_cropPending;
      exit;
    end;
  end;

PROCEDURE TDisplayMainForm.FormResize(Sender: TObject);
  VAR res:T_imageDimensions;
  begin
    res:=imageDimensions(ScrollBox1.width,ScrollBox1.height);
    if mi_scale_4_3     .checked then res:=res.getFittingRectangle(4/3);
    if mi_Scale_3_4     .checked then res:=res.getFittingRectangle(3/4);
    if mi_scale_16_10   .checked then res:=res.getFittingRectangle(16/10);
    if mi_scale_16_9    .checked then res:=res.getFittingRectangle(16/9);
    if mi_scale_1_1     .checked then res:=res.getFittingRectangle(1);
    if mi_scale_original.checked then res:=C_maxImageDimensions;
    mainWorkflow.config.sizeLimit:=res;
    if mi_scale_original.checked then mainWorkflow.config.initialResolution:=imageDimensions(1,1)
                                 else mainWorkflow.config.initialResolution:=res;
    calculateImage(false);
  end;

PROCEDURE TDisplayMainForm.geneticsButtonClick(Sender: TObject);
  begin
    timer.enabled:=false;
    if editGenetics(@genPreviewWorkflow,resetTypeComboBox.ItemIndex) then begin
      enableDynamicItems;
      calculateImage(false);
    end;
    timer.enabled:=true;
  end;

PROCEDURE TDisplayMainForm.ImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
        else if genPreviewWorkflow.algorithm^.hasScaler then begin
          selType:=for_zoom;
          zoomOnMouseDown:=P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scaler.getZoom;
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
    selectionRect0.visible:=false;
    selectionRect1.visible:=false;
    selectionRect2.visible:=false;
    if not(mouseSelection.selType in [for_light,for_julia])
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
          selectionRect0.visible:=false;
          selectionRect1.visible:=false;
          selectionRect2.visible:=false;
          exit;
        end;
      end;
      selectionRect0.visible:=true;
      selectionRect1.visible:=true;
      selectionRect2.visible:=true;
      selectionRect0.Left  :=image.Left+x0;
      selectionRect1.Left  :=image.Left+round((x0+x1)/2);
      selectionRect2.Left  :=image.Left+x0;
      selectionRect0.top   :=image.top+y0;
      selectionRect1.top   :=image.top+y0;
      selectionRect2.top   :=image.top+round((y0+y1)/2);
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
    if editingGeneration then begin
      if genPreviewWorkflow.algorithm^.hasScaler
      then statusBarParts.crosshairMessage:=P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scaler.getPositionString(x,y,', ')
      else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
    end else begin
      statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
    end;
    updateStatusBar;
    updateLight;
    updateAlgoScaler;
    updatePan;
  end;

PROCEDURE TDisplayMainForm.ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  PROCEDURE updateCrop;
    VAR cropOperation:P_simpleImageOperation;
    begin
      with mouseSelection do
      cropOperation:=cropMeta^.getOperationToCrop(
        min(downX,lastX)/image.width,
        max(downX,lastX)/image.width,
        min(downY,lastY)/image.height,
        max(downY,lastY)/image.height);
      newStepEdit.text:=cropOperation^.toString(tsm_withNiceParameterName);
      dispose(cropOperation,destroy);
    end;

  begin
    with mouseSelection do begin
      lastX:=x;
      lastY:=y;
    end;
    if mouseSelection.selType=for_crop then updateCrop;
    selectionRect0.visible:=false;
    selectionRect1.visible:=false;
    selectionRect2.visible:=false;
    updateAlgoScaler(true);
    updatePan(true);
    mouseSelection.selType:=none;
  end;

PROCEDURE TDisplayMainForm.mi_clearClick(Sender: TObject);
  begin
    WorkingDirectoryEdit.enabled:=true;
    mainWorkflow.config.setInitialImage('');
    mainWorkflow.clear;
    redisplayWorkflow;
    if mi_scale_original.checked then begin
      mi_scale_fit.checked:=true;
      FormResize(Sender);
    end;
    calculateImage(false);
    enableDynamicItems;
  end;

PROCEDURE TDisplayMainForm.mi_clearImageClick(Sender: TObject);
  begin
    mainWorkflow.config.setInitialImage('');
    calculateImage(false);
    enableDynamicItems;
  end;

PROCEDURE TDisplayMainForm.mi_hist0Click(Sender: TObject); begin openFromHistory(0); end;
PROCEDURE TDisplayMainForm.mi_hist1Click(Sender: TObject); begin openFromHistory(1); end;
PROCEDURE TDisplayMainForm.mi_hist2Click(Sender: TObject); begin openFromHistory(2); end;
PROCEDURE TDisplayMainForm.mi_hist3Click(Sender: TObject); begin openFromHistory(3); end;
PROCEDURE TDisplayMainForm.mi_hist4Click(Sender: TObject); begin openFromHistory(4); end;
PROCEDURE TDisplayMainForm.mi_hist5Click(Sender: TObject); begin openFromHistory(5); end;
PROCEDURE TDisplayMainForm.mi_hist6Click(Sender: TObject); begin openFromHistory(6); end;
PROCEDURE TDisplayMainForm.mi_hist7Click(Sender: TObject); begin openFromHistory(7); end;
PROCEDURE TDisplayMainForm.mi_hist8Click(Sender: TObject); begin openFromHistory(8); end;
PROCEDURE TDisplayMainForm.mi_hist9Click(Sender: TObject); begin openFromHistory(9); end;

PROCEDURE TDisplayMainForm.mi_loadClick(Sender: TObject);
  begin
    if (OpenDialog.execute) then openFile(OpenDialog.fileName,false);
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityHighClick(Sender: TObject);
  begin
    mi_renderQualityHigh.checked:=true;
    mi_renderQualityPreview.checked:=false;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.mi_renderQualityPreviewClick(Sender: TObject);
  begin
    mi_renderQualityHigh.checked:=false;
    mi_renderQualityPreview.checked:=true;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.mi_renderToFileClick(Sender: TObject);
  VAR workingDir:string;
  begin
    workingDir:=GetCurrentDir;
    timer.enabled:=false;
    Hide;
    showJobberForm(@mainWorkflow);
    SetCurrentDir(workingDir);
    Show;
    redisplayWorkflow;
    enableDynamicItems;
    FormResize(Sender); //Ensure scaling
    timer.enabled:=true;
  end;

PROCEDURE TDisplayMainForm.mi_saveClick(Sender: TObject);
  begin
    if editingGeneration then exit;
    if mainWorkflow.config.workflowFilename<>'' then SaveDialog.fileName:=mainWorkflow.config.workflowFilename;
    if SaveDialog.execute then begin
      if uppercase(extractFileExt(SaveDialog.fileName))=C_workflowExtension
      then begin
        mainWorkflow.saveToFile(SaveDialog.fileName);
        addToHistory(SaveDialog.fileName);
        updateFileHistory;
        SetCurrentDir(ExtractFileDir(SaveDialog.fileName));
        WorkingDirectoryEdit.caption:=GetCurrentDir;
        WorkingDirectoryEdit.enabled:=false;
      end else begin
        mainWorkflow.image.saveToFile(SaveDialog.fileName);
        addToHistory(SaveDialog.fileName);
        updateFileHistory;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.miAddCustomStepClick(Sender: TObject);
  VAR operationIndex:ptrint;
      meta:P_imageOperationMeta;
  begin
    operationIndex:=TMenuItem(Sender).Tag;
    meta:=allImageOperations[operationIndex];
    if mainWorkflow.addStep(meta^.getDefaultParameterString) then calculationPending:=true;
    redisplayWorkflow;
    enableDynamicItems;
  end;

PROCEDURE TDisplayMainForm.newStepEditEditingDone(Sender: TObject);
  begin
    editAlgorithmButton.enabled:=startsWith(newStepEdit.text,cropMeta^.getName+':');
  end;

PROCEDURE TDisplayMainForm.newStepEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key=13) and (ssShift in Shift) then begin
      if mainWorkflow.addStep(newStepEdit.text) then calculationPending:=true;
      redisplayWorkflow;
      enableDynamicItems;
    end;
  end;

PROCEDURE TDisplayMainForm.pickJuliaButtonClick(Sender: TObject);
  begin
    mouseSelection.selType:=for_julia;
  end;

PROCEDURE TDisplayMainForm.pickLightButtonClick(Sender: TObject);
  begin
    mouseSelection.selType:=for_light;
    pickLightHelperShape.visible:=true;
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseDown(Sender,button,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.top-image.top);
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseMove(Sender,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.top-image.top);
  end;

PROCEDURE TDisplayMainForm.pmi_switchModesClick(Sender: TObject);
  begin
    StepsMemo.visible:=not(StepsMemo.visible);
    StepsMemo.enabled:=not(StepsMemo.enabled);
    StepsValueListEditor.visible:=not(StepsValueListEditor.visible);
    StepsValueListEditor.enabled:=not(StepsValueListEditor.enabled);
    if StepsValueListEditor.visible then begin
      redisplayWorkflow;
      enableDynamicItems;
      calculateImage(false);
    end;
  end;

PROCEDURE TDisplayMainForm.resetButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    genPreviewWorkflow.algorithm^.prototype^.resetParameters(resetTypeComboBox.ItemIndex);
    for i:=0 to genPreviewWorkflow.algorithm^.prototype^.numberOfParameters-1 do
      ValueListEditor.Cells[1,i+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(i).toString;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.StepsListBoxKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  CONST KEY_UP       =38;
        KEY_DOWN     =40;
        KEY_DEL      =46;
        KEY_BACKSPACE= 8;
  VAR k:longint;
  begin
    if (key=KEY_UP) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsValueListEditor.selection.top-1>0) then begin
      StepsValueListEditor.EditorMode:=false;
      mainWorkflow.swapStepDown(StepsValueListEditor.selection.top-2);
      StepsValueListEditor.selection:=rect(StepsValueListEditor.selection.Left    ,
                                           StepsValueListEditor.selection.top   -1,
                                           StepsValueListEditor.selection.Right   ,
                                           StepsValueListEditor.selection.Bottom-1);
      if not mainWorkflow.executing then calculateImage(false);
      redisplayWorkflow;
      enableDynamicItems;
      key:=0;
      exit;
    end;
    if (key=KEY_DOWN) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsValueListEditor.selection.top-1<mainWorkflow.stepCount-1) then begin
      StepsValueListEditor.EditorMode:=false;
      mainWorkflow.swapStepDown(StepsValueListEditor.selection.top-1);
      StepsValueListEditor.selection:=rect(StepsValueListEditor.selection.Left    ,
                                           StepsValueListEditor.selection.top   +1,
                                           StepsValueListEditor.selection.Right   ,
                                           StepsValueListEditor.selection.Bottom+1);
      if not mainWorkflow.executing then calculateImage(false);
      redisplayWorkflow;
      enableDynamicItems;
      key:=0;
      exit;
    end;
    if ((key=KEY_DEL) or (key=KEY_BACKSPACE)) and (ssShift in Shift) then begin
      StepsValueListEditor.EditorMode:=false;
      for k:=StepsValueListEditor.selection.Bottom-1 downto StepsValueListEditor.selection.top-1 do mainWorkflow.removeStep(k);
      if not mainWorkflow.executing then calculateImage(false);
      redisplayWorkflow;
      enableDynamicItems;
      exit;
    end;
  end;

PROCEDURE TDisplayMainForm.StepsMemoEditingDone(Sender: TObject);
  VAR i:longint;
      memoText:T_arrayOfString;
  begin
    setLength(memoText,StepsMemo.lines.count);
    for i:=0 to StepsMemo.lines.count-1 do memoText[i]:=StepsMemo.lines[i];
    if not(mainWorkflow.parseWorkflow(memoText,true)) then begin
      memoText:=mainWorkflow.workflowText;
      StepsMemo.clear;
      for i:=0 to length(memoText)-1 do StepsMemo.lines.add(memoText[i]);
    end;
  end;

PROCEDURE TDisplayMainForm.StepsValueListEditorButtonClick(Sender: TObject; aCol, aRow: integer);
  begin
    stepGridSelectedRow:=aRow-1;
    if genPreviewWorkflow.startEditing(stepGridSelectedRow)
    then begin
      switchToGenerationView;
      algorithmComboBox.ItemIndex:=genPreviewWorkflow.algorithmIndex;
    end
    else begin
      StepsValueListEditor.EditorMode:=false;
      if showEditHelperForm(@mainWorkflow,stepGridSelectedRow)
      then calculateImage(false);
      redisplayWorkflow;
      enableDynamicItems;
    end;
  end;

PROCEDURE TDisplayMainForm.StepsValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
  begin
    if (aRow-1<0) or (aRow-1>=mainWorkflow.stepCount) then begin
      redisplayWorkflow;
      enableDynamicItems;
      exit;
    end;
    stepGridSelectedRow:=aRow-1;
    if  (mainWorkflow.step[stepGridSelectedRow]<>nil) and
        (mainWorkflow.step[stepGridSelectedRow]^.outputImage<>nil)
    then renderImage(mainWorkflow.step[stepGridSelectedRow]^.outputImage^);
  end;

PROCEDURE TDisplayMainForm.StepsValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR index:longint;
  begin
    index:=aRow-1;
    if (oldValue=newValue) or (index<0) or (index>=mainWorkflow.stepCount) then exit;
    if mainWorkflow.step[index]^.operation^.alterParameter(newValue) then begin
      mainWorkflow.stepChanged(index);
      redisplayWorkflow;
      enableDynamicItems;
      if not mainWorkflow.executing then calculateImage(false);
    end else begin
      newValue:=oldValue;
      redisplayWorkflow;
      enableDynamicItems;
    end;
  end;

PROCEDURE TDisplayMainForm.TimerTimer(Sender: TObject);
  VAR currentlyCalculating:boolean=false;
      timeToDisplay:double;
  PROCEDURE pollMessage;
    begin
      append(messageLog,messageQueue.getText);
      if length(messageLog)>50 then dropFirst(messageLog,length(messageLog)-50);
      StatusBar.Hint:=join(messageLog,LineEnding);
    end;

  begin
    if  editingGeneration and genPreviewWorkflow.executing or
    not(editingGeneration) and mainWorkflow.executing then begin
      renderToImageNeeded:=true;
      currentlyCalculating:=true;
    end;
    if not(editingGeneration) and not(currentlyCalculating) and calculationPending then calculateImage(false);
    pollMessage;
    inc(subTimer.counter);
    if renderToImageNeeded and not(currentlyCalculating) or
       (subTimer.counter>=subTimer.interval) and currentlyCalculating then begin
      timeToDisplay:=now;
      if editingGeneration
      then renderImage(genPreviewWorkflow.image)
      else renderImage(mainWorkflow      .image);
      renderToImageNeeded:=currentlyCalculating;
      timeToDisplay:=(timeToDisplay-now)*24*60*60*1000/timer.interval;
      subTimer.interval:=round(2*timeToDisplay);
      if subTimer.interval<10 then subTimer.interval:=10;
      subTimer.counter:=0;
    end;
    updateStatusBar;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
  begin
    algoGridSelectedRow:=aRow-1;
  end;

PROCEDURE TDisplayMainForm.ValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR index:longint;
      value:T_parameterValue;
  begin
    index:=aRow-1;
    if (newValue=oldValue) or (index<0) or (index>=genPreviewWorkflow.algorithm^.prototype^.numberOfParameters) then exit;
    value.createToParse(genPreviewWorkflow.algorithm^.prototype^.parameterDescription(index),newValue);
    if value.isValid
    then begin
      genPreviewWorkflow.algorithm^.prototype^.setParameter(index,value);
      if (genPreviewWorkflow.algorithm^.prototype^.parameterDescription(index)^.getType=pt_enum) or (index=LIGHT_NORMAL_INDEX) then
        ValueListEditor.Cells[1,index+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(index).toString();
      pickLightButton.enabled:=genPreviewWorkflow.algorithm^.hasLight and (P_functionPerPixelViaRawDataAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.lightIsRelevant);
      calculateImage(false);
    end else begin
      newValue:=oldValue;
      exit;
    end;
  end;

PROCEDURE TDisplayMainForm.zoomOutButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    if genPreviewWorkflow.algorithm^.hasScaler then begin
      P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.zoomOnPoint(image,2);
      for i:=0 to SCALER_PARAMETER_COUNT-1 do
        ValueListEditor.Cells[1,i+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(i).toString;
      calculateImage(false);
    end;
  end;

PROCEDURE TDisplayMainForm.WorkingDirectoryEditEditingDone(Sender: TObject);
  begin
    SetCurrentDir(WorkingDirectoryEdit.text);
  end;

PROCEDURE TDisplayMainForm.miDuplicateStepClick(Sender: TObject);
  VAR dupIdx:longint;
  begin
    for dupIdx:=StepsValueListEditor.selection.top-1
             to StepsValueListEditor.selection.Bottom-1 do if (dupIdx>=0) and (dupIdx<mainWorkflow.stepCount) then begin
      mainWorkflow.addStep(mainWorkflow.step[dupIdx]^.operation^.toString(tsm_withNiceParameterName));
      redisplayWorkflow;
      enableDynamicItems;
      calculationPending:=true;
    end;
  end;

PROCEDURE TDisplayMainForm.cancelButtonClick(Sender: TObject);
  begin
    switchToWorkflowView(false);
  end;

PROCEDURE TDisplayMainForm.mis_generateImageClick(Sender: TObject);
  begin
    stepGridSelectedRow:=mainWorkflow.stepCount-1;
    genPreviewWorkflow.startEditingForNewStep;
    switchToGenerationView;
    algorithmComboBox.ItemIndex:=0;
    algorithmComboBoxSelect(Sender);
  end;

PROCEDURE TDisplayMainForm.calculateImage(CONST manuallyTriggered: boolean);
  begin
    if not(manuallyTriggered or mi_renderQualityPreview.checked) then exit;
    if editingGeneration then begin
      {$ifdef debugMode}
      writeln(stdErr,'DEBUG starting generation preview calculation in background');
      {$endif}
      genPreviewWorkflow.executeWorkflowInBackground(mi_renderQualityPreview.checked);
      renderToImageNeeded:=true;
      calculationPending :=false;
    end else begin
      {$ifdef debugMode}
      writeln(stdErr,'DEBUG starting main workflow calculation in background');
      {$endif}
      mainWorkflow.executeWorkflowInBackground(mi_renderQualityPreview.checked);
      renderToImageNeeded:=true;
      calculationPending :=false;
    end;
  end;

PROCEDURE TDisplayMainForm.renderImage(VAR img: T_rawImage);
  VAR retried:longint=0;
      isOkay:boolean=false;
      xShift,yShift:longint;
  begin
    repeat
      try
        img.copyToImage(image);
        isOkay:=true;
      except
        {$ifdef debugMode}
        writeln(stdErr,'DEBUG painting image failed #',retried);
        {$endif}
      end;
      inc(retried);
    until isOkay or (retried>=3);
    if not(isOkay) then exit;
    image.width :=image.picture.width;
    image.height:=image.picture.height;
    xShift:=(ScrollBox1.width -image.width ) div 2; if xShift<0 then xShift:=0;
    yShift:=(ScrollBox1.height-image.height) div 2; if yShift<0 then yShift:=0;
    image.Left:=xShift;
    image.top :=yShift;
    pickLightHelperShape.width:=min(image.width,image.height);
    pickLightHelperShape.height:=pickLightHelperShape.width;
    pickLightHelperShape.top:=image.top+(image.height-pickLightHelperShape.height) shr 1;
    pickLightHelperShape.Left:=image.Left+(image.width-pickLightHelperShape.width) shr 1;
  end;

PROCEDURE TDisplayMainForm.updateStatusBar;
  VAR lastLogLine:string='';
  begin
    with statusBarParts do begin
      if length(messageLog)>0
      then lastLogLine:=messageLog[length(messageLog)-1];
      StatusBar.SimpleText:=lastLogLine+C_tabChar+crosshairMessage;
    end;
  end;

PROCEDURE TDisplayMainForm.updateAlgoScaler(CONST finalize: boolean);
  VAR i:longint;
  begin
    with mouseSelection do if (selType=for_zoom) and genPreviewWorkflow.algorithm^.hasScaler and finalize then begin
      if (system.sqr(lastX-downX)+system.sqr(lastY-downY)>900) then begin
        with P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scaler do begin
          recenter(transform(downX,downY));
          if cbRotateOnZoom.checked then rotateToHorizontal(lastX-downX,lastY-downY);
          setZoom(zoomOnMouseDown*0.5*system.sqrt((system.sqr(image.width)+system.sqr(image.height))/(system.sqr(lastX-downX)+system.sqr(lastY-downY))));
        end;
        for i:=0 to SCALER_PARAMETER_COUNT-1 do
          ValueListEditor.Cells[1,i+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(i).toString();
        P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage(false);
      end else with P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scaler do
        setZoom(zoomOnMouseDown);
    end;
  end;

PROCEDURE TDisplayMainForm.updateLight(CONST finalize: boolean);
  VAR c:T_Complex;
  begin
    with mouseSelection do if (selType=for_light) and genPreviewWorkflow.algorithm^.hasLight then begin
      c.re:=1-(lastX-genPreviewWorkflow.image.dimensions.width /2);
      c.im:=  (lastY-genPreviewWorkflow.image.dimensions.height/2);
      c:=c*(4/pickLightHelperShape.width);
      P_functionPerPixelViaRawDataAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.lightNormal:=toSphere(c)-BLUE;
      calculateImage(false);
      ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(LIGHT_NORMAL_INDEX).toString;
      if finalize then begin
        mouseSelection.selType:=none;
        pickLightHelperShape.visible:=false;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.updateJulia;
  begin
    with mouseSelection do if (selType=for_julia) and genPreviewWorkflow.algorithm^.hasJuliaP then begin
      with P_functionPerPixelViaRawDataJuliaAlgorithm(genPreviewWorkflow.algorithm^.prototype)^ do juliaParam:=scaler.transform(lastX,lastY);
      ValueListEditor.Cells[1,JULIA_COORD_INDEX+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(JULIA_COORD_INDEX).toString;
      mouseSelection.selType:=none;
      calculateImage(false);
    end;
  end;

PROCEDURE TDisplayMainForm.updatePan(CONST finalize: boolean);
  VAR i:longint;
  begin
    with mouseSelection do if (selType=for_pan) and (genPreviewWorkflow.algorithm^.hasScaler) then begin
      P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.panByPixels(image,lastX-downX,lastY-downY);
      downX:=lastX;
      downY:=lastY;
      if finalize then begin
        for i:=0 to SCALER_PARAMETER_COUNT-1 do begin
          ValueListEditor.Cells[1,i+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(i).toString();
        end;
        P_scaledImageGenerationAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage(false);
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.redisplayWorkflow;
  VAR i:longint;
  begin
    StepsMemo.lines.clear;
    StepsValueListEditor.rowCount:=mainWorkflow.stepCount+1;
    for i:=0 to mainWorkflow.stepCount-1 do begin
      StepsValueListEditor.Cells[0,i+1]:=mainWorkflow.step[i]^.toStringPart(false);
      StepsValueListEditor.Cells[1,i+1]:=mainWorkflow.step[i]^.toStringPart(true);
      if mainWorkflow.step[i]^.hasComplexParameterDescription
      then StepsValueListEditor.ItemProps[i].EditStyle:=esEllipsis
      else StepsValueListEditor.ItemProps[i].EditStyle:=esSimple;
      StepsMemo.lines.append(mainWorkflow.step[i]^.specification);
    end;
    WorkFlowGroupBox.caption:=C_workflowTypeString[mainWorkflow.workflowType]+' workflow';
  end;

PROCEDURE TDisplayMainForm.switchToGenerationView;
  begin
    if editingGeneration then raise Exception.create('Invalid state change to generation');
    mainWorkflow.ensureStop;
    //Adapt panel sizes
    imageGenerationPanel.width:=workflowPanel.width;
    workflowPanel.width:=0;
    //Switch
    editingGeneration:=true;
    enableDynamicItems;
  end;

PROCEDURE TDisplayMainForm.switchToWorkflowView(CONST confirmModifications: boolean);
  begin
    if not(editingGeneration) then raise Exception.create('Invalid state change to main workflow');
    genPreviewWorkflow.ensureStop;
    if confirmModifications then begin
      mainWorkflow.ensureStop;
      genPreviewWorkflow.confirmEditing;
    end;
    //Adapt panel sizes
    workflowPanel.width:=imageGenerationPanel.width;
    imageGenerationPanel.width:=0;
    //Switch
    editingGeneration:=false;
    enableDynamicItems;
    redisplayWorkflow;
    calculateImage(false);
  end;

PROCEDURE TDisplayMainForm.enableDynamicItems;
  VAR i:longint;
      resetStyles:T_arrayOfString;
      parDesc:P_parameterDescription;
      enumString:string;
  begin
    mi_scale_original   .enabled:=not(editingGeneration) and (mainWorkflow.config.initialImageName<>'') or (mainWorkflow.workflowType in [wft_fixated,wft_halfFix]);
    mi_scale_16_10      .enabled:=editingGeneration or (mainWorkflow.config.initialImageName='');
    mi_scale_16_9       .enabled:=editingGeneration or (mainWorkflow.config.initialImageName='');
    mi_Scale_3_4        .enabled:=editingGeneration or (mainWorkflow.config.initialImageName='');
    mi_scale_4_3        .enabled:=editingGeneration or (mainWorkflow.config.initialImageName='');
    mi_scale_1_1        .enabled:=editingGeneration or (mainWorkflow.config.initialImageName='');
    mi_save             .enabled:=not(editingGeneration);
    mi_clear            .enabled:=not(editingGeneration);
    mi_load             .enabled:=not(editingGeneration);
    mi_hist0            .enabled:=not(editingGeneration);
    mi_hist1            .enabled:=not(editingGeneration);
    mi_hist2            .enabled:=not(editingGeneration);
    mi_hist3            .enabled:=not(editingGeneration);
    mi_hist4            .enabled:=not(editingGeneration);
    mi_hist5            .enabled:=not(editingGeneration);
    mi_hist6            .enabled:=not(editingGeneration);
    mi_hist7            .enabled:=not(editingGeneration);
    mi_hist8            .enabled:=not(editingGeneration);
    mi_hist9            .enabled:=not(editingGeneration);
    workflowPanel       .enabled:=not(editingGeneration);
    Splitter1           .enabled:=not(editingGeneration);
    Splitter2           .enabled:=    editingGeneration;
    imageGenerationPanel.enabled:=    editingGeneration;

    submenuAddStep.enabled:=not(editingGeneration);
    submenuFile.enabled:=not(editingGeneration);

    cbRotateOnZoom .visible:=genPreviewWorkflow.algorithm^.hasScaler;
    zoomOutButton  .visible:=genPreviewWorkflow.algorithm^.hasScaler;
    pickLightButton.visible:=genPreviewWorkflow.algorithm^.hasLight;
    pickLightButton.enabled:=genPreviewWorkflow.algorithm^.hasLight and (P_functionPerPixelViaRawDataAlgorithm(genPreviewWorkflow.algorithm^.prototype)^.lightIsRelevant);
    pickJuliaButton.visible:=genPreviewWorkflow.algorithm^.hasJuliaP;
    pickJuliaButton.enabled:=true;
    geneticsButton.visible:=genPreviewWorkflow.algorithm^.isUsableForGenetics;
    geneticsButton.enabled:=true;

    resetTypeComboBox.items.clear;
    resetStyles:=genPreviewWorkflow.algorithm^.prototype^.parameterResetStyles;
    for enumString in resetStyles do  resetTypeComboBox.items.append(enumString);
    if length(resetStyles)> 0 then resetTypeComboBox.ItemIndex:=0;
    resetTypeComboBox.enabled:=length(resetStyles)>1;

    ValueListEditor.clear;
    ValueListEditor.ClearSelections;
    ValueListEditor.rowCount:=genPreviewWorkflow.algorithm^.prototype^.numberOfParameters+1;

    for i:=0 to genPreviewWorkflow.algorithm^.prototype^.numberOfParameters-1 do begin
      parDesc:=genPreviewWorkflow.algorithm^.prototype^.parameterDescription(i);
      ValueListEditor.Cells[0,i+1]:=genPreviewWorkflow.algorithm^.prototype^.parameterDescription(i)^.getName;
      ValueListEditor.Cells[1,i+1]:=genPreviewWorkflow.algorithm^.prototype^.getParameter(i).toString;
      if parDesc^.getType=pt_enum then with ValueListEditor.ItemProps[i] do begin
        EditStyle:=esPickList;
        readonly:=true;
        PickList.clear;
        for enumString in parDesc^.getEnumValues do PickList.add(enumString);
      end else if parDesc^.getType=pt_color then ValueListEditor.ItemProps[i].EditStyle:=esEllipsis
                                            else ValueListEditor.ItemProps[i].EditStyle:=esSimple;
    end;

  end;

PROCEDURE TDisplayMainForm.updateFileHistory;
  PROCEDURE setHist(CONST index:byte; CONST name:ansistring);
    VAR h:TMenuItem;
    begin
      case index of
        0: h:=mi_hist0;
        1: h:=mi_hist1;
        2: h:=mi_hist2;
        3: h:=mi_hist3;
        4: h:=mi_hist4;
        5: h:=mi_hist5;
        6: h:=mi_hist6;
        7: h:=mi_hist7;
        8: h:=mi_hist8;
        9: h:=mi_hist9;
      end;
      if name='' then begin
        h.enabled:=false;
        h.visible:=false;
      end else begin
        h.enabled:=true;
        h.visible:=true;
        h.caption:='&'+intToStr(index)+'  '+ansiReplaceStr(name,'&',' + ');
      end;
    end;
  VAR i:longint;
  begin
    limitHistory(20); //...even though only 10 items are displayed
    for i:=0 to 9 do
    if length(history)>i
    then setHist(i,history[i])
    else setHist(i,'');
  end;

PROCEDURE TDisplayMainForm.openFromHistory(CONST idx: byte);
  VAR fileSet:T_arrayOfString;
      i:longint;
  begin
    if editingGeneration or (idx>=length(history)) then exit;
    fileSet:=split(history[idx],'&');
    for i:=0 to length(fileSet)-1 do openFile(fileSet[i],true);
    historyItemRecalled(idx);
    updateFileHistory;
  end;

PROCEDURE TDisplayMainForm.openFile(CONST nameUtf8: ansistring; CONST afterRecall: boolean);
  FUNCTION isImageExtension(CONST ext:string):boolean;
    VAR s:string;
    begin
      for s in SUPPORTED_IMAGE_TYPES do if ext=s then exit(true);
      result:=false;
    end;
  begin
    if (uppercase(extractFileExt(nameUtf8))=C_workflowExtension) or
       (uppercase(extractFileExt(nameUtf8))=C_todoExtension) then begin
      mainWorkflow.readFromFile(nameUtf8,true);
      if not(afterRecall) then begin
        addToHistory(nameUtf8,mainWorkflow.config.initialImageName);
        updateFileHistory;
      end;
      SetCurrentDir(mainWorkflow.config.associatedDirectory);
      WorkingDirectoryEdit.caption:=GetCurrentDir;
      WorkingDirectoryEdit.enabled:=false;
      calculateImage(false);
      redisplayWorkflow;
    end else if isImageExtension(uppercase(extractFileExt(nameUtf8))) then begin
      mainWorkflow.config.setInitialImage(nameUtf8);
      mainWorkflow.stepChanged(0);
      if not(afterRecall) then begin
        if (mainWorkflow.config.workflowFilename<>'')
        then addToHistory(mainWorkflow.config.workflowFilename,nameUtf8)
        else addToHistory(nameUtf8);
        updateFileHistory;
      end;
      calculateImage(false);
      if not mi_scale_fit.checked then mi_scale_original.checked:=true;
    end else messageQueue.Post('Cannot handle file '+nameUtf8,true,-1,0);
    enableDynamicItems;
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
end.

