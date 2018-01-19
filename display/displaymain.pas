UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, editHelper,sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls, ValEdit, ComCtrls,math,myStringUtil,
  complex,myColors,jobberUnit,fileHistories,
  LCLTranslator, EditBtn,
  workflows,
  imageGeneration,
  ig_gradient,
  ig_perlin,
  ig_simples,
  ig_fractals,
  ig_epicycles,
  ig_ifs,
  ig_ifs2,
  ig_bifurcation,
  ig_funcTrees,
  ig_expoClouds,
  myGenerics,myParams;

TYPE
  T_formState=(fs_editingWorkflow,fs_editingGeneration,fs_geneticSelection);
  TDisplayMainForm = class(TForm)
    backToWorkflowButton: TButton;
    editAlgorithmButton: TButton;
    mi_clear: TMenuItem;
    mi_hist2: TMenuItem;
    mi_hist9: TMenuItem;
    pmi_workflowAddGeneration: TMenuItem;
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
    MenuItem4: TMenuItem;
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
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE ImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseLeave(Sender: TObject);
    PROCEDURE ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mi_clearClick(Sender: TObject);
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
    PROCEDURE pmi_workflowAddGenerationClick(Sender: TObject);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE StepsListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE StepsMemoEditingDone(Sender: TObject);
    PROCEDURE StepsValueListEditorButtonClick(Sender: TObject; aCol,
      aRow: integer);
    PROCEDURE StepsValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
    PROCEDURE StepsValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE evaluationFinished;
    PROCEDURE ValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
    PROCEDURE ValueListEditorValidateEntry(Sender: TObject; aCol,  aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE zoomOutButtonClick(Sender: TObject);
    PROCEDURE WorkingDirectoryEditEditingDone(Sender: TObject);
    PROCEDURE miDuplicateStepClick(Sender: TObject);
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE mis_generateImageClick(Sender: TObject);
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
    stepGridSelectedRow:longint;
    algoGridSelectedRow:longint;
    switchFromWorkflowEdit:boolean;
    lastLoadedImage:ansistring;
    formMode:T_formState;
    subTimer:record
      counter:longint;
      interval:longint;
    end;
    currentAlgoMeta:P_algorithmMeta;
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
    FUNCTION switchModes(CONST newMode:T_formState; CONST cancelEditing:boolean=false):boolean;
    PROCEDURE updateFileHistory;
    PROCEDURE openFromHistory(CONST idx:byte);
    PROCEDURE openFile(CONST nameUtf8:ansistring; CONST afterRecall:boolean);
  end;

VAR
  DisplayMainForm: TDisplayMainForm;
  defaultGenerationImage:T_rawImage;
IMPLEMENTATION

{$R *.lfm}

PROCEDURE TDisplayMainForm.FormCreate(Sender: TObject);
  PROCEDURE prepareAlgorithms;
    VAR i:longint;
    begin
      algorithmComboBox.items.clear;
      for i:=0 to length(algorithms)-1 do algorithmComboBox.items.append(algorithms[i]^.getName);
      if length(algorithms)>0 then algorithmComboBox.ItemIndex:=0;
      algorithmComboBoxSelect(Sender);
    end;

  PROCEDURE prepareWorkflowParts;
    VAR imt:T_imageManipulationType;
        parentItem,
        newItem:TMenuItem;
    begin
      newStepEdit.items.clear;
      for imt:=low(T_imageManipulationType) to high(T_imageManipulationType) do
      if imt<>imt_generateImage then begin
        case imageManipulationCategory[imt] of
          imc_imageAccess: parentItem:=miAccessImageRoot;
          imc_geometry   : parentItem:=miGeometryRoot;
          imc_colors     : parentItem:=miColorsRoot;
          imc_combination: parentItem:=miCombineRoot;
          imc_statistic  : parentItem:=miStatisticOpRoot;
          imc_filter     : parentItem:=miFiltersRoot;
          else             parentItem:=miMiscRoot;
        end;
        newItem:=TMenuItem.create(MainMenu);
        newItem.caption:=stepParamDescription[imt]^.name;
        newItem.Tag:=ptrint(imt);
        newItem.OnClick:=@miAddCustomStepClick;
        parentItem.add(newItem);

        if stepParamDescription[imt]^.typ=pt_none
        then newStepEdit.items.add(stepParamDescription[imt]^.name    )
        else newStepEdit.items.add(stepParamDescription[imt]^.getDefaultParameterValue.toString(tsm_withNiceParameterName));
      end;
      newStepEdit.sorted:=true;
            newStepEdit.sorted:=false;
      newStepEdit.items.Insert(0,'<GENERATE>');
      newStepEdit.ItemIndex:=0;
      editAlgorithmButton.enabled:=true;
    end;

  begin
    {$ifdef CPU32}caption:=caption+' (32bit)';{$endif}
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
    workflow.progressQueue.registerOnEndCallback(@evaluationFinished);
    formMode:=fs_editingWorkflow;
    lastLoadedImage:='';
    updateFileHistory;
    if (paramCount=1) and fileExists(expandFileName(paramStr(1))) then openFile(expandFileName(paramStr(1)),false);
  end;

PROCEDURE TDisplayMainForm.algorithmComboBoxSelect(Sender: TObject);
  VAR i,j:longint;
      resetStyles:T_arrayOfString;
      parDesc:P_parameterDescription;
  begin
    if (algorithmComboBox.ItemIndex<0) or (algorithmComboBox.ItemIndex>=length(algorithms)) then exit;
    currentAlgoMeta:=algorithms[algorithmComboBox.ItemIndex];

    zoomOutButton.visible:=currentAlgoMeta^.hasScaler;
    pickLightButton.visible:=currentAlgoMeta^.hasLight;
    pickLightButton.enabled:=false;
    pickJuliaButton.visible:=currentAlgoMeta^.hasJuliaP;
    pickJuliaButton.enabled:=true;

    resetTypeComboBox.items.clear;
    resetStyles:=currentAlgoMeta^.prototype^.parameterResetStyles;
    for i:=0 to length(resetStyles)-1 do resetTypeComboBox.items.append(resetStyles[i]);
    if length(resetStyles)>0 then resetTypeComboBox.ItemIndex:=0;

    ValueListEditor.clear;
    ValueListEditor.ClearSelections;
    ValueListEditor.RowCount:=currentAlgoMeta^.prototype^.numberOfParameters+1;

    for i:=0 to currentAlgoMeta^.prototype^.numberOfParameters-1 do begin
      parDesc:=currentAlgoMeta^.prototype^.parameterDescription(i);
      ValueListEditor.Cells[0,i+1]:=currentAlgoMeta^.prototype^.parameterDescription(i)^.name;
      ValueListEditor.Cells[1,i+1]:=currentAlgoMeta^.prototype^.getParameter(i).toString;
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
    switchModes(fs_editingWorkflow);
  end;

PROCEDURE TDisplayMainForm.editAlgorithmButtonClick(Sender: TObject);
  VAR idx:longint;
  begin
    if startsWith(newStepEdit.text,stepParamDescription[imt_crop]^.name+':') then begin
      mouseSelection.selType:=for_cropPending;
      exit;
    end;
    idx:=isPlausibleSpecification(newStepEdit.text,true);
    switchModes(fs_editingGeneration);
    switchFromWorkflowEdit:=false;
    if idx>=0 then begin
      algorithmComboBox.ItemIndex:=idx;
      algorithmComboBoxSelect(Sender);
    end;
  end;

PROCEDURE TDisplayMainForm.FormResize(Sender: TObject);
  VAR destRect:TRect;
  begin
    workflow.progressQueue.ensureStop;
    if (formMode=fs_editingWorkflow) and (inputImage<>nil) then begin
      if mi_scale_original.Checked then begin
        destRect:=rect(0,0,inputImage^.dimensions.width,inputImage^.dimensions.height);
        if (workflow.workflowImage.dimensions.width<>inputImage^.dimensions.width) or
           (workflow.workflowImage.dimensions.height<>inputImage^.dimensions.height)
        then workflow.workflowImage.copyFromPixMap(inputImage^);
      end else begin
        destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,inputImage^.dimensions.width/inputImage^.dimensions.height);
        if (workflow.workflowImage.dimensions.width<>destRect.Right) or
           (workflow.workflowImage.dimensions.height<>destRect.Bottom)
        then begin
          workflow.workflowImage.copyFromPixMap(inputImage^);
          workflow.workflowImage.resize(destRect.Right,destRect.Bottom,res_fit);
        end;
      end;
    end else begin
      destRect:=rect(0,0,ScrollBox1.width,ScrollBox1.height);
      if mi_scale_4_3  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height, 4/3);
      if mi_Scale_3_4  .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height, 3/4);
      if mi_scale_16_10.Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/10);
      if mi_scale_16_9 .Checked then destRect:=getFittingRectangle(ScrollBox1.width,ScrollBox1.height,16/9);
      workflow.workflowImage.resize(destRect.Right,destRect.Bottom,res_dataResize);
    end;
    defaultGenerationImage.resize(destRect.Right,destRect.Bottom,res_dataResize);

    if mi_scale_original.Checked then begin
      image.Left:=0;
      image.top:=0;
    end else begin
      image.Left:=(ScrollBox1.width-destRect.Right) shr 1;
      image.top :=(ScrollBox1.height-destRect.Bottom) shr 1;
    end;
    pickLightHelperShape.width:=min(destRect.Right,destRect.Bottom);
    pickLightHelperShape.height:=pickLightHelperShape.width;
    pickLightHelperShape.top:=image.top+(destRect.Bottom-pickLightHelperShape.height) shr 1;
    pickLightHelperShape.Left:=image.Left+(destRect.Right-pickLightHelperShape.width) shr 1;

    case formMode of
      fs_editingWorkflow: renderImage(workflow.workflowImage);
      fs_editingGeneration: calculateImage(false);
    end;
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
        else if currentAlgoMeta^.hasScaler then begin
          selType:=for_zoom;
          zoomOnMouseDown:=P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scaler.getZoom;
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
    if mouseSelection.selType<>for_light
    then mouseSelection.selType:=none;
  end;

PROCEDURE TDisplayMainForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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

    case formMode of
      fs_editingWorkflow: begin
        if inputImage<>nil
        then statusBarParts.crosshairMessage:=intToStr(round(x/workflow.workflowImage.dimensions.width*inputImage^.dimensions.width))+', '+
                                              intToStr(round(y/workflow.workflowImage.dimensions.height*inputImage^.dimensions.height))
        else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
      end;
      fs_editingGeneration: begin
        if currentAlgoMeta^.hasScaler
        then statusBarParts.crosshairMessage:=P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scaler.getPositionString(x,y,', ')
        else statusBarParts.crosshairMessage:=intToStr(x)+', '+intToStr(y);
      end;
    end;
    updateStatusBar;
    updateLight;
    updateAlgoScaler;
    updatePan;
  end;

PROCEDURE TDisplayMainForm.ImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  PROCEDURE updateCrop;
    VAR param:T_parameterValue;
    begin
      with mouseSelection do
      param.createFromValue(stepParamDescription[imt_crop],
                            min(downX,lastX)/image.width,
                            max(downX,lastX)/image.width,
                            min(downY,lastY)/image.height,
                            max(downY,lastY)/image.height);
      writeln(stdErr,param.toString(tsm_forSerialization));
      newStepEdit.text:=param.toString(tsm_forSerialization);
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
    if inputImage<>nil then begin
      dispose(inputImage,destroy);
      inputImage:=nil;
    end;
    lastLoadedImage:='';
    workflow.clear;
    redisplayWorkflow;
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
  VAR reloadInput:boolean=false;
  begin
    timer.enabled:=false;
    Hide;
    if inputImage<>nil then begin
      reloadInput:=true;
      dispose(inputImage,destroy);
      inputImage:=nil;
    end;
    jobberForm.init(lastLoadedImage);
    jobberForm.ShowModal;
    workflow.progressQueue.registerOnEndCallback(@evaluationFinished);
    Show;
    if workflow.isTempTodo then workflow.clear;
    redisplayWorkflow;
    if reloadInput then new(inputImage,create(lastLoadedImage));
    FormResize(Sender); //Ensure scaling
    timer.enabled:=true;
  end;

PROCEDURE TDisplayMainForm.mi_saveClick(Sender: TObject);
  begin
    if workflow.associatedFile<>'' then SaveDialog.fileName:=workflow.associatedFile;
    if SaveDialog.execute then begin
      if uppercase(extractFileExt(SaveDialog.fileName))='.WF'
      then begin
        workflow.saveToFile(SaveDialog.fileName);
        if (inputImage<>nil)
        then addToHistory(SaveDialog.fileName,lastLoadedImage)
        else addToHistory(SaveDialog.fileName);
        updateFileHistory;
        SetCurrentDir(workflow.associatedDir);
        WorkingDirectoryEdit.caption:=GetCurrentDir;
        WorkingDirectoryEdit.enabled:=false;
      end else begin
        workflow.workflowImage.saveToFile(SaveDialog.fileName);
        if (workflow.associatedFile<>'')
        then addToHistory(workflow.associatedFile,SaveDialog.fileName)
        else addToHistory(SaveDialog.fileName);
        updateFileHistory;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.miAddCustomStepClick(Sender: TObject);
  VAR imt:T_imageManipulationType;
  begin
    imt:=T_imageManipulationType(TMenuItem(Sender).Tag);
    workflow.addStep(stepParamDescription[imt]^.getDefaultParameterString);
    redisplayWorkflow;
  end;

PROCEDURE TDisplayMainForm.newStepEditEditingDone(Sender: TObject);
  begin
    editAlgorithmButton.enabled:=(newStepEdit.ItemIndex=0) or
                                 (newStepEdit.text=newStepEdit.items[0]) or
                                 startsWith(newStepEdit.text,stepParamDescription[imt_crop]^.name+':');
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
    pickLightHelperShape.visible:=true;
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseDown(Sender,button,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.top-image.top);
  end;

PROCEDURE TDisplayMainForm.pickLightHelperShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    ImageMouseMove(Sender,Shift,X+pickLightHelperShape.Left-image.Left,Y+pickLightHelperShape.top-image.top);
  end;

PROCEDURE TDisplayMainForm.pmi_switchModesClick(Sender: TObject);
  begin
    StepsMemo.visible:=not(StepsMemo.visible);
    StepsMemo.enabled:=not(StepsMemo.enabled);
    StepsValueListEditor.visible:=not(StepsValueListEditor.visible);
    StepsValueListEditor.enabled:=not(StepsValueListEditor.enabled);
    if StepsValueListEditor.visible then redisplayWorkflow;
  end;

PROCEDURE TDisplayMainForm.pmi_workflowAddGenerationClick(Sender: TObject);
  begin
    workflow.addStep(defaultGenerationString);
    stepGridSelectedRow:=workflow.stepCount-1;
    switchModes(fs_editingGeneration);
    switchFromWorkflowEdit:=true;
    algorithmComboBox.ItemIndex:=0;
    algorithmComboBoxSelect(Sender);
  end;

PROCEDURE TDisplayMainForm.resetButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    currentAlgoMeta^.prototype^.resetParameters(resetTypeComboBox.ItemIndex);
    for i:=0 to currentAlgoMeta^.prototype^.numberOfParameters-1 do
      ValueListEditor.Cells[1,i+1]:=currentAlgoMeta^.prototype^.getParameter(i).toString;
    calculateImage(true);
  end;

PROCEDURE TDisplayMainForm.StepsListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  CONST KEY_UP=38;
        KEY_DOWN=40;
        KEY_DEL=46;
        KEY_BACKSPACE=8;
  begin
    if (key=KEY_UP) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsValueListEditor.selection.top-1>0) then begin
      StepsValueListEditor.EditorMode:=false;
      workflow.swapStepDown(StepsValueListEditor.selection.top-2);
      StepsValueListEditor.selection:=rect(StepsValueListEditor.selection.Left    ,
                                           StepsValueListEditor.selection.top   -1,
                                           StepsValueListEditor.selection.Right   ,
                                           StepsValueListEditor.selection.Bottom-1);
      redisplayWorkflow;
      key:=0;
      exit;
    end;
    if (key=KEY_DOWN) and ((ssAlt in Shift) or (ssAltGr in Shift)) and (StepsValueListEditor.selection.top-1<workflow.stepCount-1) then begin
      StepsValueListEditor.EditorMode:=false;
      workflow.swapStepDown(StepsValueListEditor.selection.top-1);
      StepsValueListEditor.selection:=rect(StepsValueListEditor.selection.Left    ,
                                           StepsValueListEditor.selection.top   +1,
                                           StepsValueListEditor.selection.Right   ,
                                           StepsValueListEditor.selection.Bottom+1);
      redisplayWorkflow;
      key:=0;
      exit;
    end;
    if ((key=KEY_DEL) or (key=KEY_BACKSPACE)) and (ssShift in Shift) then begin
      StepsValueListEditor.EditorMode:=false;
      workflow.remStep(StepsValueListEditor.selection.top-1);
      redisplayWorkflow;
      exit;
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

PROCEDURE TDisplayMainForm.StepsValueListEditorButtonClick(Sender: TObject; aCol, aRow: integer);
  VAR algoIdx:longint;
  begin
    stepGridSelectedRow:=aRow-1;
    if workflow.step[stepGridSelectedRow].isGenerationStep
    then begin
      algoIdx:=isPlausibleSpecification(workflow.step[stepGridSelectedRow].toStringPart(true),true);
      switchModes(fs_editingGeneration);
      switchFromWorkflowEdit:=true;
      if algoIdx>=0 then begin
        algorithmComboBox.ItemIndex:=algoIdx;
        algorithmComboBoxSelect(Sender);
      end;
    end else begin
      StepsValueListEditor.EditorMode:=false;
      EditHelperForm.init(stepGridSelectedRow);
      workflow.stepChanged(stepGridSelectedRow);
      redisplayWorkflow;
    end;
  end;

PROCEDURE TDisplayMainForm.StepsValueListEditorSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
  begin
    if (aRow-1<0) or (aRow-1>=workflow.stepCount) then exit;
    stepGridSelectedRow:=aRow-1;
    workflow.renderIntermediate(stepGridSelectedRow,image);
  end;

PROCEDURE TDisplayMainForm.StepsValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR index:longint;
  begin
    index:=aRow-1;
    if (oldValue=newValue) or (index<0) or (index>=workflow.stepCount) then exit;
    if workflow.step[index].alterParameter(newValue) then begin
      workflow.stepChanged(index);
    end else newValue:=oldValue;
  end;

PROCEDURE TDisplayMainForm.TimerTimer(Sender: TObject);
  VAR currentlyCalculating:boolean=false;
      timeToDisplay:double;
  begin
    case formMode of
      fs_geneticSelection,fs_editingGeneration: if imageGeneration.defaultProgressQueue.calculating then begin
        statusBarParts.progressMessage:=imageGeneration.defaultProgressQueue.getProgressString;
        renderToImageNeeded:=true;
        currentlyCalculating:=true;
      end;
      fs_editingWorkflow: if workflow.progressQueue.calculating then begin
        statusBarParts.progressMessage:=workflow.progressQueue.getProgressString;
        renderToImageNeeded:=true;
        currentlyCalculating:=true;
      end;
    end;
    inc(subTimer.counter);
    if renderToImageNeeded and not(currentlyCalculating) or
       (subTimer.counter>=subTimer.interval) and currentlyCalculating then begin
      timeToDisplay:=now;
      if formMode=fs_editingWorkflow
      then renderImage(workflow.workflowImage)
      else renderImage(defaultGenerationImage);
      renderToImageNeeded:=currentlyCalculating;
      timeToDisplay:=(timeToDisplay-now)*24*60*60*1000/timer.interval;
      subTimer.interval:=round(2*timeToDisplay);
      if subTimer.interval<10 then subTimer.interval:=10;
      subTimer.counter:=0;
    end;
    updateStatusBar;
  end;

PROCEDURE TDisplayMainForm.evaluationFinished;
  begin
    renderToImageNeeded:=true;
    if formMode=fs_editingWorkflow
    then statusBarParts.progressMessage:=workflow       .progressQueue       .getProgressString
    else statusBarParts.progressMessage:=imageGeneration.defaultProgressQueue.getProgressString;
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
    if (newValue=oldValue) or (index<0) or (index>=currentAlgoMeta^.prototype^.numberOfParameters) then exit;
    value.createToParse(currentAlgoMeta^.prototype^.parameterDescription(index),newValue);
    if value.isValid
    then begin
      currentAlgoMeta^.prototype^.setParameter(index,value);
      if (currentAlgoMeta^.prototype^.parameterDescription(index)^.typ=pt_enum) or (index=LIGHT_NORMAL_INDEX) then
        ValueListEditor.Cells[1,index+1]:=currentAlgoMeta^.prototype^.getParameter(index).toString();
      pickLightButton.enabled:=currentAlgoMeta^.hasLight and (P_functionPerPixelViaRawDataAlgorithm(currentAlgoMeta^.prototype)^.lightIsRelevant);
      statusBarParts.errorMessage:='';
      calculateImage(false);
    end else begin
      statusBarParts.errorMessage:='Malformed parameter: '+currentAlgoMeta^.prototype^.parameterDescription(index)^.describe;
      newValue:=oldValue;
      exit;
    end;
  end;

PROCEDURE TDisplayMainForm.zoomOutButtonClick(Sender: TObject);
  VAR i:longint;
  begin
    if currentAlgoMeta^.hasScaler then begin
      P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.zoomOnPoint(image,2);
      for i:=0 to SCALER_PARAMETER_COUNT-1 do
        ValueListEditor.Cells[1,i+1]:=currentAlgoMeta^.prototype^.getParameter(i).toString;
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
    dupIdx:=StepsValueListEditor.row-1;
    if (dupIdx>=0) and (dupIdx<workflow.stepCount) then begin
    workflow.addStep(workflow.step[StepsValueListEditor.row-1].toString());
    redisplayWorkflow;
    end;
  end;

PROCEDURE TDisplayMainForm.cancelButtonClick(Sender: TObject);
  begin
    switchModes(fs_editingWorkflow,true);
  end;

PROCEDURE TDisplayMainForm.mis_generateImageClick(Sender: TObject);
  begin
    workflow.addStep(defaultGenerationString);
    stepGridSelectedRow:=workflow.stepCount-1;
    switchModes(fs_editingGeneration);
    switchFromWorkflowEdit:=true;
    algorithmComboBox.ItemIndex:=0;
    algorithmComboBoxSelect(Sender);
  end;

PROCEDURE TDisplayMainForm.calculateImage(CONST manuallyTriggered: boolean);
  begin
    if formMode=fs_editingWorkflow then begin
      if (workflow.workflowType=wft_manipulative) and (mi_scale_original.Checked)
      then workflow.execute(mi_renderQualityPreview.Checked,true,mi_scale_original.Checked,workflow.workflowImage.dimensions.width,workflow.workflowImage.dimensions.height,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH)
      else workflow.execute(mi_renderQualityPreview.Checked,true,mi_scale_original.Checked,workflow.workflowImage.dimensions.width,workflow.workflowImage.dimensions.height,ScrollBox1.width,ScrollBox1.height);
      renderToImageNeeded:=true;
    end else begin
      if not(manuallyTriggered or mi_renderQualityPreview.Checked) then exit;
      defaultGenerationImage.drawCheckerboard;
      if currentAlgoMeta^.prepareImageInBackground(@defaultGenerationImage,mi_renderQualityPreview.Checked)
      then begin
        renderImage(defaultGenerationImage);
        renderToImageNeeded:=false;
      end else renderToImageNeeded:=true;
    end;
  end;

PROCEDURE TDisplayMainForm.renderImage(VAR img: T_rawImage);
  VAR retried:longint=0;
      isOkay:boolean=false;
      oldWidth,oldHeight:longint;
  begin
    oldWidth :=image.width;
    oldHeight:=image.height;
    repeat
      try
        img.copyToImage(image);
        isOkay:=true;
      except
        isOkay:=false;
      end;
      inc(retried);
    until isOkay or (retried>=3);
    image.width:=image.picture.width;
    image.height:=image.picture.height;
    if not(mi_scale_original.Checked) and ((oldHeight<>image.height) or (oldWidth<>image.width)) then begin
      image.Left:=max(0,(ScrollBox1.width-image.width) shr 1);
      image.top :=max(0,(ScrollBox1.height-image.height) shr 1);
    end;
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
    with mouseSelection do if (selType=for_zoom) and currentAlgoMeta^.hasScaler and finalize then begin
      if (system.sqr(lastX-downX)+system.sqr(lastY-downY)>900) then begin
        with P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scaler do begin
          recenter(transform(downX,downY));
          setZoom(zoomOnMouseDown*0.5*system.sqrt((system.sqr(image.width)+system.sqr(image.height))/(system.sqr(lastX-downX)+system.sqr(lastY-downY))));
        end;
        for i:=0 to SCALER_PARAMETER_COUNT-1 do
          ValueListEditor.Cells[1,i+1]:=currentAlgoMeta^.prototype^.getParameter(i).toString();
        P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage(false);
      end else with P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scaler do
        setZoom(zoomOnMouseDown);
    end;
  end;

PROCEDURE TDisplayMainForm.updateLight(CONST finalize: boolean);
  VAR c:T_Complex;
  begin
    with mouseSelection do if (selType=for_light) and currentAlgoMeta^.hasLight then begin
      c.re:=1-(lastX-defaultGenerationImage.dimensions.width /2);
      c.im:=  (lastY-defaultGenerationImage.dimensions.height/2);
      c:=c*(4/pickLightHelperShape.width);
      P_functionPerPixelViaRawDataAlgorithm(currentAlgoMeta^.prototype)^.lightNormal:=toSphere(c)-BLUE;
      calculateImage(false);
      ValueListEditor.Cells[1,LIGHT_NORMAL_INDEX+1]:=currentAlgoMeta^.prototype^.getParameter(LIGHT_NORMAL_INDEX).toString;
      if finalize then begin
        mouseSelection.selType:=none;
        pickLightHelperShape.visible:=false;
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.updateJulia;
  begin
    with mouseSelection do if (selType=for_julia) and currentAlgoMeta^.hasJuliaP then begin
      with P_functionPerPixelViaRawDataJuliaAlgorithm(currentAlgoMeta^.prototype)^ do juliaParam:=scaler.transform(lastX,lastY);
      ValueListEditor.Cells[1,JULIA_COORD_INDEX+1]:=currentAlgoMeta^.prototype^.getParameter(JULIA_COORD_INDEX).toString;
      mouseSelection.selType:=none;
      calculateImage(false);
    end;
  end;

PROCEDURE TDisplayMainForm.updatePan(CONST finalize: boolean);
  VAR i:longint;
  begin
    with mouseSelection do if (selType=for_pan) and (currentAlgoMeta^.hasScaler) then begin
      P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.panByPixels(image,lastX-downX,lastY-downY);
      downX:=lastX;
      downY:=lastY;
      if finalize then begin
        for i:=0 to SCALER_PARAMETER_COUNT-1 do begin
          ValueListEditor.Cells[1,i+1]:=currentAlgoMeta^.prototype^.getParameter(i).toString();
        end;
        P_scaledImageGenerationAlgorithm(currentAlgoMeta^.prototype)^.scalerChanagedSinceCalculation:=true;
        calculateImage(false);
      end;
    end;
  end;

PROCEDURE TDisplayMainForm.redisplayWorkflow;
  VAR i:longint;
  begin
    StepsMemo.lines.clear;
    StepsValueListEditor.RowCount:=workflow.stepCount+1;
    for i:=0 to workflow.stepCount-1 do begin
      StepsValueListEditor.Cells[0,i+1]:=workflow.step[i].toStringPart(false);
      StepsValueListEditor.Cells[1,i+1]:=workflow.step[i].toStringPart(true);
      if workflow.step[i].hasComplexParameterDescription
      then StepsValueListEditor.ItemProps[i].EditStyle:=esEllipsis
      else StepsValueListEditor.ItemProps[i].EditStyle:=esSimple;
      StepsMemo.lines.append(workflow.step[i].toString());
    end;
    WorkFlowGroupBox.caption:=C_workflowTypeString[workflow.workflowType]+' workflow';
  end;

FUNCTION TDisplayMainForm.switchModes(CONST newMode: T_formState; CONST cancelEditing: boolean): boolean;
  begin
    if formMode=newMode then exit(false);
    result:=false;
    workflow.progressQueue.ensureStop;
    if (formMode=fs_editingGeneration) and (newMode=fs_editingWorkflow) then begin
      workflowPanel.width:=imageGenerationPanel.width;
      imageGenerationPanel.width:=0;
      imageGeneration.defaultProgressQueue.registerOnEndCallback(nil);

      if not(cancelEditing) then begin
        if switchFromWorkflowEdit
        then begin
          workflow.step[stepGridSelectedRow].alterParameter(currentAlgoMeta^.prototype^.toString);
          workflow.stepChanged(stepGridSelectedRow);
          redisplayWorkflow;
        end else newStepEdit.caption:=currentAlgoMeta^.prototype^.toString;
      end;
      mi_scale_original.enabled:=(inputImage<>nil);
      mi_scale_16_10.enabled:=(inputImage=nil);
      mi_scale_16_9 .enabled:=(inputImage=nil);
      mi_Scale_3_4  .enabled:=(inputImage=nil);
      mi_scale_4_3  .enabled:=(inputImage=nil);
      result:=true;
    end else if (formMode=fs_editingWorkflow) and (newMode=fs_editingGeneration) then begin
      imageGenerationPanel.width:=workflowPanel.width;
      workflowPanel.width:=0;
      imageGeneration.defaultProgressQueue.registerOnEndCallback(@evaluationFinished);

      mi_scale_original.enabled:=false;
      mi_scale_16_10.enabled:=true;
      mi_scale_16_9 .enabled:=true;
      mi_Scale_3_4  .enabled:=true;
      mi_scale_4_3  .enabled:=true;
      result:=true;
    end;
    formMode:=newMode;
    if result then begin
      workflowPanel       .enabled:=newMode=fs_editingWorkflow;
      Splitter1           .enabled:=newMode=fs_editingWorkflow;
      Splitter2           .enabled:=newMode<>fs_editingWorkflow;
      imageGenerationPanel.enabled:=newMode<>fs_editingWorkflow;
      FormResize(nil);
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
        h.caption:='&'+intToStr(index)+'  '+replaceAll(name,'&',' + ');
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
    if idx>=length(history) then exit;
    fileSet:=split(history[idx],'&');
    for i:=0 to length(fileSet)-1 do openFile(fileSet[i],true);
    historyItemRecalled(idx);
    updateFileHistory;
  end;

PROCEDURE TDisplayMainForm.openFile(CONST nameUtf8: ansistring; CONST afterRecall: boolean);
  begin
    if uppercase(extractFileExt(nameUtf8))='.WF' then begin
      workflow.progressQueue.ensureStop;
      workflow.loadFromFile(nameUtf8);
      if not(afterRecall) then begin
        if (inputImage<>nil)
        then addToHistory(nameUtf8,lastLoadedImage)
        else addToHistory(nameUtf8);
        updateFileHistory;
      end;
      SetCurrentDir(workflow.associatedDir);
      WorkingDirectoryEdit.caption:=GetCurrentDir;
      WorkingDirectoryEdit.enabled:=false;
      redisplayWorkflow;
    end else begin
      if inputImage=nil then new(inputImage,create(nameUtf8))
                        else inputImage^.loadFromFile(nameUtf8);
      lastLoadedImage:=nameUtf8;
      if not(afterRecall) then begin
        if (workflow.associatedFile<>'')
        then addToHistory(workflow.associatedFile,nameUtf8)
        else addToHistory(nameUtf8);
        updateFileHistory;
      end;
      workflow.stepChanged(0);
      mi_scale_original.enabled:=true;
      mi_scale_16_10.enabled:=false;
      mi_scale_16_9.enabled:=false;
      mi_Scale_3_4.enabled:=false;
      mi_scale_4_3.enabled:=false;
      if not mi_scale_fit.Checked then mi_scale_original.Checked:=true;
    end;
    if not(switchModes(fs_editingWorkflow,true)) then FormResize(nil);
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  defaultGenerationImage.create(1,1);
FINALIZATION
  defaultGenerationImage.destroy;
end.

