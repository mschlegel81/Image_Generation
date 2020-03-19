UNIT geneticCreation;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ValEdit, imageGeneration,workflows,imageContexts,mypics,pixMaps,myGenerics, Grids;

TYPE
  T_individualState=(is_calculation_pending,is_calculating,is_changedDuringCalculation,is_needsDrawing,is_ready);
  T_geneticsWorkflow=object(T_abstractWorkflow)
    private
      relatedEditor :P_generateImageWorkflow;
      individuals:array[0..15] of record
        image:TImage;
        box  :TGroupBox;
        isMarked:boolean;
        state:T_individualState;
        output:P_rawImage;
        parameterSet:P_generalImageGenrationAlgorithm;
      end;
      lastResolution:T_imageDimensions;
    protected
      PROCEDURE beforeAll; virtual;
      PROCEDURE headlessWorkflowExecution; virtual;
      PROCEDURE clearImages;
    public
      CONSTRUCTOR createGeneticsWorkflow;
      DESTRUCTOR destroy; virtual;
      PROCEDURE startEditing(CONST relatedEditor_:P_generateImageWorkflow; CONST resetStyle:byte);
      PROCEDURE confirmEditing;
      FUNCTION isValid: boolean; virtual;
      PROCEDURE individualChanged(CONST index:longint);
      PROCEDURE drawIndividuals;
      FUNCTION limitedDimensionsForResizeStep(CONST tgtDim:T_imageDimensions):T_imageDimensions; virtual;
    end;

  TGeneticCreationForm = class(TForm)
    cooldownCb: TCheckBox;
    MenuItem2: TMenuItem;
    mi_previewQuality: TMenuItem;
    mi_highQuality: TMenuItem;
    randStyleGroupBox: TGroupBox;
    mutationRateEdit: TEdit;
    GroupBox17: TGroupBox;
    MenuItem1: TMenuItem;
    mi_acceptAndClose: TMenuItem;
    MenuItem3: TMenuItem;
    mi_cross: TMenuItem;
    mi_randomize: TMenuItem;
    mi_reset: TMenuItem;
    mi_interpolate: TMenuItem;
    mi_clearMarks: TMenuItem;
    MenuItem9: TMenuItem;
    resetTypeComboBox: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox14: TGroupBox;
    GroupBox15: TGroupBox;
    GroupBox16: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    IndividualsPanel: TPanel;
    MainMenu1: TMainMenu;
    Panel2: TPanel;
    Splitter1: TSplitter;
    timer: TTimer;
    ValueListEditor: TValueListEditor;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE individualDblClick(Sender: TObject);
    PROCEDURE individualClick(Sender: TObject);
    PROCEDURE mi_acceptAndCloseClick(Sender: TObject);
    PROCEDURE mi_clearMarksClick(Sender: TObject);
    PROCEDURE mi_crossClick(Sender: TObject);
    PROCEDURE mi_interpolateClick(Sender: TObject);
    PROCEDURE mi_previewQualityClick(Sender: TObject);
    PROCEDURE mi_randomizeClick(Sender: TObject);
    PROCEDURE mi_resetClick(Sender: TObject);
    PROCEDURE mutationRateEditEditingDone(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE ValueListEditorValidateEntry(Sender: TObject; aCol,
      aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    workflow:T_geneticsWorkflow;
    imageDim:T_imageDimensions;
    parameterIndex:T_arrayOfLongint;
    mutationRate:double;
    FUNCTION individualIndex(CONST Sender:TObject):longint;
  public
    PROCEDURE clearMarks;
    PROCEDURE updateMark(CONST index:longint; CONST mark:boolean);
    PROCEDURE individualMarkChanged;
    PROCEDURE updateEditPanel;
  end;

FUNCTION editGenetics(CONST previewWorkflow:P_generateImageWorkflow; CONST resetStyle:byte):boolean;
IMPLEMENTATION
USES generationBasics,myParams,ig_fractals;
VAR
  GeneticCreationForm: TGeneticCreationForm=nil;

{$R *.lfm}

FUNCTION editGenetics(CONST previewWorkflow:P_generateImageWorkflow; CONST resetStyle:byte):boolean;
  begin
    if GeneticCreationForm=nil then begin
      GeneticCreationForm:=TGeneticCreationForm.create(nil);
    end;
    GeneticCreationForm.workflow.startEditing(previewWorkflow,resetStyle);
    GeneticCreationForm.mutationRate:=1;
    GeneticCreationForm.updateEditPanel;
    GeneticCreationForm.individualMarkChanged;
    GeneticCreationForm.timer.enabled:=true;
    result:=GeneticCreationForm.ShowModal=mrOk;
    //TODO: enhance genetics so that multiple individuals can be accepted
    if result then GeneticCreationForm.workflow.confirmEditing();
    GeneticCreationForm.workflow.postStop;
    GeneticCreationForm.timer.enabled:=false;
  end;

PROCEDURE TGeneticCreationForm.FormResize(Sender: TObject);
  VAR totalWidth,totalHeight,
      i,j,k:longint;
  begin
    totalWidth :=IndividualsPanel.ClientWidth ;
    totalHeight:=IndividualsPanel.ClientHeight;
    imageDim:=imageDimensions(totalWidth  div 4,
                              totalHeight div 4);
    for k:=0 to length(workflow.individuals)-1 do begin
      i:=k mod 4;
      j:=k div 4;
      with workflow.individuals[k].box do begin
        top   :=j*imageDim.height;
        Left  :=i*imageDim.width;
        height:=  imageDim.height;
        width :=  imageDim.width;
      end;
    end;
    workflow.clearImages;
  end;

PROCEDURE TGeneticCreationForm.individualDblClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=individualIndex(Sender);
    if k<0 then exit;
    clearMarks;
    updateMark(k,true);
  end;

PROCEDURE TGeneticCreationForm.individualClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=individualIndex(Sender);
    if k<0 then exit;
    updateMark(k,not(workflow.individuals[k].isMarked));
  end;

PROCEDURE TGeneticCreationForm.mi_acceptAndCloseClick(Sender: TObject);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TGeneticCreationForm.mi_clearMarksClick(Sender: TObject);
  begin
    clearMarks;
  end;

PROCEDURE TGeneticCreationForm.mi_crossClick(Sender: TObject);
  VAR k,j:longint;
      i:longint=0;
      parents:array of P_generalImageGenrationAlgorithm;
  PROCEDURE randomParents;
    VAR tries:longint=1;
    begin
      i:=random(length(parents)); j:=i;
      while (i=j) and (tries<10) do begin
        j:=random(length(parents));
        inc(tries);
      end;
    end;

  begin
    setLength(parents,16);
    for k:=0 to 15 do with workflow.individuals[k] do if isMarked then begin
      parents[i]:=parameterSet;
      inc(i);
    end;
    setLength(parents,i);
    for k:=0 to 15 do with workflow.individuals[k] do if not(isMarked) then begin
      randomParents;
      parameterSet^.genetics_cross(parents[i],parents[j],resetTypeComboBox.ItemIndex,mutationRate);
      workflow.individualChanged(k);
    end;
    if cooldownCb.checked then begin
      mutationRate:=mutationRate*0.9;
      mutationRateEdit.text:=floatToStr(mutationRate);
    end;
    setLength(parents,0);
  end;

PROCEDURE TGeneticCreationForm.mi_interpolateClick(Sender: TObject);
  VAR i0:P_generalImageGenrationAlgorithm=nil;
      i1:P_generalImageGenrationAlgorithm=nil;
      k :longint;
  FUNCTION copyOf(x:P_generalImageGenrationAlgorithm):P_generalImageGenrationAlgorithm;
    begin
      result:=P_generalImageGenrationAlgorithm(x^.meta^.getDefaultOperation);
      result^.copyParameters(x);
    end;

  begin
    for k:=0 to length(workflow.individuals)-1 do with workflow.individuals[k] do if isMarked then begin
      if i0=nil then i0:=copyOf(parameterSet) else
      if i1=nil then i1:=copyOf(parameterSet);
    end;
    if i1<>nil then for k:=0 to 15 do with workflow.individuals[k] do begin
      isMarked:=(k=0) or (k=15);
      parameterSet^.genetics_interpolate(i0,i1,k/15);
      workflow.individualChanged(k);
    end;
    individualMarkChanged;
    if i0<>nil then dispose(i0,destroy);
    if i1<>nil then dispose(i1,destroy);
  end;

PROCEDURE TGeneticCreationForm.mi_previewQualityClick(Sender: TObject);
  VAR preview:boolean;
  begin
    preview:=mi_previewQuality.checked;
    if (preview<>workflow.previewQuality) then begin
      workflow.ensureStop;
      workflow.clearImages;
    end;
  end;

PROCEDURE TGeneticCreationForm.mi_randomizeClick(Sender: TObject);
  VAR k,i:longint;
      originalParameters:array of T_parameterValue;
  begin
    for k:=0 to 15 do with workflow.individuals[k] do if not(isMarked) then begin
      setLength(originalParameters,parameterSet^.numberOfParameters);
      for i:=0 to parameterSet^.numberOfParameters-1 do if not(parameterSet^.parameterIsGenetic(i)) then originalParameters[i]:=parameterSet^.getParameter(i);
      parameterSet^.resetParameters(resetTypeComboBox.ItemIndex);
      for i:=0 to parameterSet^.numberOfParameters-1 do if not(parameterSet^.parameterIsGenetic(i)) then parameterSet^.setParameter(i,originalParameters[i]);
      setLength(originalParameters,0);
      workflow.individualChanged(k);
    end;
  end;

PROCEDURE TGeneticCreationForm.mi_resetClick(Sender: TObject);
  VAR k:longint;
  begin
    for k:=0 to 15 do with workflow.individuals[k] do begin
      parameterSet^.resetParameters(resetTypeComboBox.ItemIndex);
      workflow.individualChanged(k);
    end;
    clearMarks;
    updateEditPanel;
  end;

PROCEDURE TGeneticCreationForm.mutationRateEditEditingDone(Sender: TObject);
  VAR r:double;
  begin
    r:=strToFloatDef(mutationRateEdit.text,-1);
    if (r>=0) and (r<10)
    then mutationRate:=r
    else mutationRateEdit.text:=floatToStr(mutationRate);
  end;

PROCEDURE TGeneticCreationForm.TimerTimer(Sender: TObject);
  begin
    workflow.drawIndividuals;
    //Start workflow if not running
    if not(workflow.executing) then begin
      workflow.lastResolution:=imageDim;
      workflow.executeWorkflowInBackground(mi_previewQuality.checked);
    end;
    workflow.messageQueue^.clear;
  end;

PROCEDURE TGeneticCreationForm.ValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR index:longint;
      value:T_parameterValue;
      individual:P_generalImageGenrationAlgorithm;
      k:longint;
  begin
    individual:=workflow.individuals[0].parameterSet;
    if aRow>0
    then index:=parameterIndex[aRow-1]
    else index:=0;

    if (newValue=oldValue) or (index<0) or (index>=individual^.numberOfParameters) then exit;
    value.createToParse(individual^.parameterDescription(index),newValue);
    if value.isValid
    then begin
      for k:=0 to 15 do begin
        workflow.individuals[k].parameterSet^.setParameter(index,value);
        workflow.individualChanged(k);
      end;
      if (individual^.parameterDescription(index)^.getType=pt_enum) or (index=LIGHT_NORMAL_INDEX) then
        ValueListEditor.Cells[1,aRow]:=individual^.getParameter(index).toString();
    end else begin
      newValue:=oldValue;
      exit;
    end;
  end;

FUNCTION TGeneticCreationForm.individualIndex(CONST Sender: TObject): longint;
  VAR k:longint;
  begin
    result:=-1;
    for k:=0 to 15 do with workflow.individuals[k] do if (Sender=box) or (Sender=image) then exit(k);
  end;

PROCEDURE TGeneticCreationForm.clearMarks;
  VAR k:longint;
  begin
    for k:=0 to 15 do workflow.individuals[k].isMarked:=false;
    individualMarkChanged;
  end;

PROCEDURE TGeneticCreationForm.updateMark(CONST index: longint; CONST mark: boolean);
  begin
    workflow.individuals[index].isMarked:=mark;
    individualMarkChanged;
  end;

PROCEDURE TGeneticCreationForm.individualMarkChanged;
  VAR markCount   :longint=0;
      firstMarked :longint=-1;
      secondMarked:longint=-1;
      k:longint;
  begin
    for k:=0 to length(workflow.individuals)-1 do if workflow.individuals[k].isMarked then begin
      workflow.individuals[k].box.color:=clHighlight;
      if      firstMarked =-1 then firstMarked :=k
      else if secondMarked=-1 then secondMarked:=k;
      workflow.individuals[k].box.caption:='parent';
      inc(markCount);
    end else begin
      workflow.individuals[k].box.caption:='';
      workflow.individuals[k].box.color:=clDefault;
    end;
    if markCount=2 then begin
      workflow.individuals[firstMarked ].box.caption:='parent / start';
      workflow.individuals[secondMarked].box.caption:='parent / end';
      mi_interpolate.enabled:=true;
    end else mi_interpolate.enabled:=false;
    mi_cross.enabled:=(markCount>=1) and (markCount<16);
    mi_acceptAndClose.enabled:=(markCount=1);
  end;

PROCEDURE TGeneticCreationForm.updateEditPanel;
  VAR individual:P_generalImageGenrationAlgorithm;
      i,k:longint;
      resetStyles:T_arrayOfString;
      parDesc:P_parameterDescription;
      enumString:string;
  begin
    mutationRateEdit.text:=floatToStr(mutationRate);

    individual:=workflow.individuals[0].parameterSet;

    resetTypeComboBox.items.clear;
    resetStyles:=individual^.parameterResetStyles;
    for enumString in resetStyles do  resetTypeComboBox.items.append(enumString);
    if length(resetStyles)> 0 then resetTypeComboBox.ItemIndex:=0;
    resetTypeComboBox.enabled:=length(resetStyles)>1;

    ValueListEditor.clear;
    ValueListEditor.ClearSelections;
    setLength(parameterIndex,0);
    for i:=0 to individual^.numberOfParameters-1 do if not(individual^.parameterIsGenetic(i)) then append(parameterIndex,i);
    ValueListEditor.RowCount:=1+length(parameterIndex);
    for i:=0 to length(parameterIndex)-1 do begin
      k:=parameterIndex[i];
      parDesc:=individual^.parameterDescription(k);
      ValueListEditor.Cells[0,i+1]:=parDesc^.getName;
      ValueListEditor.Cells[1,i+1]:=individual^.getParameter(k).toString;
      if parDesc^.getType=pt_enum then with ValueListEditor.ItemProps[i] do begin
        EditStyle:=esPickList;
        readonly:=true;
        PickList.clear;
        for enumString in parDesc^.getEnumValues do PickList.add(enumString);
      end else if parDesc^.getType=pt_color then ValueListEditor.ItemProps[i].EditStyle:=esEllipsis
                                            else ValueListEditor.ItemProps[i].EditStyle:=esSimple;
    end;
 end;

PROCEDURE TGeneticCreationForm.FormCreate(Sender: TObject);
  VAR k:longint;
  begin
    workflow.createGeneticsWorkflow;
    with workflow.individuals[ 0] do begin image:=Image1 ; box:=GroupBox1 ; end;
    with workflow.individuals[ 1] do begin image:=Image2 ; box:=GroupBox2 ; end;
    with workflow.individuals[ 2] do begin image:=Image3 ; box:=GroupBox3 ; end;
    with workflow.individuals[ 3] do begin image:=Image4 ; box:=GroupBox4 ; end;
    with workflow.individuals[ 4] do begin image:=Image5 ; box:=GroupBox5 ; end;
    with workflow.individuals[ 5] do begin image:=Image6 ; box:=GroupBox6 ; end;
    with workflow.individuals[ 6] do begin image:=Image7 ; box:=GroupBox7 ; end;
    with workflow.individuals[ 7] do begin image:=Image8 ; box:=GroupBox8 ; end;
    with workflow.individuals[ 8] do begin image:=Image9 ; box:=GroupBox9 ; end;
    with workflow.individuals[ 9] do begin image:=Image10; box:=GroupBox10; end;
    with workflow.individuals[10] do begin image:=Image11; box:=GroupBox11; end;
    with workflow.individuals[11] do begin image:=Image12; box:=GroupBox12; end;
    with workflow.individuals[12] do begin image:=Image13; box:=GroupBox13; end;
    with workflow.individuals[13] do begin image:=Image14; box:=GroupBox14; end;
    with workflow.individuals[14] do begin image:=Image15; box:=GroupBox15; end;
    with workflow.individuals[15] do begin image:=Image16; box:=GroupBox16; end;
    for k:=0 to 15 do with workflow.individuals[k] do begin
      box.OnClick:=@individualClick;
      box.OnDblClick:=@individualDblClick;
      image.OnClick:=@individualClick;
      image.OnDblClick:=@individualDblClick;
    end;
    clearMarks;
  end;

PROCEDURE TGeneticCreationForm.FormDestroy(Sender: TObject);
  begin
    workflow.destroy;
  end;

PROCEDURE T_geneticsWorkflow.beforeAll;
  begin
  end;

PROCEDURE T_geneticsWorkflow.headlessWorkflowExecution;
  VAR k:longint;
  begin
    enterCriticalSection(contextCS);
    for k:=0 to 15 do if not(cancellationRequested) and ((individuals[k].state=is_calculation_pending) or (individuals[k].output=nil)) then begin
      individuals[k].state:=is_calculating;
      image.resize(lastResolution,res_dataResize);
      image.drawCheckerboard;
      leaveCriticalSection(contextCS);
      individuals[k].parameterSet^.execute(@self);
      enterCriticalSection(contextCS);
      if individuals[k].state=is_calculating
      then individuals[k].state:=is_needsDrawing
      else individuals[k].state:=is_calculation_pending;
      if (individuals[k].output=nil)
      then new(individuals[k].output,create(image))
      else individuals[k].output^.copyFromPixMap(image);
    end;
    try
      if currentExecution.workflowState=ts_evaluating
      then currentExecution.workflowState:=ts_ready
      else currentExecution.workflowState:=ts_cancelled;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE drawNoImage(VAR image:TImage);
  begin
    with image.Canvas do begin
      clear;
      Brush.color:=clBtnFace;
      Brush.style:=bsSolid;
      FillRect(0,0,width,height);
      textOut(5,height div 2,'calculation pending...');
    end;
  end;

PROCEDURE T_geneticsWorkflow.clearImages;
  VAR k:longint;
  begin
    enterCriticalSection(contextCS);
    for k:=0 to length(individuals)-1 do begin
      if individuals[k].output<>nil then dispose(individuals[k].output,destroy);
      individuals[k].output:=nil;
      individuals[k].state:=is_calculation_pending;
      drawNoImage(individuals[k].image);
    end;
    leaveCriticalSection(contextCS);
  end;

CONSTRUCTOR T_geneticsWorkflow.createGeneticsWorkflow;
  VAR ownedMessageQueue:P_structuredMessageQueue;
      k:longint;
  begin
    new(ownedMessageQueue,create);
    inherited createContext(ownedMessageQueue);
    relatedEditor:=nil;
    for k:=0 to 15 do with individuals[k] do begin
      image:=nil;
      box  :=nil;
      isMarked:=false;
      state:=is_ready;
      output:=nil;
      parameterSet:=nil;
    end;
    lastResolution:=imageDimensions(1,1);
  end;

DESTRUCTOR T_geneticsWorkflow.destroy;
  VAR ownedMessageQueue:P_structuredMessageQueue;
      k:longint;
  begin
    postStop;
    enterCriticalSection(contextCS);
    for k:=0 to 15 do with individuals[k] do begin
      if output<>nil then dispose(output,destroy);
      if parameterSet<>nil then dispose(parameterSet,destroy);
    end;
    leaveCriticalSection(contextCS);
    ownedMessageQueue:=messageQueue;
    inherited destroy;
    dispose(ownedMessageQueue,destroy);
  end;

PROCEDURE T_geneticsWorkflow.startEditing(CONST relatedEditor_: P_generateImageWorkflow; CONST resetStyle:byte);
  VAR k:longint;
  begin
    enterCriticalSection(contextCS);
    relatedEditor:=relatedEditor_;
    for k:=0 to 15 do with individuals[k] do begin
      if parameterSet<>nil then dispose(parameterSet,destroy);
      parameterSet:=P_generalImageGenrationAlgorithm(relatedEditor^.algorithm^.getDefaultOperation);
      if k=0 then parameterSet^.copyParameters(relatedEditor^.algorithm^.prototype)
             else begin
               parameterSet^.resetParameters(resetStyle);
               parameterSet^.copyNonGeneticParameters(relatedEditor^.algorithm^.prototype);
             end;
      isMarked:=k=0;
    end;
    clearImages;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_geneticsWorkflow.confirmEditing;
  VAR k:longint;
  begin
    postStop;
    for k:=0 to length(individuals)-1 do if individuals[k].isMarked then begin
      relatedEditor^.algorithm^.prototype^.copyParameters(individuals[k].parameterSet);
      exit;
    end;
  end;

FUNCTION T_geneticsWorkflow.isValid: boolean;
  VAR k:longint;
  begin
    //This is a hack: if isValid returns false then no calculation will be performed
    result:=false;
    enterCriticalSection(contextCS);
    for k:=0 to 15 do result:=result or (individuals[k].state in [is_calculation_pending,is_changedDuringCalculation]);
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_geneticsWorkflow.individualChanged(CONST index: longint);
  begin
    if (index<0) or (index>15) then exit;
    enterCriticalSection(contextCS);
    with individuals[index] do begin
      if state=is_calculating then state:=is_changedDuringCalculation
                              else state:=is_calculation_pending;
      image.Canvas.clear;
      image.Canvas.textOut(5,image.height div 2,'calculation pending...');
    end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_geneticsWorkflow.drawIndividuals;
  VAR k:longint;
  begin
    enterCriticalSection(contextCS);
    for k:=0 to length(individuals)-1 do with individuals[k] do
      if state=is_needsDrawing then begin
        assert(output<>nil);
        output^.copyToImage(image);
        state:=is_ready;
      end;
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_geneticsWorkflow.limitedDimensionsForResizeStep(CONST tgtDim: T_imageDimensions): T_imageDimensions;
  begin
    result:=lastResolution;
  end;

FINALIZATION
  if GeneticCreationForm<>nil then FreeAndNil(GeneticCreationForm);

end.

