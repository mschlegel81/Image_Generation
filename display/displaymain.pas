UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls, ValEdit, ComCtrls,math,
  imageGeneration,
  ig_gradient,
  ig_perlin,
  ig_fractals,
  myGenerics,myParams,workflows;

TYPE

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
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
    Image1: TImage;
    StepsListBox: TListBox;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
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
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE ValueListEditorEditingDone(Sender: TObject);
    PROCEDURE evaluationFinished;
  private
    previousAlgorithmParameters:array of ansistring;
    subTimerCounter:longint;
    currentAlgorithm:P_generalImageGenrationAlgorithm;
    previewUpdateNeeded:boolean;
    renderToImageNeeded:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE renderImage(VAR img:T_rawImage);
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION

{$R *.lfm}

{ TForm1 }

PROCEDURE TForm1.FormCreate(Sender: TObject);
  PROCEDURE prepareAlgorithms;
    VAR i:longint;
    begin
      algorithmComboBox.Items.clear;
      for i:=0 to length(algorithms)-1 do algorithmComboBox.Items.append(algorithms[i].name);
      if length(algorithms)>0 then algorithmComboBox.ItemIndex:=0;
      algorithmComboBoxSelect(Sender);
    end;

  begin
    subTimerCounter:=0;
    renderToImageNeeded:=false;
    previewUpdateNeeded:=false;

    prepareAlgorithms;
    progressor.registerOnEndCallback(@evaluationFinished);
  end;

PROCEDURE TForm1.algorithmComboBoxSelect(Sender: TObject);
  VAR i:longint;
      resetStyles:T_arrayOfString;
  begin
    if (algorithmComboBox.ItemIndex<0) or (algorithmComboBox.ItemIndex>=length(algorithms)) then exit;
    currentAlgorithm:=algorithms[algorithmComboBox.ItemIndex].prototype;

    resetTypeComboBox.Items.clear;
    resetStyles:=currentAlgorithm^.parameterResetStyles;
    for i:=0 to length(resetStyles)-1 do resetTypeComboBox.Items.append(resetStyles[i]);
    if length(resetStyles)>0 then resetTypeComboBox.ItemIndex:=0;

    ValueListEditor.RowCount:=currentAlgorithm^.numberOfParameters+1;
    setLength(previousAlgorithmParameters,currentAlgorithm^.numberOfParameters);
    for i:=0 to currentAlgorithm^.numberOfParameters-1 do begin
      ValueListEditor.Cells[0,i+1]:=currentAlgorithm^.parameterDescription(i)^.name;
      ValueListEditor.Cells[1,i+1]:=currentAlgorithm^.getParameter(i).toString;
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
    end;
    previewUpdateNeeded:=true;
  end;

PROCEDURE TForm1.FormResize(Sender: TObject);
  begin
    if progressor.calculating then progressor.cancelCalculation;
    progressor.waitForEndOfCalculation;
    imageGeneration.renderImage.resize(ScrollBox1.width,ScrollBox1.height,res_dataResize);
    previewUpdateNeeded:=true;
  end;

PROCEDURE TForm1.TimerTimer(Sender: TObject);
  begin
    inc(subTimerCounter);
    if progressor.calculating then begin
      StatusBar.SimpleText:=progressor.getProgressString;
      renderToImageNeeded:=true;
    end else if previewUpdateNeeded then begin
      currentAlgorithm^.prepareImage(true);
      previewUpdateNeeded:=false;
      renderToImageNeeded:=true;
    end;
    if renderToImageNeeded and (subTimerCounter and 15=0) then begin
      renderImage(imageGeneration.renderImage);
      renderToImageNeeded:=false;
    end;
  end;

PROCEDURE TForm1.evaluationFinished;
  begin
    renderToImageNeeded:=true;
    StatusBar.SimpleText:=''
  end;

PROCEDURE TForm1.ValueListEditorEditingDone(Sender: TObject);
  VAR i:longint;
      value:T_parameterValue;
  begin
    for i:=0 to currentAlgorithm^.numberOfParameters-1 do if ValueListEditor.Cells[1,i+1]<>previousAlgorithmParameters[i] then begin
      previousAlgorithmParameters[i]:=ValueListEditor.Cells[1,i+1];
      value.createToParse(currentAlgorithm^.parameterDescription(i),ValueListEditor.Cells[1,i+1]);
      if value.isValid
      then begin
        currentAlgorithm^.setParameter(i,value);
        if (currentAlgorithm^.parameterDescription(i)^.typ=pt_enum) or (i=LIGHT_NORMAL_INDEX) then
          ValueListEditor.Cells[1,i+1]:=currentAlgorithm^.getParameter(i).toString();
      end else begin
        StatusBar.SimpleText:='Malformed parameter: '+currentAlgorithm^.parameterDescription(i)^.describe;
        exit;
      end;
    end;
    previewUpdateNeeded:=true;
  end;


PROCEDURE TForm1.renderImage(VAR img: T_rawImage);
  begin
    img.copyToImage(Image1);
    Image1.width:=Image1.Picture.width;
    Image1.height:=Image1.Picture.height;
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

end.

