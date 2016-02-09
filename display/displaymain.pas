UNIT displayMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mypics,GraphType,IntfGraphics, Menus, StdCtrls, ValEdit, ComCtrls, imageGeneration,myGenerics,myParams;

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
  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE renderImage(VAR img:T_rawImage);
  end;

VAR
  Form1: TForm1;
  img:T_rawImage;
  currentAlgorithm:P_generalImageGenrationAlgorithm;
  previewUpdateNeeded:boolean=false;
  renderToImageNeeded:boolean=false;

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
    img.create(paramStr(1));
    prepareAlgorithms;
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
    for i:=0 to currentAlgorithm^.numberOfParameters-1 do begin
      ValueListEditor.Cells[0,i+1]:=currentAlgorithm^.parameterDescription(i).name;
      ValueListEditor.Cells[1,i+1]:=myParams.toString(currentAlgorithm^.parameterDescription(i),currentAlgorithm^.getParameter(i));
    end;
  end;

PROCEDURE TForm1.FormResize(Sender: TObject);
  begin
    //renderImage(img);
    if progressor.calculating then progressor.cancelCalculation;
    progressor.waitForEndOfCalculation;
    imageGeneration.renderImage.resize(ScrollBox1.width,ScrollBox1.height,res_dataResize);
    previewUpdateNeeded:=true;
  end;

PROCEDURE TForm1.TimerTimer(Sender: TObject);
  begin
    if progressor.calculating then begin
      StatusBar.SimpleText:=progressor.getProgressString;
      renderImage(imageGeneration.renderImage);
    end else if previewUpdateNeeded then begin
      currentAlgorithm^.prepareImage(true);
      previewUpdateNeeded:=false;
      renderToImageNeeded:=true;
    end else begin
      if renderToImageNeeded then begin
        renderImage(imageGeneration.renderImage);
        renderToImageNeeded:=false;
      end;
    end;
  end;

PROCEDURE TForm1.ValueListEditorEditingDone(Sender: TObject);
  VAR i:longint;
      value:T_parameterValue;
  begin
    if progressor.calculating then progressor.cancelCalculation;
    progressor.waitForEndOfCalculation;
    for i:=0 to currentAlgorithm^.numberOfParameters-1 do
    if canParseParameterValue(currentAlgorithm^.parameterDescription(i),ValueListEditor.Cells[1,i+1],value)
    then currentAlgorithm^.setParameter(i,value)
    else begin
      StatusBar.SimpleText:='Malformed parameter for: '+currentAlgorithm^.parameterDescription(i).name;
      exit;
    end;
    previewUpdateNeeded:=true;
  end;


PROCEDURE TForm1.renderImage(VAR img: T_rawImage);
  begin
    img.copyToImage(Image1);
    Image1.width:=Image1.Picture.width;
    Image1.height:=Image1.Picture.height;
  end;



end.

