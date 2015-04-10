UNIT cat2Main;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, Grids, dbEntries,entryForm, types,formWithADropDownUnit,dbFiles,dbTags,sysTools,queues;

CONST
  C_previewBoxWidth =C_thumbnailMaxWidthHeight+4;
  C_previewBoxHeight=C_thumbnailMaxWidthHeight+18;

TYPE
  { TCatMainForm }

  TCatMainForm = class(TForm)
    ClearFilterButton: TButton;
    FilterComboBox: TComboBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    miMove: TMenuItem;
    miDropEmpty: TMenuItem;
    miDeleteComplete: TMenuItem;
    miDisplayImage: TMenuItem;
    miAddTag: TMenuItem;
    miDelete: TMenuItem;
    miDelThumb: TMenuItem;
    miSearchFile: TMenuItem;
    miEdit: TMenuItem;
    miMerge: TMenuItem;
    miQuit: TMenuItem;
    Panel1: TPanel;
    ScrollBar: TScrollBar;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    markerShape: TShape;
    StatusBar1: TStatusBar;
    DBStringGrid: TStringGrid;
    Timer: TTimer;
    PROCEDURE ClearFilterButtonClick(Sender: TObject);
    PROCEDURE DBStringGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    PROCEDURE DBStringGridHeaderSizing(sender: TObject;
      CONST IsColumn: boolean; CONST aIndex, aSize: Integer);
    PROCEDURE FilterComboBoxKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE BoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE BoxKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of String);
    PROCEDURE FormKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
    PROCEDURE FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE MenuItem3Click(Sender: TObject);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miAddTagClick(Sender: TObject);
    PROCEDURE miDeleteClick(Sender: TObject);
    PROCEDURE miDeleteRigorouslyClick(Sender: TObject);
    PROCEDURE miDelThumbClick(Sender: TObject);
    PROCEDURE miDisplayImageClick(Sender: TObject);
    PROCEDURE miDropEmptyClick(Sender: TObject);
    PROCEDURE miMoveClick(Sender: TObject);
    PROCEDURE miQuitClick(Sender: TObject);
    PROCEDURE pmiEditClick(Sender: TObject);
    PROCEDURE pmiMergeClick(Sender: TObject);
    PROCEDURE ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      VAR ScrollPos: Integer);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    boxes:array of record
      box:TGroupBox;
      image:TImage;
      checked:boolean;
      imageLoaded:boolean;
      entry:P_dbEntry;
    end;
    lastClickedBoxIndex:longint;
    boxesInOneRow:longint;
    markedBoxIndex:longint;
    { private declarations }
  public
    FUNCTION getCellContent(col,row:longint):string;
    PROCEDURE updateScrollbar;
    PROCEDURE updateListView;
    PROCEDURE recolorBoxes(uncheckAll:boolean);
    PROCEDURE updateMarkerShape;
    { public declarations }
  end;

VAR
  CatMainForm: TCatMainForm;
  lastStore:double;

IMPLEMENTATION

{$R *.lfm}

{ TCatMainForm }

PROCEDURE TCatMainForm.FormCreate(Sender: TObject);
  begin
    lastStore:=now;
    setLength(boxes,0);
  end;

PROCEDURE TCatMainForm.DBStringGridHeaderSizing(sender: TObject;
  CONST IsColumn: boolean; CONST aIndex, aSize: Integer);
  VAR x,y,i:longint;
  begin
    x:=DBStringGrid.ColWidths[0]
      +DBStringGrid.ColWidths[1]
      +DBStringGrid.ColWidths[2]
      +DBStringGrid.ColWidths[3]
      +DBStringGrid.ColWidths[4]
      +DBStringGrid.ColWidths[5];
    y:=DBStringGrid.Width-5;
    if y<x then begin
      for i:=0 to 4 do DBStringGrid.ColWidths[i]:=round(DBStringGrid.ColWidths[i]/x*y);
      DBStringGrid.ColWidths[5]:=y-DBStringGrid.ColWidths[0]
                                  -DBStringGrid.ColWidths[1]
                                  -DBStringGrid.ColWidths[2]
                                  -DBStringGrid.ColWidths[3]
                                  -DBStringGrid.ColWidths[4];
    end;
  end;

PROCEDURE TCatMainForm.DBStringGridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
  begin
    case byte(index) of
      0: sort(sc_name_asc,true);
      1: sort(sc_filename_asc,true);
      2: sort(sc_tags_asc,true);
      3: sort(sc_resolution_asc,true);
      4: sort(sc_firstChange_asc,true);
      5: sort(sc_lastChange_asc,true);
    end;
    updateListView;
  end;

PROCEDURE TCatMainForm.ClearFilterButtonClick(Sender: TObject);
  begin
    clearFilter;
    ScrollBar.Position:=0;
    updateListView;
  end;

PROCEDURE TCatMainForm.FilterComboBoxKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
  begin
    if key=13 then begin
      filter(FilterComboBox.Text);
      ScrollBar.Position:=0;
      updateListView;
    end;
  end;

PROCEDURE TCatMainForm.BoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  VAR boxIdx,i,i0,i1:longint;
  begin
    boxIdx:=0;
    while (boxIdx<length(boxes)) and (boxes[boxIdx].box<>Sender) and (boxes[boxIdx].image<>Sender) do inc(boxIdx);
    if (boxIdx<length(boxes)) and (boxIdx+ScrollBar.Position<length(filteredEntries)) then begin
      if button=mbLeft then boxes[boxIdx].checked:=not(boxes[boxIdx].checked);
      if boxes[boxIdx].checked and (ssShift in Shift) then begin
        i0:=-1;
        i1:=-1;
        for i:=0 to length(boxes)-1 do if (boxes[i].box.Visible) and (boxes[i].checked) then begin
          if i0=-1 then i0:=i; i1:=i;
        end;
        for i:=i0 to i1 do boxes[i].checked:=true;
      end;
      if not(boxes[boxIdx].checked) and (ssShift in Shift) then begin
        i0:=-1;
        i1:=-1;
        for i:=0 to length(boxes)-1 do if (boxes[i].box.Visible) and not(boxes[i].checked) then begin
          if i0=-1 then i0:=i; i1:=i;
        end;
        for i:=i0 to i1 do boxes[i].checked:=false;
      end;
      recolorBoxes(false);
      lastClickedBoxIndex:=boxIdx;
      markedBoxIndex:=lastClickedBoxIndex;
      updateMarkerShape;
    end;
  end;

PROCEDURE TCatMainForm.BoxKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
  VAR i0,i1,i,boxCount:longint;
  begin
    if key=37 then begin
      if markedBoxIndex<1 then begin
        if ScrollBar.Position>0 then begin
          ScrollBar.Position:=ScrollBar.Position-1;
          updateListView;
          recolorBoxes(true);
        end;
      end else begin
        dec(markedBoxIndex);
        updateMarkerShape;
      end;
      Key:=0; //reset key to prevent other effects
    end else if key=38 then begin
      if markedBoxIndex<boxesInOneRow then begin
        if ScrollBar.Position>0 then begin
          ScrollBar.Position:=ScrollBar.Position-boxesInOneRow;
          if ScrollBar.Position<0 then ScrollBar.Position:=0;
          updateListView;
          recolorBoxes(true);
        end;
      end else begin
        dec(markedBoxIndex,boxesInOneRow);
        updateMarkerShape;
      end;
      Key:=0; //reset key to prevent other effects
    end else if key=39 then begin
      boxCount:=0; while (boxCount<length(boxes)) and (boxes[boxCount].box.Visible) do inc(boxCount);
      if markedBoxIndex>=boxCount-1 then begin
        if ScrollBar.Position<length(filteredEntries)-boxCount-1 then begin
          ScrollBar.Position:=ScrollBar.Position+1;
          updateListView;
          recolorBoxes(true);
        end;
      end else begin
        inc(markedBoxIndex);
        updateMarkerShape;
      end;
      Key:=0; //reset key to prevent other effects
    end else if key=40 then begin
      boxCount:=0; while (boxCount<length(boxes)) and (boxes[boxCount].box.Visible) do inc(boxCount);
      if markedBoxIndex>=boxCount-boxesInOneRow then begin
        if ScrollBar.Position<length(filteredEntries)-boxCount-1 then begin
          ScrollBar.Position:=ScrollBar.Position+boxesInOneRow;
          if ScrollBar.Position>length(filteredEntries)-boxCount then ScrollBar.Position:=length(filteredEntries)-boxCount;
          updateListView;
          recolorBoxes(true);
        end;
      end else begin
        inc(markedBoxIndex,boxesInOneRow);
        updateMarkerShape;
      end;
      Key:=0; //reset key to prevent other effects
    end else if key=32 then begin
      boxes[markedBoxIndex].checked:=not(boxes[markedBoxIndex].checked);
      if boxes[markedBoxIndex].checked and (ssShift in Shift) then begin
        i0:=-1;
        i1:=-1;
        for i:=0 to length(boxes)-1 do if (boxes[i].box.Visible) and (boxes[i].checked) then begin
          if i0=-1 then i0:=i; i1:=i;
        end;
        for i:=i0 to i1 do boxes[i].checked:=true;
      end;
      if not(boxes[markedBoxIndex].checked) and (ssShift in Shift) then begin
        i0:=-1;
        i1:=-1;
        for i:=0 to length(boxes)-1 do if (boxes[i].box.Visible) and not(boxes[i].checked) then begin
          if i0=-1 then i0:=i; i1:=i;
        end;
        for i:=i0 to i1 do boxes[i].checked:=false;
      end;
      recolorBoxes(false);
    end;
  end;

PROCEDURE TCatMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of String);
  VAR i:longint;
      fi:P_fileInfo;
  begin
    for i:=0 to length(FileNames)-1 do begin
      new(fi,create(FileNames[i]));
      if fi^.isExistent then addNewEntryFromDroppedFile(fi)
                        else dispose(fi,destroy);
    end;
    reapplyFilter;
    updateListView;
  end;

PROCEDURE TCatMainForm.FormKeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
  begin
    if not(DBStringGrid.Visible) then BoxKeyDown(sender,key,shift);
  end;

PROCEDURE TCatMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
  begin
    if ScrollBar.Visible then begin
      WheelDelta:=WheelDelta div 10;
      WheelDelta:=round(wheelDelta/boxesInOneRow)*boxesInOneRow;
      WheelDelta:=ScrollBar.Position-WheelDelta;
      ScrollBar.Position:=WheelDelta;
      ScrollBarScroll(sender,scEndScroll,WheelDelta);
    end;
  end;

PROCEDURE TCatMainForm.FormResize(Sender: TObject);
  VAR i,x,y,countX,countY,widthX,heightY:integer;
  begin
    if not(DBStringGrid.Visible) then begin
      markedBoxIndex:=0;
      heightY:=ClientHeight-StatusBar1.Height-FilterComboBox.Height;
      widthX:=Width-ScrollBar.Width;
      countX:=widthX div (C_previewBoxWidth);
      countY:=heightY div (C_previewBoxHeight);
      boxesInOneRow:=countX;
      i:=0;
      for y:=0 to countY-1 do for x:=0 to countX-1 do begin
        if i>=length(boxes) then begin
          setLength(boxes,i+1);
          with boxes[i] do begin
            box:=TGroupBox.create(CatMainForm);
            box.Height:=(C_previewBoxHeight);
            box.Width:=(C_previewBoxWidth);
            box.Visible:=false;
            box.Parent:=CatMainForm;
            box.Caption:='';
            image:=TImage.create(box);
            image:=TImage.create(box);
            image.Align:=alClient;
            image.Parent:=box;
            box.OnMouseDown:=@BoxMouseDown;
            box.OnKeyDown:=@BoxKeyDown;
            image.OnMouseDown:=@BoxMouseDown;
            image.OnMouseWheel:=@FormMouseWheel;
          end;
        end;
        with boxes[i] do begin
          box.Top :=FilterComboBox.Height+round((y+0.5)*(heightY/countY)-C_previewBoxHeight/2);
          box.Left:=                      round((x+0.5)*(widthX /countX)-C_previewBoxWidth /2);
          box.Visible:=true;
        end;
        inc(i);
      end;
      while i<length(boxes) do begin
        boxes[i].box.Visible:=false;
        inc(i);
      end;
      updateScrollbar;
      updateListView;
    end else begin
      for i:=0 to length(boxes)-1 do boxes[i].box.Visible:=false;
      x:=DBStringGrid.ColWidths[0]
        +DBStringGrid.ColWidths[1]
        +DBStringGrid.ColWidths[2]
        +DBStringGrid.ColWidths[3]
        +DBStringGrid.ColWidths[4]
        +DBStringGrid.ColWidths[5];
      y:=DBStringGrid.Width-5;
      for i:=0 to 4 do DBStringGrid.ColWidths[i]:=round(DBStringGrid.ColWidths[i]/x*y);
      DBStringGrid.ColWidths[5]:=y-DBStringGrid.ColWidths[0]
                                  -DBStringGrid.ColWidths[1]
                                  -DBStringGrid.ColWidths[2]
                                  -DBStringGrid.ColWidths[3]
                                  -DBStringGrid.ColWidths[4];
    end;
  end;

FUNCTION TCatMainForm.getCellContent(col,row:longint):string;
  FUNCTION myDate2Str(date:double):string;
    begin
      if (date=C_defaultFileTimeIfNotFound)
        then result:=''
        else result:=DateTimeToStr(date);
    end;

  begin
    case byte(col) of
      0: result:=filteredEntries[row-1]^.givenName;
      1: result:=filteredEntries[row-1]^.fileName;
      2: result:=getTagsForList(filteredEntries[row-1]^.tags);
      3: result:=filteredEntries[row-1]^.resolutionString;
      4: result:=myDate2Str(filteredEntries[row-1]^.MinAge);
      5: result:=myDate2Str(filteredEntries[row-1]^.MaxAge);
    end;
  end;

PROCEDURE TCatMainForm.FormShow(Sender: TObject);
  begin
    getTagsForDropDown(FilterComboBox.Items);
    updateListView;
  end;

PROCEDURE TCatMainForm.MenuItem3Click(Sender: TObject);
  begin
    MenuItem3.Checked:=true;
    MenuItem4.Checked:=false;
    DBStringGrid.TopRow:=ScrollBar.Position+1;
    DBStringGrid.Visible:=true;
    ScrollBar.Visible:=false;
    markerShape.Visible:=false;
    FormResize(Sender);
    updateListView;
  end;

PROCEDURE TCatMainForm.MenuItem4Click(Sender: TObject);
  begin
    MenuItem3.Checked:=false;
    MenuItem4.Checked:=true;
    ScrollBar.Position:=DBStringGrid.TopRow-1;
    DBStringGrid.Visible:=false;
    ScrollBar.Visible:=true;
    markerShape.Visible:=true;
    markedBoxIndex:=0;
    FormResize(Sender);
    updateListView;
  end;

PROCEDURE TCatMainForm.miAddTagClick(Sender: TObject);
  VAR i,off:longint;
  begin
    formWithADropDown.initWithTags('Add tags');
    if formWithADropDown.ShowModal=mrOK then begin
      if DBStringGrid.Visible then begin
        for i:=DBStringGrid.Selection.Bottom-1 downto DBStringGrid.Selection.Top-1 do filteredEntries[i]^.addTag(formWithADropDown.ComboBox.Text);
      end else begin
        off:=ScrollBar.Position;
        for i:=length(boxes)-1 downto 0 do if boxes[i].checked then filteredEntries[off+i]^.addTag(formWithADropDown.ComboBox.Text);
        recolorBoxes(true);
      end;
    end;
  end;

PROCEDURE TCatMainForm.miDeleteClick(Sender: TObject);
  VAR i,off:longint;
  begin
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Bottom-1 downto DBStringGrid.Selection.Top-1 do begin
        filteredEntries[i]^.markedForDeletion:=true;
        while length(filteredEntries[i]^.files)>0 do addNewEntryFromDroppedFile(filteredEntries[i]^.dropFile(0));
      end;
    end else begin
      off:=ScrollBar.Position;
      for i:=length(boxes)-1 downto 0 do if boxes[i].checked then begin
        filteredEntries[off+i]^.markedForDeletion:=true;
        while length(filteredEntries[off+i]^.files)>0 do addNewEntryFromDroppedFile(filteredEntries[off+i]^.dropFile(0));
      end;
      recolorBoxes(true);
    end;
    reapplyFilter;
    updateListView;
  end;

PROCEDURE TCatMainForm.miDeleteRigorouslyClick(Sender: TObject);
  VAR i,off:longint;
  begin
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Bottom-1 downto DBStringGrid.Selection.Top-1 do begin
        filteredEntries[i]^.markedForDeletion:=true;
        while length(filteredEntries[i]^.files)>0 do filteredEntries[i]^.dropDeleteFile(0);
      end;
    end else begin
      off:=ScrollBar.Position;
      for i:=length(boxes)-1 downto 0 do if boxes[i].checked then begin
        filteredEntries[off+i]^.markedForDeletion:=true;
        while length(filteredEntries[off+i]^.files)>0 do filteredEntries[off+i]^.dropDeleteFile(0);
      end;
      recolorBoxes(true);
    end;
    reapplyFilter;
    updateListView;
  end;

PROCEDURE TCatMainForm.miDelThumbClick(Sender: TObject);
  VAR i,off:longint;
  begin
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Top-1 to DBStringGrid.Selection.Bottom-1 do filteredEntries[i]^.dropThumbnails;
    end else begin
      off:=ScrollBar.Position;
      for i:=0 to length(boxes)-1 do if boxes[i].checked then begin
        filteredEntries[i+off]^.dropThumbnails;
        boxes[i].imageLoaded:=false;
        boxes[i].image.Picture.Clear;
      end;
    end;
    updateListView;
  end;

PROCEDURE TCatMainForm.miDisplayImageClick(Sender: TObject);
  VAR i,off,c:longint;
  begin
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Top-1 to DBStringGrid.Selection.Bottom-1 do filteredEntries[i]^.showPrimary();
    end else begin
      off:=ScrollBar.Position;
      c:=0;
      for i:=0 to length(boxes)-1 do if boxes[i].checked then begin
        filteredEntries[i+off]^.showPrimary();
        inc(c);
      end;
      if (c=0) and (boxes[markedBoxIndex].entry<>nil) then boxes[markedBoxIndex].entry^.showPrimary();
    end;
    updateListView;
  end;

PROCEDURE TCatMainForm.miDropEmptyClick(Sender: TObject);
  begin
    dropEmptyEntries;
    updateListView;
  end;

PROCEDURE TCatMainForm.miMoveClick(Sender: TObject);
  VAR i,off:longint;
      toEdit:array of P_dbEntry;
  begin
    if SelectDirectoryDialog.Execute then begin
      setLength(toEdit,0);
      if DBStringGrid.Visible then begin
        for i:=DBStringGrid.Selection.Top-1 to
               DBStringGrid.Selection.Bottom-1 do begin
          setLength(toEdit,length(toEdit)+1);
          toEdit[length(toEdit)-1]:=filteredEntries[i];
        end;
      end else begin
        off:=ScrollBar.Position;
        for i:=0 to length(boxes)-1 do if boxes[i].checked then begin
          setLength(toEdit,length(toEdit)+1);
          toEdit[length(toEdit)-1]:=filteredEntries[i+off];
        end;
      end;
      for i:=0 to length(toEdit)-1 do toEdit[i]^.moveToDirectory(SelectDirectoryDialog.FileName);
      Timer.Enabled:=true;
      setLength(toEdit,0);
      reapplyFilter;
      updateListView;
    end;
  end;

PROCEDURE TCatMainForm.miQuitClick(Sender: TObject);
begin
  close;
end;

PROCEDURE TCatMainForm.pmiEditClick(Sender: TObject);
  VAR i,off:longint;
      toEdit:array of P_dbEntry;
  begin
    setLength(toEdit,0);
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Top-1 to
             DBStringGrid.Selection.Bottom-1 do begin
        setLength(toEdit,length(toEdit)+1);
        toEdit[length(toEdit)-1]:=filteredEntries[i];
      end;
    end else begin
      off:=ScrollBar.Position;
      for i:=0 to length(boxes)-1 do if boxes[i].checked then begin
        setLength(toEdit,length(toEdit)+1);
        toEdit[length(toEdit)-1]:=filteredEntries[i+off];
      end;
      if length(toEdit)=0 then begin
        setLength(toEdit,1);
        toEdit[0]:=boxes[markedBoxIndex].entry;
      end;
    end;
    Timer.Enabled:=false;
    StatusBar1.SimpleText:='Loading metadata 1/'+IntToStr(length(toEdit));
    if length(toEdit)>0 then EntryDialog.setup(toEdit[0]);
    for i:=1 to length(toEdit)-1 do begin
      StatusBar1.SimpleText:='Loading metadata '+IntToStr(i+1)+'/'+IntToStr(length(toEdit));
      Application.ProcessMessages;
      EntryDialog.addEntry(toEdit[i]);
    end;
    Timer.Enabled:=true;
    if length(toEdit)>0 then EntryDialog.ShowModal;
    setLength(toEdit,0);
    reapplyFilter;
    updateListView;
  end;

PROCEDURE TCatMainForm.pmiMergeClick(Sender: TObject);
  VAR i,off:longint;
  begin
    if DBStringGrid.Visible then begin
      for i:=DBStringGrid.Selection.Top-1 to
             DBStringGrid.Selection.Bottom-1 do filteredEntries[i]^.markedForMerge:=true;
      performMerge;
    end else begin
      off:=ScrollBar.Position;
      for i:=0 to length(boxes)-1 do if boxes[i].checked then
        filteredEntries[i+off]^.markedForMerge:=true;
      performMerge;
      recolorBoxes(true);
    end;
    updateListView;
  end;

PROCEDURE TCatMainForm.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: Integer);
  begin
    if not(DBStringGrid.Visible) then begin
      if ScrollPos<0 then ScrollPos:=0;
      if ScrollPos>ScrollBar.Max then ScrollPos:=ScrollBar.Max;
      updateListView;
      recolorBoxes(true);
    end;
  end;

PROCEDURE TCatMainForm.TimerTimer(Sender: TObject);
  VAR i:longint;
      pic:TPicture;
  begin
    if (now-lastStore)*24*60>10 then begin
      lastStore:=now;
      storeDB;
    end;

    if (now-lastRescan)*24*60>1 then dbEntries.scanForNewFiles;
    if (getPendingCount<1) and (windowsWorkload<20) then begin
      generateRandomThumbnail(10);
      Application.ProcessMessages;
      timer.Interval:=1;
    end else timer.Interval:=100;
    StatusBar1.SimpleText:='Tasks pending: '+intToStr(getPendingCount)+'; Busy: '+IntToStr(numberOfBusyThreads)+'; CPU Workload: '+IntToStr(windowsWorkload)+'%';
    for i:=0 to length(boxes)-1 do with boxes[i] do if (box.Visible) and not(imageLoaded) and (entry<>nil) and (entry^.thumbState in [ts_unloaded,ts_ready]) then begin
      pic:=entry^.getThumb;
      if pic<>nil then begin
        image.Picture.assign(pic);
        imageLoaded:=true;
      end;
      timer.Interval:=1;
      Break;
    end;
  end;

PROCEDURE TCatMainForm.updateScrollbar;
  VAR activeBoxCount:longint=0;
  begin
    while (activeBoxCount<length(boxes)) and (boxes[activeBoxCount].box.Visible) do inc(activeBoxCount);
    ScrollBar.PageSize:=activeBoxCount;
    ScrollBar.Min:=0;
    ScrollBar.Max:=length(filteredEntries);
    ScrollBar.LargeChange:=activeBoxCount;
  end;

PROCEDURE TCatMainForm.updateListView;
  VAR i,j,off:longint;

  PROCEDURE updateBox(index:longint);
    begin
      if (index+off<length(filteredEntries)) and (index+off>=0) and (boxes[index].box.Visible) then begin
        boxes[index].entry:=filteredEntries[index+off];
        if boxes[index].entry^.thumbState=ts_ready then begin
          boxes[index].image.Picture.assign(boxes[index].entry^.getThumb);
          boxes[index].imageLoaded:=true;
        end else begin
          boxes[index].image.Picture.Clear;
          boxes[index].imageLoaded:=false;
        end;
        boxes[index].box.Caption:=filteredEntries[index+off]^.givenName;
      end else begin
        boxes[index].box.Caption:='';
        boxes[index].image.Picture.Clear;
        boxes[index].imageLoaded:=false;
        boxes[index].entry:=nil;
      end;
    end;

  begin
    if DBStringGrid.Visible then begin
      DBStringGrid.RowCount:=1+length(filteredEntries);
      for i:=1 to length(filteredEntries) do for j:=0 to 5 do DBStringGrid.Cells[j,i]:=getCellContent(j,i);
    end else begin
      off:=ScrollBar.Position;
      for i:=0 to length(boxes)-1 do updateBox(i);
      updateMarkerShape;
    end;
  end;

PROCEDURE TCatMainForm.recolorBoxes(uncheckAll: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(boxes)-1 do with boxes[i] do begin
      if uncheckAll then checked:=false;
      if checked then begin
        box.Font.Style:=[fsBold];
      end else begin
        box.Font.Style:=[];
      end;
    end;
  end;

PROCEDURE TCatMainForm.updateMarkerShape;
  begin
    markerShape.Top:=boxes[markedBoxIndex].box.Top-3;
    markerShape.Left:=boxes[markedBoxIndex].box.left-3;
    markerShape.Height:=boxes[markedBoxIndex].box.Height+6;
    markerShape.Width:=boxes[markedBoxIndex].box.Width+6;
  end;

end.

