UNIT entryForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus, ComCtrls,
  SynHighlighterBat, SynHighlighterMulti, SynMemo, SynHighlighterPas,
  formWithADropDownUnit, dbFiles, dbEntries, dbTags;

TYPE

  { TEntryDialog }

  TEntryDialog = class(TForm)
    GroupBox2: TGroupBox;
    commentMemo: TMemo;
    SynBatSyn1: TSynBatSyn;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynMemo1: TSynMemo;
    addTagComboBox: TComboBox;
    GroupBox1: TGroupBox;
    previewImage: TImage;
    filesListBox: TListBox;
    TabControl1: TTabControl;
    tagListBox: TListBox;
    miRenameInp: TMenuItem;
    miShowInp: TMenuItem;
    NameEdit: TEdit;
    FilesPopup: TPopupMenu;
    miRemoveTag: TMenuItem;
    miDropInput: TMenuItem;
    miDelInput: TMenuItem;
    PageControl1: TPageControl;
    filesTabSheet: TTabSheet;
    TabSheet2: TTabSheet;
    TagsPopup: TPopupMenu;
    PROCEDURE AddTagComboBoxKeyDown(Sender: TObject; VAR Key: word;
      Shift: TShiftState);
    PROCEDURE CommentMemoEditingDone(Sender: TObject);
    PROCEDURE filesListBoxSelectionChange(Sender: TObject; User: boolean);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE ListBoxFilesKeyDown(Sender: TObject; VAR Key: word;
      Shift: TShiftState);
    PROCEDURE miChangePrefix1Click(Sender: TObject);
    PROCEDURE miDelInputClick(Sender: TObject);
    PROCEDURE miDropInputClick(Sender: TObject);
    PROCEDURE miRemoveTagClick(Sender: TObject);
    PROCEDURE miRenameInpClick(Sender: TObject);
    PROCEDURE miShowInpClick(Sender: TObject);
    PROCEDURE NameEditEditingDone(Sender: TObject);
    PROCEDURE TabControl1Change(Sender: TObject);
  private
    { private declarations }
    PROCEDURE updateGUI(complete:boolean);
  public
    PROCEDURE setup(forEntry:P_dbEntry);
    PROCEDURE addEntry(forEntry:P_dbEntry);
    { public declarations }
  end;

VAR
  EntryDialog: TEntryDialog;
  entry:array of P_dbEntry;
  entryIndex:longint=0;

IMPLEMENTATION

{$R *.lfm}

{ TEntryDialog }

PROCEDURE TEntryDialog.ListBoxFilesKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
  begin
    if (key=38) and (ssAlt in Shift) and (filesListBox.ItemIndex>0) then begin
      //swap up
      entry[entryIndex]^.swapFiles(filesListBox.ItemIndex,filesListBox.ItemIndex-1);
      filesListBox.ItemIndex:=filesListBox.ItemIndex-1;
      entry[entryIndex]^.getFileList(filesListBox.Items);
    end else if (key=40) and (ssAlt in Shift) and (filesListBox.ItemIndex<filesListBox.Items.Count-1) then begin
      //swap down
      entry[entryIndex]^.swapFiles(filesListBox.ItemIndex,filesListBox.ItemIndex+1);
      filesListBox.ItemIndex:=filesListBox.ItemIndex+1;
      entry[entryIndex]^.getFileList(filesListBox.Items);
    end;
  end;

PROCEDURE TEntryDialog.miChangePrefix1Click(Sender: TObject);
begin
  //formWithADropDown.init('New prefix');
  //formWithADropDown.ComboBox.Text:=entry^.commonPrefix;
  //if formWithADropDown.ShowModal=mrOK then begin
  //  entry^.changeCommonPrefix(formWithADropDown.ComboBox.Text);
  //  entry^.getInputFileList(ListBoxFiles.Items);
  //  entry^.getImageFileList(ListBoxImages.Items);
  //end;
end;

PROCEDURE TEntryDialog.miDelInputClick(Sender: TObject);
  begin
    if filesListBox.ItemIndex>=0 then begin
      entry[entryIndex]^.dropDeleteFile(filesListBox.ItemIndex);
      entry[entryIndex]^.getFileList(filesListBox.Items);
    end;
  end;

PROCEDURE TEntryDialog.miDropInputClick(Sender: TObject);
  begin
    if filesListBox.ItemIndex>=0 then begin
      addNewEntryFromDroppedFile(entry[entryIndex]^.dropFile(filesListBox.ItemIndex));
      entry[entryIndex]^.getFileList(filesListBox.Items);
    end;
  end;

PROCEDURE TEntryDialog.miRemoveTagClick(Sender: TObject);
  begin
    if (TagListBox.ItemIndex>=0) then begin
      entry[entryIndex]^.removeTag(TagListBox.Items[TagListBox.ItemIndex]);
      getTags(entry[entryIndex]^.tags,TagListBox.Items);
    end;
  end;

PROCEDURE TEntryDialog.miRenameInpClick(Sender: TObject);
  begin
    if filesListBox.ItemIndex>=0 then begin
      formWithADropDown.init('Rename file to');
      formWithADropDown.ComboBox.Text:=entry[entryIndex]^.files[filesListBox.ItemIndex]^.getNameAsString;
      if (formWithADropDown.ShowModal=mrOK) and entry[entryIndex]^.files[filesListBox.ItemIndex]^.moveTo(T_structuredPath(string(formWithADropDown.ComboBox.Text))) then begin
        entry[entryIndex]^.updateAutomaticFields;
        entry[entryIndex]^.getFileList(filesListBox.Items);
      end;
    end;
  end;

PROCEDURE TEntryDialog.miShowInpClick(Sender: TObject);
begin
  entry[entryIndex]^.files[filesListBox.ItemIndex]^.showFile();
end;

PROCEDURE TEntryDialog.NameEditEditingDone(Sender: TObject);
  begin
    entry[entryIndex]^.givenName:=NameEdit.Text;
    Caption:=entry[entryIndex]^.givenName;
    TabControl1.Tabs[entryIndex]:=entry[entryIndex]^.givenName;
  end;

PROCEDURE TEntryDialog.TabControl1Change(Sender: TObject);
  begin
    entryIndex:=TabControl1.TabIndex;
    updateGUI(true);
  end;

PROCEDURE TEntryDialog.updateGUI(complete:boolean);
  VAR i:longint;
  begin
    Caption:=entry[entryIndex]^.givenName;
    getTagsForDropDown(AddTagComboBox.Items);
    addTagComboBox.Sorted:=true;
    getTags(entry[entryIndex]^.tags,TagListBox.Items);
    CommentMemo.Text:=entry[entryIndex]^.comment;
    entry[entryIndex]^.getFileList(filesListBox.Items);
    NameEdit.Text:=entry[entryIndex]^.givenName;
    if filesListBox.Items.Count>0 then filesListBox.ItemIndex:=0;
    if complete then filesListBoxSelectionChange(self,false);
    for i:=0 to length(entry[entryIndex]^.files)-1 do entry[entryIndex]^.files[i]^.generateThumbnail();
  end;

PROCEDURE TEntryDialog.CommentMemoEditingDone(Sender: TObject);
  begin
    entry[entryIndex]^.comment:=CommentMemo.Text;
  end;

PROCEDURE TEntryDialog.filesListBoxSelectionChange(Sender: TObject;
  User: boolean);
  VAR fileToView:P_fileInfo;
  begin
    if (filesListBox.ItemIndex>=0) and (filesListBox.ItemIndex<length(entry[entryIndex]^.files)) then begin
      fileToView:=entry[entryIndex]^.files[filesListBox.ItemIndex];
      if fileToView^.isImage and FileExists(fileToView^.getPreviewName) then begin
        try
          previewImage.Picture.LoadFromFile(fileToView^.getPreviewName);
          previewImage.Visible:=true;
        except
          writeln('error loading preview ',fileToView^.getPreviewName,' for file ',fileToView^.getNameAsString);
          previewImage.Picture.Clear;
          previewImage.Visible:=false;
        end;
        SynMemo1.ClearAll;
        SynMemo1.Visible:=false;
      end else if fileToView^.isSource then begin
        previewImage.Picture.Clear;
        previewImage.Visible:=false;
        SynMemo1.Lines.LoadFromFile(fileToView^.getNameAsString);
        SynMemo1.Visible:=true;
        //if fileToView^.getNormalizedExtension='.PAS'
        //  then SynMemo1.Highlighter:=SynFreePascalSyn1
        //  else SynMemo1.Highlighter:=SynBatSyn1;
      end else begin
        previewImage.Picture.Clear;
        previewImage.Visible:=false;
        SynMemo1.ClearAll;
        SynMemo1.Visible:=false;
      end;
    end else begin
      previewImage.Picture.Clear;
      previewImage.Visible:=false;
      SynMemo1.ClearAll;
      SynMemo1.Visible:=false;
    end;
  end;

PROCEDURE TEntryDialog.FormDestroy(Sender: TObject);
  begin

  end;

PROCEDURE TEntryDialog.AddTagComboBoxKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
  begin
    if key=13 then begin
      entry[entryIndex]^.addTag(AddTagComboBox.Text);
      getTagsForDropDown(AddTagComboBox.Items);
      addTagComboBox.Sorted:=true;
      getTags(entry[entryIndex]^.tags,TagListBox.Items);
    end;
  end;

PROCEDURE TEntryDialog.setup(forEntry: P_dbEntry);
  begin
    entryIndex:=0;
    setLength(entry,1);
    entry[entryIndex]:=forEntry;
    updateGUI(true);
    TabControl1.Tabs.Clear;
    TabControl1.Tabs.Add(forEntry^.givenName);
  end;

PROCEDURE TEntryDialog.addEntry(forEntry: P_dbEntry);
  begin
    entryIndex:=length(entry);
    setLength(entry,entryIndex+1);
    entry[entryIndex]:=forEntry;
    TabControl1.Tabs.Add(forEntry^.givenName);
    updateGUI(false);
    entryIndex:=0;
    updateGUI(false);
  end;



end.

