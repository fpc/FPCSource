unit inieditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn, ExtCtrls,
  IniFiles, lazutf8, SynEdit,SynMemo,{SynEditTypes,}SynHighlighterIni, SynEditTypes;

type

  { TFormIniEditor }

  TFormIniEditor = class(TForm)
    GUITimer: TIdleTimer;
    Label1: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    ProfileSelect: TComboBox;
    FileNameEdit: TFileNameEdit;
    INIFileLabel: TLabel;
    ProfileLabel: TLabel;
    SynIniHighlighter: TSynIniSyn;
    SynMemo: TSynMemo;
    procedure CancelButtonClick(Sender: TObject);

    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure GUITimerTimer(Sender: TObject);
    procedure ProfileSelectSelect(Sender: TObject);
    procedure SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FIniFilename: string;
    FIniStream: TMemoryStream;
    FMustReloadProfileSelect: boolean;
    FSaveChanges: boolean; //If off, don't save any changes (e.g. when canceling form)
    FProfileSelectKey: string;
    FProfileSelectSection: string;
    FProfileSelectValue: string;
    procedure LoadGUI(KeepCursorPosition: boolean; LoadSynmemo: boolean; LoadProfileselect: boolean); //Loads GUI elements from backing stream
    procedure SetIniFilename(AValue: string); //Loads file into stream
    procedure SetProfileSelectKey(AValue: string);
    procedure SetProfileSelectSection(AValue: string);
    // Saves synmemo contents to memory stream and optionally to file, too
    procedure SynMemoSave(AlsoToFile: boolean);
    { private declarations }
  public
    // File to be edited/viewed:
    property INIFile: string read FIniFilename write SetIniFilename;
    // Section name where profile selection is to be stored. If nothing, the root/top level will be used
    property ProfileSelectSection: string read FProfileSelectSection write SetProfileSelectSection;
    // Key to be used for storing selected profile
    // If empty, don't use the select profile combobox
    property ProfileSelectKey: string read FProfileSelectKey write SetProfileSelectKey;
    // The value the user selected
    property ProfileSelectValue: string read FProfileSelectValue;
  end;

var
  Form1: TFormIniEditor;

implementation

{$R *.lfm}

{ TFormIniEditor }


procedure TFormIniEditor.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  // Go through property for reload file+reload GUI code
  IniFile:=FileNames[0];
end;

procedure TFormIniEditor.FormShow(Sender: TObject);
begin
  if FileNameEdit.FileName='' then
    FileNameEdit.FileName:=ExpandFileNameUTF8(FIniFilename);
end;

procedure TFormIniEditor.GUITimerTimer(Sender: TObject);
begin
  if FMustReloadProfileSelect then
    LoadGUI(true,false,true);
end;

procedure TFormIniEditor.ProfileSelectSelect(Sender: TObject);
// Save selected profile
var
  MyIniFile: TIniFile;
begin
  FProfileSelectValue:=ProfileSelect.Text;
  if FProfileSelectValue<>'' then
  begin
    // Save any changes in synmemo
    SynMemoSave(false);
    FIniStream.Position:=0; //Load from beginning
    MyIniFile := TIniFile.Create(FIniStream, True);
    FIniStream.Position:=0; //Needed to save again
    try
      MyIniFile.WriteString(FProfileSelectSection,FProfileSelectKey,FProfileSelectValue);
    finally
      MyIniFile.Free;
    end;
    LoadGUI(true,true,true);
  end;
end;

procedure TFormIniEditor.SynMemoStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  // If user edits the profile name directly or edits/deletes/adds
  // one of the section names, the combobox will need to be refreshed.
  // Pass this on to a timer so it gets done without disturbing the updates
  if (FMustReloadProfileSelect=false) and (scModified in Changes) then
  begin
    //todo: test for section line or key=value
    FMustReloadProfileSelect:=true;
  end;
end;

procedure TFormIniEditor.LoadGUI(KeepCursorPosition: boolean; LoadSynmemo: boolean; LoadProfileselect: boolean);
var
  MyIniFile : TIniFile;
  OldPos: TPoint;
  ProfIndex: integer;
  Sections: TStringList;
begin
  SynMemo.BeginUpdate(false);
  if LoadSynMemo then
  begin
    OldPos:=SynMemo.CaretXY;
    FIniStream.Position:=0; //Load from beginning
    SynMemo.Lines.LoadFromStream(FIniStream);
  end;
  FIniStream.Position:=0; //Load from beginning
  MyIniFile := TIniFile.Create(FIniStream, True);
  FIniStream.Position:=0; //In case we want to save
  Sections:=TStringList.Create;
  try
    if LoadProfileselect then
    begin
      Sections.Sorted:=true;
      MyIniFile.ReadSections(Sections);
      // Now take out the one where the profile is stored as it will confuse users
      if Sections.Find(FProfileSelectSection, ProfIndex) then
        Sections.Delete(ProfIndex);
      ProfileSelect.Clear;
      ProfileSelect.Items.AddStrings(Sections);
      FProfileSelectValue:=MyIniFile.ReadString(FProfileSelectSection,FProfileSelectKey,'');
      ProfileSelect.Text:=FProfileSelectValue;
      FMustReloadProfileSelect:=false;
    end;

    // restore cursor/caret position
    if LoadSynMemo and KeepCursorPosition then
      SynMemo.CaretXY:=OldPos;
  finally
    MyIniFile.Free;
    Sections.Free;
    SynMemo.EndUpdate;
  end;
end;

procedure TFormIniEditor.SetIniFilename(AValue: string);
begin
  if FIniFilename=AValue then Exit;
  FIniFilename:=AValue;
  if FIniFileName<>'' then
  begin
    try
      FIniStream.Clear;
      FIniStream.LoadFromFile(FIniFilename);
    except
      on E: Exception do
        ShowMessage('Error loading file '+FIniFilename+': '+E.Message);
    end;
    LoadGUI(false,true,true);
  end;
end;

procedure TFormIniEditor.SetProfileSelectKey(AValue: string);
begin
  if FProfileSelectKey=AValue then Exit;
  FProfileSelectKey:=AValue;
  if (FProfileSelectKey<>'') and (FProfileSelectSection<>'') then
    LoadGUI(false,true,true);
end;

procedure TFormIniEditor.SetProfileSelectSection(AValue: string);
begin
  if FProfileSelectSection=AValue then Exit;
  FProfileSelectSection:=AValue;
  if (FProfileSelectKey<>'') and (FProfileSelectSection<>'') then
    LoadGUI(false,true,true);
end;

procedure TFormIniEditor.SynMemoSave(AlsoToFile: boolean);
begin
  if FSaveChanges then
  begin
    FIniStream.Clear;
    FIniStream.Position:=0; //Save from beginning
    SynMemo.Lines.SaveToStream(FiniStream);
    FIniStream.Position:=0; //Save from beginning
    if AlsoToFile and (FIniFileName<>'') then
      FIniStream.SaveToFile(FIniFilename);
  end;
end;


procedure TFormIniEditor.CancelButtonClick(Sender: TObject);
begin
  FSaveChanges:=false;
end;



procedure TFormIniEditor.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  // go through the property to load new file
  INIFile:=Value;
end;

procedure TFormIniEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Save any uncommitted changes
  //todo: only if dirty synmemo changestamp?; also check if this is best event
  SynMemoSave(true);
end;

procedure TFormIniEditor.FormCreate(Sender: TObject);
begin
  FIniStream:=TMemoryStream.Create;
  FSaveChanges:=true; //allow editing
end;

procedure TFormIniEditor.FormDestroy(Sender: TObject);
begin
  FIniStream.Free;
end;

end.

