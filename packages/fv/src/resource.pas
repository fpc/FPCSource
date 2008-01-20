
asjgfsdkjsfld
{ Resource Unit

  Programmer: Brad Williams
  BitSoft Development, L.L.C.
  Copyright (c) 1996
  Version 1.1

Revision History

1.1   (12/26/97)
  - updated to add cdResource directive so that can use standard TStringList
    resources created by TVRW and TVDT

1.0
  - original implementation }

unit Resource;

interface

{
  The Resource unit provides global variables which are used to build and
  access resource files.  InitRez must always be called before accessing any
  variables in the Resource unit.  The programmer should also always call
  Done to free all file handles allocated to the program.
}

{$i platform.inc}

{$ifdef PPC_FPC}
  {$H-}
{$else}
  {$F+,O+,E+,N+}
{$endif}
{$X+,R-,I-,Q-,V-}
{$ifndef OS_UNIX}
  {$S-}
{$endif}

uses

  FVConsts, Objects, Dos;

const

  RezExt: ExtStr = '.RES';
    { The file extension used on all resource files. }
  RezBufferSize: Word = 4096;
    { RezBufferSize is the number of bytes to use for the resource file's
      stream's buffer.  RezBufferSize is passed to TBufStream.Init. }

    { reXXXX constants are used with resource files to retrieve the standard
      Free Vision dialogs.  The constant is followed by the Unit in which it
      is used and the resource which is stored separated by a period. }

  reChDirDialog = 'ChDirDialog';  { StdDlg.TChDirDialog }
  reEditChDirDialog = 'EditChDirDialog';  { StdDlg.TEditChDirDialog }
  reFindTextDlg = 'FindTextDlg';  { Editors.CreateFindDialog }
  reHints = 'Hints'; { Resource.Hints }
  reJumpLineDlg = 'JumpLineDlg';  { Editors.MakeJumpLineDlg }
  reLabels = 'Labels';  { Resource.Labels }
  reMenuBar = 'MenuBar';  { App.MenuBar }
  reOpenDlg = 'OpenDlg';  { StdDlg.TFileDialog - Open }
  reReformDocDlg = 'ReformDocDlg';  { Editors.MakeReformDocDlg }
  reReplaceDlg = 'ReplaceDlg';  { Editors.CreateReplaceDialog }
  reRightMarginDlg = 'RightMarginDlg';  { Editors.MakeRightMarginDlg }
  reStatusLine = 'StatusLine';  { App.StatusLine }
  reStrings = 'Strings';  { Resource.Strings }
  reSaveAsDlg = 'SaveAsDlg';  { StdDlg.TFileDialog - Save As }
  reTabStopDlg = 'TabStopDlg';  { Editors.MakeTabStopDlg }
  reWindowListDlg = 'WindowListDlg';  { Editors.MakeWindowListDlg }
  reAboutDlg = 'About';  { App unit about dialog }

  {$I str.inc}
    { STR.INC declares all the string list constants used in the standard
      Free Vision library units.  They are placed in a separate file as a
      template for use by the resource file generator, MakeRez.

      Applications which use resource files and need to add strings of their
      own should use STR.INC as the start for the resource file.

      See MakeRez.PAS for more information about generating resource files.}

type


  PConstant = ^TConstant;
  TConstant = object(TObject)
    Value: Word;
      { The value assigned to the constant. }
    constructor Init (AValue: Word; AText: string);
      { Init assigns AValue to Value to AText to Text.  AText may be an empty
        string.

        If an error occurs Init fails. }
    destructor Done; virtual;
      { Done disposes of Text then calls the inherited destructor. }
    procedure SetText (AText: string);
      { SetText changes FText to the word equivalent of AText. }
    procedure SetValue (AValue: string);
      { SetValue changes Value to the word equivalent of AValue. }
    function Text: string;
      { Text returns a string equivalent to FText.  If FText is nil, an
        empty string is returned. }
    function ValueAsString: string;
      { ValueAsString returns the string equivalent of Value. }
      private
    FText: PString;
      { The text to display for the constant. }
  end;  { of TConstant }


  PMemStringList = ^TMemStringList;
  TMemStringList = object(TSortedCollection)
    { A TMemStringList combines the functions of a TStrListMaker and a
      TStringList into one object, allowing generation and use of string
      lists in the same application.  TMemStringList is fully compatible
      with string lists created using TStrListMaker, so legacy applications
      will work without problems.

      When using a string list in the same program as it is created, a
      resource file is not required.  This allows language independant coding
      of units without the need for conditional defines and recompiling. }
    constructor Init;
      { Creates an empty, in-memory string list that is not associated with a
        resource file. }
    constructor Load (var S: TStream);
      { Load creates a TStringList from which it gets its strings upon a call
        to Get.  The strings on the resource file may be loaded into memory
        for editing by calling LoadList.

        If initialized with Load, the stream must remain valid for the life
        of this object. }
    destructor Done; virtual;
      { Done deallocates the memory allocated to the string list. }
    function Compare (Key1, Key2: Pointer): Sw_Integer; virtual;
      { Compare assumes Key1 and Key2 are Word values and returns:

            -1  if Key1 < Key2
             0  if Key1 = Key2
             1  if Key1 > Key2 }
    function Get (Key: Word): String; virtual;
      { GetKey searches for a string with a key matching Key and returns it.
        An empty string is returned if a string with a matching Key is not
        found.

        If Count > 0, the in memory collection is searched.  If List^.Count
        is 0, the inherited Get method is called. }
    procedure Insert (Item: Pointer); virtual;
      { If Item is not nil, Insert attempts to insert the item into the
        collection.  If a collection expansion error occurs Insert disposes
        of Item by calling FreeItem.

        Item must be a pointer to a TConstant or its descendant. }
    function KeyOf (Item: Pointer): Pointer; virtual;
      { KeyOf returns a pointer to TConstant.Value. }
    function LoadStrings: Sw_Integer;
      { LoadStrings reads all strings the associated resource file into
        memory, places them in the collection, and returns 0.

        If an error occurs LoadStrings returns the stream status error code
        or a DOS error code.  Possible DOS error codes include:

               2:   no associated resource file
               8:   out of memory }
    function NewConstant (Value: Word; S: string): PConstant; virtual;
      { NewConstant is called by LoadStrings. }
    procedure Put (Key: Word; S: String); virtual;
      { Put creates a new PConstant containing Key and Word then calls
        Insert to place it in the collection. }
    procedure Store (var S: TStream);
      { Store creates a TStrListMaker, fills it with the items in List,
        writes the TStrListMaker to the stream by calling
        TStrListMaker.Store, then disposes of the TStrListMaker. }
  private
    StringList: PStringList;
  end;  { of TMemStringList) }


var

  {$ifdef cdResource}
  Hints: PStringList;
  {$else}
  Hints: PMemStringList;
  {$endif cdResource}
    { Hints is a string list for use within the application to provide
      context sensitive help on the command line.  Hints is always used in
      the application. }

  {$ifdef cdResource}
  Strings: PStringList;
  {$else}
  Strings: PMemStringList;
  {$endif cdResource}
    { Strings holds messages such as errors and general information that are
      displayed at run-time, normally with MessageBox.  Strings is always
      used in the application. }

  {$ifdef cdResource}
  Labels: PStringList;
  {$else}
  Labels: PMemStringList;
  {$endif cdResource}
    { Labels is a string list for use within the application when a
      resource file is not used, or when creating a resource file.  Labels
      contains all text used in dialog titles, labels, buttons, menus,
      statuslines, etc., used in the application which can be burned into
      language specific resources.  It does not contain any messages
      displayed at run-time using MessageBox or the status line hints.

      Using the Labels variable when creating views allows language
      independant coding of views such as the MessageBox, StdDlg and Editors
      units. }

  RezFile: PResourceFile;
    { RezFile is a global variable used when the Free Vision library
      is compiled using the cdResource conditional define, or when creating
      resource files.

      All standard Free Vision application resources are accessed from the
      resource file using the reXXXX constants.  Modify the STR.INC under a
      new file name to create new language specific resource files.  See the
      MakeRez program file for more information. }



procedure DoneResource;
  { Done destructs all objects initialized in this unit and frees all
    allocated heap. }

{$ifndef cdResource}
function InitResource: Boolean;
{$endif cdResource}
  { Init initializes the Hints and Strings for use with in memory strings
    lists.  Init should be used in applications which do not use a resource
    file, or when creating resource files.  }

{$ifdef cdResource}
function InitRezFile (AFile: FNameStr; Mode: Word;
                      var AResFile: PResourceFile): Sw_Integer;
{$endif cdResource}
  { InitRezFile initializes a new PResourceFile using the name passed in
    AFile and the stream mode passed in Mode and returns 0.

    If an error occurs InitRezFile returns the DOS error and AResFile is
    invalid.  Possible DOS error values include:

        2: file not found or other stream initialization error
        11: invalid format - not a valid resource file }

{$ifdef cdResource}
function LoadResource (AFile: FNameStr): Boolean;
{$endif cdResource}
  { Load is used to open a resource file for use in the application.

    For Load to return True, the resource file must be properly opened and
    assigned to RezFile and the Hints string list must be successfully loaded
    from the stream.  If an error occurs, Load displays an English error
    message using PrintStr and returns False. }

function MergeLists (Source, Dest: PMemStringList): Sw_Integer;
  { MergeLists moves all key/string pairs from Source to destination,
    deleting them from Source.  Duplicate strings are ignored. }


const
  RMemStringList: TStreamRec = (
    ObjType: idMemStringList;
    VmtLink: Ofs(TypeOf(TMemStringList)^);
    Load: @TMemStringList.Load;
    Store: @TMemStringList.Store);


implementation

{****************************************************************************}
{                           Private Declarations                             }
{****************************************************************************}

uses
  {Memory, }Drivers;

{****************************************************************************}
{ TConstant object                                                           }
{****************************************************************************}
{****************************************************************************}
{ TConstant.Init                                                             }
{****************************************************************************}
constructor TConstant.Init (AValue: Word; AText: string);
begin
  if not inherited Init then
    Fail;
  Value := AValue;
  FText := NewStr(AText);
  if (FText = nil) and (AText <> '') then
  begin
    inherited Done;
    Fail;
  end;
end;

{****************************************************************************}
{ TConstant.Done                                                             }
{****************************************************************************}
destructor TConstant.Done;
begin
  DisposeStr(FText);
  inherited Done;
end;

{****************************************************************************}
{ TConstant.SetText                                                          }
{****************************************************************************}
procedure TConstant.SetText (AText: string);
begin
  DisposeStr(FText);
  FText := NewStr(AText);
end;

{****************************************************************************}
{ TConstant.SetValue                                                         }
{****************************************************************************}
procedure TConstant.SetValue (AValue: string);
var
  N: Word;
  ErrorCode: Integer;
begin
  Val(AValue,N,ErrorCode);
  if ErrorCode = 0 then
    Value := N;
end;

{****************************************************************************}
{ TConstant.Text                                                             }
{****************************************************************************}
function TConstant.Text: string;
begin
  if (FText = nil) then
    Text := ''
  else Text := FText^;
end;

{****************************************************************************}
{ TConstant.ValueAsString                                                    }
{****************************************************************************}
function TConstant.ValueAsString: string;
var
  S: string[5];
begin
  Str(Value,S);
  ValueAsString := S;
end;

{****************************************************************************}
{ TMemStringList Object                                                      }
{****************************************************************************}
{****************************************************************************}
{ TMemStringList.Init                                                        }
{****************************************************************************}
constructor TMemStringList.Init;
begin
  if not inherited Init(10,10) then
    Fail;
  StringList := nil;
end;

{****************************************************************************}
{ TMemStringList.Load                                                        }
{****************************************************************************}
constructor TMemStringList.Load (var S: TStream);
begin
  if not inherited Init(10,10) then
    Fail;
  StringList := New(PStringList,Load(S));
end;

{****************************************************************************}
{ TMemStringList.Done                                                        }
{****************************************************************************}
destructor TMemStringList.Done;
begin
  if (StringList <> nil) then
    Dispose(StringList,Done);
  inherited Done;
end;

{****************************************************************************}
{ TMemStringList.Compare                                                     }
{****************************************************************************}
function TMemStringList.Compare (Key1, Key2: Pointer): Sw_Integer;
begin
  if Word(Key1^) < Word(Key2^) then
    Compare := -1
  else Compare := Byte(Word(Key1^) > Word(Key2^));
end;

{****************************************************************************}
{ TMemStringList.Get                                                         }
{****************************************************************************}
function TMemStringList.Get (Key: Word): string;
var
  i: Sw_Integer;
  S: string;
begin
  if (StringList = nil) then
  begin  { started with Init, use in memory string list }
    if Search(@Key,i) then
      Get := PConstant(At(i))^.Text
    else Get := '';
  end
  else begin
    S := StringList^.Get(Key);
    Get := S;
  end;
end;

{****************************************************************************}
{ TMemStringList.Insert                                                      }
{****************************************************************************}
procedure TMemStringList.Insert (Item: Pointer);
var
  i: Sw_Integer;
begin
  if (Item <> nil) then
  begin
    i := Count;
    inherited Insert(Item);
    if (i = Count) then  { collection expansion failed }
      Dispose(PConstant(Item),Done);
  end;
end;

{****************************************************************************}
{ TMemStringList.KeyOf                                                       }
{****************************************************************************}
function TMemStringList.KeyOf (Item: Pointer): Pointer;
begin
  KeyOf := @(PConstant(Item)^.Value);
end;

{****************************************************************************}
{ TMemStringList.LoadStrings                                                 }
{****************************************************************************}
function TMemStringList.LoadStrings: Sw_Integer;
  procedure MakeEditableString (var Str: string);
  const
    SpecialChars: array[1..3] of Char = #3#10#13;
  var
    i, j: Byte;
  begin
    for i := 1 to 3 do
      while (Pos(SpecialChars[i],Str) <> 0) do
      begin
        j := Pos(SpecialChars[i],Str);
        System.Delete(Str,j,1);
        case i of
          1: System.Insert('#3',Str,j);
          2: System.Insert('#10',Str,j);
          3: System.Insert('#13',Str,j);
        end;
      end;
  end;
var
  Constant: PConstant;
  i: Word;
  S: string;
begin
  LoadStrings := 0;
  if (StringList = nil) then
  begin
    LoadStrings := 2;
    Exit;
  end;
  for i := 0 to 65535 do
  begin
    S := StringList^.Get(i);
    if (S <> '') then
    begin
      MakeEditableString(S);
      Constant := NewConstant(i,S);
(*
      if LowMemory then
      begin
        if (Constant <> nil) then
          Dispose(Constant,Done);
        LoadStrings := 8;  { out of memory }
        Exit;
      end;
*)
      Insert(Constant);
    end;
  end;
end;

{****************************************************************************}
{ TMemStringList.NewConstant                                                 }
{****************************************************************************}
function TMemStringList.NewConstant (Value: Word; S: string): PConstant;
begin
  NewConstant := New(PConstant,Init(Value,S));
end;

{****************************************************************************}
{ TMemStringList.Put                                                         }
{****************************************************************************}
procedure TMemStringList.Put (Key: Word; S: string);
begin
  Insert(New(PConstant,Init(Key,S)));
end;

{****************************************************************************}
{ TMemStringList.Store                                                       }
{****************************************************************************}
procedure TMemStringList.Store (var S: TStream);
var
  StrList: PStrListMaker;
  Size: Word;
  procedure Total (Constant: PConstant);{$ifndef FPC}far;{$endif}
  begin
    with Constant^ do
      Inc(Size,Succ(Length(Text)));
  end;
  procedure AddString (Constant: PConstant);{$ifndef FPC}far;{$endif}
  const
    Numbers = ['0'..'9'];
  var
    i, j: Byte;
    N: Byte;
    ErrorCode: Integer;
    S: string;
  begin
    with Constant^ do
    begin
        { convert formatting characters }
      S := Text;
      while (Pos('#',S) <> 0) do
      begin
        i := Succ(Pos('#',S));
        j := i;
        if (Length(S) > j) then
          Inc(j,Byte(S[Succ(j)] in Numbers));
        Val(Copy(S,i,j-i+1),N,ErrorCode);
        System.Delete(S,Pred(i),j-i+2);
        System.Insert(Char(N),S,Pred(i));
      end;
      StrList^.Put(Value,Text)
    end;
  end;
begin
  Size := 0;
  ForEach(@Total);
  StrList := New(PStrListMaker,Init(Size,Count * 6));
  if (StrList = nil) then
  begin
    S.Status := 8;  { DOS error not enough memory }
    Exit;
  end;
  ForEach(@AddString);
  StrList^.Store(S);
  Dispose(StrList,Done);
end;

{****************************************************************************}
{                       Public Procedures and Functions                      }
{****************************************************************************}

{****************************************************************************}
{ Done                                                                       }
{****************************************************************************}
procedure DoneResource;
begin
  if (RezFile <> nil) then
    begin
      Dispose(RezFile,Done);
      RezFile:=nil;
    end;
  if (Strings <> nil) then
    begin
      Dispose(Strings,Done);
      Strings:=nil;
    end;
  if (Hints <> nil) then
    begin
      Dispose(Hints,Done);
      Hints:=nil;
    end;
  if (Labels <> nil) then
    begin
      Dispose(Labels,Done);
      Labels:=nil;
    end;
end;

{****************************************************************************}
{ Init                                                                       }
{****************************************************************************}
{$ifndef cdResource}

{$I strtxt.inc}
  { strtxt.inc contains the real strings and procedures InitRes... which
    is converted from str.inc }

function InitResource: Boolean;
begin
  InitResource := False;
  Hints := New(PMemStringList,Init);
  if (Hints = nil) then
  begin
    PrintStr('Fatal error.  Could not create Hints list.');
    Exit;
  end;
  Strings := New(PMemStringList,Init);
  if (Strings = nil) then
  begin
    DoneResource;
    Exit;
  end;
  Labels := New(PMemStringList,Init);
  if (Labels = nil) then
  begin
    DoneResource;
    Exit;
  end;
{ now load the defaults }
  InitResLabels;
  InitResStrings;
  InitResource := True;
end;
{$endif cdResource}

{****************************************************************************}
{ InitRezFile                                                                }
{****************************************************************************}
{$ifdef cdResource}
function InitRezFile (AFile: FNameStr; Mode: Word;
                      var AResFile: PResourceFile): Sw_Integer;
var
  Stream: PBufStream;
  Result: Sw_Integer;
begin
  Stream := New(PBufStream,Init(AFile,Mode,RezBufferSize));
  if (Stream = nil) then
    Result := 2  { file not found; could also be out of memory }
  else begin
    AResFile := New(PResourceFile,Init(Stream));
    if (AResFile = nil) then
    begin
      Dispose(Stream,Done);
      Result := 11;
    end
    else Result := 0;
  end;
  InitRezFile := Result;
end;
{$endif cdResource}

{****************************************************************************}
{ Load                                                                       }
{****************************************************************************}
{$ifdef cdResource}
function LoadResource (AFile: FNameStr): Boolean;
var
  Stream: PBufStream;
begin
  Load := False;
  Stream := New(PBufStream,Init(AFile,stOpenRead,RezBufferSize));
  if (Stream = nil) or (Stream^.Status <> 0) then
  begin
    Done;
    PrintStr('Fatal error.  Could not open resource file: ' + AFile);
    Exit;
  end;
  RezFile := New(PResourceFile,Init(Stream));
  if (RezFile = nil) then
  begin
    Dispose(Stream,Done);
    Done;
    PrintStr('Fatal error.  Could not initialize resource file.');
    Exit;
  end;
  Hints := PStringList(RezFile^.Get(reHints));
  if (Hints = nil) then
  begin
    Done;
    PrintStr('Fatal error.  Could not load Hints string list.');
    Exit;
  end;
  Strings := PStringList(RezFile^.Get(reStrings));
  if (Strings = nil) then
  begin
    Done;
    PrintStr('Fatal error.  Could not load Strings string list.');
    Exit;
  end;
  Load := True;
end;
{$endif cdResource}

{****************************************************************************}
{ MergeLists                                                                 }
{****************************************************************************}
function MergeLists (Source, Dest: PMemStringList): Sw_Integer;
var
  Result: Sw_Integer;
  procedure MoveItem (Constant: PConstant);{$ifndef FPC}far;{$endif}
  var
    j: Sw_Integer;
  begin
    if (Result = 0) and (not Dest^.Search(Dest^.KeyOf(Constant),j)) then
    begin
      j := Dest^.Count;
      Dest^.Insert(Constant);
      if (j = Dest^.Count) then
        Result := 8
      else Source^.Delete(Constant);
    end;
  end;
begin
  if (Source = nil) or (Dest = nil) then
  begin
    MergeLists := 6;
    Exit;
  end;
  Result := 0;
  Source^.ForEach(@MoveItem);
  MergeLists := Result;
end;

{****************************************************************************}
{                            Unit Initialization                             }
{****************************************************************************}

begin
  RezFile := nil;
  Hints := nil;
  Strings := nil;
  Labels := nil;
end.
