unit DDG_DS;

{$define dsdebug}

interface

uses Db, Classes, DDG_Rec;

type

  PInteger =  ^Integer;

  // Bookmark information record to support TDataset bookmarks:
  PDDGBookmarkInfo = ^TDDGBookmarkInfo;
  TDDGBookmarkInfo = record
    BookmarkData: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  // List used to maintain access to file of record:
  TIndexList = class(TList)
  public
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
  end;

  // Specialized DDG TDataset descendant for our "table" data:
  TDDGDataSet = class(TDataSet)
  private
    function GetDataFileSize: Integer;
  public
    FDataFile: TDDGDataFile;
    FIdxName: string;
    FIndexList: TIndexList;
    FTableName: string;
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBufferSize: Integer;
    procedure SetTableName(const Value: string);
  protected
    { Mandatory overrides }
    // Record buffer methods:
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    // Misc methods:
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    // Additional procedures
    procedure EmptyTable;
  published
    property Active;
    property TableName: string read FTableName write SetTableName;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;

    // Additional Properties
    property DataFileSize: Integer read GetDataFileSize;
  end;

implementation

uses SysUtils;

const
  feDDGTable = '.ddg';
  feDDGIndex = '.ddx';
  // note that file is not being locked!

{ TIndexList }

procedure TIndexList.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TIndexList.LoadFromStream(Stream: TStream);
var
  Value: PtrInt;
begin
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(Value, SizeOf(Value));
    Add(Pointer(Value));
  end;
end;

procedure TIndexList.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TIndexList.SaveToStream(Stream: TStream);
var
  i: Integer;
  Value: PtrInt;
begin
  for i := 0 to Count - 1 do
  begin
    Value := PtrInt(Items[i]);
    Stream.Write(Value, SizeOf(Value));
  end;
end;

{ TDDGDataSet }

constructor TDDGDataSet.Create(AOwner: TComponent);
begin
  FIndexList := TIndexList.Create;
  FRecordSize := SizeOf(TDDGData);
  FBufferSize := FRecordSize + SizeOf(TDDGBookmarkInfo);
  inherited Create(AOwner);
end;

destructor TDDGDataSet.Destroy;
begin
  inherited Destroy;
  FIndexList.Free;
end;

function TDDGDataSet.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(FBufferSize);
end;

procedure TDDGDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

procedure TDDGDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FBufferSize, 0);
end;

function TDDGDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  IndexPos: Integer;
begin
 if FIndexList.Count < 1 then
    Result := grEOF
  else begin
    Result := grOk;
    case GetMode of
      gmPrior:
        if FRecordPos <= 0 then
        begin
          Result := grBOF;
          FRecordPos := -1;
        end
        else
          Dec(FRecordPos);
      gmCurrent:
        if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
           Result := grError;
      gmNext:
        if FRecordPos >= RecordCount-1 then
          Result := grEOF
        else
          Inc(FRecordPos);
    end;
    if Result = grOk then
    begin
      IndexPos := Integer(FIndexList[FRecordPos]);
      Seek(FDataFile, IndexPos);
      BlockRead(FDataFile, PDDGData(Buffer)^, 1);
      with PDDGBookmarkInfo(Buffer + FRecordSize)^ do
      begin
        BookmarkData := FRecordPos;
        BookmarkFlag := bfCurrent;
      end;
    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No records');
  end;
end;

function TDDGDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TDDGDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := True;
  case Field.Index of
    0:
      begin
        Move(ActiveBuffer^, Buffer^, Field.Size);
        Result := PChar(Buffer)^ <> #0;
      end;
    1: Move(PDDGData(ActiveBuffer)^.Height, Buffer^, Field.DataSize);
    2: Move(PDDGData(ActiveBuffer)^.LongField, Buffer^, Field.DataSize);
    3: Move(PDDGData(ActiveBuffer)^.ShoeSize, Buffer^, Field.DataSize);
    4: Move(PDDGData(ActiveBuffer)^.WordField, Buffer^, Field.DataSize);
    5: Move(PDDGData(ActiveBuffer)^.DateTimeField, Buffer^, Field.DataSize);
    6: Move(PDDGData(ActiveBuffer)^.TimeField, Buffer^, Field.DataSize);
    7: Move(PDDGData(ActiveBuffer)^.DateField, Buffer^, Field.DataSize);
    8: Move(PDDGData(ActiveBuffer)^.Even, Buffer^, Field.DataSize);
  end;
end;

procedure TDDGDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
  case Field.Index of
    0: Move(Buffer^, ActiveBuffer^, Field.Size);
    1: Move(Buffer^, PDDGData(ActiveBuffer)^.Height, Field.DataSize);
    2: Move(Buffer^, PDDGData(ActiveBuffer)^.LongField, Field.DataSize);
    3: Move(Buffer^, PDDGData(ActiveBuffer)^.ShoeSize, Field.DataSize);
    4: Move(Buffer^, PDDGData(ActiveBuffer)^.WordField, Field.DataSize);
    5: Move(Buffer^, PDDGData(ActiveBuffer)^.DateTimeField, Field.DataSize);
    6: Move(Buffer^, PDDGData(ActiveBuffer)^.TimeField, Field.DataSize);
    7: Move(Buffer^, PDDGData(ActiveBuffer)^.DateField, Field.DataSize);
    8: Move(Buffer^, PDDGData(ActiveBuffer)^.Even, Field.DataSize);
  end;
  DataEvent(deFieldChange, Ptrint(Field));
end;

procedure TDDGDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PDDGBookmarkInfo(Buffer + FRecordSize)^.BookmarkData;
end;

function TDDGDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PDDGBookmarkInfo(Buffer + FRecordSize)^.BookmarkFlag;
end;

procedure TDDGDataSet.InternalGotoBookmark(ABookmark: Pointer);
begin
  FRecordPos := PInteger(ABookmark)^;
  Writeln ('Bookmark : Setting record position to : ',FrecordPos);
end;

procedure TDDGDataSet.InternalSetToRecord(Buffer: PChar);
begin
  // bookmark value is the same as an offset into the file
  FRecordPos := PDDGBookmarkInfo(Buffer + FRecordSize)^.Bookmarkdata;
end;

procedure TDDGDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PDDGBookmarkInfo(Buffer + FRecordSize)^.BookmarkData := PInteger(Data)^;
end;

procedure TDDGDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PDDGBookmarkInfo(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TDDGDataSet.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TDDGDataSet.InternalInitFieldDefs;
begin
  // create FieldDefs which map to each field in the data record
  FieldDefs.Clear;
  TFieldDef.Create(FieldDefs, 'Name', ftString, SizeOf(TNameStr), False, 1);
  TFieldDef.Create(FieldDefs, 'Height', ftFloat, 0, False, 2);
  TFieldDef.Create(FieldDefs, 'LongField',ftInteger, 0, False, 3);
  TFieldDef.Create(FieldDefs, 'ShoeSize', ftSmallint, 0, False, 4);
  TFieldDef.Create(FieldDefs, 'WordField', ftword, 0, false, 5);
  TFieldDef.Create(FieldDefs, 'DateTimeField', ftDateTime, 0, false, 6);
  TFieldDef.Create(FieldDefs, 'TimeField',ftTime, 0, false, 7);
  TFieldDef.Create(FieldDefs, 'DateField',ftDate, 0, false, 8);
  TFieldDef.Create(FieldDefs, 'Booleanfield',ftboolean, 0, False, 9);
end;

procedure TDDGDataSet.InternalLast;
begin
  FRecordPos := FIndexList.Count;
end;

procedure TDDGDataSet.InternalClose;
begin
  if FileRec(FDataFile).Mode <> 0 then
    CloseFile(FDataFile);
  FIndexList.SaveToFile(FIdxName);
  FIndexList.Clear;
  if DefaultFields then
    DestroyFields;
  FRecordPos := -1;
  FillChar(FDataFile, SizeOf(FDataFile), 0);
end;

procedure TDDGDataSet.InternalHandleException;
begin
  // standard implementation for this method:
  // Application.HandleException(Self);
end;

procedure TDDGDataSet.InternalDelete;
begin
  FIndexList.Delete(FRecordPos);
  if FRecordPos >= FIndexList.Count then Dec(FRecordPos);
end;

procedure TDDGDataSet.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var
  RecPos: Integer;
begin
  Seek(FDataFile, FileSize(FDataFile));
  BlockWrite(FDataFile, PDDGData(Buffer)^, 1);
  if DoAppend then
  begin
    FIndexList.Add(Pointer(FileSize(FDataFile) - 1));
    InternalLast;
  end
  else begin
    if FRecordPos = -1 then RecPos := 0
    else RecPos := FRecordPos;
    FIndexList.Insert(RecPos, Pointer(FileSize(FDataFile) - 1));
  end;
  FIndexList.SaveToFile(FIdxName);
end;

procedure TDDGDataSet.InternalOpen;
var
  HFile: THandle;
begin
  // make sure table and index files exist
  FIdxName := ChangeFileExt(FTableName, feDDGIndex);
  if not (FileExists(FTableName) and FileExists(FIdxName)) then
    begin
 {
    if MessageDlg('Table or index file not found.  Create new table?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      HFile := FileCreate(FTableName);
      if HFile = -1 then
        DatabaseError('Error creating table file');
      FileClose(HFile);
      HFile := FileCreate(FIdxName);
      if HFile = -1 then
        DatabaseError('Error creating index file');
      FileClose(HFile);
    end
   else
 }
      DatabaseError('Could not open table');
  end;
  // open data file
  FileMode := fmOpenReadWrite;
  Writeln ('OPening data file');
  AssignFile(FDataFile, FTableName);
  Reset(FDataFile);
  try
    writeln ('Loading index file');
    FIndexList.LoadFromFile(FIdxName); // initialize index TList from file
    FRecordPos := -1;                  // initial record pos before BOF
    BookmarkSize := SizeOf(Integer);   // initialize bookmark size for VCL
    InternalInitFieldDefs;             // initialize FieldDef objects
    // Create TField components when no persistent fields have been created
    {$ifdef dsdebug}
    writeln ('Creating Fields');
    {$endif}
    if DefaultFields then CreateFields;
    {$ifdef dsdebug}
    writeln ('Binding Fields');
    {$endif}
    BindFields(True);                  // bind FieldDefs to actual data
  except
    {$ifdef dsdebug}
    Writeln ('Caught Exception !!');
    {$endif}
    CloseFile(FDataFile);
    FillChar(FDataFile, SizeOf(FDataFile), 0);
    raise;
  end;
 {$ifdef dsdebug}
  Writeln ('End of internalopen');
 {$endif}
end;

procedure TDDGDataSet.InternalPost;
var
  RecPos, InsPos: PtrInt;
begin
 {$ifdef dsdebug}
  Writeln ('Starting internal post.');
 {$endif}
  if FRecordPos = -1 then
    RecPos := 0
  else begin
    if State = dsEdit then RecPos := Integer(FIndexList[FRecordPos])
    else RecPos := FileSize(FDataFile);
  end;
  Seek(FDataFile, RecPos);
 {$ifdef dsdebug}
  Writeln ('Writing record to disk.');
 {$endif}
  BlockWrite(FDataFile, PDDGData(ActiveBuffer)^, 1);
  if State <> dsEdit then
  begin
    if FRecordPos = -1 then InsPos := 0
    else InsPos := FRecordPos;
    FIndexList.Insert(InsPos, Pointer(RecPos));
  end;
 {$ifdef dsdebug}
  Writeln ('Writing index to disk.');
 {$endif}
  FIndexList.SaveToFile(FIdxName);
end;

function TDDGDataSet.IsCursorOpen: Boolean;
begin
  Result := FileRec(FDataFile).Mode <> 0;
end;

function TDDGDataSet.GetRecordCount: Integer;
begin
  Result := FIndexList.Count;
end;

function TDDGDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FRecordPos + 1;
end;

procedure TDDGDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= FIndexList.Count-1) then
  begin
    FRecordPos := Value - 1;
    Resync([]);
  end;
end;

procedure TDDGDataSet.SetTableName(const Value: string);
begin
  CheckInactive;
  FTableName := Value;
  if ExtractFileExt(FTableName) = '' then
    FTableName := FTableName + feDDGTable;
  FIdxName := ChangeFileExt(FTableName, feDDGIndex);
end;

function TDDGDataSet.GetDataFileSize: Integer;
begin
  Result := FileSize(FDataFile);
end;

procedure TDDGDataSet.EmptyTable;
var
  HFile: THandle;
begin
  Close;

  DeleteFile(FTableName);
  HFile := FileCreate(FTableName);
  FileClose(HFile);

  DeleteFile(FIdxName);
  HFile := FileCreate(FIdxName);
  FileClose(HFile);

  Open;
end;

end.
  $Log: ddg_ds.pp,v $
  Revision 1.6  2005/02/14 17:13:12  peter
    * truncate log

}
