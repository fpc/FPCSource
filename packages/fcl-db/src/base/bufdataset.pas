{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Joost van der Sluis, member of the
    Free Pascal development team

    BufDataset implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit BufDataset;

{$mode objfpc}
{$h+}

interface

uses Classes,Sysutils,db,bufdataset_parser;

type
  TBufDataset = Class;

  TResolverErrorEvent = procedure(Sender: TObject; DataSet: TBufDataset; E: EUpdateError;
    UpdateKind: TUpdateKind; var Response: TResolverResponse) of object;

  { TBufBlobStream }

  PBlobBuffer = ^TBlobBuffer;
  TBlobBuffer = record
    FieldNo : integer;
    OrgBufID: integer;
    Buffer  : pointer;
    Size    : ptrint;
  end;

   TBufBlobStream = class(TStream)
  private
    FBlobBuffer : PBlobBuffer;
    FPosition   : ptrint;
    FDataset    : TBufDataset;
  protected
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
  end;

  { TBufDataset }

  PBufRecLinkItem = ^TBufRecLinkItem;
  TBufRecLinkItem = record
    prior   : PBufRecLinkItem;
    next    : PBufRecLinkItem;
  end;

  PBufBookmark = ^TBufBookmark;
  TBufBookmark = record
    BookmarkData : PBufRecLinkItem;
    BookmarkInt  : integer;
    BookmarkFlag : TBookmarkFlag;
  end;

  TRecUpdateBuffer = record
    UpdateKind         : TUpdateKind;
{  BookMarkData:
     - Is -1 if the update has canceled out. For example: a appended record has been deleted again
     - If UpdateKind is ukInsert it contains a bookmark to the new created record
     - If UpdateKind is ukModify it contains a bookmark to the record with the new data
     - If UpdateKind is ukDelete it contains a bookmark to the deleted record (ie: the record is still there)
}
    BookmarkData       : TBufBookmark;
{  NextBookMarkData:
     - If UpdateKind is ukDelete it contains a bookmark to the record just after the deleted record
}
    NextBookmarkData   : TBufBookmark;
{  OldValuesBuffer:
     - If UpdateKind is ukModify it contains a record-buffer which contains the old data
     - If UpdateKind is ukDelete it contains a record-buffer with the data of the deleted record
}
    OldValuesBuffer    : pchar;
  end;

  PBufBlobField = ^TBufBlobField;
  TBufBlobField = record
    ConnBlobBuffer : array[0..11] of byte; // It's here where the db-specific data is stored
    BlobBuffer     : PBlobBuffer;
  end;

  TCompareFunc = function(subValue, aValue: pointer; options: TLocateOptions): int64;
  TRecordsUpdateBuffer = array of TRecUpdateBuffer;

  TDBCompareRec = record
                   Comparefunc : TCompareFunc;
                   Off1,Off2   : PtrInt;
                   Options     : TLocateOptions;
                   Desc        : Boolean;
                  end;
  TDBCompareStruct = array of TDBCompareRec;

  { TBufIndex }

  TBufIndex = class(TObject)
  private
    FDataset : TBufDataset;
  protected
    function GetBookmarkSize: integer; virtual; abstract;
    function GetCurrentBuffer: Pointer; virtual; abstract;
    function GetCurrentRecord: PChar; virtual; abstract;
    function GetIsInitialized: boolean; virtual; abstract;
    function GetSpareBuffer: PChar; virtual; abstract;
    function GetSpareRecord: PChar; virtual; abstract;
  public
    DBCompareStruct : TDBCompareStruct;
    Name            : String;
    Fields          : TField;
    FieldsName      : String;
    CaseinsFields   : String;
    DescFields      : String;
    Options         : TIndexOptions;
    IndNr           : integer;
    constructor Create(const ADataset : TBufDataset); virtual;
    function ScrollBackward : TGetResult; virtual; abstract;
    function ScrollForward : TGetResult;  virtual; abstract;
    function GetCurrent : TGetResult;  virtual; abstract;
    function ScrollFirst : TGetResult;  virtual; abstract;
    procedure ScrollLast; virtual; abstract;

    procedure SetToFirstRecord; virtual; abstract;
    procedure SetToLastRecord; virtual; abstract;

    procedure StoreCurrentRecord;  virtual; abstract;
    procedure RestoreCurrentRecord;  virtual; abstract;

    function CanScrollForward : Boolean;  virtual; abstract;
    procedure DoScrollForward;  virtual; abstract;

    procedure StoreCurrentRecIntoBookmark(const ABookmark: PBufBookmark);  virtual; abstract;
    procedure StoreSpareRecIntoBookmark(const ABookmark: PBufBookmark);  virtual; abstract;
    procedure GotoBookmark(const ABookmark : PBufBookmark); virtual; abstract;
    function BookmarkValid(const ABookmark: PBufBookmark): boolean; virtual;

    procedure InitialiseIndex; virtual; abstract;

    procedure InitialiseSpareRecord(const ASpareRecord : PChar); virtual; abstract;
    procedure ReleaseSpareRecord; virtual; abstract;

    procedure BeginUpdate; virtual; abstract;
    // Adds a record to the end of the index as the new last record (spare record)
    // Normally only used in GetNextPacket
    procedure AddRecord(Const ARecord : PChar); virtual; abstract;
    // Inserts a record before the current record, or if the record is sorted,
    // insert it to the proper position
    procedure InsertRecordBeforeCurrentRecord(Const ARecord : PChar); virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    
    procedure RemoveRecordFromIndex(const ABookmark : TBufBookmark); virtual; abstract;
    
    function CompareBookmarks(const ABookmark1, ABookmark2 : PBufBookmark) : boolean; virtual;
    Function GetRecNo(const ABookmark : PBufBookmark) : integer; virtual; abstract;


    property SpareRecord : PChar read GetSpareRecord;
    property SpareBuffer : PChar read GetSpareBuffer;
    property CurrentRecord : PChar read GetCurrentRecord;
    property CurrentBuffer : Pointer read GetCurrentBuffer;
    property IsInitialized : boolean read GetIsInitialized;
    property BookmarkSize : integer read GetBookmarkSize;
  end;
  
  TDataPacketFormat = (dfBinary,dfXML,dfXMLUTF8,dfAny);

  { TDoubleLinkedBufIndex }

  TDoubleLinkedBufIndex = class(TBufIndex)
  private
    FCursOnFirstRec : boolean;

    FStoredRecBuf  : PBufRecLinkItem;
    FCurrentRecBuf  : PBufRecLinkItem;
  protected
    function GetBookmarkSize: integer; override;
    function GetCurrentBuffer: Pointer; override;
    function GetCurrentRecord: PChar; override;
    function GetIsInitialized: boolean; override;
    function GetSpareBuffer: PChar; override;
    function GetSpareRecord: PChar; override;
  public
    FLastRecBuf     : PBufRecLinkItem;
    FFirstRecBuf    : PBufRecLinkItem;
    FNeedScroll     : Boolean;
    function ScrollBackward : TGetResult; override;
    function ScrollForward : TGetResult; override;
    function GetCurrent : TGetResult; override;
    function ScrollFirst : TGetResult; override;
    procedure ScrollLast; override;

    procedure SetToFirstRecord; override;
    procedure SetToLastRecord; override;

    procedure StoreCurrentRecord; override;
    procedure RestoreCurrentRecord; override;

    function CanScrollForward : Boolean; override;
    procedure DoScrollForward; override;

    procedure StoreCurrentRecIntoBookmark(const ABookmark: PBufBookmark); override;
    procedure StoreSpareRecIntoBookmark(const ABookmark: PBufBookmark); override;
    procedure GotoBookmark(const ABookmark : PBufBookmark); override;

    procedure InitialiseIndex; override;

    procedure InitialiseSpareRecord(const ASpareRecord : PChar); override;
    procedure ReleaseSpareRecord; override;

    procedure RemoveRecordFromIndex(const ABookmark : TBufBookmark); override;
    Function GetRecNo(const ABookmark : PBufBookmark) : integer; override;

    procedure BeginUpdate; override;
    procedure AddRecord(Const ARecord : PChar); override;
    procedure InsertRecordBeforeCurrentRecord(Const ARecord : PChar); override;
    procedure EndUpdate; override;
  end;

  { TArrayBufIndex }

  TArrayBufIndex = class(TBufIndex)
  private
    FStoredRecBuf  : integer;

    FInitialBuffers,
    FGrowBuffer     : integer;
    Function GetRecordFromBookmark(ABookmark: TBufBookmark) : integer;
  protected
    function GetBookmarkSize: integer; override;
    function GetCurrentBuffer: Pointer; override;
    function GetCurrentRecord: PChar; override;
    function GetIsInitialized: boolean; override;
    function GetSpareBuffer: PChar; override;
    function GetSpareRecord: PChar; override;
  public
    FCurrentRecInd  : integer;
    FRecordArray    : array of Pointer;
    FLastRecInd     : integer;
    FNeedScroll     : Boolean;
    constructor Create(const ADataset: TBufDataset); override;
    function ScrollBackward : TGetResult; override;
    function ScrollForward : TGetResult; override;
    function GetCurrent : TGetResult; override;
    function ScrollFirst : TGetResult; override;
    procedure ScrollLast; override;

    procedure SetToFirstRecord; override;
    procedure SetToLastRecord; override;

    procedure StoreCurrentRecord; override;
    procedure RestoreCurrentRecord; override;

    function CanScrollForward : Boolean; override;
    procedure DoScrollForward; override;

    procedure StoreCurrentRecIntoBookmark(const ABookmark: PBufBookmark); override;
    procedure StoreSpareRecIntoBookmark(const ABookmark: PBufBookmark); override;
    procedure GotoBookmark(const ABookmark : PBufBookmark); override;

    procedure InitialiseIndex; override;

    procedure InitialiseSpareRecord(const ASpareRecord : PChar); override;
    procedure ReleaseSpareRecord; override;

    Function GetRecNo(const ABookmark : PBufBookmark) : integer; override;
    procedure RemoveRecordFromIndex(const ABookmark : TBufBookmark); override;
    procedure InsertRecordBeforeCurrentRecord(Const ARecord : PChar); override;

    procedure BeginUpdate; override;
    procedure AddRecord(Const ARecord : PChar); override;
    procedure EndUpdate; override;
  end;


  { TBufDatasetReader }

type
  TRowStateValue = (rsvOriginal, rsvDeleted, rsvInserted, rsvUpdated, rsvDetailUpdates);
  TRowState = set of TRowStateValue;

type

  { TDataPacketReader }

  TDatapacketReaderClass = class of TDatapacketReader;
  TDataPacketReader = class(TObject)
    FStream : TStream;
  protected
    class function RowStateToByte(const ARowState : TRowState) : byte;
    class function ByteToRowState(const AByte : Byte) : TRowState;
  public
    constructor create(AStream : TStream); virtual;
    // Load a dataset from stream:
    // Load the field-definitions from a stream.
    procedure LoadFieldDefs(AFieldDefs : TFieldDefs); virtual; abstract;
    // Is called before the records are loaded
    procedure InitLoadRecords; virtual; abstract;
    // Return the RowState of the current record, and the order of the update
    function GetRecordRowState(out AUpdOrder : Integer) : TRowState; virtual; abstract;
    // Returns if there is at least one more record available in the stream
    function GetCurrentRecord : boolean; virtual; abstract;
    // Store a record from stream in the current record-buffer
    procedure RestoreRecord(ADataset : TBufDataset); virtual; abstract;
    // Move the stream to the next record
    procedure GotoNextRecord; virtual; abstract;

    // Store a dataset to stream:
    // Save the field-definitions to a stream.
    procedure StoreFieldDefs(AFieldDefs : TFieldDefs); virtual; abstract;
    // Save a record from the current record-buffer to the stream
    procedure StoreRecord(ADataset : TBufDataset; ARowState : TRowState; AUpdOrder : integer = 0); virtual; abstract;
    // Is called after all records are stored
    procedure FinalizeStoreRecords; virtual; abstract;
    // Checks if the provided stream is of the right format for this class
    class function RecognizeStream(AStream : TStream) : boolean; virtual; abstract;
    property Stream: TStream read FStream;
  end;

  { TFpcBinaryDatapacketReader }

  TFpcBinaryDatapacketReader = class(TDataPacketReader)
  public
    procedure LoadFieldDefs(AFieldDefs : TFieldDefs); override;
    procedure StoreFieldDefs(AFieldDefs : TFieldDefs); override;
    function GetRecordRowState(out AUpdOrder : Integer) : TRowState; override;
    procedure FinalizeStoreRecords; override;
    function GetCurrentRecord : boolean; override;
    procedure GotoNextRecord; override;
    procedure InitLoadRecords; override;
    procedure RestoreRecord(ADataset : TBufDataset); override;
    procedure StoreRecord(ADataset : TBufDataset; ARowState : TRowState; AUpdOrder : integer = 0); override;
    class function RecognizeStream(AStream : TStream) : boolean; override;
  end;

  TBufDataset = class(TDBDataSet)
  private
    FFileName: string;
    FFileStream     : TFileStream;
    FDatasetReader  : TDataPacketReader;
    FIndexes        : array of TBufIndex;
    FMaxIndexesCount: integer;

    FIndexesCount   : integer;
    FCurrentIndex   : TBufIndex;

    FFilterBuffer   : pchar;
    FBRecordCount   : integer;

    FPacketRecords  : integer;
    FRecordSize     : Integer;
    FNullmaskSize   : byte;
    FOpen           : Boolean;
    FUpdateBuffer   : TRecordsUpdateBuffer;
    FCurrentUpdateBuffer : integer;
    
    FIndexDefs      : TIndexDefs;

    FParser         : TBufDatasetParser;

    FFieldBufPositions : array of longint;

    FAllPacketsFetched : boolean;
    FOnUpdateError  : TResolverErrorEvent;

    FBlobBuffers      : array of PBlobBuffer;
    FUpdateBlobBuffers: array of PBlobBuffer;
    
    procedure FetchAll;
    procedure BuildIndex(var AIndex : TBufIndex);
    function GetIndexDefs : TIndexDefs;
    function  GetCurrentBuffer: PChar;
    procedure CalcRecordSize;
    function GetIndexFieldNames: String;
    function GetIndexName: String;
    function LoadBuffer(Buffer : PChar): TGetResult;
    function GetFieldSize(FieldDef : TFieldDef) : longint;
    function GetRecordUpdateBuffer(const ABookmark : TBufBookmark; IncludePrior : boolean = false; AFindNext : boolean = false) : boolean;
    function GetRecordUpdateBufferCached(const ABookmark : TBufBookmark; IncludePrior : boolean = false) : boolean;
    function GetActiveRecordUpdateBuffer : boolean;
    procedure ProcessFieldCompareStruct(AField: TField; var ACompareRec : TDBCompareRec);
    procedure SetIndexFieldNames(const AValue: String);
    procedure SetIndexName(AValue: String);
    procedure SetMaxIndexesCount(const AValue: Integer);
    procedure SetPacketRecords(aValue : integer);
    function  IntAllocRecordBuffer: PChar;
    procedure DoFilterRecord(var Acceptable: Boolean);
    procedure ParseFilter(const AFilter: string);
    procedure IntLoadFielddefsFromFile;
    procedure IntLoadRecordsFromFile;
  protected
    procedure UpdateIndexDefs; override;
    function GetNewBlobBuffer : PBlobBuffer;
    function GetNewWriteBlobBuffer : PBlobBuffer;
    procedure FreeBlobBuffer(var ABlobBuffer: PBlobBuffer);
    procedure SetRecNo(Value: Longint); override;
    function  GetRecNo: Longint; override;
    function GetChangeCount: integer; virtual;
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function  GetCanModify: Boolean; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure DoBeforeClose; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function getnextpacket : integer;
    function GetRecordSize: Word; override;
    procedure InternalAddIndex(const AName, AFields : string; AOptions : TIndexOptions; const ADescFields: string;
      const ACaseInsFields: string); virtual;
    procedure InternalPost; override;
    procedure InternalCancel; Override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function IsCursorOpen: Boolean; override;
    function  GetRecordCount: Longint; override;
    procedure ApplyRecUpdate(UpdateKind : TUpdateKind); virtual;
    procedure SetOnUpdateError(const aValue: TResolverErrorEvent);
    procedure SetFilterText(const Value: String); override; {virtual;}
    procedure SetFiltered(Value: Boolean); override; {virtual;}
  {abstracts, must be overidden by descendents}
    function Fetch : boolean; virtual;
    function LoadField(FieldDef : TFieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; virtual;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField); virtual; abstract;
    function IsReadFromPacket : Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    function GetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ApplyUpdates; virtual; overload;
    procedure ApplyUpdates(MaxErrors: Integer); virtual; overload;
    procedure CancelUpdates; virtual;
    destructor Destroy; override;
    function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean; override;
    function UpdateStatus: TUpdateStatus; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure AddIndex(const AName, AFields : string; AOptions : TIndexOptions; const ADescFields: string = '';
      const ACaseInsFields: string = ''); virtual;

    procedure SetDatasetPacket(AReader : TDataPacketReader);
    procedure GetDatasetPacket(AWriter : TDataPacketReader);
    procedure LoadFromStream(AStream : TStream; Format: TDataPacketFormat = dfAny);
    procedure SaveToStream(AStream : TStream; Format: TDataPacketFormat = dfBinary);
    procedure LoadFromFile(AFileName: string = ''; Format: TDataPacketFormat = dfAny);
    procedure SaveToFile(AFileName: string = ''; Format: TDataPacketFormat = dfBinary);
    procedure CreateDataset;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; override;

    property ChangeCount : Integer read GetChangeCount;
    property MaxIndexesCount : Integer read FMaxIndexesCount write SetMaxIndexesCount;
  published
    property FileName : string read FFileName write FFileName;
    property PacketRecords : Integer read FPacketRecords write SetPacketRecords default 10;
    property OnUpdateError: TResolverErrorEvent read FOnUpdateError write SetOnUpdateError;
    property IndexDefs : TIndexDefs read GetIndexDefs;
    property IndexName : String read GetIndexName write SetIndexName;
    property IndexFieldNames : String read GetIndexFieldNames write SetIndexFieldNames;
  end;

procedure RegisterDatapacketReader(ADatapacketReaderClass : TDatapacketReaderClass; AFormat : TDataPacketFormat);

implementation

uses variants, dbconst;

Type TDatapacketReaderRegistration = record
                                       ReaderClass : TDatapacketReaderClass;
                                       Format      : TDataPacketFormat;
                                     end;

var RegisteredDatapacketReaders : Array of TDatapacketReaderRegistration;

procedure RegisterDatapacketReader(ADatapacketReaderClass : TDatapacketReaderClass; AFormat : TDataPacketFormat);
begin
  setlength(RegisteredDatapacketReaders,length(RegisteredDatapacketReaders)+1);
  with RegisteredDatapacketReaders[length(RegisteredDatapacketReaders)-1] do
    begin
    Readerclass := ADatapacketReaderClass;
    Format      := AFormat;
    end;
end;

function GetRegisterDatapacketReader(AStream : TStream; AFormat : TDataPacketFormat; var ADataReaderClass : TDatapacketReaderRegistration) : boolean;
var i : integer;
begin
  Result := False;
  for i := 0 to length(RegisteredDatapacketReaders)-1 do if ((AFormat=dfAny) or (AFormat=RegisteredDatapacketReaders[i].Format)) then
    begin
    if (AStream=nil) or (RegisteredDatapacketReaders[i].ReaderClass.RecognizeStream(AStream)) then
      begin
      ADataReaderClass := RegisteredDatapacketReaders[i];
      Result := True;
      if (AStream <> nil) then AStream.Seek(0,soFromBeginning);
      break;
      end;
    AStream.Seek(0,soFromBeginning);
    end;
end;

function DBCompareText(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  if loCaseInsensitive in options then
    Result := AnsiCompareText(pchar(subValue),pchar(aValue))
  else
    Result := AnsiCompareStr(pchar(subValue),pchar(aValue));
end;

function DBCompareByte(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PByte(subValue)^-PByte(aValue)^;
end;

function DBCompareSmallInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PSmallInt(subValue)^-PSmallInt(aValue)^;
end;

function DBCompareInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PInteger(subValue)^-PInteger(aValue)^;
end;

function DBCompareLargeInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  // A simple subtraction doesn't work, since it could be that the result
  // doesn't fit into a LargeInt
  if PLargeInt(subValue)^ < PLargeInt(aValue)^ then
    result := -1
  else if PLargeInt(subValue)^  > PLargeInt(aValue)^ then
    result := 1
  else
    result := 0;
end;

function DBCompareWord(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PWord(subValue)^-PWord(aValue)^;
end;

function DBCompareQWord(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  // A simple subtraction doesn't work, since it could be that the result
  // doesn't fit into a LargeInt
  if PQWord(subValue)^ < PQWord(aValue)^ then
    result := -1
  else if PQWord(subValue)^  > PQWord(aValue)^ then
    result := 1
  else
    result := 0;
end;

function DBCompareDouble(subValue, aValue: pointer; options: TLocateOptions): LargeInt;
begin
  // A simple subtraction doesn't work, since it could be that the result
  // doesn't fit into a LargeInt
  if PDouble(subValue)^ < PDouble(aValue)^ then
    result := -1
  else if PDouble(subValue)^  > PDouble(aValue)^ then
    result := 1
  else
    result := 0;
end;

function IndexCompareRecords(Rec1,Rec2 : pointer; ADBCompareRecs : TDBCompareStruct) : LargeInt;
var IndexFieldNr : Integer;
begin
  for IndexFieldNr:=0 to length(ADBCompareRecs)-1 do with ADBCompareRecs[IndexFieldNr] do
    begin
    Result := Comparefunc(Rec1+Off1,Rec2+Off2,Options);
    if Result <> 0 then
      begin
      if Desc then
        Result := -Result;
      break;
      end;
    end;
end;

procedure unSetFieldIsNull(NullMask : pbyte;x : longint); //inline;
begin
  NullMask[x div 8] := (NullMask[x div 8]) and not (1 shl (x mod 8));
end;

procedure SetFieldIsNull(NullMask : pbyte;x : longint); //inline;
begin
  NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));
end;

function GetFieldIsNull(NullMask : pbyte;x : longint) : boolean; //inline;
begin
  result := ord(NullMask[x div 8]) and (1 shl (x mod 8)) > 0
end;

{ ---------------------------------------------------------------------
    TBufDataSet
  ---------------------------------------------------------------------}

constructor TBufDataset.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FMaxIndexesCount:=2;
  FIndexesCount:=0;
  InternalAddIndex('DEFAULT_ORDER','',[],'','');
  FCurrentIndex:=FIndexes[0];
  InternalAddIndex('','',[],'','');

  FIndexDefs := TIndexDefs.Create(Self);

  SetLength(FUpdateBuffer,0);
  SetLength(FBlobBuffers,0);
  SetLength(FUpdateBlobBuffers,0);
  BookmarkSize := FCurrentIndex.BookmarkSize;
  FParser := nil;
  FPacketRecords := 10;
end;

procedure TBufDataset.SetPacketRecords(aValue : integer);
begin
  if (aValue = -1) or (aValue > 0) then FPacketRecords := aValue
    else DatabaseError(SInvPacketRecordsValue);
end;

destructor TBufDataset.Destroy;

Var
  I : Integer;
begin
  if Active then Close;
  SetLength(FUpdateBuffer,0);
  SetLength(FBlobBuffers,0);
  SetLength(FUpdateBlobBuffers,0);
  For I:=0 to Length(FIndexes)-1 do
    FreeAndNil(Findexes[I]);
  SetLength(FIndexes,0);
  FreeAndNil(FIndexDefs);
  inherited destroy;
end;

procedure TBufDataset.FetchAll;
begin
  repeat
  until (getnextpacket < FPacketRecords) or (FPacketRecords = -1);
end;

procedure TBufDataset.BuildIndex(var AIndex: TBufIndex);

var PCurRecLinkItem : PBufRecLinkItem;
    p,l,q           : PBufRecLinkItem;
    i,k,psize,qsize : integer;
    MergeAmount     : integer;
    PlaceQRec       : boolean;

    IndexFields     : TList;
    DescIndexFields : TList;
    CInsIndexFields : TList;
    FieldsAmount    : Integer;
    FieldNr         : integer;
    AField          : TField;

  procedure PlaceNewRec(var e: PBufRecLinkItem; var esize: integer);
  begin
    if (AIndex as TDoubleLinkedBufIndex).FFirstRecBuf=nil then
     begin
     (AIndex as TDoubleLinkedBufIndex).FFirstRecBuf:=e;
     e[(AIndex as TDoubleLinkedBufIndex).IndNr].prior:=nil;
     l:=e;
     end
   else
     begin
     l[(AIndex as TDoubleLinkedBufIndex).IndNr].next:=e;
     e[(AIndex as TDoubleLinkedBufIndex).IndNr].prior:=l;
     l:=e;
     end;
   e := e[(AIndex as TDoubleLinkedBufIndex).IndNr].next;
   dec(esize);
  end;

begin
  // Build the DBCompareStructure
  with AIndex do
    begin
    IndexFields := TList.Create;
    DescIndexFields := TList.Create;
    CInsIndexFields := TList.Create;
    try
      GetFieldList(IndexFields,FieldsName);
      FieldsAmount:=IndexFields.Count;
      GetFieldList(DescIndexFields,DescFields);
      GetFieldList(CInsIndexFields,CaseinsFields);
      if FieldsAmount=0 then
        DatabaseError(SNoIndexFieldNameGiven);
      SetLength(DBCompareStruct,FieldsAmount);
      for FieldNr:=0 to FieldsAmount-1 do
        begin
        AField := TField(IndexFields[FieldNr]);
        ProcessFieldCompareStruct(AField,DBCompareStruct[FieldNr]);

        DBCompareStruct[FieldNr].Desc := (DescIndexFields.IndexOf(AField)>-1);
        if (CInsIndexFields.IndexOf(AField)>-1) then
          DBCompareStruct[FieldNr].Options := [loCaseInsensitive]
        else
          DBCompareStruct[FieldNr].Options := [];

        end;
    finally
      CInsIndexFields.Free;
      DescIndexFields.Free;
      IndexFields.Free;
    end;
    end;

// This simply copies the index...
  PCurRecLinkItem:=(FIndexes[0] as TDoubleLinkedBufIndex).FFirstRecBuf;
  PCurRecLinkItem[(AIndex as TDoubleLinkedBufIndex).IndNr].next := PCurRecLinkItem[0].next;
  PCurRecLinkItem[(AIndex as TDoubleLinkedBufIndex).IndNr].prior := PCurRecLinkItem[0].prior;

  if PCurRecLinkItem <> (FIndexes[0] as TDoubleLinkedBufIndex).FLastRecBuf then
    begin
    while PCurRecLinkItem^.next<>(FIndexes[0] as TDoubleLinkedBufIndex).FLastRecBuf do
      begin
      PCurRecLinkItem:=PCurRecLinkItem^.next;

      PCurRecLinkItem[(AIndex as TDoubleLinkedBufIndex).IndNr].next := PCurRecLinkItem[0].next;
      PCurRecLinkItem[(AIndex as TDoubleLinkedBufIndex).IndNr].prior := PCurRecLinkItem[0].prior;
      end;
    end;

// Set FirstRecBuf and FCurrentRecBuf
  (AIndex as TDoubleLinkedBufIndex).FFirstRecBuf:=(FIndexes[0] as TDoubleLinkedBufIndex).FFirstRecBuf;
  (FCurrentIndex as TDoubleLinkedBufIndex).FCurrentRecBuf:=(AIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
// Link in the FLastRecBuf that belongs to this index
  PCurRecLinkItem[(AIndex as TDoubleLinkedBufIndex).IndNr].next:=(AIndex as TDoubleLinkedBufIndex).FLastRecBuf;
  (AIndex as TDoubleLinkedBufIndex).FLastRecBuf[(AIndex as TDoubleLinkedBufIndex).IndNr].prior:=PCurRecLinkItem;

// Mergesort. Used the algorithm as described here by Simon Tatham
// http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
// The comments in the code are from this website.

// In each pass, we are merging lists of size K into lists of size 2K.
// (Initially K equals 1.)
  k:=1;

  repeat

// So we start by pointing a temporary pointer p at the head of the list,
// and also preparing an empty list L which we will add elements to the end
// of as we finish dealing with them.

  p := (AIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
  (AIndex as TDoubleLinkedBufIndex).ffirstRecBuf := nil;
  q := p;
  MergeAmount := 0;

// Then:
//    * If p is null, terminate this pass.
  while p <> (AIndex as TDoubleLinkedBufIndex).FLastRecBuf do
    begin

//    * Otherwise, there is at least one element in the next pair of length-K
//      lists, so increment the number of merges performed in this pass.

    inc(MergeAmount);

//    * Point another temporary pointer, q, at the same place as p. Step q along
//      the list by K places, or until the end of the list, whichever comes
//      first. Let psize be the number of elements you managed to step q past.

    i:=0;
    while (i<k) and (q<>(AIndex as TDoubleLinkedBufIndex).FLastRecBuf) do
      begin
      inc(i);
      q := q[(AIndex as TDoubleLinkedBufIndex).IndNr].next;
      end;
    psize :=i;

//    * Let qsize equal K. Now we need to merge a list starting at p, of length
//      psize, with a list starting at q of length at most qsize.

    qsize:=k;

//    * So, as long as either the p-list is non-empty (psize > 0) or the q-list
//      is non-empty (qsize > 0 and q points to something non-null):

    while (psize>0) or ((qsize>0) and (q <> (AIndex as TDoubleLinkedBufIndex).FLastRecBuf)) do
      begin
//          o Choose which list to take the next element from. If either list
//            is empty, we must choose from the other one. (By assumption, at
//            least one is non-empty at this point.) If both lists are
//            non-empty, compare the first element of each and choose the lower
//            one. If the first elements compare equal, choose from the p-list.
//            (This ensures that any two elements which compare equal are never
//            swapped, so stability is guaranteed.)
      if (psize=0)  then
        PlaceQRec := true
      else if (qsize=0) or (q = (AIndex as TDoubleLinkedBufIndex).FLastRecBuf) then
        PlaceQRec := False
      else if IndexCompareRecords(p,q,aindex.DBCompareStruct) <= 0 then
        PlaceQRec := False
      else
        PlaceQRec := True;
        
//          o Remove that element, e, from the start of its list, by advancing
//            p or q to the next element along, and decrementing psize or qsize.
//          o Add e to the end of the list L we are building up.
      if PlaceQRec then
        PlaceNewRec(q,qsize)
      else
        PlaceNewRec(p,psize);
      end;
//    * Now we have advanced p until it is where q started out, and we have
//      advanced q until it is pointing at the next pair of length-K lists to
//      merge. So set p to the value of q, and go back to the start of this loop.
    p:=q;
    end;

// As soon as a pass like this is performed and only needs to do one merge, the
// algorithm terminates, and the output list L is sorted. Otherwise, double the
// value of K, and go back to the beginning.

  l[(AIndex as TDoubleLinkedBufIndex).IndNr].next:=(AIndex as TDoubleLinkedBufIndex).FLastRecBuf;

  k:=k*2;

  until MergeAmount = 1;
  (AIndex as TDoubleLinkedBufIndex).FLastRecBuf[(AIndex as TDoubleLinkedBufIndex).IndNr].next:=(AIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
  (AIndex as TDoubleLinkedBufIndex).FLastRecBuf[(AIndex as TDoubleLinkedBufIndex).IndNr].prior:=l;
end;

function TBufDataset.GetIndexDefs : TIndexDefs;

begin
  Result := FIndexDefs;
end;

procedure TBufDataset.UpdateIndexDefs;
var i : integer;
begin
  FIndexDefs.Clear;
  for i := 0 to high(FIndexes) do with FIndexDefs.AddIndexDef do
    begin
    Name := FIndexes[i].Name;
    Fields := FIndexes[i].FieldsName;
    DescFields:= FIndexes[i].DescFields;
    CaseInsFields:=FIndexes[i].CaseinsFields;
    Options:=FIndexes[i].Options;
    end;
end;

Function TBufDataset.GetCanModify: Boolean;
begin
  Result:= True;
end;

function TBufDataset.intAllocRecordBuffer: PChar;
begin
  // Note: Only the internal buffers of TDataset provide bookmark information
  result := AllocMem(FRecordsize+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
end;

function TBufDataset.AllocRecordBuffer: PChar;
begin
  result := AllocMem(FRecordsize + BookmarkSize + CalcfieldsSize);
// The records are initialised, or else the fields of an empty, just-opened dataset
// are not null
  InitRecord(result);
end;

procedure TBufDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  ReAllocMem(Buffer,0);
end;

procedure TBufDataset.ClearCalcFields(Buffer: PChar);
begin
  if CalcFieldsSize > 0 then
    FillByte((Buffer+RecordSize)^,CalcFieldsSize,0);
end;

procedure TBufDataset.InternalOpen;

var IndexNr : integer;

begin
  if not Assigned(FDatasetReader) and (FileName<>'') then
    begin
    FFileStream := TFileStream.Create(FileName,fmOpenRead);
    FDatasetReader := TFpcBinaryDatapacketReader.Create(FFileStream);
    end;
  if assigned(FDatasetReader) then IntLoadFielddefsFromFile;
  CalcRecordSize;

  FBRecordcount := 0;

  for IndexNr:=0 to FIndexesCount-1 do with FIndexes[IndexNr] do
    InitialiseSpareRecord(IntAllocRecordBuffer);

  FAllPacketsFetched := False;

  FOpen:=True;

  // parse filter expression
  try
    ParseFilter(Filter);
  except
    // oops, a problem with parsing, clear filter for now
    on E: Exception do Filter := EmptyStr;
  end;

  if assigned(FDatasetReader) then IntLoadRecordsFromFile;
end;

procedure TBufDataset.InternalClose;

var r  : integer;
    iGetResult : TGetResult;
    pc : pchar;

begin
  FOpen:=False;
  with FIndexes[0] do if IsInitialized then
    begin
    iGetResult:=ScrollFirst;
    while iGetResult = grOK do
      begin
      pc := pointer(CurrentRecord);
      iGetResult:=ScrollForward;
      FreeRecordBuffer(pc);
      end;
    end;

  for r := 0 to FIndexesCount-1 do with FIndexes[r] do if IsInitialized then
    begin
    pc := SpareRecord;
    ReleaseSpareRecord;
    FreeRecordBuffer(pc);
    end;

  if Length(FUpdateBuffer) > 0 then
    begin
    for r := 0 to length(FUpdateBuffer)-1 do with FUpdateBuffer[r] do
      begin
      if assigned(OldValuesBuffer) then
        FreeRecordBuffer(OldValuesBuffer);
      end;
    end;
  SetLength(FUpdateBuffer,0);
  
  for r := 0 to High(FBlobBuffers) do
    FreeBlobBuffer(FBlobBuffers[r]);
  for r := 0 to High(FUpdateBlobBuffers) do
    FreeBlobBuffer(FUpdateBlobBuffers[r]);

  SetLength(FBlobBuffers,0);
  SetLength(FUpdateBlobBuffers,0);

  SetLength(FFieldBufPositions,0);

  if assigned(FParser) then FreeAndNil(FParser);
end;

procedure TBufDataset.InternalFirst;
begin
  with FCurrentIndex do
    begin
// if FCurrentRecBuf = FLastRecBuf then the dataset is just opened and empty
// in which case InternalFirst should do nothing (bug 7211)
    SetToFirstRecord;
    end;
end;

procedure TBufDataset.InternalLast;
begin
  FetchAll;
  with FCurrentIndex do
  SetToLastRecord;
end;

function TDoubleLinkedBufIndex.GetCurrentRecord: PChar;
begin
  Result := pchar(FCurrentRecBuf);
end;

function TDoubleLinkedBufIndex.GetBookmarkSize: integer;
begin
  Result:=sizeof(TBufBookmark);
end;

function TDoubleLinkedBufIndex.GetCurrentBuffer: Pointer;
begin
  Result := pointer(FCurrentRecBuf)+(sizeof(TBufRecLinkItem)*FDataset.MaxIndexesCount);
end;

function TDoubleLinkedBufIndex.GetIsInitialized: boolean;
begin
  Result := (FFirstRecBuf<>nil);
end;

function TDoubleLinkedBufIndex.GetSpareBuffer: PChar;
begin
  Result := pointer(FLastRecBuf)+(sizeof(TBufRecLinkItem)*FDataset.MaxIndexesCount);
end;

function TDoubleLinkedBufIndex.GetSpareRecord: PChar;
begin
  Result := pchar(FLastRecBuf);
end;

constructor TBufIndex.Create(const ADataset: TBufDataset);
begin
  inherited create;
  FDataset := ADataset;
end;

function TBufIndex.BookmarkValid(const ABookmark: PBufBookmark): boolean;
begin
  Result := assigned(ABookmark) and assigned(ABookmark^.BookmarkData);
end;

function TBufIndex.CompareBookmarks(const ABookmark1, ABookmark2: PBufBookmark): boolean;
begin
  result := (ABookmark1^.BookmarkData=ABookmark2^.BookmarkData);
end;

function TDoubleLinkedBufIndex.ScrollBackward: TGetResult;
begin
  if not assigned(FCurrentRecBuf[IndNr].prior) then
    begin
    Result := grBOF;
    end
  else
    begin
    Result := grOK;
    FCurrentRecBuf := FCurrentRecBuf[IndNr].prior;
    end;
end;

function TDoubleLinkedBufIndex.ScrollForward: TGetResult;
begin
  if (FCurrentRecBuf = FLastRecBuf) or // just opened
     (FCurrentRecBuf[IndNr].next = FLastRecBuf) then
    result := grEOF
  else
    begin
    FCurrentRecBuf := FCurrentRecBuf[IndNr].next;
    Result := grOK;
    end;
end;

function TDoubleLinkedBufIndex.GetCurrent: TGetResult;
begin
  if FFirstRecBuf = FLastRecBuf then
    Result := grError
  else
    begin
    Result := grOK;
    if FCurrentRecBuf = FLastRecBuf then
      FCurrentRecBuf:=FLastRecBuf[IndNr].prior;
    end;
end;

function TDoubleLinkedBufIndex.ScrollFirst: TGetResult;
begin
  FCurrentRecBuf:=FFirstRecBuf;
  if (FCurrentRecBuf = FLastRecBuf) then
    result := grEOF
  else
    result := grOK;
end;

procedure TDoubleLinkedBufIndex.ScrollLast;
begin
  FCurrentRecBuf:=FLastRecBuf;
end;

procedure TDoubleLinkedBufIndex.SetToFirstRecord;
begin
  FLastRecBuf[IndNr].next:=FFirstRecBuf;
  FCurrentRecBuf := FLastRecBuf;
end;

procedure TDoubleLinkedBufIndex.SetToLastRecord;
begin
  if FLastRecBuf <> FFirstRecBuf then FCurrentRecBuf := FLastRecBuf;
end;

procedure TDoubleLinkedBufIndex.StoreCurrentRecord;
begin
  FStoredRecBuf:=FCurrentRecBuf;
end;

procedure TDoubleLinkedBufIndex.RestoreCurrentRecord;
begin
  FCurrentRecBuf:=FStoredRecBuf;
end;

procedure TDoubleLinkedBufIndex.DoScrollForward;
begin
  FCurrentRecBuf := FCurrentRecBuf[IndNr].next;
end;

procedure TDoubleLinkedBufIndex.StoreCurrentRecIntoBookmark(const ABookmark: PBufBookmark);
begin
  ABookmark^.BookmarkData:=FCurrentRecBuf;
end;

procedure TDoubleLinkedBufIndex.StoreSpareRecIntoBookmark(
  const ABookmark: PBufBookmark);
begin
  ABookmark^.BookmarkData:=FLastRecBuf;
end;

procedure TDoubleLinkedBufIndex.GotoBookmark(const ABookmark : PBufBookmark);
begin
  FCurrentRecBuf := ABookmark^.BookmarkData;
end;

procedure TDoubleLinkedBufIndex.InitialiseIndex;
begin
// Do nothing
end;

function TDoubleLinkedBufIndex.CanScrollForward: Boolean;
begin
  if (FCurrentRecBuf[IndNr].next = FLastRecBuf) then
    Result := False
  else
    Result := True;
end;

procedure TDoubleLinkedBufIndex.InitialiseSpareRecord(const ASpareRecord : PChar);
begin
  FFirstRecBuf := pointer(ASpareRecord);
  FLastRecBuf := FFirstRecBuf;
  FLastRecBuf[IndNr].next:=FLastRecBuf;
  FCurrentRecBuf := FLastRecBuf;
end;

procedure TDoubleLinkedBufIndex.ReleaseSpareRecord;
begin
  FFirstRecBuf:= nil;
end;

procedure TDoubleLinkedBufIndex.RemoveRecordFromIndex(const ABookmark : TBufBookmark);
var ARecord : PBufRecLinkItem;
begin
  ARecord := ABookmark.BookmarkData;
  if ARecord = FCurrentRecBuf then DoScrollForward;
  if ARecord <> FFirstRecBuf then
    ARecord[IndNr].prior[IndNr].next := ARecord[IndNr].next
  else
    begin
    FFirstRecBuf := ARecord[IndNr].next;
    FLastRecBuf[IndNr].next := FFirstRecBuf;
    end;
  ARecord[IndNr].next[IndNr].prior := ARecord[IndNr].prior;
end;

function TDoubleLinkedBufIndex.GetRecNo(const ABookmark: PBufBookmark): integer;
Var TmpRecBuffer    : PBufRecLinkItem;
    recnr           : integer;
begin
  TmpRecBuffer := FFirstRecBuf;
  recnr := 1;
  while TmpRecBuffer <> ABookmark^.BookmarkData do
    begin
    inc(recnr);
    TmpRecBuffer := TmpRecBuffer^.next;
    end;
  Result := recnr;
end;

procedure TDoubleLinkedBufIndex.BeginUpdate;
begin
  if FCurrentRecBuf = FLastRecBuf then
    FCursOnFirstRec := True
  else
    FCursOnFirstRec := False;
end;

procedure TDoubleLinkedBufIndex.AddRecord(Const ARecord : PChar);
begin
  FLastRecBuf[IndNr].next := pointer(ARecord);
  FLastRecBuf[IndNr].next[IndNr].prior := FLastRecBuf;

  FLastRecBuf := FLastRecBuf[IndNr].next;
end;

procedure TDoubleLinkedBufIndex.InsertRecordBeforeCurrentRecord(const ARecord: PChar);
var ANewRecord : PBufRecLinkItem;
begin
  ANewRecord:=PBufRecLinkItem(ARecord);
  ANewRecord[IndNr].prior:=FCurrentRecBuf[IndNr].prior;
  ANewRecord[IndNr].Next:=FCurrentRecBuf;

  if FCurrentRecBuf=FFirstRecBuf then
    begin
    FFirstRecBuf:=ANewRecord;
    ANewRecord[IndNr].prior:=nil;
    end
  else
    ANewRecord[IndNr].Prior[IndNr].next:=ANewRecord;
  ANewRecord[IndNr].next[IndNr].prior:=ANewRecord;
end;

procedure TDoubleLinkedBufIndex.EndUpdate;
begin
  FLastRecBuf[IndNr].next := FFirstRecBuf;
  if FCursOnFirstRec then FCurrentRecBuf:=FLastRecBuf;
end;

function TBufDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

var Acceptable : Boolean;
    SaveState : TDataSetState;
    ABookMark : PBufBookmark;

begin
  Result := grOK;
  with FCurrentIndex do
    begin
    repeat
    Acceptable := True;
    case GetMode of
      gmPrior : Result := ScrollBackward;
      gmCurrent : Result := GetCurrent;
      gmNext : begin
               if not CanScrollForward and (getnextpacket = 0) then result := grEOF
               else
                 begin
                 result := grOK;
                 DoScrollForward;
                 end;
               end;
    end;

    if Result = grOK then
      begin
      with FCurrentIndex do
        begin
        move(CurrentBuffer^,buffer^,FRecordSize);
        ABookMark:=PBufBookmark(Buffer + FRecordSize);
        ABookmark^.BookmarkFlag:=bfCurrent;
        StoreCurrentRecIntoBookmark(ABookMark);
        end;

      GetCalcFields(Buffer);

      if Filtered then
        begin
        FFilterBuffer := Buffer;
        SaveState := SetTempState(dsFilter);
        DoFilterRecord(Acceptable);
        if (GetMode = gmCurrent) and not Acceptable then
          begin
          Acceptable := True;
          Result := grError;
          end;
        RestoreState(SaveState);
        end;
      end
    else if (Result = grError) and doCheck then
      DatabaseError('No record');
    until Acceptable;
  end;
end;

procedure TBufDataset.DoBeforeClose;
begin
  inherited DoBeforeClose;
  if FFileName<>'' then
    SaveToFile(FFileName);
end;

function TBufDataset.GetActiveRecordUpdateBuffer : boolean;

var ABookmark : TBufBookmark;

begin
  GetBookmarkData(ActiveBuffer,@ABookmark);
  result := GetRecordUpdateBufferCached(ABookmark);
end;

procedure TBufDataset.ProcessFieldCompareStruct(AField: TField; var ACompareRec : TDBCompareRec);
begin
  case AField.DataType of
    ftString : ACompareRec.Comparefunc := @DBCompareText;
    ftSmallint : ACompareRec.Comparefunc := @DBCompareSmallInt;
    ftInteger, ftBCD : ACompareRec.Comparefunc :=
      @DBCompareInt;
    ftWord : ACompareRec.Comparefunc := @DBCompareWord;
    ftBoolean : ACompareRec.Comparefunc := @DBCompareByte;
    ftFloat, ftCurrency : ACompareRec.Comparefunc := @DBCompareDouble;
    ftDateTime, ftDate, ftTime : ACompareRec.Comparefunc :=
      @DBCompareDouble;
    ftLargeint : ACompareRec.Comparefunc := @DBCompareLargeInt;
  else
    DatabaseErrorFmt(SErrIndexBasedOnInvField, [Fieldtypenames[AField.DataType]]);
  end;

  ACompareRec.Off1:=sizeof(TBufRecLinkItem)*FMaxIndexesCount+
    FFieldBufPositions[AField.FieldNo-1];
  ACompareRec.Off2:=ACompareRec.Off1;
end;

procedure TBufDataset.SetIndexFieldNames(const AValue: String);
begin
  if AValue<>'' then
    begin
    FIndexes[1].FieldsName:=AValue;
    FCurrentIndex:=FIndexes[1];
    if active then
      begin
      BuildIndex(FIndexes[1]);
      Resync([rmCenter]);
      end;
    end
  else
    SetIndexName('');
end;

procedure TBufDataset.SetIndexName(AValue: String);
var i : integer;
begin
  if AValue='' then AValue := 'DEFAULT_ORDER';
  for i := 0 to FIndexesCount-1 do
    if SameText(FIndexes[i].Name,AValue) then
      begin
      (FIndexes[i] as TDoubleLinkedBufIndex).FCurrentRecBuf:=(FCurrentIndex as TDoubleLinkedBufIndex).FCurrentRecBuf;
      FCurrentIndex:=FIndexes[i];
      if active then Resync([rmCenter]);
      exit;
      end;
end;

procedure TBufDataset.SetMaxIndexesCount(const AValue: Integer);
begin
  CheckInactive;
  if AValue > 1 then
    FMaxIndexesCount:=AValue
  else
    DatabaseError(SMinIndexes);
end;

procedure TBufDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentIndex.GotoBookmark(PBufBookmark(Buffer+FRecordSize));
end;

procedure TBufDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PBufBookmark(Buffer + FRecordSize)^.BookmarkData := pointer(Data^);
end;

procedure TBufDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PBufBookmark(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TBufDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  pointer(Data^) := PBufBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TBufDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PBufBookmark(Buffer + FRecordSize)^.BookmarkFlag;
end;

procedure TBufDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  // note that ABookMark should be a PBufBookmark. But this way it can also be
  // a pointer to a TBufRecLinkItem
  FCurrentIndex.GotoBookmark(ABookmark);
end;

function TBufDataset.getnextpacket : integer;

var i : integer;
    pb : pchar;

begin
  if FAllPacketsFetched then
    begin
    result := 0;
    exit;
    end;

  FCurrentIndex.BeginUpdate;

  i := 0;
  pb := FIndexes[0].SpareBuffer;
  while ((i < FPacketRecords) or (FPacketRecords = -1)) and (loadbuffer(pb) = grOk) do
    begin
    with FIndexes[0] do
      begin
      AddRecord(IntAllocRecordBuffer);
      pb := SpareBuffer;
      end;
    inc(i);
    end;

  FCurrentIndex.EndUpdate;
  FBRecordCount := FBRecordCount + i;
  result := i;
end;

function TBufDataset.GetFieldSize(FieldDef : TFieldDef) : longint;

begin
  case FieldDef.DataType of
    ftString,
      ftGuid,
      ftFixedChar: result := FieldDef.Size + 1;
    ftFixedWideChar,
      ftWideString:result := (FieldDef.Size + 1)*2;
    ftSmallint,
      ftInteger,
      ftword     : result := sizeof(longint);
    ftBoolean    : result := sizeof(wordbool);
    ftBCD        : result := sizeof(currency);
    ftFloat,
      ftCurrency : result := sizeof(double);
    ftLargeInt   : result := sizeof(largeint);
    ftTime,
      ftDate,
      ftDateTime : result := sizeof(TDateTime);
    ftBlob,
      ftMemo,
      ftGraphic,
      ftFmtMemo,
      ftParadoxOle,
      ftDBaseOle,
      ftTypedBinary,
      ftOraBlob,
      ftOraClob,
      ftWideMemo : result := sizeof(TBufBlobField)
  else Result := 10
  end;
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  result:=Align(result,4);
{$ENDIF}
end;

function TBufDataset.GetRecordUpdateBuffer(const ABookmark : TBufBookmark; IncludePrior : boolean = false; AFindNext : boolean = false): boolean;

var x        : integer;
    StartBuf : integer;

begin
  if AFindNext then
    StartBuf:=FCurrentUpdateBuffer+1
  else
    StartBuf := 0;
  Result := False;
  for x := StartBuf to high(FUpdateBuffer) do
   if FCurrentIndex.CompareBookmarks(@FUpdateBuffer[x].BookmarkData,@ABookmark) or
      (IncludePrior and (FUpdateBuffer[x].UpdateKind=ukDelete) and FCurrentIndex.CompareBookmarks(@FUpdateBuffer[x].NextBookmarkData,@ABookmark)) then
    begin
    FCurrentUpdateBuffer := x;
    Result := True;
    break;
    end;
end;

function TBufDataset.GetRecordUpdateBufferCached(const ABookmark: TBufBookmark;
  IncludePrior: boolean): boolean;
begin
  // if the current update buffer complies, immediately return true
  if (FCurrentUpdateBuffer < length(FUpdateBuffer)) and (
      FCurrentIndex.CompareBookmarks(@FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData,@ABookmark) or
      (IncludePrior and (FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind=ukDelete) and FCurrentIndex.CompareBookmarks(@FUpdateBuffer[FCurrentUpdateBuffer].NextBookmarkData,@ABookmark))) then
    Result := True
  else
    Result := GetRecordUpdateBuffer(ABookmark,IncludePrior);
end;

function TBufDataset.LoadBuffer(Buffer : PChar): TGetResult;

var NullMask        : pbyte;
    x               : longint;
    CreateblobField : boolean;
    BufBlob         : PBufBlobField;

begin
  if not Fetch then
    begin
    Result := grEOF;
    FAllPacketsFetched := True;
    // This code has to be placed elsewhere. At least it should also run when
    // the datapacket is loaded from file
    if FIndexesCount>0 then for x := 1 to FIndexesCount-1 do
      begin
      if not ((x=1) and (FIndexes[1].FieldsName='')) then
        begin
        BuildIndex(FIndexes[x]);
        (FCurrentIndex as TDoubleLinkedBufIndex).FCurrentRecBuf:=(FCurrentIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
        end;
      end;
    Exit;
    end;

  NullMask := pointer(buffer);
  fillchar(Nullmask^,FNullmaskSize,0);
  inc(buffer,FNullmaskSize);

  for x := 0 to FieldDefs.count-1 do
    begin
    if not LoadField(FieldDefs[x],buffer,CreateblobField) then
      SetFieldIsNull(NullMask,x)
    else if CreateblobField then
      begin
      BufBlob := PBufBlobField(Buffer);
      BufBlob^.BlobBuffer := GetNewBlobBuffer;
      LoadBlobIntoBuffer(FieldDefs[x],BufBlob);
      end;
    inc(buffer,GetFieldSize(FieldDefs[x]));
    end;
  Result := grOK;
end;

function TBufDataset.GetCurrentBuffer: PChar;
begin
  if State = dsFilter then Result := FFilterBuffer
  else if state = dsCalcFields then Result := CalcBuffer
  else Result := ActiveBuffer;
end;


function TBufDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  Result := GetFieldData(Field, Buffer);
end;

function TBufDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

var CurrBuff : pchar;

begin
  Result := False;
  if state = dsOldValue then
    begin
    if not GetActiveRecordUpdateBuffer then
      begin
      // There is no old value available
      result := false;
      exit;
      end;
    currbuff := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer;
    end
  else
    begin
    CurrBuff := GetCurrentBuffer;
    if not assigned(CurrBuff) then
      begin
      result := false;
      exit;
      end;
    end;

  If Field.Fieldno > 0 then // If = 0, then calculated field or something similar
    begin
    if GetFieldIsnull(pbyte(CurrBuff),Field.Fieldno-1) then
      begin
      result := false;
      exit;
      end;
    if assigned(buffer) then
      begin
      inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
      Move(CurrBuff^, Buffer^, GetFieldSize(FieldDefs[Field.FieldNo-1]));
      end;
    Result := True;
    end
  else
    begin
    Inc(CurrBuff, GetRecordSize + Field.Offset);
    Result := Boolean(CurrBuff^);
    if result and assigned(Buffer) then
      begin
      inc(CurrBuff);
      Move(CurrBuff^, Buffer^, Field.Datasize);
      end;
    end;
end;

procedure TBufDataset.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
begin
  SetFieldData(Field,Buffer);
end;

procedure TBufDataset.SetFieldData(Field: TField; Buffer: Pointer);

var CurrBuff : pointer;
    NullMask : pbyte;

begin
  if not (state in [dsEdit, dsInsert, dsFilter, dsCalcFields]) then
    begin
    DatabaseErrorFmt(SNotEditing,[Name],self);
    exit;
    end;
  CurrBuff := GetCurrentBuffer;
  If Field.Fieldno > 0 then // If = 0, then calculated field or something
    begin
    NullMask := CurrBuff;

    inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
    if assigned(buffer) then
      begin
      Move(Buffer^, CurrBuff^, GetFieldSize(FieldDefs[Field.FieldNo-1]));
      unSetFieldIsNull(NullMask,Field.FieldNo-1);
      end
    else
      SetFieldIsNull(NullMask,Field.FieldNo-1);
    end
  else
    begin
    Inc(CurrBuff, GetRecordSize + Field.Offset);
    Boolean(CurrBuff^) := Buffer <> nil;
    inc(CurrBuff);
    if assigned(Buffer) then
      Move(Buffer^, CurrBuff^, Field.Datasize);
    end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Ptrint(Field));
end;

procedure TBufDataset.InternalDelete;
var i         : Integer;
    StartInd  : Integer;
    RemRec    : pointer;
    RemRecBookmrk : TBufBookmark;
begin
  InternalSetToRecord(ActiveBuffer);
  // Remove the record from all active indexes
  FCurrentIndex.StoreCurrentRecIntoBookmark(@RemRecBookmrk);
  RemRec := FCurrentIndex.CurrentBuffer;
  FIndexes[0].RemoveRecordFromIndex(RemRecBookmrk);
  if FCurrentIndex=FIndexes[1] then StartInd := 1 else StartInd := 2;
  for i := StartInd to FIndexesCount-1 do
    findexes[i].RemoveRecordFromIndex(RemRecBookmrk);

  if not GetActiveRecordUpdateBuffer then
    begin
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := IntAllocRecordBuffer;
    move(RemRec^, FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer^,FRecordSize);
    end
  else //with FIndexes[0] do
    begin
    if FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind <> ukModify then
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := nil;  //this 'disables' the updatebuffer
    end;
  FCurrentIndex.StoreCurrentRecIntoBookmark(@FUpdateBuffer[FCurrentUpdateBuffer].NextBookmarkData);
  FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := RemRecBookmrk;
  FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukDelete;

  dec(FBRecordCount);
end;


procedure TBufDataset.ApplyRecUpdate(UpdateKind : TUpdateKind);

begin
  raise EDatabaseError.Create(SApplyRecNotSupported);
end;

procedure TBufDataset.CancelUpdates;
var StoreRecBM     : TBufBookmark;
  procedure CancelUpdBuffer(var AUpdBuffer : TRecUpdateBuffer);
  var
    TmpBuf         : PChar;
    StoreUpdBuf    : integer;
    Bm             : TBufBookmark;
    x              : Integer;
  begin
    with AUpdBuffer do if assigned(BookmarkData.BookmarkData) then // this is used to exclude buffers which are already handled
      begin
      if (UpdateKind = ukModify) then
        begin
        FCurrentIndex.GotoBookmark(@BookmarkData);
        move(pchar(OldValuesBuffer)^,pchar(FCurrentIndex.CurrentBuffer)^,FRecordSize);
        FreeRecordBuffer(OldValuesBuffer);
        end
      else if (UpdateKind = ukDelete) and (assigned(OldValuesBuffer)) then
        begin
        FCurrentIndex.GotoBookmark(@NextBookmarkData);
        FCurrentIndex.InsertRecordBeforeCurrentRecord(PChar(BookmarkData.BookmarkData));
        FCurrentIndex.ScrollBackward;
        move(pchar(OldValuesBuffer)^,pchar(FCurrentIndex.CurrentBuffer)^,FRecordSize);

{        for x := length(FUpdateBuffer)-1 downto 0 do
          begin
          if (FUpdateBuffer[x].UpdateKind=ukDelete) and FCurrentIndex.CompareBookmarks(@FUpdateBuffer[x].NextBookmarkData,@BookmarkData) then
            CancelUpdBuffer(FUpdateBuffer[x]);
          end;}
        FreeRecordBuffer(OldValuesBuffer);
        inc(FBRecordCount);
        end
      else if (UpdateKind = ukInsert) then
        begin
        // Process all upd-buffers linked to this record before this record is removed
        StoreUpdBuf:=FCurrentUpdateBuffer;
        Bm := BookmarkData;
        BookmarkData.BookmarkData:=nil; // Avoid infinite recursion...
        if GetRecordUpdateBuffer(Bm,True,False) then
          begin
          repeat
          if (FCurrentUpdateBuffer<>StoreUpdBuf) then CancelUpdBuffer(FUpdateBuffer[FCurrentUpdateBuffer]);
          until not GetRecordUpdateBuffer(Bm,True,True);
          end;
        FCurrentUpdateBuffer:=StoreUpdBuf;

        FCurrentIndex.GotoBookmark(@Bm);
        TmpBuf:=FCurrentIndex.CurrentRecord;
        // resync won't work if the currentbuffer is freed...
        if FCurrentIndex.CompareBookmarks(@Bm,@StoreRecBM) then with FCurrentIndex do
          begin
          GotoBookmark(@StoreRecBM);
          if ScrollForward = grEOF then
            ScrollBackward;
          StoreCurrentRecIntoBookmark(@StoreRecBM);
          end;
        FCurrentIndex.RemoveRecordFromIndex(Bm);
        FreeRecordBuffer(TmpBuf);
        dec(FBRecordCount);
        end;
      BookmarkData.BookmarkData:=nil;
      end;
  end;

var r              : Integer;

begin
  CheckBrowseMode;

  if Length(FUpdateBuffer) > 0 then
    begin
    FCurrentIndex.StoreCurrentRecIntoBookmark(@StoreRecBM);
    r := Length(FUpdateBuffer) -1;
    while r > -1 do
      begin
      CancelUpdBuffer(FUpdateBuffer[r]);
      dec(r)
      end;

    SetLength(FUpdateBuffer,0);
    
    FCurrentIndex.GotoBookmark(@StoreRecBM);
    
    Resync([]);
    end;
end;

procedure TBufDataset.SetOnUpdateError(const AValue: TResolverErrorEvent);

begin
  FOnUpdateError := AValue;
end;

procedure TBufDataset.ApplyUpdates; // For backwards-compatibility

begin
  ApplyUpdates(0);
end;

procedure TBufDataset.ApplyUpdates(MaxErrors: Integer);

var r            : Integer;
    FailedCount  : integer;
    Response     : TResolverResponse;
    StoreCurrRec : TBufBookmark;
    AUpdateErr   : EUpdateError;

begin
  CheckBrowseMode;

  FCurrentIndex.StoreCurrentRecIntoBookmark(@StoreCurrRec);

  r := 0;
  FailedCount := 0;
  Response := rrApply;
  DisableControls;
  try
    while (r < Length(FUpdateBuffer)) and (Response <> rrAbort) do
      begin
      // If the record is first inserted and afterwards deleted, do nothing
      if not ((FUpdateBuffer[r].UpdateKind=ukDelete) and not (assigned(FUpdateBuffer[r].OldValuesBuffer))) then
        begin
        FCurrentIndex.GotoBookmark(@FUpdateBuffer[r].BookmarkData);
        // Synchronise the Currentbuffer to the ActiveBuffer
        GetRecord(ActiveBuffer,gmCurrent,True);
        Response := rrApply;
        try
          ApplyRecUpdate(FUpdateBuffer[r].UpdateKind);
        except
          on E: EDatabaseError do
            begin
            Inc(FailedCount);
            if failedcount > word(MaxErrors) then Response := rrAbort
            else Response := rrSkip;
            if assigned(FOnUpdateError) then
              begin
              AUpdateErr := EUpdateError.Create(SOnUpdateError,E.Message,0,0,Exception(AcquireExceptionObject));
              FOnUpdateError(Self,Self,AUpdateErr,FUpdateBuffer[r].UpdateKind,Response);
              AUpdateErr.Free;
              if Response in [rrApply, rrIgnore] then dec(FailedCount);
              if Response = rrApply then dec(r);
              end
            else if Response = rrAbort then
              Raise EUpdateError.Create(SOnUpdateError,E.Message,0,0,Exception(AcquireExceptionObject));
            end
          else
            raise;
        end;
        if response in [rrApply, rrIgnore] then
          begin
          FreeRecordBuffer(FUpdateBuffer[r].OldValuesBuffer);
          if FUpdateBuffer[r].UpdateKind = ukDelete then
            FreeRecordBuffer(PChar(FUpdateBuffer[r].BookmarkData.BookmarkData));
          FUpdateBuffer[r].BookmarkData.BookmarkData := nil;
          end
        end;
      inc(r);
      end;
  finally
    if failedcount = 0 then
      begin
      SetLength(FUpdateBuffer,0);

      if assigned(FUpdateBlobBuffers) then for r:=0 to length(FUpdateBlobBuffers)-1 do
       if assigned(FUpdateBlobBuffers[r]) then
        begin
        if FUpdateBlobBuffers[r]^.OrgBufID >= 0 then
          begin
          Freemem(FBlobBuffers[FUpdateBlobBuffers[r]^.OrgBufID]^.Buffer);
          Dispose(FBlobBuffers[FUpdateBlobBuffers[r]^.OrgBufID]);
          FBlobBuffers[FUpdateBlobBuffers[r]^.OrgBufID] :=FUpdateBlobBuffers[r];
          end
        else
          begin
          setlength(FBlobBuffers,length(FBlobBuffers)+1);
          FUpdateBlobBuffers[r]^.OrgBufID := high(FBlobBuffers);
          FBlobBuffers[high(FBlobBuffers)] := FUpdateBlobBuffers[r];
          
          end;
        end;
      SetLength(FUpdateBlobBuffers,0);
      end;

    InternalGotoBookmark(@StoreCurrRec);
    Resync([]);
    EnableControls;
  end;
end;


procedure TBufDataset.InternalCancel;

Var i            : integer;

begin
  if assigned(FUpdateBlobBuffers) then for i:=0 to length(FUpdateBlobBuffers)-1 do
   if assigned(FUpdateBlobBuffers[i]) and (FUpdateBlobBuffers[i]^.FieldNo>0) then
    begin
    Reallocmem(FUpdateBlobBuffers[i]^.Buffer,0);
    Dispose(FUpdateBlobBuffers[i]);
    FUpdateBlobBuffers[i] := nil;
    end;
end;

procedure TBufDataset.InternalPost;

Var CurrBuff     : PChar;
    i            : integer;
    blobbuf      : tbufblobfield;
    NullMask     : pbyte;

begin
  inherited InternalPost;
  if assigned(FUpdateBlobBuffers) then for i:=0 to length(FUpdateBlobBuffers)-1 do
   if assigned(FUpdateBlobBuffers[i]) and (FUpdateBlobBuffers[i]^.FieldNo>0) then
    begin
    blobbuf.BlobBuffer := FUpdateBlobBuffers[i];
    CurrBuff := ActiveBuffer;
    NullMask := pbyte(CurrBuff);

    inc(CurrBuff,FFieldBufPositions[FUpdateBlobBuffers[i]^.FieldNo-1]);
    Move(blobbuf, CurrBuff^, GetFieldSize(FieldDefs[FUpdateBlobBuffers[i]^.FieldNo-1]));
    unSetFieldIsNull(NullMask,FUpdateBlobBuffers[i]^.FieldNo-1);
    
    FUpdateBlobBuffers[i]^.FieldNo := -1;
    end;

  if state = dsInsert then
    begin
    if GetBookmarkFlag(ActiveBuffer) = bfEOF then
      FIndexes[0].ScrollLast
    else
      // The active buffer is the newly created TDataset record,
      // from which the bookmark is set to the record where the new record should be
      // inserted
      InternalSetToRecord(ActiveBuffer);

    with FIndexes[0] do
      begin
      // Create the new record buffer
      FCurrentIndex.InsertRecordBeforeCurrentRecord(IntAllocRecordBuffer);
      ScrollBackward;
      // Add the record to the other indexes
      for i := 1 to FIndexesCount-1 do if ((i>1) or (FIndexes[i]=FCurrentIndex)) then
        FIndexes[i].InsertRecordBeforeCurrentRecord(CurrentRecord);
      end;

    // Link the newly created record buffer to the newly created TDataset record
    with PBufBookmark(ActiveBuffer + FRecordSize)^ do
      begin
      FCurrentIndex.StoreCurrentRecIntoBookmark(@BookmarkData);
      BookmarkFlag := bfInserted;
      end;
      
    inc(FBRecordCount);
    end
  else
    InternalSetToRecord(ActiveBuffer);


  // If there is no updatebuffer already, add one
  if not GetActiveRecordUpdateBuffer then
    begin
    // Add a new updatebuffer
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    // Store a bookmark of the current record into the updatebuffer's bookmark
    FCurrentIndex.StoreCurrentRecIntoBookmark(@FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData);

    if state = dsEdit then
      begin
      // Create an oldvalues buffer with the old values of the record
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := intAllocRecordBuffer;
      with FCurrentIndex do
        // Move only the real data
        move(CurrentBuffer^,FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer^,FRecordSize);
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukModify;
      end
    else
      begin
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukInsert;
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := nil;
      end;
    end;

  move(ActiveBuffer^,FCurrentIndex.CurrentBuffer^,FRecordSize);
end;

procedure TBufDataset.CalcRecordSize;

var x : longint;

begin
  FNullmaskSize := 1+((FieldDefs.count-1) div 8);
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  FNullmaskSize:=Align(FNullmaskSize,4);
{$ENDIF}
  FRecordSize := FNullmaskSize;
  SetLength(FFieldBufPositions,FieldDefs.count);
  for x := 0 to FieldDefs.count-1 do
    begin
    FFieldBufPositions[x] := FRecordSize;
    inc(FRecordSize, GetFieldSize(FieldDefs[x]));
    end;
end;

function TBufDataset.GetIndexFieldNames: String;
begin
  if FCurrentIndex<>FIndexes[1] then
    result := ''
  else
    result := FCurrentIndex.FieldsName;
end;

function TBufDataset.GetIndexName: String;
begin
  result := FCurrentIndex.Name;
end;

function TBufDataset.GetRecordSize : Word;

begin
  result := FRecordSize + BookmarkSize;
end;

function TBufDataset.GetChangeCount: integer;

begin
  result := length(FUpdateBuffer);
end;


procedure TBufDataset.InternalInitRecord(Buffer: PChar);

begin
  FillChar(Buffer^, FRecordSize, #0);

  fillchar(Buffer^,FNullmaskSize,255);
end;

procedure TBufDataset.SetRecNo(Value: Longint);

var
    recnr        : integer;
    TmpRecBuffer : PBufRecLinkItem;

begin
  checkbrowsemode;
  if value > RecordCount then
    begin
    repeat until (getnextpacket < FPacketRecords) or (value <= RecordCount) or (FPacketRecords = -1);
    if value > RecordCount then
      begin
      DatabaseError(SNoSuchRecord,self);
      exit;
      end;
    end;
  TmpRecBuffer := (FCurrentIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
  for recnr := 1 to value-1 do
    TmpRecBuffer := TmpRecBuffer^.next;
  GotoBookmark(@TmpRecBuffer);
end;

function TBufDataset.GetRecNo: Longint;

Var abuf            : PChar;

begin
  abuf := GetCurrentBuffer;
  // If abuf isn't assigned, the recordset probably isn't opened.
  if assigned(abuf) and (FBRecordCount>0) and (state <> dsInsert) then
    Result:=FCurrentIndex.GetRecNo(PBufBookmark(abuf+FRecordSize))
  else
    result := 0;
end;

function TBufDataset.IsCursorOpen: Boolean;

begin
  Result := FOpen;
end;

Function TBufDataset.GetRecordCount: Longint;

begin
  Result := FBRecordCount;
end;

Function TBufDataSet.UpdateStatus: TUpdateStatus;

begin
  Result:=usUnmodified;
  if GetActiveRecordUpdateBuffer then
    case FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind of
      ukModify : Result := usModified;
      ukInsert : Result := usInserted;
      ukDelete : Result := usDeleted;
    end;
end;

function TbufDataset.GetNewBlobBuffer : PBlobBuffer;

var ABlobBuffer : PBlobBuffer;

begin
  setlength(FBlobBuffers,length(FBlobBuffers)+1);
  new(ABlobBuffer);
  fillbyte(ABlobBuffer^,sizeof(ABlobBuffer^),0);
  ABlobBuffer^.OrgBufID := high(FUpdateBlobBuffers);
  FBlobBuffers[high(FBlobBuffers)] := ABlobBuffer;
  result := ABlobBuffer;
end;

function TbufDataset.GetNewWriteBlobBuffer : PBlobBuffer;

var ABlobBuffer : PBlobBuffer;

begin
  setlength(FUpdateBlobBuffers,length(FUpdateBlobBuffers)+1);
  new(ABlobBuffer);
  fillbyte(ABlobBuffer^,sizeof(ABlobBuffer^),0);
  FUpdateBlobBuffers[high(FUpdateBlobBuffers)] := ABlobBuffer;
  result := ABlobBuffer;
end;

procedure TBufDataset.FreeBlobBuffer(var ABlobBuffer: PBlobBuffer);

begin
  if not Assigned(ABlobBuffer) then Exit;
  FreeMem(ABlobBuffer^.Buffer, ABlobBuffer^.Size);
  Dispose(ABlobBuffer);
  ABlobBuffer := Nil;
end;

function TBufBlobStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=FBlobBuffer^.Size+Offset;
    soFromCurrent   : FpoSition:=FPosition+Offset;
  end;
  Result:=FPosition;
end;


function TBufBlobStream.Read(var Buffer; Count: Longint): Longint;

var ptr : pointer;

begin
  if FPosition + count > FBlobBuffer^.Size then
    count := FBlobBuffer^.Size-FPosition;
  ptr := FBlobBuffer^.Buffer+FPosition;
  move(ptr^,buffer,count);
  inc(FPosition,count);
  result := count;
end;

function TBufBlobStream.Write(const Buffer; Count: Longint): Longint;

var ptr : pointer;

begin
  ReAllocMem(FBlobBuffer^.Buffer,FPosition+Count);
  ptr := FBlobBuffer^.Buffer+FPosition;
  move(buffer,ptr^,count);
  inc(FBlobBuffer^.Size,count);
  inc(FPosition,count);
  Result := count;
end;

constructor TBufBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);

var bufblob : TBufBlobField;

begin
  FDataset := Field.DataSet as TBufDataset;
  if mode = bmread then
    begin
    if not field.getData(@bufblob) then
      DatabaseError(SFieldIsNull);
    if not assigned(bufblob.BlobBuffer) then with FDataSet do
      begin
      FBlobBuffer := GetNewBlobBuffer;
      bufblob.BlobBuffer := FBlobBuffer;
      LoadBlobIntoBuffer(FieldDefs[field.FieldNo-1],@bufblob);
      end
    else
      FBlobBuffer := bufblob.BlobBuffer;
    end
  else if mode=bmWrite then with FDataSet as TBufDataset do
    begin
    FBlobBuffer := GetNewWriteBlobBuffer;
    FBlobBuffer^.FieldNo := Field.FieldNo;
    if (field.getData(@bufblob)) and assigned(bufblob.BlobBuffer) then
      FBlobBuffer^.OrgBufID := bufblob.BlobBuffer^.OrgBufID
    else
      FBlobBuffer^.OrgBufID := -1;
    end;
end;

function TBufDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;

var bufblob : TBufBlobField;

begin
  result := nil;
  if mode=bmread then
    begin
    if not field.getData(@bufblob) then
      exit;

    result := TBufBlobStream.Create(Field as tblobfield,bmread);
    end
  else if mode=bmWrite then
    begin
    if not (state in [dsEdit, dsInsert, dsFilter, dsCalcFields]) then
      begin
      DatabaseErrorFmt(SNotEditing,[Name],self);
      exit;
      end;

    result := TBufBlobStream.Create(Field as tblobfield,bmWrite);

    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Ptrint(Field));
    end;
end;

procedure TBufDataset.AddIndex(const AName, AFields : string; AOptions : TIndexOptions; const ADescFields: string = '';
                               const ACaseInsFields: string = '');
begin
  if AFields='' then DatabaseError(SNoIndexFieldNameGiven);
  
  if active and (FIndexesCount=FMaxIndexesCount) then
    DatabaseError(SMaxIndexes);

  // If not all packets are fetched, you can not sort properly.
  if not active then
    FPacketRecords:=-1;
  InternalAddIndex(AName,AFields,AOptions,ADescFields,ACaseInsFields);
end;

procedure TBufDataset.SaveToFile(AFileName: string;
  Format: TDataPacketFormat);
var AFileStream : TFileStream;
begin
  if AFileName='' then AFileName := FFileName;
  AFileStream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(AFileStream, Format);
  finally
    AFileStream.Free;
  end;
end;

procedure TBufDataset.SetDatasetPacket(AReader: TDataPacketReader);
begin
  FDatasetReader := AReader;
  try
    Open;
  finally
    FDatasetReader := nil;
  end;
end;

procedure TBufDataset.GetDatasetPacket(AWriter: TDataPacketReader);

  procedure StoreUpdateBuffer(AUpdBuffer : TRecUpdateBuffer; var ARowState: TRowState);
  var AThisRowState : TRowState;
  begin
    FFilterBuffer:=AUpdBuffer.OldValuesBuffer;
    if AUpdBuffer.UpdateKind = ukModify then
      begin
      AThisRowState := [rsvOriginal];
      ARowState:=[rsvUpdated];
      end
    else if AUpdBuffer.UpdateKind = ukDelete then
      AThisRowState := [rsvDeleted]
    else // ie: updatekind = ukInsert
      begin
      ARowState := [rsvInserted];
      Exit;
      end;
    FDatasetReader.StoreRecord(Self,AThisRowState,FCurrentUpdateBuffer);
  end;

  procedure HandleUpdateBuffersFromRecord(ARecBookmark : TBufBookmark; var ARowState: TRowState);
  begin
    if GetRecordUpdateBuffer(ARecBookmark,True) then
      begin
      // Loop to see if there is more then one update-buffer
      // linked to the current record
      repeat
      StoreUpdateBuffer(FUpdateBuffer[FCurrentUpdateBuffer], ARowState);
      until not GetRecordUpdateBuffer(ARecBookmark,True,True)
      end
    else
      ARowState:=[];
  end;

var ScrollResult   : TGetResult;
    StoreDSState   : TDataSetState;
    ABookMark      : PBufBookmark;
    ATBookmark     : TBufBookmark;
    RowState       : TRowState;

begin
  FDatasetReader := AWriter;
  try
    //CheckActive;
    ABookMark:=@ATBookmark;
    FDatasetReader.StoreFieldDefs(FieldDefs);

    StoreDSState:=State;
    SetTempState(dsFilter);
    ScrollResult:=FCurrentIndex.ScrollFirst;
    while ScrollResult=grOK do
      begin
      RowState:=[];
      FCurrentIndex.StoreCurrentRecIntoBookmark(ABookmark);
      HandleUpdateBuffersFromRecord(ABookmark^,RowState);
      FFilterBuffer:=FCurrentIndex.CurrentBuffer;
      if RowState=[] then
        FDatasetReader.StoreRecord(Self,[])
      else
        FDatasetReader.StoreRecord(Self,RowState,FCurrentUpdateBuffer);

      ScrollResult:=FCurrentIndex.ScrollForward;
      if ScrollResult<>grOK then
        begin
        if getnextpacket>0 then
          ScrollResult := FCurrentIndex.ScrollForward;
        end;
      end;
    // There could be a update-buffer linked to the last (spare) record
    FCurrentIndex.StoreSpareRecIntoBookmark(ABookmark);
    HandleUpdateBuffersFromRecord(ABookmark^,RowState);

    RestoreState(StoreDSState);

    FDatasetReader.FinalizeStoreRecords;
  finally
    FDatasetReader := nil;
  end;
end;

procedure TBufDataset.LoadFromStream(AStream: TStream; Format: TDataPacketFormat);
var APacketReaderReg : TDatapacketReaderRegistration;
    APacketReader : TDataPacketReader;
begin
  if GetRegisterDatapacketReader(AStream,format,APacketReaderReg) then
    APacketReader := APacketReaderReg.ReaderClass.create(AStream)
  else if TFpcBinaryDatapacketReader.RecognizeStream(AStream) then
    begin
    AStream.Seek(0,soFromBeginning);
    APacketReader := TFpcBinaryDatapacketReader.create(AStream)
    end
  else
    DatabaseError(SStreamNotRecognised);
  try
    SetDatasetPacket(APacketReader);
  finally
    APacketReader.Free;
  end;
end;

procedure TBufDataset.SaveToStream(AStream: TStream; Format: TDataPacketFormat);
var APacketReaderReg : TDatapacketReaderRegistration;
    APacketWriter : TDataPacketReader;
begin
  if GetRegisterDatapacketReader(Nil,format,APacketReaderReg) then
    APacketWriter := APacketReaderReg.ReaderClass.create(AStream)
  else if Format = dfBinary then
    APacketWriter := TFpcBinaryDatapacketReader.create(AStream)
  else
    DatabaseError(SNoReaderClassRegistered);
  try
    GetDatasetPacket(APacketWriter);
  finally
    APacketWriter.Free;
  end;
end;

procedure TBufDataset.LoadFromFile(AFileName: string; Format: TDataPacketFormat);
var AFileStream : TFileStream;
begin
  if AFileName='' then AFileName := FFileName;
  AFileStream := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadFromStream(AFileStream, Format);
  finally
    AFileStream.Free;
  end;
end;

procedure TBufDataset.CreateDataset;
begin
  CheckInactive;
  CreateFields;
end;

function TBufDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  Result:=FCurrentIndex.BookmarkValid(ABookmark);
end;

function TBufDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark
  ): Longint;
begin
  if FCurrentIndex.CompareBookmarks(Bookmark1,Bookmark2) then
    Result := 0
  else
    Result := -1;
end;

procedure TBufDataset.IntLoadFielddefsFromFile;

begin
  FDatasetReader.LoadFielddefs(FieldDefs);
  if DefaultFields then CreateFields;
end;

procedure TBufDataset.IntLoadRecordsFromFile;

var StoreState      : TDataSetState;
    AddRecordBuffer : boolean;
    ARowState       : TRowState;
    AUpdOrder       : integer;

begin
  FDatasetReader.InitLoadRecords;
  StoreState:=SetTempState(dsFilter);

  while FDatasetReader.GetCurrentRecord do
    begin
    ARowState := FDatasetReader.GetRecordRowState(AUpdOrder);
    if rsvOriginal in ARowState then
      begin
      if length(FUpdateBuffer) < (AUpdOrder+1) then
        SetLength(FUpdateBuffer,AUpdOrder+1);

      FCurrentUpdateBuffer:=AUpdOrder;

      FFilterBuffer:=IntAllocRecordBuffer;
      fillchar(FFilterBuffer^,FNullmaskSize,0);
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := FFilterBuffer;
      FDatasetReader.RestoreRecord(self);

      FDatasetReader.GotoNextRecord;
      if not FDatasetReader.GetCurrentRecord then
        DatabaseError(SStreamNotRecognised);
      ARowState := FDatasetReader.GetRecordRowState(AUpdOrder);
      if rsvUpdated in ARowState then
        FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind:= ukModify
      else
        DatabaseError(SStreamNotRecognised);

      FFilterBuffer:=FIndexes[0].SpareBuffer;
      FIndexes[0].StoreSpareRecIntoBookmark(@FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData);
      fillchar(FFilterBuffer^,FNullmaskSize,0);

      FDatasetReader.RestoreRecord(self);
      FIndexes[0].AddRecord(IntAllocRecordBuffer);
      inc(FBRecordCount);

      AddRecordBuffer:=False;

      end
    else if rsvDeleted in ARowState then
      begin
      if length(FUpdateBuffer) < (AUpdOrder+1) then
        SetLength(FUpdateBuffer,AUpdOrder+1);

      FCurrentUpdateBuffer:=AUpdOrder;

      FFilterBuffer:=IntAllocRecordBuffer;
      fillchar(FFilterBuffer^,FNullmaskSize,0);
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := FFilterBuffer;
      FDatasetReader.RestoreRecord(self);

      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind:= ukDelete;
      FIndexes[0].StoreSpareRecIntoBookmark(@FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData);

      AddRecordBuffer:=False;
      end
    else
      AddRecordBuffer:=True;

    if AddRecordBuffer then
      begin
      FFilterBuffer:=FIndexes[0].SpareBuffer;
      fillchar(FFilterBuffer^,FNullmaskSize,0);

      FDatasetReader.RestoreRecord(self);

      if rsvInserted in ARowState then
        begin
        if length(FUpdateBuffer) < (AUpdOrder+1) then
          SetLength(FUpdateBuffer,AUpdOrder+1);
        FCurrentUpdateBuffer:=AUpdOrder;
        FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind:= ukInsert;
        FCurrentIndex.StoreSpareRecIntoBookmark(@FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData);
        end;

      FIndexes[0].AddRecord(IntAllocRecordBuffer);
      inc(FBRecordCount);
      end;

    FDatasetReader.GotoNextRecord;
    end;

  RestoreState(StoreState);
  FIndexes[0].SetToFirstRecord;
  FAllPacketsFetched:=True;
  if assigned(FFileStream) then
    begin
    FreeAndNil(FFileStream);
    FreeAndNil(FDatasetReader);
    end;
end;

procedure TBufDataset.InternalAddIndex(const AName, AFields : string; AOptions : TIndexOptions; const ADescFields: string;
                                       const ACaseInsFields: string);
var StoreIndNr : Integer;
begin
  if Active then FetchAll;
  if FIndexesCount>0 then
    StoreIndNr:=FCurrentIndex.IndNr
  else
    StoreIndNr:=0;
  inc(FIndexesCount);
  setlength(FIndexes,FIndexesCount); // This invalidates the currentindex! -> not anymore
  FCurrentIndex:=FIndexes[StoreIndNr];
  FIndexes[FIndexesCount-1] := TDoubleLinkedBufIndex.Create(self);
//  FIndexes[FIndexesCount-1] := TArrayBufIndex.Create(self);
  FIndexes[FIndexesCount-1].InitialiseIndex;
  with (FIndexes[FIndexesCount-1] as TBufIndex) do
    begin
    Name:=AName;
    FieldsName:=AFields;
    DescFields:=ADescFields;
    CaseinsFields:=ACaseInsFields;
    Options:=AOptions;
    IndNr:=FIndexesCount-1;
    end;

  if Active then
    begin
    (FIndexes[FIndexesCount-1] as TDoubleLinkedBufIndex).FFirstRecBuf := pointer(IntAllocRecordBuffer);
    (FIndexes[FIndexesCount-1] as TDoubleLinkedBufIndex).FLastRecBuf := (FIndexes[FIndexesCount-1] as TDoubleLinkedBufIndex).FFirstRecBuf;
    (FCurrentIndex as TDoubleLinkedBufIndex).FCurrentRecBuf := (FIndexes[FIndexesCount-1] as TDoubleLinkedBufIndex).FLastRecBuf;
    BuildIndex(FIndexes[FIndexesCount-1]);
    end
  else if FIndexesCount>FMaxIndexesCount then
    FMaxIndexesCount := FIndexesCount;
end;

procedure TBufDataset.DoFilterRecord(var Acceptable: Boolean);
begin
  Acceptable := true;
  // check user filter
  if Assigned(OnFilterRecord) then
    OnFilterRecord(Self, Acceptable);

  // check filtertext
  if Acceptable and (Length(Filter) > 0) then
    Acceptable := Boolean((FParser.ExtractFromBuffer(GetCurrentBuffer))^);

end;

procedure TBufDataset.SetFilterText(const Value: String);
begin
  if Value = Filter then
    exit;

  // parse
  ParseFilter(Value);

  // call dataset method
  inherited;

  // refilter dataset if filtered
  if IsCursorOpen and Filtered then Refresh;
end;

procedure TBufDataset.SetFiltered(Value: Boolean); {override;}
begin
  if Value = Filtered then
    exit;

  // pass on to ancestor
  inherited;

  // only refresh if active
  if IsCursorOpen then
    Refresh;
end;

function TBufDataset.Fetch: boolean;
begin
  // Empty procedure to make it possible to use TBufDataset as a memory dataset
  Result := False;
end;

function TBufDataset.LoadField(FieldDef: TFieldDef; buffer: pointer; out
  CreateBlob: boolean): boolean;
begin
  // Empty procedure to make it possible to use TBufDataset as a memory dataset
  CreateBlob := False;
  Result := False;
end;

function TBufDataset.IsReadFromPacket: Boolean;
begin
  Result := (FDatasetReader<>nil) or (FFileName<>'');
end;

procedure TBufDataset.ParseFilter(const AFilter: string);
begin
  // parser created?
  if Length(AFilter) > 0 then
  begin
    if (FParser = nil) and IsCursorOpen then
    begin
      FParser := TBufDatasetParser.Create(Self);
    end;
    // have a parser now?
    if FParser <> nil then
    begin
      // set options
      FParser.PartialMatch := not (foNoPartialCompare in FilterOptions);
      FParser.CaseInsensitive := foCaseInsensitive in FilterOptions;
      // parse expression
      FParser.ParseExpression(AFilter);
    end;
  end;
end;

function TArrayBufIndex.GetRecordFromBookmark(ABookMark: TBufBookmark) : integer;
begin
  // ABookmark.BookMarkBuf is nil if SetRecNo calls GotoBookmark
  if (ABookmark.BookmarkData<>nil) and (FRecordArray[ABookmark.BookmarkInt]<>ABookmark.BookmarkData) then
    begin
    // Start searching two records before the expected record
    if ABookmark.BookmarkInt > 2 then
      Result := ABookmark.BookmarkInt-2
    else
      Result := 0;

    while (Result<FLastRecInd) do
      begin
      if (FRecordArray[Result] = ABookmark.BookmarkData) then exit;
      inc(Result);
      end;

    Result:=0;
    while (Result<ABookmark.BookmarkInt) do
      begin
      if (FRecordArray[Result] = ABookmark.BookmarkData) then exit;
      inc(Result);
      end;

    DatabaseError(SInvalidBookmark)
    end
  else
    Result := ABookmark.BookmarkInt;
end;

Function TBufDataset.Locate(const KeyFields: string; const KeyValues: Variant; options: TLocateOptions) : boolean;

var CurrLinkItem    : PBufRecLinkItem;
    bm              : TBufBookmark;
    SearchFields    : TList;
    FieldsAmount    : Integer;
    DBCompareStruct : TDBCompareStruct;
    FieldNr         : Integer;
    StoreDSState    : TDataSetState;
    FilterBuffer    : PChar;


begin
  Result := False;
  if IsEmpty then exit;

  // Build the DBCompare structure
  SearchFields := TList.Create;
  try
    GetFieldList(SearchFields,KeyFields);
    FieldsAmount:=SearchFields.Count;
    if FieldsAmount=0 then exit;

    SetLength(DBCompareStruct,FieldsAmount);
    for FieldNr:=0 to FieldsAmount-1 do
      ProcessFieldCompareStruct(TField(SearchFields[FieldNr]),DBCompareStruct[FieldNr]);
  finally
    SearchFields.Free;
  end;

  // Set The filter-buffer
  StoreDSState:=State;
  FFilterBuffer:=FCurrentIndex.SpareBuffer;
  SetTempState(dsFilter);
  SetFieldValues(keyfields,KeyValues);
  CurrLinkItem := (FCurrentIndex as TDoubleLinkedBufIndex).FFirstRecBuf;
  FilterBuffer:=IntAllocRecordBuffer;
  move((FCurrentIndex as TDoubleLinkedBufIndex).FLastRecBuf^,FilterBuffer^,FRecordsize+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
  SetTempState(StoreDSState);

  // Iterate through the records until a match is found
  while (CurrLinkItem <> (FCurrentIndex as TDoubleLinkedBufIndex).FLastRecBuf) do
    begin
    if (IndexCompareRecords(FilterBuffer,CurrLinkItem,DBCompareStruct) = 0) then
      begin
      Result := True;
      break;
      end;
    CurrLinkItem := CurrLinkItem^.next;
    if CurrLinkItem = (FCurrentIndex as TDoubleLinkedBufIndex).FLastRecBuf then
      getnextpacket;
    end;
    
  FreeRecordBuffer(FilterBuffer);

  // If a match is found, jump to the found record
  if Result then
    begin
    bm.BookmarkData := CurrLinkItem;
    bm.BookmarkFlag := bfCurrent;
    GotoBookmark(@bm);
    end;
end;

{ TArrayBufIndex }

function TArrayBufIndex.GetBookmarkSize: integer;
begin
  Result:=Sizeof(TBufBookmark);
end;

function TArrayBufIndex.GetCurrentBuffer: Pointer;
begin
  Result:=pchar(FRecordArray[FCurrentRecInd]);
end;

function TArrayBufIndex.GetCurrentRecord: PChar;
begin
  Result:=GetCurrentBuffer;
end;

function TArrayBufIndex.GetIsInitialized: boolean;
begin
  Result:=Length(FRecordArray)>0;
end;

function TArrayBufIndex.GetSpareBuffer: PChar;
begin
  if FLastRecInd>-1 then
    Result:=pchar(FRecordArray[FLastRecInd])
  else
    Result := nil;
end;

function TArrayBufIndex.GetSpareRecord: PChar;
begin
  Result := GetSpareBuffer;
end;

constructor TArrayBufIndex.Create(const ADataset: TBufDataset);
begin
  Inherited create(ADataset);
  FInitialBuffers:=10000;
  FGrowBuffer:=1000;
end;

function TArrayBufIndex.ScrollBackward: TGetResult;
begin
  if FCurrentRecInd>0 then
    begin
    dec(FCurrentRecInd);
    Result := grOK;
    end
  else
    Result := grBOF;
end;

function TArrayBufIndex.ScrollForward: TGetResult;
begin
  if FCurrentRecInd = FLastRecInd-1 then
    result := grEOF
  else
    begin
    Result:=grOK;
    inc(FCurrentRecInd);
    end;
end;

function TArrayBufIndex.GetCurrent: TGetResult;
begin
  if FLastRecInd=0 then
    Result := grError
  else
    begin
    Result := grOK;
    if FCurrentRecInd = FLastRecInd then
      dec(FCurrentRecInd);
    end;
end;

function TArrayBufIndex.ScrollFirst: TGetResult;
begin
  FCurrentRecInd:=0;
  if (FCurrentRecInd = FLastRecInd) then
    result := grEOF
  else
    result := grOk;
end;

procedure TArrayBufIndex.ScrollLast;
begin
  FCurrentRecInd:=FLastRecInd;
end;

procedure TArrayBufIndex.SetToFirstRecord;
begin
// if FCurrentRecBuf = FLastRecBuf then the dataset is just opened and empty
// in which case InternalFirst should do nothing (bug 7211)
  if FCurrentRecInd <> FLastRecInd then
    FCurrentRecInd := -1;
end;

procedure TArrayBufIndex.SetToLastRecord;
begin
  if FLastRecInd <> 0 then FCurrentRecInd := FLastRecInd;
end;

procedure TArrayBufIndex.StoreCurrentRecord;
begin
  FStoredRecBuf := FCurrentRecInd;
end;

procedure TArrayBufIndex.RestoreCurrentRecord;
begin
  FCurrentRecInd := FStoredRecBuf;
end;

function TArrayBufIndex.CanScrollForward: Boolean;
begin
  Result := (FCurrentRecInd < FLastRecInd-1);
end;

procedure TArrayBufIndex.DoScrollForward;
begin
  inc(FCurrentRecInd);
end;

procedure TArrayBufIndex.StoreCurrentRecIntoBookmark(const ABookmark: PBufBookmark);
begin
  with ABookmark^ do
    begin
    BookmarkInt := FCurrentRecInd;
    BookmarkData := FRecordArray[FCurrentRecInd];
    end;
end;

procedure TArrayBufIndex.StoreSpareRecIntoBookmark(const ABookmark: PBufBookmark
  );
begin
  with ABookmark^ do
    begin
    BookmarkInt := FLastRecInd;
    BookmarkData := FRecordArray[FLastRecInd];
    end;
end;

procedure TArrayBufIndex.GotoBookmark(const ABookmark : PBufBookmark);
begin
  FCurrentRecInd:=GetRecordFromBookmark(ABookmark^);
end;

procedure TArrayBufIndex.InitialiseIndex;
begin
//  FRecordArray:=nil;
  setlength(FRecordArray,FInitialBuffers);
  FCurrentRecInd:=-1;
  FLastRecInd:=-1;
end;

procedure TArrayBufIndex.InitialiseSpareRecord(const ASpareRecord: PChar);
begin
  FLastRecInd := 0;
 // FCurrentRecInd := 0;
  FRecordArray[0] := ASpareRecord;
end;

procedure TArrayBufIndex.ReleaseSpareRecord;
begin
  SetLength(FRecordArray,FInitialBuffers);
end;

function TArrayBufIndex.GetRecNo(const ABookmark: PBufBookmark): integer;
begin
  Result := GetRecordFromBookmark(ABookmark^)+1;
end;

procedure TArrayBufIndex.RemoveRecordFromIndex(const ABookmark : TBufBookmark);
var ARecordInd : integer;
begin
  ARecordInd:=GetRecordFromBookmark(ABookmark);
  Move(FRecordArray[ARecordInd+1],FRecordArray[ARecordInd],sizeof(Pointer)*(FLastRecInd-ARecordInd));
  dec(FLastRecInd);
end;

procedure TArrayBufIndex.InsertRecordBeforeCurrentRecord(const ARecord: PChar);
begin
  inc(FLastRecInd);
  if FLastRecInd >= length(FRecordArray) then
    SetLength(FRecordArray,length(FRecordArray)+FGrowBuffer);

  Move(FRecordArray[FCurrentRecInd],FRecordArray[FCurrentRecInd+1],sizeof(Pointer)*(FLastRecInd-FCurrentRecInd));
  FRecordArray[FCurrentRecInd]:=ARecord;
  inc(FCurrentRecInd);
end;

procedure TArrayBufIndex.BeginUpdate;
begin
//  inherited BeginUpdate;
end;

procedure TArrayBufIndex.AddRecord(const ARecord: PChar);
begin
  inc(FLastRecInd);
  if FLastRecInd >= length(FRecordArray) then
    SetLength(FRecordArray,length(FRecordArray)+FGrowBuffer);
  FRecordArray[FLastRecInd]:=ARecord;
end;

procedure TArrayBufIndex.EndUpdate;
begin
//  inherited EndUpdate;
end;

{ TDataPacketReader }

class function TDataPacketReader.RowStateToByte(const ARowState: TRowState
  ): byte;
var RowStateInt : Byte;
begin
  RowStateInt:=0;
  if rsvOriginal in ARowState then RowStateInt := RowStateInt+1;
  if rsvDeleted in ARowState then RowStateInt := RowStateInt+2;
  if rsvInserted in ARowState then RowStateInt := RowStateInt+4;
  if rsvUpdated in ARowState then RowStateInt := RowStateInt+8;
  Result := RowStateInt;
end;

class function TDataPacketReader.ByteToRowState(const AByte: Byte): TRowState;
begin
  result := [];
  if (AByte and 1)=1 then Result := Result+[rsvOriginal];
  if (AByte and 2)=2 then Result := Result+[rsvDeleted];
  if (AByte and 4)=4 then Result := Result+[rsvInserted];
  if (AByte and 8)=8 then Result := Result+[rsvUpdated];
end;

constructor TDataPacketReader.create(AStream: TStream);
begin
  FStream := AStream;
end;

{ TFpcBinaryDatapacketReader }

const FpcBinaryIdent = 'BinBufDataset';

procedure TFpcBinaryDatapacketReader.LoadFieldDefs(AFieldDefs: TFieldDefs);

var FldCount : word;
    i        : integer;

begin
  if not RecognizeStream(Stream) then
    DatabaseError(SStreamNotRecognised);

  FldCount:=Stream.ReadWord;
  for i := 0 to FldCount -1 do with TFieldDef.create(AFieldDefs) do
    begin
    Name := Stream.ReadAnsiString;
    Displayname := Stream.ReadAnsiString;
    Size := Stream.ReadWord;
    DataType := TFieldType(Stream.ReadWord);

    if Stream.ReadByte = 1 then
      Attributes := Attributes + [faReadonly];
    end;
end;

procedure TFpcBinaryDatapacketReader.StoreFieldDefs(AFieldDefs: TFieldDefs);
var i : integer;
begin
  Stream.Write(FpcBinaryIdent[1],length(FpcBinaryIdent));

  Stream.WriteWord(AFieldDefs.Count);
  for i := 0 to AFieldDefs.Count -1 do with AFieldDefs[i] do
    begin
    Stream.WriteAnsiString(Name);
    Stream.WriteAnsiString(DisplayName);
    Stream.WriteWord(size);
    Stream.WriteWord(ord(DataType));

    if faReadonly in Attributes then
      Stream.WriteByte(1)
    else
      Stream.WriteByte(0);
    end;
end;

function TFpcBinaryDatapacketReader.GetRecordRowState(out AUpdOrder : Integer) : TRowState;
var Buf : byte;
begin
  Stream.Read(Buf,1);
  Result := ByteToRowState(Buf);
  if Result<>[] then
    Stream.ReadBuffer(AUpdOrder,sizeof(integer))
  else
    AUpdOrder := 0;
end;

procedure TFpcBinaryDatapacketReader.FinalizeStoreRecords;
begin
//  Do nothing
end;

function TFpcBinaryDatapacketReader.GetCurrentRecord: boolean;
var Buf : byte;
begin
  Result := (Stream.Read(Buf,1)=1) and (Buf=$fe);
end;

procedure TFpcBinaryDatapacketReader.GotoNextRecord;
begin
//  Do Nothing
end;

procedure TFpcBinaryDatapacketReader.InitLoadRecords;
begin
//  SetLength(AChangeLog,0);
end;

procedure TFpcBinaryDatapacketReader.RestoreRecord(ADataset: TBufDataset);
begin
  Stream.ReadBuffer(ADataset.GetCurrentBuffer^,ADataset.FRecordSize);
end;

procedure TFpcBinaryDatapacketReader.StoreRecord(ADataset: TBufDataset;
  ARowState: TRowState; AUpdOrder : integer);
begin
  // Ugly because private members of ADataset are used...
  Stream.WriteByte($fe);
  Stream.WriteByte(RowStateToByte(ARowState));
  if ARowState<>[] then
    Stream.WriteBuffer(AUpdOrder,sizeof(integer));
  Stream.WriteBuffer(ADataset.GetCurrentBuffer^,ADataset.FRecordSize);
end;

class function TFpcBinaryDatapacketReader.RecognizeStream(AStream: TStream
  ): boolean;
var s        : string;
    len      : integer;
begin
  Len := length(FpcBinaryIdent);
  setlength(s,len);
  if (AStream.Read (s[1],len) = len)
  and (s=FpcBinaryIdent) then
    Result := True
  else
    Result := False;
end;

initialization
  setlength(RegisteredDatapacketReaders,0);
finalization
  setlength(RegisteredDatapacketReaders,0);
end.
