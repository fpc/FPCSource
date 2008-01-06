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

{ $DEFINE ARRAYBUF}
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
{$IFDEF ARRAYBUF}
    BookmarkData : integer;
    BookMarkBuf  : Pointer;
{$ELSE}
    BookmarkData : PBufRecLinkItem;
{$ENDIF}
    BookmarkFlag : TBookmarkFlag;
  end;

  TRecUpdateBuffer = record
    UpdateKind         : TUpdateKind;
{$IFDEF ARRAYBUF}
{  BookMarkData:
     - Is -1 if the update has canceled out. For example: a appended record has been deleted again
     - If UpdateKind is ukInsert it contains a bookmark to the new created record
     - If UpdateKind is ukModify it contains a bookmark to the record with the new data
     - If UpdateKind is ukDelete it contains a bookmark to the record just after the deleted record
}
    Bookmark           : TBufBookmark;
{$ELSE}
{  BookMarkData:
     - Is nil if the update has canceled out. For example: a appended record has been deleted again
     - If UpdateKind is ukInsert it contains the PBufRecLinkItem of the created record
     - If UpdateKind is ukModify it contains the PBufRecLinkItem of the record with the new data
     - If UpdateKind is ukDelete it contains the PBufRecLinkItem of the deleted record
}
    BookmarkData       : pointer;
{$ENDIF}
{  OldValuesBuffer:
     - If UpdateKind is ukModify it contains a record-buffer which contains the old data
     - If UpdateKind is ukDelete it contains the PBufRecLinkItem of the deleted record
}
    OldValuesBuffer    : pchar;
  end;

  PBufBlobField = ^TBufBlobField;
  TBufBlobField = record
    ConnBlobBuffer : array[0..11] of byte; // It's here where the db-specific data is stored
    BlobBuffer     : PBlobBuffer;
  end;

  TRecordsUpdateBuffer = array of TRecUpdateBuffer;

  PBufIndex = ^TBufIndex;
  TBufIndex = record
    Name            : String;
    Fields          : TField;
    FieldsName      : String;
    CaseinsFields   : String;
    DescFields      : String;
{$IFDEF ARRAYBUF}
    FCurrentRecInd  : integer;
    FRecordArray    : array of Pointer;
    FLastRecInd     : integer;
{$ELSE}
    FCurrentRecBuf  : PBufRecLinkItem;
    FLastRecBuf     : PBufRecLinkItem;
    FFirstRecBuf    : PBufRecLinkItem;
{$ENDIF ARRAYBUF}
    IndNr           : integer;
  end;

  TBufDataset = class(TDBDataSet)
  private
    FIndexes        : array of TBufIndex;
{$IFDEF ARRAYBUF}
    FInitialBuffers : integer;
    FGrowBuffer     : integer;
{$ELSE}
    FMaxIndexesCount: integer;
{$ENDIF ARRAYBUF}

    FIndexesCount   : integer;
    FCurrentIndex   : PBufIndex;

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

    procedure BuildIndex(var AIndex : TBufIndex);
    function GetIndexDefs : TIndexDefs;
{$IFDEF ARRAYBUF}
    procedure AddRecordToIndex(var AIndex: TBufIndex; ARecBuf: pchar);
{$ELSE}
    procedure AddRecordToIndex(var AIndex: TBufIndex; ARecBuf: PBufRecLinkItem);
{$ENDIF}
    function  GetCurrentBuffer: PChar;
    procedure CalcRecordSize;
    function GetIndexName: String;
    procedure InitialiseIndex(AIndex: TBufIndex);
    function LoadBuffer(Buffer : PChar): TGetResult;
    function GetFieldSize(FieldDef : TFieldDef) : longint;
    function GetRecordUpdateBuffer : boolean;
    procedure SetIndexName(const AValue: String);
{$IFNDEF ARRAYBUF}
    procedure SetMaxIndexesCount(const AValue: Integer);
{$ENDIF}
    procedure SetPacketRecords(aValue : integer);
    function  IntAllocRecordBuffer: PChar;
    procedure DoFilterRecord(var Acceptable: Boolean);
    procedure ParseFilter(const AFilter: string);
{$IFDEF ARRAYBUF}
    Function GetRecordFromBookmark(ABookmark: TBufBookmark) : integer;
{$ENDIF}
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
    procedure InternalInitRecord(Buffer: PChar); override;
    function  GetCanModify: Boolean; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function getnextpacket : integer;
    function GetRecordSize: Word; override;
    procedure InternalAddIndex(const AName, AFields : string); virtual;
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
    function Fetch : boolean; virtual; abstract;
    function LoadField(FieldDef : TFieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; virtual; abstract;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField); virtual; abstract;

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
    procedure AddIndex(const AName, AFields : string); virtual;
    property ChangeCount : Integer read GetChangeCount;
{$IFNDEF ARRAYBUF}
    property MaxIndexesCount : Integer read FMaxIndexesCount write SetMaxIndexesCount;
{$ENDIF ARRAYBUF}
  published
    property PacketRecords : Integer read FPacketRecords write SetPacketRecords default 10;
    property OnUpdateError: TResolverErrorEvent read FOnUpdateError write SetOnUpdateError;
    property IndexDefs : TIndexDefs read GetIndexDefs;
    property IndexName : String read GetIndexName write SetIndexName;
  end;

implementation

uses variants, dbconst;

type TCompareFunc = function(subValue, aValue: pointer; options: TLocateOptions): int64;

function DBCompareTextLen(substr, astr: pchar; len : integer; options: TLocateOptions): int64;

var
  i : integer; Chr1, Chr2: byte;
begin
  result := 0;
  i := 0;
  chr1 := 1;
  while (result=0) and (i<=len) and (chr1 <> 0) do
    begin
    Chr1 := byte(substr[i]);
    Chr2 := byte(astr[i]);
    inc(i);
    if loCaseInsensitive in options then
      begin
      if Chr1 in [97..122] then
        dec(Chr1,32);
      if Chr2 in [97..122] then
        dec(Chr2,32);
      end;
    result := Chr1 - Chr2;
    end;
  if (result <> 0) and (chr1 = 0) and (loPartialKey in options) then result := 0;
end;

function DBCompareText(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  DBCompareTextLen(subValue,aValue,Length(pchar(subValue)),options);
end;

function DBCompareByte(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PByte(aValue)^-PByte(subValue)^;
end;

function DBCompareSmallInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PSmallInt(aValue)^-PSmallInt(subValue)^;
end;

function DBCompareInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PInteger(aValue)^-PInteger(subValue)^;
end;

function DBCompareLargeInt(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PInt64(aValue)^-PInt64(subValue)^;
end;

function DBCompareWord(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PWord(aValue)^-PWord(subValue)^;
end;

function DBCompareQWord(subValue, aValue: pointer; options: TLocateOptions): LargeInt;

begin
  Result := PQWord(aValue)^-PQWord(subValue)^;
end;

function DBCompareDouble(subValue, aValue: pointer; options: TLocateOptions): LargeInt;
var Dbl : Double;
begin
  Dbl := PDouble(aValue)^-PDouble(subValue)^;
  if dbl < 0 then result := -1
  else if dbl > 0 then result := 1
  else result := 0;
end;

{ ---------------------------------------------------------------------
    TBufDataSet
  ---------------------------------------------------------------------}

constructor TBufDataset.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
{$IFDEF ARRAYBUF}
  FInitialBuffers:=10000;
  FGrowBuffer:=1000;
{$ELSE}
  FMaxIndexesCount:=2;
{$ENDIF}
  FIndexesCount:=0;
  InternalAddIndex('DEFAULT_ORDER','');
  FCurrentIndex:=@FIndexes[0];

  FIndexDefs := TIndexDefs.Create(Self);

  SetLength(FUpdateBuffer,0);
  SetLength(FBlobBuffers,0);
  SetLength(FUpdateBlobBuffers,0);
  BookmarkSize := sizeof(TBufBookmark);
  FParser := nil;
  FPacketRecords := 10;
end;

procedure TBufDataset.SetPacketRecords(aValue : integer);
begin
  if (aValue = -1) or (aValue > 0) then FPacketRecords := aValue
    else DatabaseError(SInvPacketRecordsValue);
end;

destructor TBufDataset.Destroy;
begin
  FreeAndNil(FIndexDefs);
  inherited destroy;
end;

procedure TBufDataset.BuildIndex(var AIndex: TBufIndex);
var PCurRecLinkItem : PBufRecLinkItem;
    p,l,q           : PBufRecLinkItem;
    i,k,psize,qsize : integer;
    MergeAmount     : integer;
    PlaceQRec       : boolean;
    Comparefunc     : TCompareFunc;

  procedure PlaceNewRec(var e: PBufRecLinkItem; var esize: integer);
  begin
    if AIndex.FFirstRecBuf=nil then
     begin
     AIndex.FFirstRecBuf:=e;
     e[AIndex.IndNr].prior:=nil;
     l:=e;
     end
   else
     begin
     l[AIndex.IndNr].next:=e;
     e[AIndex.IndNr].prior:=l;
     l:=e;
     end;
   e := e[AIndex.IndNr].next;
   dec(esize);
  end;

begin
// This simply copies the index...
{$IFNDEF ARRAYBUF}
  case AIndex.Fields.DataType of
    ftString : Comparefunc := @DBCompareText;
    ftSmallint : Comparefunc := @DBCompareSmallInt;
    ftInteger,ftCurrency,ftBCD : Comparefunc := @DBCompareInt;
    ftWord : Comparefunc := @DBCompareWord;
    ftBoolean : Comparefunc := @DBCompareByte;
    ftFloat : Comparefunc := @DBCompareDouble;
    ftDateTime,ftDate,ftTime : Comparefunc := @DBCompareDouble;
  else
    DatabaseErrorFmt(SErrIndexBasedOnInvField,[aindex.fields.Name]);
  end;

  PCurRecLinkItem:=FIndexes[0].FFirstRecBuf;
  PCurRecLinkItem[AIndex.IndNr].next := PCurRecLinkItem[0].next;
  PCurRecLinkItem[AIndex.IndNr].prior := PCurRecLinkItem[0].prior;

  if PCurRecLinkItem <> FIndexes[0].FLastRecBuf then
    begin
    while PCurRecLinkItem^.next<>FIndexes[0].FLastRecBuf do
      begin
      PCurRecLinkItem:=PCurRecLinkItem^.next;

      PCurRecLinkItem[AIndex.IndNr].next := PCurRecLinkItem[0].next;
      PCurRecLinkItem[AIndex.IndNr].prior := PCurRecLinkItem[0].prior;
      end;
    end;

// Set FirstRecBuf and FCurrentRecBuf
  AIndex.FFirstRecBuf:=FIndexes[0].FFirstRecBuf;
  AIndex.FCurrentRecBuf:=AIndex.FFirstRecBuf;
// Link in the FLastRecBuf that belongs to this index
  PCurRecLinkItem[AIndex.IndNr].next:=AIndex.FLastRecBuf;
  AIndex.FLastRecBuf[AIndex.IndNr].prior:=PCurRecLinkItem;

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

  p := AIndex.FFirstRecBuf;
  AIndex.ffirstRecBuf := nil;
  q := p;
  MergeAmount := 0;

// Then:
//    * If p is null, terminate this pass.
  while p <> AIndex.FLastRecBuf do
    begin

//    * Otherwise, there is at least one element in the next pair of length-K
//      lists, so increment the number of merges performed in this pass.

    inc(MergeAmount);

//    * Point another temporary pointer, q, at the same place as p. Step q along
//      the list by K places, or until the end of the list, whichever comes
//      first. Let psize be the number of elements you managed to step q past.

    i:=0;
    while (i<k) and (q<>AIndex.FLastRecBuf) do
      begin
      inc(i);
      q := q[AIndex.IndNr].next;
      end;
    psize :=i;

//    * Let qsize equal K. Now we need to merge a list starting at p, of length
//      psize, with a list starting at q of length at most qsize.

    qsize:=k;

//    * So, as long as either the p-list is non-empty (psize > 0) or the q-list
//      is non-empty (qsize > 0 and q points to something non-null):

    while (psize>0) or ((qsize>0) and (q <> AIndex.FLastRecBuf)) do
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
      else if (qsize=0) or (q = AIndex.FLastRecBuf) then
        PlaceQRec := False
      else if DBCompareText(pchar(p)+sizeof(TBufRecLinkItem)*FMaxIndexesCount+FFieldBufPositions[AIndex.Fields.FieldNo-1],pchar(q)+sizeof(TBufRecLinkItem)*FMaxIndexesCount+FFieldBufPositions[AIndex.Fields.FieldNo-1],[]) <= 0 then
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

  l[AIndex.IndNr].next:=AIndex.FLastRecBuf;

  k:=k*2;

  until MergeAmount = 1;
  AIndex.FLastRecBuf[AIndex.IndNr].next:=nil;
  AIndex.FLastRecBuf[AIndex.IndNr].prior:=l;

{$ENDIF}
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
    end;
end;

Function TBufDataset.GetCanModify: Boolean;
begin
  Result:= False;
end;

function TBufDataset.intAllocRecordBuffer: PChar;
begin
  // Note: Only the internal buffers of TDataset provide bookmark information
{$IFDEF ARRAYBUF}
  result := AllocMem(FRecordsize);
{$ELSE}
  result := AllocMem(FRecordsize+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
{$ENDIF}
end;

function TBufDataset.AllocRecordBuffer: PChar;
begin
  result := AllocMem(FRecordsize + sizeof(TBufBookmark) + CalcfieldsSize);
// The records are initialised, or else the fields of an empty, just-opened dataset
// are not null
  InitRecord(result);
end;

procedure TBufDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  ReAllocMem(Buffer,0);
end;

procedure TBufDataset.InternalOpen;

var IndexNr : integer;

begin
  CalcRecordSize;

  FBRecordcount := 0;

{$IFNDEF ARRAYBUF}
  for IndexNr:=0 to FIndexesCount-1 do with FIndexes[IndexNr] do
    begin
    FFirstRecBuf := pointer(IntAllocRecordBuffer);
    FLastRecBuf := FFirstRecBuf;
    FCurrentRecBuf := FLastRecBuf;
    end;
{$ELSE}
  for IndexNr:=0 to FIndexesCount-1 do with FIndexes[IndexNr] do
    begin
    FLastRecInd := 0;
    FCurrentRecInd := 0;
    FRecordArray[0] := IntAllocRecordBuffer;
    end;
{$ENDIF}

  FAllPacketsFetched := False;

  FOpen:=True;

  // parse filter expression
  try
    ParseFilter(Filter);
  except
    // oops, a problem with parsing, clear filter for now
    on E: Exception do Filter := EmptyStr;
  end;

end;

procedure TBufDataset.InternalClose;

var r  : integer;
{$IFNDEF ARRAYBUF}
    pc : pchar;
{$ENDIF}

begin
  FOpen:=False;
  with FIndexes[0] do
    begin
{$IFDEF ARRAYBUF}
    for r := 0 to FLastRecInd-1 do FreeRecordBuffer(FRecordArray[r]);
{$ELSE}
    FCurrentRecBuf := FFirstRecBuf;
    while assigned(FCurrentRecBuf) do
      begin
      pc := pointer(FCurrentRecBuf);
      FCurrentRecBuf := FCurrentRecBuf[IndNr].next;
      FreeRecordBuffer(pc);
      end;
{$ENDIF}
    end;

  for r := 0 to FIndexesCount-1 do with FIndexes[r] do
    begin
{$IFDEF ARRAYBUF}
    FreeRecordBuffer(FRecordArray[FLastRecInd]);
    SetLength(FRecordArray,FInitialBuffers);
{$ELSE}
    FFirstRecBuf:= nil;
{$ENDIF}
    end;

  if Length(FUpdateBuffer) > 0 then
    begin
    for r := 0 to length(FUpdateBuffer)-1 do with FUpdateBuffer[r] do
      begin
{$IFDEF ARRAYBUF}
      if Bookmark.BookmarkData > 0 then
{$ELSE}
      if assigned(BookmarkData) then
{$ENDIF}
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
// if FCurrentRecBuf = FLastRecBuf then the dataset is just opened and empty
// in which case InternalFirst should do nothing (bug 7211)
  with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
    if FCurrentRecInd <> FLastRecInd then
      FCurrentRecInd := -1;
{$ELSE}
    if FCurrentRecBuf <> FLastRecBuf then
      FCurrentRecBuf := nil;
{$ENDIF}
end;

procedure TBufDataset.InternalLast;
begin
  repeat
  until (getnextpacket < FPacketRecords) or (FPacketRecords = -1);
  with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
    if FLastRecInd <> 0 then FCurrentRecInd := FLastRecInd;
{$ELSE}
    if FLastRecBuf <> FFirstRecBuf then FCurrentRecBuf := FLastRecBuf;
{$ENDIF}
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

function TBufDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

var Acceptable : Boolean;
    SaveState: TDataSetState;


begin
  Result := grOK;
  with FCurrentIndex^ do
    begin
    repeat
    Acceptable := True;
    case GetMode of
      gmPrior :
{$IFDEF ARRAYBUF}
        if FCurrentRecInd=0 then
          Result := grBOF
        else
          Dec(FCurrentRecInd);
{$ELSE}
        if not assigned(FCurrentRecBuf[IndNr].prior) then
          begin
          Result := grBOF;
          end
        else
          begin
          FCurrentRecBuf := FCurrentRecBuf[IndNr].prior;
          end;
{$ENDIF}
      gmCurrent :
{$IFDEF ARRAYBUF}
        if FCurrentRecInd = FLastRecInd then
          Result := grError;
{$ELSE}
        if FCurrentRecBuf = FLastRecBuf then
          Result := grError;
{$ENDIF}
      gmNext :
{$IFDEF ARRAYBUF}
        if FCurrentRecInd = FLastRecInd then // Dataset is empty (just opened)
          begin
          if getnextpacket = 0 then result := grEOF;
          end
        else if FCurrentRecInd = -1 then FCurrentRecInd := 0
        else if FCurrentRecInd = FLastRecInd-1 then
          begin
          if getnextpacket > 0 then
            begin
            inc(FCurrentRecInd);
            end
          else
            begin
            result:=grEOF;
            end
          end
        else
          begin
          inc(FCurrentRecInd);
          end;
{$ELSE}
        if FCurrentRecBuf = FLastRecBuf then // Dataset is empty (just opened)
          begin
          if getnextpacket = 0 then result := grEOF;
          end
        else if FCurrentRecBuf = nil then FCurrentRecBuf := FFirstRecBuf
        else if (FCurrentRecBuf[IndNr].next = FLastRecBuf) then
          begin
          if getnextpacket > 0 then
            begin
            FCurrentRecBuf := FCurrentRecBuf[IndNr].next;
            end
          else
            begin
            result:=grEOF;
            end
          end
        else
          begin
          FCurrentRecBuf := FCurrentRecBuf[IndNr].next;
          end;
{$ENDIF}
    end;

    if Result = grOK then
      begin

      with PBufBookmark(Buffer + FRecordSize)^ do
        begin
{$IFDEF ARRAYBUF}
        BookmarkData := FCurrentIndex^.FCurrentRecInd;
        BookMarkBuf := FCurrentIndex^.FRecordArray[FCurrentIndex^.FCurrentRecInd];
{$ELSE}
        BookmarkData := FCurrentRecBuf;
{$ENDIF}
        BookmarkFlag := bfCurrent;
        end;
{$IFDEF ARRAYBUF}
      with FCurrentIndex^ do
        move((FRecordArray[FCurrentRecInd])^,buffer^,FRecordSize);
{$ELSE}
      move((pointer(FCurrentRecBuf)+sizeof(TBufRecLinkItem)*FMaxIndexesCount)^,buffer^,FRecordSize);
{$ENDIF}

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

function TBufDataset.GetRecordUpdateBuffer : boolean;

var x : integer;
{$IFDEF ARRAYBUF}
    ABookmark : TBufBookmark;
    CurrBuff  : Integer;
{$ELSE}
    CurrBuff : PChar;
{$ENDIF}

  function CompareBuf(const ABuf : integer) : boolean; inline;
  
  begin
{$IFDEF ARRAYBUF}
    result := (FUpdateBuffer[ABuf].Bookmark.BookMarkBuf<>nil) // Record is verwijderd, onmogelijk om UpdateBuffer te achterhalen.
          and (GetRecordFromBookmark(FUpdateBuffer[ABuf].Bookmark) = CurrBuff);
{$ELSE}
    result := (FUpdateBuffer[ABuf].BookmarkData = CurrBuff);
{$ENDIF}
  end;

begin
{$IFDEF ARRAYBUF}
  GetBookmarkData(ActiveBuffer,@ABookmark);
  CurrBuff:=GetRecordFromBookmark(ABookmark);
{$ELSE}
  GetBookmarkData(ActiveBuffer,@CurrBuff);
{$ENDIF}
  if (FCurrentUpdateBuffer >= length(FUpdateBuffer)) or not CompareBuf(FCurrentUpdateBuffer) then
   for x := 0 to high(FUpdateBuffer) do
    if CompareBuf(x) then
      begin
      FCurrentUpdateBuffer := x;
      break;
      end;
  Result := (FCurrentUpdateBuffer < length(FUpdateBuffer))  and CompareBuf(FCurrentUpdateBuffer);
end;

procedure TBufDataset.SetIndexName(const AValue: String);
var i : integer;
begin
  for i := 0 to FIndexesCount-1 do
    if SameText(FIndexes[i].Name,AValue) then
      begin
      FCurrentIndex:=@FIndexes[i];
      if active then Resync([rmCenter]);
      exit;
      end;
end;

{$IFNDEF ARRAYBUF}
procedure TBufDataset.SetMaxIndexesCount(const AValue: Integer);
begin
  CheckInactive;
  if AValue > 1 then
    FMaxIndexesCount:=AValue
  else
    DatabaseError(SMinIndexes);
end;
{$ENDIF}

procedure TBufDataset.InternalSetToRecord(Buffer: PChar);
begin
{$IFDEF ARRAYBUF}
  FCurrentIndex^.FCurrentRecInd:=GetRecordFromBookmark(PBufBookmark(Buffer + FRecordSize)^);
{$ELSE}
  FCurrentIndex^.FCurrentRecBuf := PBufBookmark(Buffer + FRecordSize)^.BookmarkData;
{$ENDIF}
end;

procedure TBufDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
{$IFDEF ARRAYBUF}
  PBufBookmark(Buffer + FRecordSize)^.BookmarkData := integer(Data^);
{$ELSE}
  PBufBookmark(Buffer + FRecordSize)^.BookmarkData := pointer(Data^);
{$ENDIF}
end;

procedure TBufDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PBufBookmark(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TBufDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
{$IFDEF ARRAYBUF}
  PBufBookmark(Data)^ := PBufBookmark(Buffer + FRecordSize)^;
{$ELSE}
  pointer(Data^) := PBufBookmark(Buffer + FRecordSize)^.BookmarkData;
{$ENDIF}
end;

function TBufDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PBufBookmark(Buffer + FRecordSize)^.BookmarkFlag;
end;

procedure TBufDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  // note that ABookMark should be a PBufBookmark. But this way it can also be
  // a pointer to a TBufRecLinkItem
{$IFDEF ARRAYBUF}
  FCurrentIndex^.FCurrentRecInd:=GetRecordFromBookmark(PBufBookmark(ABookmark)^);
{$ELSE}
  FCurrentIndex^.FCurrentRecBuf := pointer(ABookmark^);
{$ENDIF}
end;

{$IFDEF ARRAYBUF}
procedure TBufDataset.AddRecordToIndex(var AIndex: TBufIndex; ARecBuf : pchar);
{$ELSE}
procedure TBufDataset.AddRecordToIndex(var AIndex: TBufIndex; ARecBuf : PBufRecLinkItem);
{$ENDIF}
var cp : integer;
    NewValueBufLen : Integer;
{$IFDEF ARRAYBUF}
    NewValueBuf,CompValueBuf : pchar;
    RecInd : integer;
    HighVal,LowVal : Integer;
{$ELSE}
    NewValueBuf : pchar;
    CompBuf : PBufRecLinkItem;
{$ENDIF}
begin
  if not assigned(AIndex.Fields) then
    AIndex.Fields := FieldByName(AIndex.FieldsName);

  NewValueBuf:=pchar(ARecBuf);
  inc(NewValueBuf,FFieldBufPositions[AIndex.Fields.FieldNo-1]);

{$IFDEF ARRAYBUF}
  NewValueBufLen:= Length(NewValueBuf);
  HighVal := AIndex.FLastRecInd;
  LowVal := 0;

  repeat
  RecInd := lowval+((HighVal-LowVal) div 2);
  CompValueBuf:=AIndex.FRecordArray[RecInd]+FFieldBufPositions[AIndex.Fields.FieldNo-1];
  if AIndex.Fields.DataType = ftString then
    begin
    cp := DBCompareText(NewValueBuf,CompValueBuf,NewValueBufLen,[]);
    if cp >0 then
      LowVal := RecInd
    else
      HighVal := RecInd;
    end;
  until abs(HighVal-LowVal)<2;
  if cp <0 then RecInd:=RecInd else RecInd := RecInd+1;
  if recind > AIndex.FLastRecInd then recind := AIndex.FLastRecInd;
{
  Write('New: ' + NewValueBuf);
  Write(' Verg: ' + CompValueBuf);
  CompValueBuf:=AIndex.FRecordArray[LowVal]+FFieldBufPositions[AIndex.SortField.FieldNo-1];
  Write(' Low: ' + CompValueBuf + '('+inttostr(LowVal)+')');
  CompValueBuf:=AIndex.FRecordArray[HighVal]+FFieldBufPositions[AIndex.SortField.FieldNo-1];
  Write(' High: ' + CompValueBuf + '('+inttostr(HighVal)+')');
  CompValueBuf:=AIndex.FRecordArray[RecInd]+FFieldBufPositions[AIndex.SortField.FieldNo-1];
  Write(' RecIND: ' + CompValueBuf + '('+inttostr(RecInd)+')');
  Writeln(' cp: ' + inttostr(cp));
}

  if (AIndex.FLastRecInd+1) >= length(AIndex.FRecordArray) then
    SetLength(AIndex.FRecordArray,length(AIndex.FRecordArray)+FGrowBuffer);

  move(AIndex.FRecordArray[RecInd],AIndex.FRecordArray[RecInd+1],sizeof(pointer)*(AIndex.FLastRecInd-RecInd+5)); // Let op. Moet zijn +1?
  AIndex.FRecordArray[RecInd]:= ARecBuf;
  inc(AIndex.FLastRecInd)
{$ELSE}
  inc(NewValueBuf,sizeof(TBufRecLinkItem)*FMaxIndexesCount);
  NewValueBufLen:= Length(pchar(NewValueBuf));
  CompBuf:=AIndex.FFirstRecBuf;

  cp := 1;
  while (cp>0) and (CompBuf<>AIndex.FLastRecBuf) do
    begin
    if AIndex.Fields.DataType = ftString then
      begin
      cp := DBCompareTextLen(pointer(NewValueBuf),pchar(CompBuf)+sizeof(TBufRecLinkItem)*FMaxIndexesCount+FFieldBufPositions[AIndex.Fields.FieldNo-1],NewValueBufLen,[]);
      if cp > 0 then
        CompBuf := CompBuf[AIndex.IndNr].next;
      end;
    end;

  ARecBuf[AIndex.IndNr].next:= CompBuf;
  ARecBuf[AIndex.IndNr].prior:= CompBuf[AIndex.IndNr].prior;

  if assigned(CompBuf[AIndex.IndNr].prior) then
    CompBuf[AIndex.IndNr].prior[AIndex.IndNr].next := ARecBuf
  else
    AIndex.FFirstRecBuf:=ARecBuf;
  CompBuf[AIndex.IndNr].prior := ARecBuf;
{$ENDIF}
end;

function TBufDataset.getnextpacket : integer;

var i : integer;
    pb : pchar;
    IndexNr : integer;
    
begin
  if FAllPacketsFetched then
    begin
    result := 0;
    exit;
    end;
  i := 0;
{$IFDEF ARRAYBUF}
  with FCurrentIndex^ do
    pb := pchar(FRecordArray[FLastRecInd]);
{$ELSE}
  pb := pchar(pointer(FIndexes[0].FLastRecBuf)+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
{$ENDIF}
  while ((i < FPacketRecords) or (FPacketRecords = -1)) and (loadbuffer(pb) = grOk) do
    begin
{$IFDEF ARRAYBUF}
    with FIndexes[0] do
      begin
      inc(FLastRecInd);
      if FLastRecInd >= length(FRecordArray) then
        SetLength(FRecordArray,length(FRecordArray)+FGrowBuffer);
      FRecordArray[FLastRecInd]:=IntAllocRecordBuffer;
      end;

    for IndexNr:= 1 to FIndexesCount-1 do
      AddRecordToIndex(FIndexes[IndexNr],pb);

    pb := pchar(FCurrentIndex^.FRecordArray[FCurrentIndex^.FLastRecInd]);
{$ELSE}
    with FIndexes[0] do
      begin
      FLastRecBuf^.next := pointer(IntAllocRecordBuffer);
      FLastRecBuf^.next^.prior := FLastRecBuf;

      for IndexNr:= 1 to FIndexesCount-1 do
        AddRecordToIndex(FIndexes[IndexNr],FLastRecBuf);

      FLastRecBuf := FLastRecBuf^.next;

      pb := pchar(pointer(FLastRecBuf)+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
      end;
{$ENDIF}
    inc(i);
    end;
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
    ftFloat      : result := sizeof(double);
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
    if not GetRecordUpdateBuffer then
      begin
      // There is no old value available
      result := false;
      exit;
      end;
    currbuff := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer+sizeof(TBufRecLinkItem)*FMaxIndexesCount;
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
  if state = dsFilter then  // Set the value into the 'temporary' FLastRecBuf buffer for Locate and Lookup
    with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
      CurrBuff := FRecordArray[FLastRecInd]
{$ELSE}
      CurrBuff := pointer(FLastRecBuf) + sizeof(TBufRecLinkItem)*FMaxIndexesCount
{$ENDIF}
  else
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
{$IFDEF ARRAYBUF}
var ABookmark : TBufBookmark;
{$ENDIF}
begin
  InternalSetToRecord(ActiveBuffer);
{$IFNDEF ARRAYBUF}
  with FCurrentIndex^ do
    begin
    if FCurrentRecBuf <> FFirstRecBuf then FCurrentRecBuf^.prior^.next := FCurrentRecBuf^.next
    else FFirstRecBuf := FCurrentRecBuf^.next;

    FCurrentRecBuf^.next^.prior :=  FCurrentRecBuf^.prior;
    end;
{$ENDIF}

  if not GetRecordUpdateBuffer then
    begin
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    with FCurrentIndex^ do
      begin
{$IFDEF ARRAYBUF}
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := FRecordArray[FCurrentRecInd];
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookMarkBuf:=nil;
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookmarkData := FCurrentRecInd;
{$ELSE}
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := pchar(FCurrentRecBuf);
      FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := FCurrentRecBuf;

      FCurrentRecBuf := FCurrentRecBuf^.next;
{$ENDIF}
      end;
    end
  else with FCurrentIndex^ do
    begin
    if FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind = ukModify then
      begin
{$IFDEF ARRAYBUF}
      FreeRecordBuffer(FRecordArray[FCurrentRecInd]);
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookmarkData := FCurrentRecInd;
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookMarkBuf := nil;
{$ELSE}
      FCurrentRecBuf := FCurrentRecBuf^.next;
      FreeRecordBuffer(pchar(FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData));
      FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer;
{$ENDIF}
      end
    else
      begin
{$IFDEF ARRAYBUF}
      FreeRecordBuffer(pchar(FCurrentIndex^.FRecordArray[GetRecordFromBookmark(FUpdateBuffer[FCurrentUpdateBuffer].Bookmark)]));
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookmarkData := -1;  //this 'disables' the updatebuffer
{$ELSE}
      FCurrentRecBuf := FCurrentRecBuf^.next;
      FreeRecordBuffer(pchar(FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData));
      FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := nil;  //this 'disables' the updatebuffer
{$ENDIF}
      end;
    end;

{$IFDEF ARRAYBUF}
  with FCurrentIndex^ do
    begin
    Move(FRecordArray[FCurrentRecInd+1],FRecordArray[FCurrentRecInd],sizeof(Pointer)*(FLastRecInd-FCurrentRecInd));
    dec(FLastRecInd);
    end;
{$ENDIF}

  dec(FBRecordCount);
  FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukDelete;
end;


procedure TBufDataset.ApplyRecUpdate(UpdateKind : TUpdateKind);

begin
  raise EDatabaseError.Create(SApplyRecNotSupported);
end;

procedure TBufDataset.CancelUpdates;

var r              : Integer;
{$IFDEF ARRAYBUF}
    RecInd         : integer;
{$ENDIF}

begin
  CheckBrowseMode;

  if Length(FUpdateBuffer) > 0 then
    begin
    r := Length(FUpdateBuffer) -1;
    while r > -1 do with FUpdateBuffer[r] do
      begin
{$IFDEF ARRAYBUF}
      if FUpdateBuffer[r].Bookmark.BookmarkData <> -1 then
{$ELSE}
      if assigned(FUpdateBuffer[r].BookmarkData) then
{$ENDIF}
        begin
        if UpdateKind = ukModify then
          begin
{$IFDEF ARRAYBUF}
          with FCurrentIndex^ do
            begin
            FreeRecordBuffer(FRecordArray[Bookmark.BookmarkData]);
            FRecordArray[Bookmark.BookmarkData] := OldValuesBuffer;
            end;
{$ELSE}
          move(pchar(OldValuesBuffer+sizeof(TBufRecLinkItem)*FMaxIndexesCount)^,pchar(BookmarkData+sizeof(TBufRecLinkItem)*FMaxIndexesCount)^,FRecordSize);
          FreeRecordBuffer(OldValuesBuffer);
{$ENDIF}
          end
        else if UpdateKind = ukDelete then
          begin
{$IFDEF ARRAYBUF}
          RecInd := GetRecordFromBookmark(Bookmark);
          with FCurrentIndex^ do
            begin
            move(FRecordArray[RecInd],FRecordArray[RecInd+1],sizeof(Pointer)*(FLastRecInd-RecInd+1));
            FRecordArray[RecInd] := OldValuesBuffer;
            inc(FLastRecInd);
            end;
{$ELSE}
          if assigned(PBufRecLinkItem(BookmarkData)^.prior) then  // or else it was the first record
            PBufRecLinkItem(BookmarkData)^.prior^.next := BookmarkData
          else
            FCurrentIndex^.FFirstRecBuf := BookmarkData;
          PBufRecLinkItem(BookmarkData)^.next^.prior := BookmarkData;
{$ENDIF}
          inc(FBRecordCount);
          end
        else if UpdateKind = ukInsert then
          begin
{$IFDEF ARRAYBUF}
          RecInd := GetRecordFromBookmark(Bookmark);
          FreeRecordBuffer(FCurrentIndex^.FRecordArray[RecInd]);
          move(FCurrentIndex^.FRecordArray[RecInd+1],FCurrentIndex^.FRecordArray[RecInd],sizeof(Pointer)*(FCurrentIndex^.FLastRecInd-RecInd));
          dec(FCurrentIndex^.FLastRecInd);
{$ELSE}
          if assigned(PBufRecLinkItem(BookmarkData)^.prior) then // or else it was the first record
            PBufRecLinkItem(BookmarkData)^.prior^.next := PBufRecLinkItem(BookmarkData)^.next
          else
            FCurrentIndex^.FFirstRecBuf := PBufRecLinkItem(BookmarkData)^.next;
          PBufRecLinkItem(BookmarkData)^.next^.prior := PBufRecLinkItem(BookmarkData)^.prior;
          // resync won't work if the currentbuffer is freed...
          with FCurrentIndex^ do
          if FCurrentRecBuf = BookmarkData then FCurrentRecBuf := FCurrentRecBuf^.next;
          FreeRecordBuffer(BookmarkData);
{$ENDIF}
          dec(FBRecordCount);
          end;
        end;
      dec(r)
      end;

    SetLength(FUpdateBuffer,0);
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
    StoreRecBuf  : PBufRecLinkItem;
    AUpdateErr   : EUpdateError;

begin
{$IFDEF ARRAYBUF}
  DatabaseError('ApplyUpdates is not supported');
{$ELSE}
  CheckBrowseMode;

  StoreRecBuf := FCurrentIndex^.FCurrentRecBuf;

  r := 0;
  FailedCount := 0;
  Response := rrApply;
  try
    while (r < Length(FUpdateBuffer)) and (Response <> rrAbort) do
      begin
      if assigned(FUpdateBuffer[r].BookmarkData) then
        begin
        InternalGotoBookmark(@FUpdateBuffer[r].BookmarkData);
        Resync([rmExact,rmCenter]);
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
          FUpdateBuffer[r].BookmarkData := nil;
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

    FCurrentIndex^.FCurrentRecBuf := StoreRecBuf;
    Resync([]);
  end;
{$ENDIF}
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

Var tmpRecBuffer : PBufRecLinkItem;
    CurrBuff     : PChar;
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
      // Append
      with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
        FCurrentRecInd := FLastRecInd
{$ELSE}
        FCurrentRecBuf := FLastRecBuf
{$ENDIF}
    else
      // The active buffer is the newly created TDataset record,
      // from which the bookmark is set to the record where the new record should be
      // inserted
      InternalSetToRecord(ActiveBuffer);

    with FCurrentIndex^ do
      begin
{$IFDEF ARRAYBUF}
      inc(FLastRecInd);
      if FLastRecInd >= length(FRecordArray) then
        SetLength(FRecordArray,length(FRecordArray)+FGrowBuffer);
      Move(FRecordArray[FCurrentRecInd],FRecordArray[FCurrentRecInd+1],sizeof(Pointer)*(FLastRecInd-FCurrentRecInd));
      FRecordArray[FCurrentRecInd]:=pointer(IntAllocRecordBuffer);
{$ELSE}
    // Create the new record buffer
      tmpRecBuffer := FCurrentIndex^.FCurrentRecBuf^.prior;

      FCurrentRecBuf^.prior := pointer(IntAllocRecordBuffer);
      FCurrentRecBuf^.prior^.next := FCurrentRecBuf;
      FCurrentRecBuf := FCurrentRecBuf^.prior;
      If assigned(tmpRecBuffer) then // if not, it's the first record
        begin
        FCurrentRecBuf^.prior := tmpRecBuffer;
        tmpRecBuffer^.next := FCurrentRecBuf
        end
      else
        FFirstRecBuf := FCurrentRecBuf;
{$ENDIF}
      end;

    // Link the newly created record buffer to the newly created TDataset record
    with PBufBookmark(ActiveBuffer + FRecordSize)^ do
      begin
{$IFDEF ARRAYBUF}
      BookmarkData := FCurrentIndex^.FCurrentRecInd;
{$ELSE}
      BookmarkData := FCurrentIndex^.FCurrentRecBuf;
{$ENDIF}
      BookmarkFlag := bfInserted;
      end;
      
    inc(FBRecordCount);
    end
  else
    InternalSetToRecord(ActiveBuffer);

  if not GetRecordUpdateBuffer then
    begin
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    with FCurrentIndex^ do
      begin
{$IFDEF ARRAYBUF}
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookmarkData := FCurrentRecInd;
      FUpdateBuffer[FCurrentUpdateBuffer].Bookmark.BookMarkBuf := FRecordArray[FCurrentRecInd];
{$ELSE}
    FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := FCurrentRecBuf;
{$ENDIF}
      end;

    if state = dsEdit then
      begin
      // Update the oldvalues-buffer
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := intAllocRecordBuffer;
      with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
        move(FRecordArray[FCurrentRecInd]^,FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer^,FRecordSize);
{$ELSE}
        move(FCurrentRecBuf^,FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer^,FRecordSize+sizeof(TBufRecLinkItem)*FMaxIndexesCount);
{$ENDIF}
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukModify;
      end
    else
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukInsert;
    end;

  with FCurrentIndex^ do
{$IFDEF ARRAYBUF}
    move(ActiveBuffer^,FRecordArray[FCurrentRecInd]^,FRecordSize);
{$ELSE}
    CurrBuff := pchar(FCurrentRecBuf);
  inc(Currbuff,sizeof(TBufRecLinkItem)*FMaxIndexesCount);
  move(ActiveBuffer^,CurrBuff^,FRecordSize);
{$ENDIF}
end;

procedure TBufDataset.CalcRecordSize;

var x : longint;

begin
  FNullmaskSize := 1+((FieldDefs.count-1) div 8);
  FRecordSize := FNullmaskSize;
  SetLength(FFieldBufPositions,FieldDefs.count);
  for x := 0 to FieldDefs.count-1 do
    begin
    FFieldBufPositions[x] := FRecordSize;
    inc(FRecordSize, GetFieldSize(FieldDefs[x]));
    end;
end;

function TBufDataset.GetIndexName: String;
begin
  result := FCurrentIndex^.Name;
end;

function TBufDataset.GetRecordSize : Word;

begin
  result := FRecordSize + sizeof(TBufBookmark);
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
{$IFDEF ARRAYBUF}
    ABookMark    : TBufBookmark;
{$ELSE}
    recnr        : integer;
    TmpRecBuffer : PBufRecLinkItem;
{$ENDIF}

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
{$IFDEF ARRAYBUF}
  ABookMark.BookMarkBuf:=nil;
  ABookMark.BookmarkData:=Value-1;
  GotoBookmark(@ABookMark);
{$ELSE}
  TmpRecBuffer := FCurrentIndex^.FFirstRecBuf;
  for recnr := 1 to value-1 do
    TmpRecBuffer := TmpRecBuffer^.next;
  GotoBookmark(@TmpRecBuffer);
{$ENDIF}
end;

function TBufDataset.GetRecNo: Longint;

Var SearchRecBuffer : PBufRecLinkItem;
    TmpRecBuffer    : PBufRecLinkItem;
    recnr           : integer;
    abuf            : PChar;
{$IFDEF ARRAYBUF}
    ABookMark       : TBufBookmark;
{$ENDIF}

begin
  abuf := GetCurrentBuffer;
  // If abuf isn't assigned, the recordset probably isn't opened.
  if assigned(abuf) and (FBRecordCount>0) and (state <> dsInsert) then
    begin
{$IFDEF ARRAYBUF}
    GetBookmarkData(abuf,@ABookMark);
    recnr:=GetRecordFromBookmark(ABookMark);
    inc(recnr);
{$ELSE}
    GetBookmarkData(abuf,@SearchRecBuffer);
    TmpRecBuffer := FCurrentIndex^.FFirstRecBuf;
    recnr := 1;
    while TmpRecBuffer <> SearchRecBuffer do
      begin
      inc(recnr);
      TmpRecBuffer := TmpRecBuffer^.next;
      end;
{$ENDIF}
    result := recnr;
    end
  else result := 0;
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
  if GetRecordUpdateBuffer then
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

procedure TBufDataset.AddIndex(const AName, AFields: string);
begin
  if AFields='' then DatabaseError(SNoIndexFieldNameGiven);
  
{$IFNDEF ARRAYBUF}
  if active and (FIndexesCount=FMaxIndexesCount-1) then
    DatabaseError(SMaxIndexes);
{$ENDIF}

  InternalAddIndex(AName,AFields);
  // If not all packets are fetched, you can not sort properly.
  FPacketRecords:=-1;
end;

procedure TBufDataset.InternalAddIndex(const AName, AFields: string);
var StoreIndNr : Integer;
begin
  if FIndexesCount>0 then
    StoreIndNr:=FCurrentIndex^.IndNr
  else
    StoreIndNr:=0;
  inc(FIndexesCount);
  setlength(FIndexes,FIndexesCount); // This invalidates the currentindex!
  FCurrentIndex:=@FIndexes[StoreIndNr];
  InitialiseIndex(FIndexes[FIndexesCount-1]);
  FIndexes[FIndexesCount-1].Name:=AName;
  FIndexes[FIndexesCount-1].FieldsName:=AFields;
  FIndexes[FIndexesCount-1].IndNr:=FIndexesCount-1;
{$IFDEF ARRAYBUF}
  setlength(FIndexes[FIndexesCount-1].FRecordArray,FInitialBuffers);
{$ENDIF}
  if Active then
    begin
    FIndexes[FIndexesCount-1].Fields := FieldByName(AFields);
    FIndexes[FIndexesCount-1].FFirstRecBuf := pointer(IntAllocRecordBuffer);
    FIndexes[FIndexesCount-1].FLastRecBuf := FIndexes[FIndexesCount-1].FFirstRecBuf;
    FIndexes[FIndexesCount-1].FCurrentRecBuf := FIndexes[FIndexesCount-1].FLastRecBuf;
    BuildIndex(FIndexes[FIndexesCount-1]);
    end
{$IFNDEF ARRAYBUF}
  else if FIndexesCount>FMaxIndexesCount then
    FMaxIndexesCount := FIndexesCount;
{$ENDIF}
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

procedure TBufDataset.InitialiseIndex(AIndex : TBufIndex);
begin
{$IFDEF ARRAYBUF}
  AIndex.FRecordArray:=nil;
  AIndex.FCurrentRecInd:=-1;
  AIndex.FLastRecInd:=-1;
{$ENDIF}
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

{$IFDEF ARRAYBUF}
function TBufDataset.GetRecordFromBookmark(ABookMark: TBufBookmark) : integer;
begin
  // ABookmark.BookMarkBuf is nil if SetRecNo calls GotoBookmark
  if (ABookmark.BookMarkBuf<>nil) and (FCurrentIndex^.FRecordArray[ABookmark.BookmarkData]<>ABookmark.BookMarkBuf) then
    begin
    if ABookmark.BookmarkData > 2 then
      Result := ABookmark.BookmarkData-2
    else
      Result := 0;

    while (Result<FCurrentIndex^.FLastRecInd) do
      begin
      if (FCurrentIndex^.FRecordArray[Result] = ABookmark.BookMarkBuf) then exit;
      inc(Result);
      end;

    Result:=0;
    while (Result<ABookmark.BookmarkData) do
      begin
      if (FCurrentIndex^.FRecordArray[Result] = ABookmark.BookMarkBuf) then exit;
      inc(Result);
      end;

    DatabaseError(SInvalidBookmark,self)
    end
  else
    Result := ABookmark.BookmarkData;
end;
{$ENDIF}

Function TBufDataset.Locate(const KeyFields: string; const KeyValues: Variant; options: TLocateOptions) : boolean;

var keyfield    : TField;     // Field to search in
    ValueBuffer : pchar;      // Pointer to value to search for, in TField' internal format
    VBLength    : integer;

    FieldBufPos : PtrInt;     // Amount to add to the record buffer to get the FieldBuffer
    CurrLinkItem: PBufRecLinkItem;
    CurrBuff    : pchar;
    bm          : TBufBookmark;

    CheckNull   : Boolean;
    SaveState   : TDataSetState;

begin
{$IFDEF ARRAYBUF}
  DatabaseError('Locate is not supported');
{$ELSE}
// For now it is only possible to search in one field at the same time
  result := False;

  if IsEmpty then exit;

  keyfield := FieldByName(keyfields);
  CheckNull := VarIsNull(KeyValues);

  if not CheckNull then
    begin
    SaveState := State;
    SetTempState(dsFilter);
    keyfield.Value := KeyValues;
    RestoreState(SaveState);

    FieldBufPos := FFieldBufPositions[keyfield.FieldNo-1];
    VBLength := keyfield.DataSize;
    ValueBuffer := AllocMem(VBLength);
    currbuff := pointer(FCurrentIndex^.FLastRecBuf)+sizeof(TBufRecLinkItem)*FMaxIndexesCount+FieldBufPos;
    move(currbuff^,ValueBuffer^,VBLength);
    end;

  CurrLinkItem := FCurrentIndex^.FFirstRecBuf;

  if CheckNull then
    begin
    repeat
    currbuff := pointer(CurrLinkItem)+sizeof(TBufRecLinkItem)*FMaxIndexesCount;
    if GetFieldIsnull(pbyte(CurrBuff),keyfield.Fieldno-1) then
      begin
      result := True;
      break;
      end;
    CurrLinkItem := CurrLinkItem^.next;
    if CurrLinkItem = FCurrentIndex^.FLastRecBuf then getnextpacket;
    until CurrLinkItem = FCurrentIndex^.FLastRecBuf;
    end
  else if keyfield.DataType = ftString then
    begin
    repeat
    currbuff := pointer(CurrLinkItem)+sizeof(TBufRecLinkItem)*FMaxIndexesCount;
    if not GetFieldIsnull(pbyte(CurrBuff),keyfield.Fieldno-1) then
      begin
      inc(CurrBuff,FieldBufPos);
      if DBCompareTextLen(ValueBuffer,CurrBuff,VBLength,options) = 0 then
        begin
        result := True;
        break;
        end;
      end;
    CurrLinkItem := CurrLinkItem^.next;
    if CurrLinkItem = FCurrentIndex^.FLastRecBuf then getnextpacket;
    until CurrLinkItem = FCurrentIndex^.FLastRecBuf;
    end
  else
    begin
    repeat
    currbuff := pointer(CurrLinkItem)+sizeof(TBufRecLinkItem)*FMaxIndexesCount;
    if not GetFieldIsnull(pbyte(CurrBuff),keyfield.Fieldno-1) then
      begin
      inc(CurrBuff,FieldBufPos);
      if CompareByte(ValueBuffer^,CurrBuff^,VBLength) = 0 then
        begin
        result := True;
        break;
        end;
      end;

    CurrLinkItem := CurrLinkItem^.next;
    if CurrLinkItem = FCurrentIndex^.FLastRecBuf then getnextpacket;
    until CurrLinkItem = FCurrentIndex^.FLastRecBuf;
    end;


  if Result then
    begin
{$IFDEF ARRAYBUF}
//    bm.BookmarkData := CurrLinkItem;
{$ELSE}
    bm.BookmarkData := CurrLinkItem;
{$ENDIF}
    bm.BookmarkFlag := bfCurrent;
    GotoBookmark(@bm);
    end;

  ReAllocmem(ValueBuffer,0);
{$ENDIF}
end;

begin
end.
