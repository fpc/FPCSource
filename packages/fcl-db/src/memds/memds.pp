{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2007 by the Free Pascal development team
    Some modifications (c) 2007 by Martin Schreiber

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}
{
  TMemDataset : In-memory dataset.
  - Has possibility to copy Structure/Data from other dataset.
  - Can load/save to/from stream.
  Ideas taken from THKMemTab Component by Harri Kasulke - Hamburg/Germany
  E-mail: harri.kasulke@okay.net
}

unit memds;

interface

uses
 sysutils, classes, db, types;

const
  // Stream Markers.
  MarkerSize  = SizeOf(Integer);

  smEOF       = 0;
  smFieldDefs = 1;
  smData      = 2;

type
  {$IFNDEF FPC}
  {$i memdsdelphi.inc} // should set ptrint is longint|intptr
		       // & trecordbuffer ( if <2009)
  {$ENDIF}

  MDSError=class(Exception);

  PRecInfo=^TMTRecInfo;
  TMTRecInfo=record
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;

  { TMemDataset }

  TMemDataset=class(TDataSet)
  private
    FOpenStream : TStream;
    FFileName : String;
    FFileModified : Boolean;
    FStream: TMemoryStream;
    FRecInfoOffset: integer;
    FRecCount: integer;
    FRecSize: integer;
    FCurrRecNo: integer;
    FIsOpen: boolean;
    FTableIsCreated: boolean;
    FFilterBuffer: TRecordBuffer;
    ffieldoffsets: PInteger;
    ffieldsizes: PInteger;
    function GetRecordBufferPointer(p:TRecordBuffer; Pos:Integer):TRecordBuffer;
    function GetIntegerPointer(p:PInteger; Pos:Integer):PInteger;

    procedure calcrecordlayout;
    function  MDSGetRecordOffset(ARecNo: integer): longint;
    function  MDSGetFieldOffset(FieldNo: integer): integer;
    function  MDSGetBufferSize(FieldNo: integer): integer;
    function  MDSGetActiveBuffer(out Buffer: TRecordBuffer): Boolean;
    procedure MDSReadRecord(Buffer:TRecordBuffer;ARecNo:Integer);
    procedure MDSWriteRecord(Buffer:TRecordBuffer;ARecNo:Integer);
    procedure MDSAppendRecord(Buffer:TRecordBuffer);
    function  MDSFilterRecord(Buffer:TRecordBuffer): Boolean;
    function  MDSLocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; out ARecNo: integer): Boolean;
  protected
    // Mandatory
    function  AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function  GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function  IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    // Optional.
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;

    // Own.
    Procedure RaiseError(Fmt : String; Args : Array of const);
    Procedure CheckMarker(F : TStream; Marker : Integer);
    Procedure WriteMarker(F : TStream; Marker : Integer);
    procedure ReadFieldDefsFromStream(F : TStream);
    procedure SaveFieldDefsToStream(F : TStream);
    // These should be overridden if you want to load more data.
    // E.g. index defs.
    Procedure LoadDataFromStream(F : TStream); virtual;
    // If SaveData=False, a size 0 block should be written.
    Procedure SaveDataToStream(F : TStream; SaveData : Boolean); virtual;


  public
    constructor Create(AOwner:tComponent); override;
    destructor Destroy; override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    procedure CreateTable;

    Function  DataSize : Integer;

    procedure Clear(ClearDefs : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    procedure Clear;{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToFile(AFileName : String);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToFile(AFileName : String; SaveData : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToStream(F : TStream); {$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToStream(F : TStream; SaveData : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure LoadFromStream(F : TStream);
    Procedure LoadFromFile(AFileName : String);
    Procedure CopyFromDataset(DataSet : TDataSet); {$IFNDEF FPC} overload; {$ENDIF}
    Procedure CopyFromDataset(DataSet : TDataSet; CopyData : Boolean); {$IFNDEF FPC} overload; {$ENDIF}

    Property FileModified : Boolean Read FFileModified;

  published
    Property FileName : String Read FFileName Write FFileName;
    property Filtered;
    Property Active;
    Property FieldDefs;
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
    property OnNewRecord;
    property OnPostError;
    property OnFilterRecord;
  end;

implementation

uses
  Variants, FmtBCD;

ResourceString
  SErrFieldTypeNotSupported = 'Fieldtype of Field "%s" not supported.';
  SErrBookMarkNotFound      = 'Bookmark %d not found.';
  SErrInvalidDataStream     = 'Error in data stream at position %d';
  SErrInvalidMarkerAtPos    = 'Wrong data stream marker at position %d. Got %d, expected %d';
  SErrNoFileName            = 'Filename must not be empty.';

Const
  SizeRecInfo = SizeOf(TMTRecInfo);

procedure unsetfieldisnull(nullmask: pbyte; const x: integer);

begin
 inc(nullmask,(x shr 3));
 nullmask^:= nullmask^ or (1 shl (x and 7));
end;


procedure setfieldisnull(nullmask: pbyte; const x: integer);

begin
 inc(nullmask,(x shr 3));
 nullmask^:= nullmask^ and Not (1 shl (x and 7));
end;


function getfieldisnull(nullmask: pbyte; const x: integer): boolean;

begin
 inc(nullmask,(x shr 3));
 result:= nullmask^ and (1 shl (x and 7)) = 0;
end;


{ ---------------------------------------------------------------------
    Stream functions
  ---------------------------------------------------------------------}

Function ReadInteger(S : TStream) : Integer;

begin
  S.ReadBuffer(Result,SizeOf(Result));
end;

Function ReadString(S : TStream) : String;

Var
  L : Integer;

begin
  L:=ReadInteger(S);
  Setlength(Result,L);
  If (L<>0) then
    S.ReadBuffer(Result[1],L);
end;

Procedure WriteInteger(S : TStream; Value : Integer);

begin
  S.WriteBuffer(Value,SizeOf(Value));
end;

Procedure WriteString(S : TStream; Value : String);

Var
  L : Integer;

begin
  L:=Length(Value);
  WriteInteger(S,Length(Value));
  If (L<>0) then
    S.WriteBuffer(Value[1],L);
end;

{ ---------------------------------------------------------------------
    TMemDataset
  ---------------------------------------------------------------------}


constructor TMemDataset.Create(AOwner:tComponent);

begin
  inherited create(aOwner);
  FStream:=TMemoryStream.Create;
  FRecCount:=0;
  FRecSize:=0;
  FRecInfoOffset:=0;
  FCurrRecNo:=-1;
  BookmarkSize := sizeof(Longint);
  FIsOpen:=False;
end;

destructor TMemDataset.Destroy;
begin
  FStream.Free;
  FreeMem(FFieldOffsets);
  FreeMem(FFieldSizes);
  inherited Destroy;
end;

function TMemDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
var
  ReqBookmark: integer;
begin
  Result := False;
  if ABookMark=nil then exit;
  ReqBookmark:=PInteger(ABookmark)^;
  Result := (ReqBookmark>=0) and (ReqBookmark<FRecCount);
end;

function TMemDataset.MDSGetRecordOffset(ARecNo: integer): longint;
begin
  Result:=FRecSize*ARecNo
end;

function TMemDataset.MDSGetFieldOffset(FieldNo: integer): integer;
begin
 result:= getIntegerpointer(ffieldoffsets, fieldno-1)^;
end;

procedure TMemDataset.RaiseError(Fmt: String; Args: array of const);

begin
  Raise MDSError.CreateFmt(Fmt,Args);
end;

function TMemDataset.MDSGetBufferSize(FieldNo: integer): integer;
var
 FD: TFieldDef;
begin
 FD := FieldDefs.Items[FieldNo-1];
 case FD.DataType of
  ftString,
    ftGuid:   result:=FD.Size+1;
  ftFixedChar:result:=FD.Size+1;
  ftBoolean:  result:=SizeOf(Wordbool);
  ftCurrency,
  ftFloat:    result:=SizeOf(Double);
  ftBCD:      result:=SizeOf(currency);
  ftLargeInt: result:=SizeOf(int64);
  ftSmallInt: result:=SizeOf(SmallInt);
  ftWord,
  ftAutoInc,
  ftInteger:  result:=SizeOf(longint);
  ftDateTime,
    ftTime,
    ftDate:   result:=SizeOf(TDateTime);
  ftFmtBCD:   result:=SizeOf(TBCD);
  ftWideString,
  ftFixedWideChar: result:=(FD.Size+1)*SizeOf(WideChar);
  ftBytes:    result := FD.Size;
  ftVarBytes: result := FD.Size + SizeOf(Word);
 else
  RaiseError(SErrFieldTypeNotSupported,[FD.Name]);
 end;
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
 Result:=Align(Result,4);
{$ENDIF}
end;

function TMemDataset.MDSGetActiveBuffer(out Buffer: TRecordBuffer): Boolean;

begin
 case State of
   dsBrowse:
     if IsEmpty then
       Buffer:=nil
     else
       Buffer:=ActiveBuffer;
  dsEdit,
  dsInsert:
     Buffer:=ActiveBuffer;
  dsFilter:
     Buffer:=FFilterBuffer;
  dsCalcFields:
     Buffer:=CalcBuffer;
 else
   Buffer:=nil;
 end;
 Result:=(Buffer<>nil);
end;

procedure TMemDataset.MDSReadRecord(Buffer:TRecordBuffer;ARecNo:Integer);   //Reads a Rec from Stream in Buffer
begin
  FStream.Position:=MDSGetRecordOffset(ARecNo);
  FStream.ReadBuffer(Buffer^, FRecSize);
end;

procedure TMemDataset.MDSWriteRecord(Buffer:TRecordBuffer;ARecNo:Integer);  //Writes a Rec from Buffer to Stream
begin
  FStream.Position:=MDSGetRecordOffset(ARecNo);
  FStream.WriteBuffer(Buffer^, FRecSize);
  FFileModified:=True;
end;

procedure TMemDataset.MDSAppendRecord(Buffer:TRecordBuffer);   //Appends a Rec (from Buffer) to Stream
begin
  FStream.Position:=MDSGetRecordOffset(FRecCount);
  FStream.WriteBuffer(Buffer^, FRecSize);
  FFileModified:=True;
end;

//Abstract Overrides
function TMemDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecSize+CalcFieldsSize);
end;

procedure TMemDataset.FreeRecordBuffer (var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
end;

procedure TMemDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^,FRecSize,0);
end;

procedure TMemDataset.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

procedure TMemDataset.InternalDelete;

Var
  TS : TMemoryStream;

begin
  if (FCurrRecNo<0) or (FCurrRecNo>=FRecCount) then
    Exit;
  // Very inefficient. We should simply move the last part closer to the beginning in
  // The FStream.
  TS:=TMemoryStream.Create;
  Try
    if FCurrRecNo>0 then
      begin
      FStream.Position:=MDSGetRecordOffset(0);      //Delete Rec
      if FCurrRecNo<FRecCount-1 then
        begin
        TS.CopyFrom(FStream, MDSGetRecordOffset(FCurrRecNo)-MDSGetRecordOffset(0));
        FStream.Position:=MDSGetRecordOffset(FCurrRecNo+1);
        TS.CopyFrom(FStream,(MDSGetRecordOffset(FRecCount))-MDSGetRecordOffset(FCurrRecNo+1));
        end
      else
        TS.CopyFrom(FStream,MDSGetRecordOffset(FRecCount-1));
      end
    else
      begin                                  //Delete first Rec
      FStream.Position:=MDSGetRecordOffset(FCurrRecNo+1);
      TS.CopyFrom(FStream,(MDSGetRecordOffset(FRecCount))-MDSGetRecordOffset(FCurrRecNo+1));
      end;
    FStream.LoadFromStream(TS);
    Dec(FRecCount);
    if FRecCount=0 then
      FCurrRecNo:=-1
    else
      if FCurrRecNo>=FRecCount then FCurrRecNo:=FRecCount-1;
  Finally
    TS.Free;
  end;
  FFileModified:=True;
end;

procedure TMemDataset.InternalInitFieldDefs;

begin
  If (FOpenStream<>Nil) then
    ReadFieldDefsFromStream(FOpenStream);
end;

procedure TMemDataset.CheckMarker(F: TStream; Marker: Integer);

Var
  I,P : Integer;

begin
  P:=F.Position;
  If F.Read(I,MarkerSize)<>MarkerSize then
    RaiseError(SErrInvalidDataStream,[P])
  else
    if (I<>Marker) then
      RaiseError(SErrInvalidMarkerAtPos,[P,I,Marker]);
end;

procedure TMemDataset.ReadFieldDefsFromStream(F : TStream);

Var
  I,ACount : Integer;
  FN : String;
  FS : Integer;
  B : Boolean;
  FT : TFieldType;

begin
  CheckMarker(F,smFieldDefs);
  FieldDefs.Clear;
  ACount:=ReadInteger(F);
  For I:=1 to ACount do
    begin
    FN:=ReadString(F);
    FS:=ReadInteger(F);
    FT:=TFieldType(ReadInteger(F));
    B:=ReadInteger(F)<>0;
    TFieldDef.Create(FieldDefs,FN,ft,FS,B,I);
    end;
  FTableIsCreated:=False;
end;

procedure TMemDataset.InternalFirst;
begin
  FCurrRecNo:=-1;
end;

procedure TMemDataset.InternalLast;
begin
  FCurrRecNo:=FRecCount;
end;

procedure TMemDataset.InternalOpen;

begin
  If (FFileName<>'') then
    FOpenStream:=TFileStream.Create(FFileName,fmOpenRead);
  Try
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(True); // BindFields computes CalcFieldsSize
    if not FTableIsCreated then
      CreateTable;
    FCurrRecNo:=-1;
    If (FOpenStream<>Nil) then
      begin
      LoadDataFromStream(FOpenStream);
      CheckMarker(FOpenStream,smEOF);
      end;
  Finally
    FreeAndNil(FOpenStream);
  end;
  FIsOpen:=True;
end;

procedure TMemDataset.LoadDataFromStream(F: TStream);

Var
  Size : Integer;

begin
  CheckMarker(F,smData);
  Size:=ReadInteger(F);
  FStream.Clear;
  FStream.CopyFrom(F,Size);
  FRecCount:=Size div FRecSize;
  FCurrRecNo:=-1;
end;

procedure TMemDataset.LoadFromStream(F: TStream);

begin
  Close;
  ReadFieldDefsFromStream(F);
  CreateTable;
  LoadDataFromStream(F);
  CheckMarker(F,smEOF);
  FFileModified:=False;
end;

procedure TMemDataset.LoadFromFile(AFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    LoadFromStream(F);
  Finally
    F.Free;
  end;
end;


procedure TMemDataset.SaveToFile(AFileName: String);

begin
  SaveToFile(AFileName,True);
end;

procedure TMemDataset.SaveToFile(AFileName: String; SaveData: Boolean);

Var
  F : TFileStream;

begin
  If (AFileName='') then
    RaiseError(SErrNoFileName,[]);
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(F,SaveData);
  Finally
    F.Free;
  end;
end;

procedure TMemDataset.WriteMarker(F: TStream; Marker: Integer);

begin
  Writeinteger(F,Marker);
end;

procedure TMemDataset.SaveToStream(F: TStream);

begin
  SaveToStream(F,True);
end;

procedure TMemDataset.SaveToStream(F: TStream; SaveData: Boolean);

begin
  SaveFieldDefsToStream(F);
  If SaveData then
    SaveDataToStream(F,SaveData);
  WriteMarker(F,smEOF);
end;

procedure TMemDataset.SaveFieldDefsToStream(F: TStream);

Var
  I : Integer;
  FD : TFieldDef;

begin
  WriteMarker(F,smFieldDefs);
  WriteInteger(F,FieldDefs.Count);
  For I:=1 to FieldDefs.Count do
    begin
    FD:=FieldDefs[I-1];
    WriteString(F,FD.Name);
    WriteInteger(F,FD.Size);
    WriteInteger(F,Ord(FD.DataType));
    WriteInteger(F,Ord(FD.Required));
    end;
end;

procedure TMemDataset.SaveDataToStream(F: TStream; SaveData: Boolean);

begin
  if SaveData then
    begin
    WriteMarker(F,smData);
    WriteInteger(F,FStream.Size);
    FStream.Position:=0;
    F.CopyFrom(FStream,FStream.Size);
    FFileModified:=False;
    end
  else
    begin
    WriteMarker(F,smData);
    WriteInteger(F,0);
    end;
end;

procedure TMemDataset.InternalClose;

begin
 if (FFileModified) and (FFileName<>'') then begin
  SaveToFile(FFileName,True);
 end;
 FIsOpen:=False;
 FFileModified:=False;
 // BindFields(False);
 if DefaultFields then begin
  DestroyFields;
 end;
end;

procedure TMemDataset.InternalPost;
begin
  CheckActive;
  if not (State in [dsEdit, dsInsert]) then
    Exit;
  inherited InternalPost;
  if (State=dsEdit) then
    MDSWriteRecord(ActiveBuffer, FCurrRecNo)
  else
    InternalAddRecord(ActiveBuffer,True);
end;

function TMemDataset.IsCursorOpen: Boolean;

begin
  Result:=FIsOpen;
end;

function TMemDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

var
  Accepted: Boolean;

begin
  Result:=grOk;
  Accepted:=False;
  if (FRecCount<1) then
    begin
    Result:=grEOF;
    exit;
    end;
  repeat
    case GetMode of
      gmCurrent:
        if (FCurrRecNo>=FRecCount) or (FCurrRecNo<0) then
          Result:=grError;
      gmNext:
        if (FCurrRecNo<FRecCount-1) then
          Inc(FCurrRecNo)
        else
          Result:=grEOF;
      gmPrior:
        if (FCurrRecNo>0) then
          Dec(FCurrRecNo)
        else
          result:=grBOF;
    end;
    if result=grOK then
      begin
      MDSReadRecord(Buffer, FCurrRecNo);
      PRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=FCurrRecNo;
      PRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag:=bfCurrent;
      GetCalcFields(Buffer);
      if (Filtered) then
        Accepted:=MDSFilterRecord(Buffer) //Filtering
      else
        Accepted:=True;
      if (GetMode=gmCurrent) and not Accepted then
        result:=grError;
      end;
  until (result<>grOK) or Accepted;
end;

function TMemDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
 SrcBuffer: TRecordBuffer;
 I: integer;
begin
 I:= Field.FieldNo - 1;
 result := MDSGetActiveBuffer(SrcBuffer);
 if not result then Exit;

 if I >= 0 then
   begin
   result := not getfieldisnull(pointer(srcbuffer),I);
   if result and assigned(Buffer) then
     Move(GetRecordBufferPointer(SrcBuffer, GetIntegerPointer(ffieldoffsets,I)^)^, Buffer^, GetIntegerPointer(FFieldSizes, I)^);
   end
 else // Calculated, Lookup
   begin
   Inc(SrcBuffer, RecordSize + Field.Offset);
   result := Boolean(SrcBuffer[0]);
   if result and assigned(Buffer) then
     Move(SrcBuffer[1], Buffer^, Field.DataSize);
   end;
end;

procedure TMemDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
 DestBuffer: TRecordBuffer;
 I,J: integer;

begin
 I:= Field.FieldNo - 1;
 if not MDSGetActiveBuffer(DestBuffer) then Exit;

 if I >= 0 then
   begin
   if State in [dsEdit, dsInsert, dsNewValue] then
     Field.Validate(Buffer);
   if Buffer = nil then
     setfieldisnull(pointer(DestBuffer),I)
   else
     begin
     unsetfieldisnull(pointer(DestBuffer),I);
     J:=GetIntegerPointer(FFieldSizes, I)^;
     if Field.DataType=ftString then
       Dec(J); // Do not move terminating 0, which is in the size.
     Move(Buffer^, GetRecordBufferPointer(DestBuffer, getIntegerPointer(FFieldOffsets, I)^)^, J);
     end;
   end
 else // Calculated, Lookup
   begin
   Inc(DestBuffer, RecordSize + Field.Offset);
   Boolean(DestBuffer[0]) := Buffer <> nil;
   if assigned(Buffer) then
     Move(Buffer^, DestBuffer[1], Field.DataSize);
   end;

 if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
   DataEvent(deFieldChange, PtrInt(Field));
end;

function TMemDataset.GetRecordSize: Word;

begin
 Result:= FRecSize;
end;

procedure TMemDataset.InternalGotoBookmark(ABookmark: Pointer);

var
  ReqBookmark: integer;

begin
  ReqBookmark:=PInteger(ABookmark)^;
  if (ReqBookmark>=0) and (ReqBookmark<FRecCount) then
    FCurrRecNo:=ReqBookmark
  else
    RaiseError(SErrBookMarkNotFound,[ReqBookmark]);
end;

procedure TMemDataset.InternalSetToRecord(Buffer: TRecordBuffer);

var
  ReqBookmark: integer;

begin
  ReqBookmark:=PRecInfo(Buffer+FRecInfoOffset)^.Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

function TMemDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;

begin
  Result:=PRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag;
end;

procedure TMemDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);

begin
  PRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag := Value;
end;

procedure TMemDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);

begin
  if Data<>nil then
    PInteger(Data)^:=PRecInfo(Buffer+FRecInfoOffset)^.Bookmark;
end;

procedure TMemDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);

begin
  if Data<>nil then
    PRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=PInteger(Data)^
  else
    PRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=0;
end;

function TMemDataset.MDSFilterRecord(Buffer: TRecordBuffer): Boolean;

var
  SaveState: TDatasetState;

begin
  Result:=True;
  if not Assigned(OnFilterRecord) then
    Exit;
  SaveState:=SetTempState(dsFilter);
  Try
    FFilterBuffer:=Buffer;
    OnFilterRecord(Self,Result);
  Finally  
    RestoreState(SaveState);
  end;  
end;

function TMemDataset.DataSize: Integer;

begin
  Result:=FStream.Size;
end;

procedure TMemDataset.Clear;

begin
  Clear(True);
end;

procedure TMemDataset.Clear(ClearDefs : Boolean);

begin
  FStream.Clear;
  FRecCount:=0;
  FCurrRecNo:=-1;
  if Active then
    Resync([]);
  If ClearDefs then
    begin
    Close;
    FieldDefs.Clear;
    FTableIsCreated:=False;
    end;
end;

procedure TMemDataset.calcrecordlayout;
var
  i,Count : integer;
begin
 Count := FieldDefs.Count;
 // Avoid mem-leak if CreateTable is called twice
 FreeMem(FFieldOffsets);
 Freemem(FFieldSizes);
 {$IFDEF FPC}
 FFieldOffsets:=getmem(Count*sizeof(integer));
 FFieldSizes:=getmem(Count*sizeof(integer));
 {$ELSE}
 getmem(FFieldOffsets, Count*sizeof(integer));
 getmem(FFieldSizes, Count*sizeof(integer));
 {$ENDIF}
 FRecSize:= (Count+7) div 8; //null mask
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
 FRecSize:=Align(FRecSize,4);
{$ENDIF}
 for i:= 0 to Count-1 do
   begin
   GetIntegerPointer(FFieldOffsets, i)^ := FRecSize;
   GetIntegerPointer(FFieldSizes,   i)^ := MDSGetbufferSize(i+1);
   FRecSize:= FRecSize+GetIntegerPointer(FFieldSizes, i)^;
   end;
 FRecInfoOffset:=FRecSize;
 FRecSize:=FRecSize+SizeRecInfo;
end;

procedure TMemDataset.CreateTable;

begin
  CheckInactive;
  FStream.Clear;
  FRecCount:=0;
  FCurrRecNo:=-1;
  FIsOpen:=False;
  calcrecordlayout;
  FTableIsCreated:=True;
end;

procedure TMemDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);

begin
  MDSAppendRecord(ActiveBuffer);
  InternalLast;
  Inc(FRecCount);
end;

procedure TMemDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value>=1) and (Value<=FRecCount) then
    begin
    FCurrRecNo:=Value-1;
    Resync([]);
    end;
end;

function TMemDataset.GetRecNo: Integer;

begin
  UpdateCursorPos;
  if (FCurrRecNo<0) or (FRecCount=0) or (State=dsInsert) then
    Result:=0
  else
    Result:=FCurrRecNo+1;
end;

function TMemDataset.GetRecordCount: Integer;

begin
  CheckActive;
  Result:=FRecCount;
end;

procedure TMemDataset.CopyFromDataset(DataSet: TDataSet);

begin
  CopyFromDataset(Dataset,True);
end;

procedure TMemDataset.CopyFromDataset(DataSet: TDataSet; CopyData: Boolean);

Var
  I  : Integer;
  F,F1,F2 : TField;
  L1,L2  : TList;
  N : String;

begin
  Clear(True);
  // NOT from FieldDefs. The data may not be available in buffers !!
  For I:=0 to Dataset.FieldCount-1 do
    begin
    F:=Dataset.Fields[I];
    TFieldDef.Create(FieldDefs,F.FieldName,F.DataType,F.Size,F.Required,F.FieldNo);
    end;
  CreateTable;
  If CopyData then
    begin
    Open;
    L1:=TList.Create;
    Try
      L2:=TList.Create;
      Try
        For I:=0 to FieldDefs.Count-1 do
          begin
          N:=FieldDefs[I].Name;
          F1:=FieldByName(N);
          F2:=DataSet.FieldByName(N);
          L1.Add(F1);
          L2.Add(F2);
          end;
        Dataset.DisableControls;
        Try
          Dataset.Open;
          Dataset.First; //make sure we copy from the beginning
          While not Dataset.EOF do
            begin
            Append;
            For I:=0 to L1.Count-1 do
              begin
              F1:=TField(L1[i]);
              F2:=TField(L2[I]);
              Case F1.DataType of
                ftFixedChar,
                ftString   : F1.AsString:=F2.AsString;
                ftBoolean  : F1.AsBoolean:=F2.AsBoolean;
                ftFloat    : F1.AsFloat:=F2.AsFloat;
                ftLargeInt : F1.AsInteger:=F2.AsInteger;
                ftSmallInt : F1.AsInteger:=F2.AsInteger;
                ftInteger  : F1.AsInteger:=F2.AsInteger;
                ftDate     : F1.AsDateTime:=F2.AsDateTime;
                ftTime     : F1.AsDateTime:=F2.AsDateTime;
                ftDateTime : F1.AsDateTime:=F2.AsDateTime;
                else         F1.AsString:=F2.AsString;
              end;
              end;
            Try
              Post;
            except
              Cancel;
              Raise;
            end;
            Dataset.Next;
            end;
        Finally
          Dataset.EnableControls;
        end;
      finally
        L2.Free;
      end;
    finally
      l1.Free;
    end;
    end;
end;

function TMemDataset.GetRecordBufferPointer(p:TRecordBuffer; Pos:Integer):TRecordBuffer;
begin
  Result:=p;
  inc(Result, Pos);
end;

function TMemDataset.GetIntegerPointer(p:PInteger; Pos:Integer):PInteger;
begin
  Result:=p;
  inc(Result, Pos);
end;

function TMemDataset.MDSLocateRecord(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions; out ARecNo: integer): Boolean;
var
  SaveState: TDataSetState;
  lstKeyFields: TList;
  Matched: boolean;
  AKeyValues: variant;
  i: integer;
  AField: TField;
  s1,s2: string;
begin
  Result := false;
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := TempBuffer;
  lstKeyFields := TList.Create;
  try
    GetFieldList(lstKeyFields, KeyFields);
    if VarArrayDimCount(KeyValues) = 0 then
      begin
      Matched := lstKeyFields.Count = 1;
      AKeyValues := VarArrayOf([KeyValues]);
      end
    else if VarArrayDimCount(KeyValues) = 1 then
      begin
      Matched := VarArrayHighBound(KeyValues,1) + 1 = lstKeyFields.Count;
      AKeyValues := KeyValues;
      end
    else
      Matched := false;

    if Matched then
    begin
      ARecNo:=0;
      while ARecNo<FRecCount do
      begin
        MDSReadRecord(FFilterBuffer, ARecNo);
        if Filtered then
          Result:=MDSFilterRecord(FFilterBuffer)
        else
          Result:=true;
        // compare field by field
        i:=0;
        while Result and (i<lstKeyFields.Count) do
        begin
          AField := TField(lstKeyFields[i]);
          // string fields
          if AField.DataType in [ftString, ftFixedChar] then
          begin
            s1 := AField.AsString;
            s2 := VarToStr(AKeyValues[i]);
            if loPartialKey in Options then
              s1 := copy(s1, 1, length(s2));
            if loCaseInsensitive in Options then
              Result := AnsiCompareText(s1, s2)=0
            else
              Result := s1=s2;
          end
          // all other fields
          else
            Result := AField.Value=AKeyValues[i];
          inc(i);
        end;
        if Result then
          break;
        inc(ARecNo);
      end;
    end;
  finally
    lstKeyFields.Free;
    RestoreState(SaveState);
  end;
end;

function TMemDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): boolean;
var
  ARecNo: integer;
begin
  // Call inherited to make sure the dataset is bi-directional
  Result := inherited;
  CheckActive;

  Result:=MDSLocateRecord(KeyFields, KeyValues, Options, ARecNo);
  if Result then begin
    // TODO: generate scroll events if matched record is found
    FCurrRecNo:=ARecNo;
    Resync([]);
  end;
end;

function TMemDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  ARecNo: integer;
  SaveState: TDataSetState;
begin
  if MDSLocateRecord(KeyFields, KeyValues, [], ARecNo) then
  begin
    SaveState := SetTempState(dsCalcFields);
    try
      // FFilterBuffer contains found record
      CalculateFields(FFilterBuffer); // CalcBuffer is set to FFilterBuffer
      Result:=FieldValues[ResultFields];
    finally
      RestoreState(SaveState);
    end;
  end
  else
    Result:=Null;
end;

end.
