{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    BufDataset implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
    TBufDataSet
  ---------------------------------------------------------------------}

constructor TBufDataset.Create(AOwner : TComponent);

begin
  Inherited Create(AOwner);
// temporary set it here
  FPacketRecords := 10;
end;

destructor TBufDataset.Destroy;

begin
  inherited destroy;
end;

Function TBufDataset.GetCanModify: Boolean;

begin
  Result:= False;
end;

function TBufDataset.AllocRecordBuffer: PChar;

begin
  result := AllocMem(FRecordsize + sizeof(TBufBookmark));
end;

procedure TBufDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  ReAllocMem(Buffer,0);
end;

procedure TBufDataset.InternalOpen;

begin
  CalcRecordSize;

  FBRecordcount := 0;
  FBBuffercount := 0;
  FBCurrentrecord := -1;
  FOpen:=True;
  FIsEOF := false;
  FIsbOF := true;
end;

procedure TBufDataset.InternalClose;

var i : integer;

begin
  FOpen:=False;
  for i := 0 to FBRecordCount-1 do FreeRecordBuffer(FBBuffers[i]);
  If FBRecordCount > 0 then ReAllocMem(FBBuffers,0);
  FBRecordcount := 0;
  FBBuffercount := 0;
  FBCurrentrecord := -1;
  FIsEOF := true;
  FIsbOF := true;
end;

procedure TBufDataset.InternalFirst;
begin
  FBCurrentRecord := -1;
  FIsEOF := false;
end;

procedure TBufDataset.InternalLast;
begin
  repeat
  until getnextpacket < FPacketRecords;
  FIsBOF := false;
  FBCurrentRecord := FBRecordcount;
end;

function TBufDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  begin
    Result := grOK;
    case GetMode of
      gmPrior :
        if FIsBOF then
          result := grBOF
        else if FBCurrentRecord <= 0 then
          begin
          Result := grBOF;
          FBCurrentRecord := -1;
          end
        else
          begin
          Dec(FBCurrentRecord);
          FIsEof := false;
          end;
      gmCurrent :
        if (FBCurrentRecord < 0) or (FBCurrentRecord >= RecordCount) then
          Result := grError;
      gmNext :
        if FIsEOF then
          result := grEOF
        else if FBCurrentRecord >= (FBRecordCount - 1) then
          begin
          if getnextpacket > 0 then
            begin
            Inc(FBCurrentRecord);
            FIsBof := false;
            end
          else
            begin
            FIsEOF := true;
            result:=grEOF;
            end
          end
        else
          begin
          Inc(FBCurrentRecord);
          FIsBof := false;
          end;
    end;
  end;

  if Result = grOK then
    begin
    with PBufBookmark(Buffer + RecordSize)^ do
      begin
      BookmarkData := FBCurrentRecord;
      BookmarkFlag := bfCurrent;
      end;
      move(FBBuffers[FBCurrentRecord]^,buffer^,RecordSize);
    end
  else if (Result = grError) and doCheck then
    DatabaseError('No record');
end;

procedure TBufDataset.InternalSetToRecord(Buffer: PChar);
begin
  FBCurrentRecord := PBufBookmark(Buffer + RecordSize)^.BookmarkData;
  FIsEOF := False;
  FIsBOF := False;
end;

procedure TBufDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PBufBookmark(Buffer + RecordSize)^.BookmarkData := PInteger(Data)^;
end;

procedure TBufDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PBufBookmark(Buffer + RecordSize)^.BookmarkFlag := Value;
end;

procedure TBufDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PBufBookmark(Buffer + RecordSize)^.BookmarkData;
end;

function TBufDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PBufBookmark(Buffer + RecordSize)^.BookmarkFlag;
end;

procedure TBufDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FBCurrentRecord := Plongint(ABookmark)^;
  FIsEOF := False;
  FIsBOF := False;
end;

function TBufDataset.getnextpacket : integer;

var i : integer;
    b : boolean;

begin
  i := 0;
  if FPacketRecords > 0 then
    begin
    FBBufferCount := FBBuffercount + FPacketRecords;
    ReAllocMem(FBBuffers,FBBuffercount*SizeOf(PChar));

    repeat
    FBBuffers[FBRecordCount+i] := AllocRecordBuffer;
    b := (loadbuffer(FBBuffers[FBRecordCount+i])<>grOk);
    inc(i);
    until (i = FPacketRecords) or b;
    if b then
      begin
      dec(i);
      FreeRecordBuffer(FBBuffers[FBRecordCount+i]);
      end;
    FBRecordCount := FBRecordCount + i;
    end;
  result := i;
end;

function TBufDataset.GetFieldSize(FieldDef : TFieldDef) : longint;

begin
  case FieldDef.DataType of
    ftString     : result := FieldDef.Size + 1;
    ftSmallint,
      ftInteger,
      ftword     : result := sizeof(longint);
    ftBoolean    : result := sizeof(boolean);
    ftBCD        : result := sizeof(currency);
    ftFloat      : result := sizeof(double);
    ftTime,
      ftDate,
      ftDateTime : result := sizeof(TDateTime)
  else Result := 10
  end;
  
end;

function TBufDataset.LoadBuffer(Buffer : PChar): TGetResult;

var NullMask     : pbyte;
    x            : longint;

begin
  if not Fetch then
    begin
    Result := grEOF;
    Exit;
    end;

  NullMask := pointer(buffer);
  fillchar(Nullmask^,FNullmaskSize,0);

  inc(buffer,FNullmaskSize);

  for x := 0 to FieldDefs.count-1 do
    begin
    if not LoadField(FieldDefs[x],buffer) then
      NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));

    inc(buffer,GetFieldSize(FieldDefs[x]));
    end;
  Result := grOK;
end;

function TBufDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

var
  x        : longint;
  CurrBuff : pchar;

begin
  Result := False;
  
  If Field.Fieldno > 0 then // If = 0, then calculated field or something similar
    begin
    CurrBuff := ActiveBuffer;

    if ord(currbuff[(Field.Fieldno-1) div 8]) and (1 shl ((Field.Fieldno-1) mod 8)) > 0 then
      begin
      result := false;
      exit;
      end;
    inc(Currbuff,FNullmaskSize);

    for x := 0 to FieldDefs.count-1 do
      begin
      if (Field.FieldName = FieldDefs[x].Name) then
        begin
        Move(CurrBuff^, Buffer^, GetFieldSize(FieldDefs[x]));
        Result := True;
        Break;
        end
      else Inc(CurrBuff, GetFieldSize(FieldDefs[x]));
      end;
    end;
end;

procedure TBufDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  x        : longint;
  CurrBuff : pointer;
  NullMask : pbyte;

begin
  If Field.Fieldno > 0 then // If = 0, then calculated field or something
    begin
    CurrBuff := ActiveBuffer;
    
    if buffer = nil  then
      NullMask[(Field.fieldno-1) div 8] := (NullMask[(Field.fieldno-1) div 8]) or (1 shl ((Field.fieldno-1) mod 8))
    else
      begin
      NullMask[(Field.fieldno-1) div 8] := (NullMask[(Field.fieldno-1) div 8]) and not (1 shl ((Field.fieldno-1) mod 8));

      inc(Currbuff,FNullmaskSize);

      for x := 0 to FieldDefs.count-1 do
        begin
        if (Field.FieldName = FieldDefs[x].Name) then
          begin
          Move(Buffer^, CurrBuff^, GetFieldSize(FieldDefs[x]));
          Break;
          end
        else Inc(CurrBuff, GetFieldSize(FieldDefs[x]));
        end;
      end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
    end;
end;

procedure TBufDataset.internalpost;

begin
  if state=dsEdit then
    begin
    end;
end;

procedure TBufDataset.CalcRecordSize;

var x : longint;

begin
  FNullmaskSize := 1+((FieldDefs.count-1) div 8);
  FRecordSize := FNullmaskSize;
  for x := 0 to FieldDefs.count-1 do
    inc(FRecordSize, GetFieldSize(FieldDefs[x]));
end;

function TBufDataset.GetRecordSize : Word;

begin
  result := FRecordSize;
end;

procedure TBufDataset.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FRecordSize, #0);
end;

procedure TBufDataset.SetRecNo(Value: Longint);

begin
  GotoBookmark(@value);
end;

function TBufDataset.GetRecNo: Longint;

begin
  If FBCurrentRecord = -1 then Result := 0
    else if FBCurrentRecord = FBRecordcount then Result := FBCurrentRecord-1
    else Result := FBCurrentRecord;
end;

function TBufDataset.IsCursorOpen: Boolean;

begin
  Result := FOpen;
end;

Function TBufDataset.GetRecordCount: Longint;

begin
  Result := FBRecordCount;
end;



