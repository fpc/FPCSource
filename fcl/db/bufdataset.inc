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
  SetLength(FUpdateBuffer,0);
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
  CancelUpdates;
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

var x         : longint;
    RecUpdBuf : PRecUpdateBuffer;
    FieldUpdBuf : PFieldUpdateBuffer;
    NullMask     : pbyte;

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

  if Result = grOK then
    begin
    with PBufBookmark(Buffer + RecordSize)^ do
      begin
      BookmarkData := FBCurrentRecord;
      BookmarkFlag := bfCurrent;
      end;
    move(FBBuffers[FBCurrentRecord]^,buffer^,RecordSize);
// Cached Updates:
    If GetRecordUpdateBuffer(FBCurrentRecord,RecUpdBuf) then
      begin
      NullMask := pbyte(buffer);
      inc(buffer,FNullmaskSize);

      for x := 0 to FieldDefs.count-1 do
        begin
        if GetFieldUpdateBuffer(x,RecUpdBuf,FieldUpdBuf) then
          If not FieldUpdBuf^.IsNull then
            begin
            NullMask[x div 8] := (NullMask[x div 8]) and not (1 shl (x mod 8));
            move(FieldUpdBuf^.NewValue^,buffer^,GetFieldSize(FieldDefs[x]));
            end
          else
            NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));
        Inc(Buffer, GetFieldSize(FieldDefs[x]));
        end;
      end;
    end
  else if (Result = grError) and doCheck then
    DatabaseError('No record');
end;

function TBufDataset.GetRecordUpdateBuffer(rno : integer;var RecUpdBuf : PRecUpdateBuffer) : boolean;

var r : integer;

begin
  Result := False;
  for r := 0 to high(FUpdateBuffer) do
    if (FUpdateBuffer[r].RecordNo = rno) and (@FUpdateBuffer[r] <> FEditBuf) then // Neglect the edit-buffer
      begin
      RecUpdBuf := @FUpdateBuffer[r];
      Result := True;
      Break;
      end;
end;

function TBufDataset.GetFieldUpdateBuffer(fieldno : integer;RecUpdBuf : PRecUpdateBuffer;var FieldUpdBuf : pFieldUpdateBuffer) : boolean;

var f : integer;

begin
  Result := False;
  for f := 0 to High(RecUpdBuf^.FieldsUpdateBuffer) do
    if RecUpdBuf^.FieldsUpdateBuffer[f].FieldNo = fieldno then
      begin
      FieldUpdBuf := @RecUpdBuf^.FieldsUpdateBuffer[f];
      Result := True;
      Break;
      end;
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
    if state = dsOldValue then
      CurrBuff := FBBuffers[GetRecNo]
    else
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
  FieldUpdBuf : PFieldUpdateBuffer;

begin
  If Field.Fieldno > 0 then // If = 0, then calculated field or something
    begin
    CurrBuff := ActiveBuffer;
    NullMask := CurrBuff;

    inc(Currbuff,FNullmaskSize);

    for x := 0 to FieldDefs.count-1 do
      begin
      if (Field.FieldName = FieldDefs[x].Name) then
        begin
        if assigned(buffer) then
          begin
          Move(Buffer^, CurrBuff^, GetFieldSize(FieldDefs[x]));
          NullMask[x div 8] := (NullMask[x div 8]) and not (1 shl (x mod 8));
          end
        else
          NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));

        // cached updates
        with FEditBuf^ do
          begin
          if not GetFieldUpdateBuffer(x,FEditBuf,FieldUpdBuf) then
            begin
            SetLength(FieldsUpdateBuffer,length(FieldsUpdateBuffer)+1);
            FieldUpdBuf := @FieldsUpdateBuffer[high(FieldsUpdateBuffer)];
            GetMem(FieldUpdBuf^.NewValue,GetFieldSize(FieldDefs[x]));
            FieldUpdBuf^.FieldNo := x;
            end;
          if assigned(buffer) then
            begin
            Move(Buffer^, FieldUpdBuf^.NewValue^, GetFieldSize(FieldDefs[x]));
            FieldUpdBuf^.IsNull := False;
            end
          else FieldUpdBuf^.IsNull := True;
          end;
        Break;
        end
      else Inc(CurrBuff, GetFieldSize(FieldDefs[x]));
      end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Ptrint(Field));
    end;
end;

procedure TBufDataset.InternalEdit;

begin
  if not GetRecordUpdateBuffer(recno,FEditBuf) then
    begin
    If not assigned(FEditBuf) then
      begin
      SetLength(FUpdateBuffer,length(FUpdateBuffer)+1);
      FEditBuf := @FUpdateBuffer[high(FUpdateBuffer)];
      end;
    FEditBuf^.RecordNo := getrecno;
    end;
end;

function TBufDataset.ApplyRecUpdate : boolean;

begin
  Result := False;
end;

procedure TBufDataset.CancelUpdates;

var r,f : integer;

begin
  for r := 0 to high(FUpdateBuffer) do
    for f := 0 to high(FUpdateBuffer[r].FieldsUpdateBuffer) do
      FreeMem(FUpdateBuffer[r].FieldsUpdateBuffer[f].newvalue);
  SetLength(FUpdateBuffer,0);
end;

procedure TBufDataset.ApplyUpdates;

var SaveBookmark : Integer;
    r,i          : Integer;
    buffer       : PChar;
    x            : integer;
    FieldUpdBuf : PFieldUpdateBuffer;
    NullMask    : pbyte;

begin
  SaveBookMark := GetRecNo;
  
  r := 0;
  while r < Length(FUpdateBuffer) do
    begin
    if @FUpdateBuffer[r] <> FEditBuf then // Neglect edit-buffer
      begin
      SetRecNo(FUpdateBuffer[r].RecordNo);
      if ApplyRecUpdate then
        begin
        buffer := FBBuffers[FUpdateBuffer[r].RecordNo];
        NullMask := pbyte(buffer);

        inc(buffer,FNullmaskSize);

        for x := 0 to FieldDefs.count-1 do
          begin
          if GetFieldUpdateBuffer(x,@FUpdateBuffer[r],FieldUpdBuf) then
            If not FieldUpdBuf^.IsNull then
              begin
              NullMask[x div 8] := (NullMask[x div 8]) and not (1 shl (x mod 8));
              move(FieldUpdBuf^.NewValue^,buffer^,GetFieldSize(FieldDefs[x]));
              FreeMem(FieldUpdBuf^.NewValue);
              end
            else
              NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));
          Inc(Buffer, GetFieldSize(FieldDefs[x]));
          end;

        for i := r to high(FUpdateBuffer)-1 do
          FUpdateBuffer[i] := FupdateBuffer[i+1];
        dec(r);
        SetLength(FUpdateBuffer,high(FUpdateBuffer));
        end;
      end;
    inc(r);
    end;
  Refresh;
  SetRecNo(SaveBookMark);
end;

procedure TBufDataset.InternalPost;

begin
  if state=dsEdit then
    begin
    if Length(FUpdateBuffer[High(FUpdateBuffer)].FieldsUpdateBuffer) > 0 then
      FEditBuf := nil;
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
  GetBookmarkData(ActiveBuffer,@Result);
end;

function TBufDataset.IsCursorOpen: Boolean;

begin
  Result := FOpen;
end;

Function TBufDataset.GetRecordCount: Longint;

begin
  Result := FBRecordCount;
end;



