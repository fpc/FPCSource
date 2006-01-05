{
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
  BookmarkSize := sizeof(TBufBookmark);
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
  result^ := #1; // this 'deletes' the record
end;

procedure TBufDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  ReAllocMem(Buffer,0);
end;

procedure TBufDataset.InternalOpen;

begin
  CalcRecordSize;

  FBRecordcount := 0;
  FBDeletedRecords := 0;
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
  If FBBufferCount > 0 then ReAllocMem(FBBuffers,0);
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

procedure unSetDeleted(NullMask : pbyte); //inline;
begin
  NullMask[0] := NullMask[0] and not 1;
end;

procedure SetDeleted(NullMask : pbyte); //inline;
begin
  NullMask[0] := NullMask[0] or 1;
end;

function GetDeleted(NullMask : pbyte) : boolean; //inline;
begin
  result := (NullMask[0] and 1) = 1;
end;

procedure unSetFieldIsNull(NullMask : pbyte;x : longint); //inline;
begin
  inc(x);
  NullMask[x div 8] := (NullMask[x div 8]) and not (1 shl (x mod 8));
end;

procedure SetFieldIsNull(NullMask : pbyte;x : longint); //inline;
begin
  inc(x);
  NullMask[x div 8] := (NullMask[x div 8]) or (1 shl (x mod 8));
end;

function GetFieldIsNull(NullMask : pbyte;x : longint) : boolean; //inline;
begin
  inc(x);
  result := ord(NullMask[x div 8]) and (1 shl (x mod 8)) > 0
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
      if (FBCurrentRecord < 0) or (FBCurrentRecord >= FBRecordCount) then
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
    if GetDeleted(pbyte(FBBuffers[FBCurrentRecord])) then
      begin
      if getmode = gmCurrent then
        if DoCheck then
          begin
          Result := grError;
          DatabaseError(SDeletedRecord);
          exit;
          end
        else
          getmode := gmnext;
      Result := GetRecord(Buffer,getmode,DoCheck);
      exit
      end;

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
            unSetFieldIsNull(NullMask,x);
            move(FieldUpdBuf^.NewValue^,buffer^,GetFieldSize(FieldDefs[x]));
            end
          else
            SetFieldIsNull(NullMask,x);
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
    if FBBufferCount < FBRecordCount+FPacketRecords then
      begin
      FBBufferCount := FBBuffercount + FPacketRecords;
      ReAllocMem(FBBuffers,FBBuffercount*SizeOf(PChar));
      end;

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
    ftString,
      ftFixedChar: result := FieldDef.Size + 1;
    ftSmallint,
      ftInteger,
      ftword     : result := sizeof(longint);
    ftBoolean    : result := sizeof(wordbool);
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
      SetFieldIsNull(NullMask,x);

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
      begin
      if FApplyingUpdates then
        CurrBuff := FBBuffers[fbcurrentrecord] // This makes it possible for ApplyUpdates to get values from deleted records
      else
        CurrBuff := FBBuffers[GetRecNo];
      end
    else
      begin
      CurrBuff := ActiveBuffer;
      if not assigned(CurrBuff) or GetDeleted(pbyte(CurrBuff)) then
        begin
        result := false;
        exit;
        end;
      end;

    if GetFieldIsnull(pbyte(CurrBuff),Field.Fieldno-1) then
      begin
      result := false;
      exit;
      end;
    inc(Currbuff,FNullmaskSize);

    for x := 0 to FieldDefs.count-1 do
      begin
      if (Field.FieldName = FieldDefs[x].Name) then
        begin
        // a nil-buffer is allowed for the fields.isNull function
        if assigned(buffer) then Move(CurrBuff^, Buffer^, GetFieldSize(FieldDefs[x]));
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
  if not (state in [dsEdit, dsInsert]) then
    begin
    DatabaseErrorFmt(SNotInEditState,[NAme],self);
    exit;
    end;
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
          unSetFieldIsNull(NullMask,x);
          end
        else
          SetFieldIsNull(NullMask,x);
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
    FEditBuf^.UpdateKind := ukModify;
    FEditBuf^.RecordNo := getrecno;
    end;
end;

procedure TBufDataset.InternalInsert;

begin
  if FBRecordCount > FBBufferCount-1 then
    begin
    inc(FBBufferCount);
    ReAllocMem(FBBuffers,FBBuffercount*SizeOf(PChar));
    end;

  inc(FBRecordCount);
  FBCurrentRecord := FBRecordCount -1;
  FBBuffers[FBCurrentRecord] := AllocRecordBuffer;
  fillchar(FBBuffers[FBCurrentRecord]^,FNullmaskSize,255);
  unSetDeleted(pbyte(FBBuffers[FBCurrentRecord]));
  fillchar(ActiveBuffer^,FNullmaskSize,255);
  unSetDeleted(pbyte(ActiveBuffer));

  // cached updates:
  If not assigned(FEditBuf) then
    begin
    SetLength(FUpdateBuffer,length(FUpdateBuffer)+1);
    FEditBuf := @FUpdateBuffer[high(FUpdateBuffer)];
    end;
  FEditBuf^.RecordNo := FBCurrentRecord;
  FEditBuf^.UpdateKind := ukInsert;

  with PBufBookmark(ActiveBuffer + RecordSize)^ do
    begin
    BookmarkData := FBCurrentRecord;
    BookmarkFlag := bfInserted;
    end;
end;

procedure TBufDataset.InternalDelete;

var tel : integer;

begin
  SetDeleted(pbyte(FBBuffers[FBCurrentRecord]));
  SetDeleted(pbyte(ActiveBuffer));
  inc(FBDeletedRecords);

  if GetRecordUpdateBuffer(recno,FEditBuf) and (FEditBuf^.UpdateKind = ukInsert) then
    begin
    if assigned(FEditBuf^.FieldsUpdateBuffer) then
      for tel := 0 to high(FEditBuf^.FieldsUpdateBuffer) do
        if not FEditBuf^.FieldsUpdateBuffer[tel].IsNull then
          freemem(FEditBuf^.FieldsUpdateBuffer[tel].NewValue);
    setlength(FEditBuf^.FieldsUpdateBuffer,0);
    FEditBuf^.RecordNo := -1;
    end
  else
    begin
    If not assigned(FEditBuf) then
      begin
      SetLength(FUpdateBuffer,length(FUpdateBuffer)+1);
      FEditBuf := @FUpdateBuffer[high(FUpdateBuffer)];
      end;
    FEditBuf^.RecordNo := FBCurrentRecord;
    FEditBuf^.UpdateKind := ukDelete;
    end;
  FEditBuf := nil;
end;


function TBufDataset.ApplyRecUpdate(UpdateKind : TUpdateKind) : boolean;

begin
  Result := False;
end;

procedure TBufDataset.CancelUpdates;

var r,f : integer;

begin
  for r := 0 to high(FUpdateBuffer) do
    begin
    if FUpdateBuffer[r].RecordNo > -1 then
     if FUpdateBuffer[r].UpdateKind = ukDelete then
      begin
      dec(FBDeletedRecords);
      unSetDeleted(pbyte(FBBuffers[FUpdateBuffer[r].RecordNo]));
      end
    else if FUpdateBuffer[r].UpdateKind = ukInsert then
      begin
      inc(FBDeletedRecords);
      SetDeleted(pbyte(FBBuffers[FUpdateBuffer[r].RecordNo]));
      end;
    for f := 0 to high(FUpdateBuffer[r].FieldsUpdateBuffer) do
      FreeMem(FUpdateBuffer[r].FieldsUpdateBuffer[f].newvalue);

    end;
  SetLength(FUpdateBuffer,0);
  if FOpen then Resync([]);
end;

procedure TBufDataset.ApplyUpdates;

var SaveBookmark : Integer;
    r,i          : Integer;
    buffer       : PChar;
    x            : integer;
    FieldUpdBuf : PFieldUpdateBuffer;
    NullMask    : pbyte;

begin
  CheckBrowseMode;
  SaveBookMark := GetRecNo;

  r := 0;
  while r < Length(FUpdateBuffer) do
    begin
    if (@FUpdateBuffer[r] <> FEditBuf) and // Neglect edit-buffer
       (FUpdateBuffer[r].RecordNo <> -1) then // And the 'deleted' buffers
      begin
      FApplyingUpdates := true;
      if FUpdateBuffer[r].UpdateKind = ukDelete then
        InternalGotoBookmark(@(FUpdateBuffer[r].RecordNo))
      else
        begin
        InternalGotoBookMark(@FUpdateBuffer[r].RecordNo);
        Resync([rmExact,rmCenter]);
        end;
      if ApplyRecUpdate(FUpdateBuffer[r].UpdateKind) then
        begin
        buffer := FBBuffers[FUpdateBuffer[r].RecordNo];
        NullMask := pbyte(buffer);

        inc(buffer,FNullmaskSize);

        for x := 0 to FieldDefs.count-1 do
          begin
          if GetFieldUpdateBuffer(x,@FUpdateBuffer[r],FieldUpdBuf) then
            If not FieldUpdBuf^.IsNull then
              begin
              unSetFieldIsNull(NullMask,x);
              move(FieldUpdBuf^.NewValue^,buffer^,GetFieldSize(FieldDefs[x]));
              FreeMem(FieldUpdBuf^.NewValue);
              end
            else
              SetFieldIsNull(NullMask,x);
          Inc(Buffer, GetFieldSize(FieldDefs[x]));
          end;

        for i := r to high(FUpdateBuffer)-1 do
          FUpdateBuffer[i] := FupdateBuffer[i+1];
        dec(r);
        SetLength(FUpdateBuffer,high(FUpdateBuffer));
        end;
      FApplyingUpdates := False;
      end;
    inc(r);
    end;
  Refresh;
  if not GetDeleted(pbyte(FBBuffers[savebookmark])) then
    begin
    InternalGotoBookMark(@SaveBookMark);
    Resync([rmExact,rmCenter]);
    end;

end;

procedure TBufDataset.InternalPost;

begin
  if state in [dsEdit, dsInsert] then
    begin
    if Length(FUpdateBuffer[High(FUpdateBuffer)].FieldsUpdateBuffer) > 0 then
      FEditBuf := nil;
    end;
end;

procedure TBufDataset.InternalCancel;

var tel : integer;

begin
  if state in [dsEdit, dsInsert] then
    begin
    if state = dsInsert then
      begin
      SetDeleted(pbyte(FBBuffers[FBCurrentRecord]));
      SetDeleted(pbyte(ActiveBuffer));
      inc(FBDeletedRecords);
      end;
    FEditBuf^.RecordNo := -1;

    // clear the fieldbuffers
    if assigned(FEditBuf^.FieldsUpdateBuffer) then
      for tel := 0 to high(FEditBuf^.FieldsUpdateBuffer) do
        if not FEditBuf^.FieldsUpdateBuffer[tel].IsNull then
          freemem(FEditBuf^.FieldsUpdateBuffer[tel].NewValue);
    setlength(FEditBuf^.FieldsUpdateBuffer,0);
    end;
end;


procedure TBufDataset.CalcRecordSize;

var x : longint;

begin
  FNullmaskSize := 1+((FieldDefs.count) div 8);
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
  Result := FBRecordCount-FBDeletedRecords;
end;



