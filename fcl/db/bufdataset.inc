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
  FPacketRecords := 10;
end;

procedure TBufDataset.SetPacketRecords(aValue : integer);
begin
  if aValue > 0 then FPacketRecords := aValue
    else DatabaseError(SInvPacketRecordsValue);
end;

destructor TBufDataset.Destroy;
begin
  inherited destroy;
end;

Function TBufDataset.GetCanModify: Boolean;
begin
  Result:= False;
end;

function TBufDataset.intAllocRecordBuffer: PChar;
begin
  // Note: Only the internal buffers of TDataset provide bookmark information
  result := AllocMem(FRecordsize+sizeof(TBufRecLinkItem));
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

//  FBRecordcount := 0;
//  FBDeletedRecords := 0;

  FFirstRecBuf := pointer(IntAllocRecordBuffer);
  FLastRecBuf := FFirstRecBuf;
  FCurrentRecBuf := FLastRecBuf;

  FOpen:=True;
end;

procedure TBufDataset.InternalClose;

var pc : pchar;

begin
  FOpen:=False;
  CancelUpdates;
  FCurrentRecBuf := FFirstRecBuf;
  while assigned(FCurrentRecBuf) do
    begin
    pc := pointer(FCurrentRecBuf);
    FCurrentRecBuf := FCurrentRecBuf^.next;
    FreeRecordBuffer(pc);
    end;
  SetLength(FFieldBufPositions,0);
end;

procedure TBufDataset.InternalFirst;
begin
  FCurrentRecBuf := FFirstRecBuf;
end;

procedure TBufDataset.InternalLast;
begin
  repeat
  until getnextpacket < FPacketRecords;
  if FLastRecBuf <> FFirstRecBuf then
    FCurrentRecBuf := FLastRecBuf;
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

begin
  Result := grOK;
  case GetMode of
    gmPrior :
      if not assigned(PBufRecLinkItem(FCurrentRecBuf)^.prior) then
        begin
        Result := grBOF;
        end
      else
        begin
        FCurrentRecBuf := PBufRecLinkItem(FCurrentRecBuf)^.prior;
        end;
    gmCurrent :
      if FCurrentRecBuf = FLastRecBuf then
        Result := grError;
    gmNext :
      if FCurrentRecBuf = FLastRecBuf then // Dataset is empty (just opened)
        begin
        if getnextpacket = 0 then result := grEOF;
        end
      else if (PBufRecLinkItem(FCurrentRecBuf)^.next = FLastRecBuf) then
        begin
        if getnextpacket > 0 then
          begin
          FCurrentRecBuf := PBufRecLinkItem(FCurrentRecBuf)^.next;
          end
        else
          begin
          result:=grEOF;
          end
        end
      else
        begin
        FCurrentRecBuf := PBufRecLinkItem(FCurrentRecBuf)^.next;
        end;
  end;

  if Result = grOK then
    begin

    with PBufBookmark(Buffer + RecordSize)^ do
      begin
      BookmarkData := FCurrentRecBuf;
      BookmarkFlag := bfCurrent;
      end;
    move((pointer(FCurrentRecBuf)+sizeof(TBufRecLinkItem))^,buffer^,RecordSize);
    end
  else if (Result = grError) and doCheck then
    DatabaseError('No record');
end;

function TBufDataset.GetRecordUpdateBuffer : boolean;

var x : integer;
    CurrBuff : PChar;

begin
  GetBookmarkData(ActiveBuffer,@CurrBuff);
  if (FCurrentUpdateBuffer >= length(FUpdateBuffer)) or (FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData <> CurrBuff) then
   for x := 0 to high(FUpdateBuffer) do
    if FUpdateBuffer[x].BookmarkData = CurrBuff then
      begin
      FCurrentUpdateBuffer := x;
      break;
      end;
  Result := (FCurrentUpdateBuffer < length(FUpdateBuffer))  and (FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData = CurrBuff);
end;

procedure TBufDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentRecBuf := PBufBookmark(Buffer + RecordSize)^.BookmarkData;
end;

procedure TBufDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PBufBookmark(Buffer + RecordSize)^.BookmarkData := pointer(Data^);
end;

procedure TBufDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PBufBookmark(Buffer + RecordSize)^.BookmarkFlag := Value;
end;

procedure TBufDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  pointer(Data^) := PBufBookmark(Buffer + RecordSize)^.BookmarkData;
end;

function TBufDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PBufBookmark(Buffer + RecordSize)^.BookmarkFlag;
end;

procedure TBufDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecBuf := ABookmark;
end;

function TBufDataset.getnextpacket : integer;

var i : integer;
    pb : pchar;
    
begin
  for i := 0 to FPacketRecords-1 do
    begin
    pb := pchar(pointer(FLastRecBuf)+sizeof(TBufRecLinkItem));
    if (loadbuffer(pb)<>grOk) then break;
    PBufRecLinkItem(FLastRecBuf)^.next := pointer(IntAllocRecordBuffer);
    PBufRecLinkItem(PBufRecLinkItem(FLastRecBuf)^.next)^.prior := FLastRecBuf;
    FLastRecBuf := PBufRecLinkItem(FLastRecBuf)^.next;
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

function TBufDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  Result := GetFieldData(Field, Buffer);
end;

function TBufDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

var CurrBuff : pchar;

begin
  Result := False;
  If Field.Fieldno > 0 then // If = 0, then calculated field or something similar
    begin
    if state = dsOldValue then
      begin
      if not GetRecordUpdateBuffer then
        begin
        // There is no old value available
        result := false;
        exit;
        end;
      currbuff := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer+sizeof(TBufRecLinkItem);
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

    inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
    if assigned(buffer) then Move(CurrBuff^, Buffer^, GetFieldSize(FieldDefs[Field.FieldNo-1]));
    Result := True;
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
  if not (state in [dsEdit, dsInsert]) then
    begin
    DatabaseErrorFmt(SNotInEditState,[NAme],self);
    exit;
    end;
  If Field.Fieldno > 0 then // If = 0, then calculated field or something
    begin
    CurrBuff := ActiveBuffer;
    NullMask := CurrBuff;

    inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
    if assigned(buffer) then
      begin
      Move(Buffer^, CurrBuff^, GetFieldSize(FieldDefs[Field.FieldNo-1]));
      unSetFieldIsNull(NullMask,Field.FieldNo-1);
      end
    else
      SetFieldIsNull(NullMask,Field.FieldNo-1);
      
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Ptrint(Field));
    end;
end;

procedure TBufDataset.InternalDelete;

var RecToDelete : PBufRecLinkItem;

begin
  GetBookmarkData(ActiveBuffer,@RecToDelete);
  SetDeleted(pbyte(ActiveBuffer));

  if RecToDelete <> FFirstRecBuf then RecToDelete^.prior^.next := RecToDelete^.next
  else FFirstRecBuf := RecToDelete^.next;

  RecToDelete^.next^.prior :=  RecToDelete^.prior;

  FCurrentRecBuf := RecToDelete^.next;
  
  if not GetRecordUpdateBuffer then
    begin
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := pchar(RecToDelete);
    FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := RecToDelete;
    end
  else
    begin
    FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer;
    FreeRecordBuffer(pchar(RecToDelete));
    end;

  FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukDelete;
end;


function TBufDataset.ApplyRecUpdate(UpdateKind : TUpdateKind) : boolean;

begin
  Result := False;
end;

procedure TBufDataset.CancelUpdates;

begin
// To be implemented
{  for r := 0 to high(FUpdateBuffer) do
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
  if FOpen then Resync([]);}
end;

procedure TBufDataset.ApplyUpdates;

var SaveBookmark : pchar;
    r            : Integer;
    FailedCount  : integer;

begin
  CheckBrowseMode;
  
  // There is no bookmark available if the dataset is empty
  if not IsEmpty then
    GetBookmarkData(ActiveBuffer,@SaveBookmark);

  r := 0;
  FailedCount := 0;
  while r < Length(FUpdateBuffer) do
    begin
    if assigned(FUpdateBuffer[r].BookmarkData) then
      begin
      InternalGotoBookmark(FUpdateBuffer[r].BookmarkData);
      Resync([rmExact,rmCenter]);
      if ApplyRecUpdate(FUpdateBuffer[r].UpdateKind) then
        begin
        FreeRecordBuffer(FUpdateBuffer[r].OldValuesBuffer);
        FUpdateBuffer[r].BookmarkData := nil;
        end
      else
        Inc(FailedCount);
      end;
    inc(r);
    end;
  if failedcount = 0 then
    SetLength(FUpdateBuffer,0);

  if not IsEmpty then
    begin
    InternalGotoBookMark(SaveBookMark);
    Resync([rmExact,rmCenter]);
    end
  else
    InternalFirst;
end;

procedure TBufDataset.InternalPost;

Var tmpRecBuffer : PBufRecLinkItem;
    CurrBuff     : PChar;

begin
  if state = dsInsert then
    begin
    if GetBookmarkFlag(ActiveBuffer) = bfEOF then
      // Append
      FCurrentRecBuf := FLastRecBuf
    else
      // The active buffer is the newly created TDataset record,
      // from which the bookmark is set to the record where the new record should be
      // inserted
      GetBookmarkData(ActiveBuffer,@FCurrentRecBuf);

    // Create the new record buffer
    tmpRecBuffer := FCurrentRecBuf^.prior;

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

    // Link the newly created record buffer to the newly created TDataset record
    with PBufBookmark(ActiveBuffer + RecordSize)^ do
      begin
      BookmarkData := FCurrentRecBuf;
      BookmarkFlag := bfInserted;
      end;
    end
  else
    GetBookmarkData(ActiveBuffer,@FCurrentRecBuf);

  if not GetRecordUpdateBuffer then
    begin
    FCurrentUpdateBuffer := length(FUpdateBuffer);
    SetLength(FUpdateBuffer,FCurrentUpdateBuffer+1);

    FUpdateBuffer[FCurrentUpdateBuffer].BookmarkData := FCurrentRecBuf;

    if state = dsEdit then
      begin
      // Update the oldvalues-buffer
      FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer := intAllocRecordBuffer;
      move(FCurrentRecBuf^,FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer^,RecordSize+sizeof(TBufRecLinkItem));
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukModify;
      end
    else
      FUpdateBuffer[FCurrentUpdateBuffer].UpdateKind := ukInsert;
    end;

  CurrBuff := pchar(FCurrentRecBuf);
  inc(Currbuff,sizeof(TBufRecLinkItem));
  move(ActiveBuffer^,CurrBuff^,RecordSize);
end;

procedure TBufDataset.CalcRecordSize;

var x : longint;

begin
  FNullmaskSize := 1+((FieldDefs.count) div 8);
  FRecordSize := FNullmaskSize;
  SetLength(FFieldBufPositions,FieldDefs.count);
  for x := 0 to FieldDefs.count-1 do
    begin
    FFieldBufPositions[x] := FRecordSize;
    inc(FRecordSize, GetFieldSize(FieldDefs[x]));
    end;
end;

function TBufDataset.GetRecordSize : Word;

begin
  result := FRecordSize;
end;

procedure TBufDataset.InternalInitRecord(Buffer: PChar);

begin
  FillChar(Buffer^, FRecordSize, #0);

  fillchar(Buffer^,FNullmaskSize,255);
  unSetDeleted(pbyte(Buffer));
end;

procedure TBufDataset.SetRecNo(Value: Longint);

begin
  GotoBookmark(@value);
end;

function TBufDataset.GetRecNo: Longint;

begin
//  GetBookmarkData(ActiveBuffer,@Result);
end;

function TBufDataset.IsCursorOpen: Boolean;

begin
  Result := FOpen;
end;

Function TBufDataset.GetRecordCount: Longint;

begin
//  Result := FBRecordCount-FBDeletedRecords;
end;

Function TBufDataset.Locate(const KeyFields: string; const KeyValues: Variant; options: TLocateOptions) : boolean;

var keyfield    : TField;     // Field to search in
    ValueBuffer : pchar;      // Pointer to value to search for, in TField' internal format
    VBLength    : integer;

    FieldBufPos : PtrInt;     // Amount to add to the record buffer to get the FieldBuffer
    li          : Longint;
    i           : longint;
    CurrBuff    : pchar;
    cb          : pchar;
    bm          : TBufBookmark;
    
    CheckNull   : Boolean;

begin
// For now it is only possible to search in one field at the same time
{  result := False;

  keyfield := FieldByName(keyfields);
  CheckNull := VarIsNull(KeyValues);

  if not CheckNull then
    begin
    case keyfield.DataType of
      ftInteger : begin
                  li := KeyValues;
                  ValueBuffer := @li;
                  VBLength := sizeof(li);
                  end;
    else
      DatabaseErrorFmt(SInvalidSearchFieldType,[Fieldtypenames[keyfield.DataType]],self);
    end;

    FieldBufPos := FFieldBufPositions[keyfield.FieldNo-1];
    end;

  i := 0;
  repeat
  CurrBuff := FBBuffers[i];
  GetBookmarkData(CurrBuff,@bm);

  if not GetDeleted(pbyte(CurrBuff)) then
    begin

    If CheckNull then
      begin
      if GetFieldIsnull(pbyte(CurrBuff),keyfield.Fieldno-1) then
        begin
        result := True;
        break;
        end;
      end
    else if not GetFieldIsnull(pbyte(CurrBuff),keyfield.Fieldno-1) then
      begin
      inc(CurrBuff,FieldBufPos);
      if CompareByte(ValueBuffer^,CurrBuff^,VBLength) = 0 then
        begin
        result := True;
        break;
        end;
      end;

    end;
  if i = FBRecordCount -1 then getnextpacket;
  inc(i)
  until i = FBRecordCount;

  if Result then
    begin
//    bm.BookmarkData := i;
    bm.BookmarkFlag := bfCurrent;
    GotoBookmark(@bm);
    end;}
end;

