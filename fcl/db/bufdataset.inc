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

function TBufDataset.AllocRecordBuffer: PChar;

begin
  result := AllocRecord;
  ReAllocMem(result,RecordSize+sizeof(TBufBookmark));
end;

procedure TBufDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

procedure TBufDataset.InternalOpen;

begin
  FBRecordcount := 0;
  FBBuffercount := 0;
  FBCurrentrecord := -1;
  FIsEOF := false;
  FIsbOF := true;
end;

procedure TBufDataset.InternalClose;

var i : integer;

begin
  for i := 0 to FBRecordCount-1 do FreeRecord(FBBuffers[i]);
  freemem(FBBuffers);
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
  if FIsEOF then
    Result := grEOF
  else begin
    Result := grOK;
    case GetMode of
      gmPrior :
        if FBCurrentRecord <= 0 then
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
        if FBCurrentRecord >= (FBRecordCount - 1) then
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
  FBCurrentRecord := PInteger(ABookmark)^;
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
    FBBuffers[FBRecordCount+i] := AllocRecord;
    b := (getnextrecord(FBBuffers[FBRecordCount+i])<>grOk);
    inc(i);
    until (i = FPacketRecords) or b;
    if b then
      begin
      dec(i);
      FreeRecord(FBBuffers[FBRecordCount+i]);
      end;
    FBRecordCount := FBRecordCount + i;
    end;
  result := i;
end;

