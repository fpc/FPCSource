{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    Dataset implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
    TDataSet
  ---------------------------------------------------------------------}

Const
  DefaultBufferCount = 10;

constructor TDataSet.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FFieldDefs:=TFieldDefs.Create(Self);
  FFieldList:=TFields.Create(Self);
  FDataSources:=TList.Create;
end;



destructor TDataSet.Destroy;

var
  i: Integer;

begin
  Active:=False;
  FFieldDefs.Free;
  FFieldList.Free;
  With FDatasources do
    begin
    While Count>0 do
      TDatasource(Items[Count - 1]).DataSet:=Nil;
    Free;
    end;
  if Assigned(FBuffers) then
    begin
    for i := 0 to FBufferCount do
      FreeRecordBuffer(FBuffers[i]);
    FreeMem(FBuffers);
    end;
  Inherited Destroy;
end;

// This procedure must be called when the first record is made/read
Procedure TDataset.ActivateBuffers;

begin
  FBOF:=False;
  FEOF:=False;
  FActiveRecord:=0;
end;

Procedure TDataset.UpdateFieldDefs;

begin
  //!! To be implemented
end;

Procedure TDataset.BindFields(Binding: Boolean);

// Var I : longint;

begin
  {
     Here some magic will be needed later; for now just simply set
     Just set fieldno from listindex...
     Later we should take it from the fielddefs.
     // ATM Set by CreateField ...
  For I:=0 to FFieldList.Count-1 do
    FFieldList[i].FFieldNo:=I;
  }
end;

Function TDataset.BookmarkAvailable: Boolean;

Const BookmarkStates = [dsBrowse,dsEdit,dsInsert];

begin
  Result:=(Not IsEmpty) and (State in BookmarkStates)
          and (getBookMarkFlag(ActiveBuffer)=bfCurrent);
end;

Procedure TDataset.CalculateFields(Buffer: PChar);

begin
  { no internal calced fields or caches yet }
  DoOnCalcFields;
end;

Procedure TDataset.CheckActive;

begin
  If Not Active then
    DataBaseError(SInactiveDataset);
end;

Procedure TDataset.CheckInactive;

begin
  If Active then
    DataBaseError(SActiveDataset);
end;

Procedure TDataset.ClearBuffers;

begin
  FRecordCount:=0;
  FactiveRecord:=0;
  FCurrentRecord:=-1;
  FBOF:=True;
  FEOF:=True;
end;

Procedure TDataset.ClearCalcFields(Buffer: PChar);

begin
  //!! To be implemented
end;

Procedure TDataset.CloseBlob(Field: TField);

begin
  //!! To be implemented
end;

Procedure TDataset.CloseCursor;

begin
  //!! To be implemented
end;

Procedure TDataset.CreateFields;

Var I : longint;

begin
{$ifdef DSDebug}
  Writeln ('Creating fields');
  Writeln ('Count : ',fielddefs.Count);
  For I:=0 to FieldDefs.Count-1 do
    Writeln('Def ',I,' : ',Fielddefs.items[i].Name,'(',Fielddefs.items[i].FieldNo,')');
{$endif}
  For I:=0 to fielddefs.Count-1 do
    With Fielddefs.Items[I] do
      If DataType<>ftUnknown then
        begin
        {$ifdef DSDebug}
        Writeln('About to create field',FieldDefs.Items[i].Name);
        {$endif}
        CreateField(self);
        end;
end;

Procedure TDataset.DataEvent(Event: TDataEvent; Info: Ptrint);

Var
  i : longint;

begin
  // Do some bookkeeping;
  case Event of
    deFieldChange :
      begin
      if TField(Info).FieldKind in [fkData,fkInternalCalc] then
        SetModified(True);
      if FInternalCalcFields and (TField(Info).FieldKind = fkData) then
          RefreshInternalCalcFields(ActiveBuffer)
      else if (FCalcFieldsSize <> 0) and FAutoCalcFields and
        (TField(Info).FieldKind = fkData) then
        CalculateFields(ActiveBuffer);
      TField(Info).Change;
      end;
  end;
  // Distribute event to datasets;
  if FDisableControlsCount = 0 then
    for I := 0 to FDataSources.Count - 1 do
      TDataSource(FDataSources[I]).ProcessEvent(Event, Info);
end;

Procedure TDataset.DestroyFields;

begin
  FFieldList.Clear;
end;

Procedure TDataset.DoAfterCancel;

begin
 If assigned(FAfterCancel) then
   FAfterCancel(Self);
end;

Procedure TDataset.DoAfterClose;

begin
 If assigned(FAfterClose) then
   FAfterClose(Self);
end;

Procedure TDataset.DoAfterDelete;

begin
 If assigned(FAfterDelete) then
   FAfterDelete(Self);
end;

Procedure TDataset.DoAfterEdit;

begin
 If assigned(FAfterEdit) then
   FAfterEdit(Self);
end;

Procedure TDataset.DoAfterInsert;

begin
 If assigned(FAfterInsert) then
   FAfterInsert(Self);
end;

Procedure TDataset.DoAfterOpen;

begin
 If assigned(FAfterOpen) then
   FAfterOpen(Self);
end;

Procedure TDataset.DoAfterPost;

begin
 If assigned(FAfterPost) then
   FAfterPost(Self);
end;

Procedure TDataset.DoAfterScroll;

begin
 If assigned(FAfterScroll) then
   FAfterScroll(Self);
end;

Procedure TDataset.DoBeforeCancel;

begin
 If assigned(FBeforeCancel) then
   FBeforeCancel(Self);
end;

Procedure TDataset.DoBeforeClose;

begin
 If assigned(FBeforeClose) then
   FBeforeClose(Self);
end;

Procedure TDataset.DoBeforeDelete;

begin
 If assigned(FBeforeDelete) then
   FBeforeDelete(Self);
end;

Procedure TDataset.DoBeforeEdit;

begin
 If assigned(FBeforeEdit) then
   FBeforeEdit(Self);
end;

Procedure TDataset.DoBeforeInsert;

begin
 If assigned(FBeforeInsert) then
   FBeforeInsert(Self);
end;

Procedure TDataset.DoBeforeOpen;

begin
 If assigned(FBeforeOpen) then
   FBeforeOpen(Self);
end;

Procedure TDataset.DoBeforePost;

begin
 If assigned(FBeforePost) then
   FBeforePost(Self);
end;

Procedure TDataset.DoBeforeScroll;

begin
 If assigned(FBeforeScroll) then
   FBeforeScroll(Self);
end;

Procedure TDataset.DoInternalOpen;

begin
  FDefaultFields:=FieldCount=0;
  DoBeforeOpen;
  Try
{$ifdef dsdebug}
    Writeln ('Calling internal open');
{$endif}
    InternalOpen;
    FBOF:=True;
{$ifdef dsdebug}
    Writeln ('Calling RecalcBufListSize');
{$endif}
    FRecordcount := 0;
    RecalcBufListSize;
    FEOF := (FRecordcount = 0);
{$ifdef dsdebug}
    Writeln ('Setting state to browse');
{$endif}
    SetState(dsBrowse);
    DoAfterOpen;
    DoAfterScroll;
  except
    DoInternalClose(false);
    raise;
  end;
end;

Procedure TDataset.DoInternalClose(DoCheck : Boolean);

begin
  if DoCheck then
    CheckBrowsemode;
  FreeFieldBuffers;
  ClearBuffers;
  SetBufListSize(-1);
  SetState(dsInactive);
  InternalClose;
end;

Procedure TDataset.DoOnCalcFields;

begin
 If assigned(FOnCalcfields) then
   FOnCalcFields(Self);
end;

Procedure TDataset.DoOnNewRecord;

begin
 If assigned(FOnNewRecord) then
   FOnNewRecord(Self);
end;

Function TDataset.FieldByNumber(FieldNo: Longint): TField;

begin
  Result:=FFieldList.FieldByNumber(FieldNo);
end;

Function TDataset.FindRecord(Restart, GoForward: Boolean): Boolean;

begin
  //!! To be implemented
end;

Procedure TDataset.FreeFieldBuffers;

Var I : longint;

begin
  For I:=0 to FFieldList.Count-1 do
    FFieldList[i].FreeBuffers;
end;

Function TDataset.GetBookmarkStr: TBookmarkStr;

begin
  Result:='';
  If BookMarkAvailable then
    begin
    SetLength(Result,FBookMarkSize);
    GetBookMarkData(ActiveBuffer,Pointer(Result));
    end
end;

Function TDataset.GetBuffer (Index : longint) : Pchar;

begin
  Result:=FBuffers[Index];
end;

Procedure TDataset.GetCalcFields(Buffer: PChar);

begin
  //!! To be implemented
end;

Function TDataset.GetCanModify: Boolean;

begin
  Result:= not FIsUnidirectional;
end;

Procedure TDataset.GetChildren(Proc: TGetChildProc; Root: TComponent);

begin
  //!! To be implemented
end;

Function TDataset.GetDataSource: TDataSource;
begin
  Result:=nil;
end;

Function TDataset.GetField (Index : Longint) : TField;

begin
  Result:=FFIeldList[index];
end;

{
  This is not yet allowed, FPC doesn't allow typed consts of Classes...

Const
  DefFieldClasses : Array [TFieldType] of TFieldClass =
    ( { ftUnknown} Tfield,
      { ftString} TStringField,
      { ftSmallint} TLongIntField,
      { ftInteger} TLongintField,
      { ftWord} TLongintField,
      { ftBoolean} TBooleanField,
      { ftFloat} TFloatField,
      { ftDate} TDateField,
      { ftTime} TTimeField,
      { ftDateTime} TDateTimeField,
      { ftBytes} TBytesField,
      { ftVarBytes} TVarBytesField,
      { ftAutoInc} TAutoIncField,
      { ftBlob} TBlobField,
      { ftMemo} TMemoField,
      { ftGraphic} TGraphicField,
      { ftFmtMemo} TMemoField,
      { ftParadoxOle} Nil,
      { ftDBaseOle} Nil,
      { ftTypedBinary} Nil,
      { ftCursor} Nil
    );
}

Function TDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;

begin
  Case FieldType of
     ftUnknown : Result:=Tfield;
     ftString: Result := TStringField;
     ftSmallint: Result := TSmallIntField;
     ftInteger: Result := TLongintField;
     ftWord: Result := TWordField;
     ftBoolean: Result := TBooleanField;
     ftFloat: Result := TFloatField;
     ftBCD: Result := TBCDField;
     ftDate: Result := TDateField;
     ftTime: Result := TTimeField;
     ftDateTime: Result := TDateTimeField;
     ftBytes: Result := TBytesField;
     ftVarBytes: Result := TVarBytesField;
     ftAutoInc: Result := TAutoIncField;
     ftBlob: Result := TBlobField;
     ftMemo: Result := TMemoField;
     ftGraphic: Result := TGraphicField;
     ftFmtMemo: Result := TMemoField;
     ftParadoxOle: Result := Nil;
     ftDBaseOle: Result := Nil;
     ftTypedBinary: Result := Nil;
     ftCursor: Result := Nil;
  end;
end;

Function TDataset.GetIsIndexField(Field: TField): Boolean;

begin
  //!! To be implemented
end;


Function TDataset.GetNextRecord: Boolean;

  procedure ExchangeBuffers(var buf1,buf2 : pointer);

  var tempbuf : pointer;

  begin
    tempbuf := buf1;
    buf1 := buf2;
    buf2 := tempbuf;
  end;

begin
{$ifdef dsdebug}
  Writeln ('Getting next record. Internal RecordCount : ',FRecordCount);
{$endif}
  If FRecordCount>0 Then SetCurrentRecord(FRecordCount-1);
  Result:=GetRecord(FBuffers[FBuffercount],gmNext,True)=grOK;

  if result then
    begin
      If FRecordCount=0 then ActivateBuffers;
      if FRecordcount=FBuffercount then
        shiftbuffersbackward
      else
        begin
          inc(FRecordCount);
          FCurrentRecord:=FRecordCount - 1;
          ExchangeBuffers(Fbuffers[FCurrentRecord],FBuffers[FBuffercount]);
        end;
    end
  else
    cursorposchanged;
{$ifdef dsdebug}
  Writeln ('Result getting next record : ',Result);
{$endif}
end;

Function TDataset.GetNextRecords: Longint;

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting next record(s), need :',FBufferCount);
{$endif}
  While (FRecordCount<FBufferCount) and GetNextRecord do
    Inc(Result);
{$ifdef dsdebug}
  Writeln ('Result Getting next record(S), GOT :',RESULT);
{$endif}
end;

Function TDataset.GetPriorRecord: Boolean;

begin
{$ifdef dsdebug}
  Writeln ('GetPriorRecord: Getting previous record');
{$endif}
  If FRecordCount>0 Then SetCurrentRecord(0);
  Result:=GetRecord(FBuffers[FBuffercount],gmPrior,True)=grOK;
  if result then
    begin
      If FRecordCount=0 then ActivateBuffers;
      shiftbuffersforward;

      if FRecordcount<FBuffercount then
        inc(FRecordCount);
    end
  else
    cursorposchanged;
{$ifdef dsdebug}
  Writeln ('Result getting prior record : ',Result);
{$endif}
end;

Function TDataset.GetPriorRecords: Longint;

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting previous record(s), need :',FBufferCount);
{$endif}
  While (FRecordCount<FbufferCount) and GetPriorRecord do
    Inc(Result);
end;

Function TDataset.GetRecNo: Longint;

begin
  //!! To be implemented
end;

Function TDataset.GetRecordCount: Longint;

begin
  //!! To be implemented
end;

Procedure TDataset.InitFieldDefs;

begin
  //!! To be implemented
end;

Procedure TDataset.InitRecord(Buffer: PChar);

begin
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
end;

Procedure TDataset.InternalCancel;

begin
  //!! To be implemented
end;

Procedure TDataset.InternalEdit;

begin
  //!! To be implemented
end;

Procedure TDataset.InternalRefresh;

begin
  //!! To be implemented
end;

Procedure TDataset.OpenCursor(InfoQuery: Boolean);

begin
  //!! To be implemented
end;

Procedure TDataset.RefreshInternalCalcFields(Buffer: PChar);

begin
  //!! To be implemented
end;

Function TDataset.SetTempState(const Value: TDataSetState): TDataSetState;

begin
  result := FState;
  FState := value;
  inc(FDisableControlsCount);
end;

Procedure TDataset.RestoreState(const Value: TDataSetState);

begin
  FState := value;
  dec(FDisableControlsCount);
end;

function TDataset.GetActive : boolean;

begin
  result := FState <> dsInactive;
end;

Procedure TDataset.SetActive (Value : Boolean);

begin
  if value and (Fstate = dsInactive) then
    begin
    if csLoading in ComponentState then
      begin
      FOpenAfterRead := true;
      exit;
      end
    else
      DoInternalOpen;
    end
  else if not value and (Fstate <> dsinactive) then
    DoInternalClose(True);
end;

procedure TDataset.Loaded;

begin
  inherited;
  if FOpenAfterRead then SetActive(true);
end;


procedure TDataSet.RecalcBufListSize;

var
  i, j, ABufferCount: Integer;
  DataLink: TDataLink;

begin
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size - check cursor');
{$endif}
  If Not IsCursorOpen Then
    Exit;
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size');
{$endif}
  ABufferCount := DefaultBufferCount;
  for i := 0 to FDataSources.Count - 1 do
    for j := 0 to TDataSource(FDataSources[i]).DataLinks.Count - 1 do
      begin
      DataLink:=TDataLink(TDataSource(FDataSources[i]).DataLinks[j]);
      if DataLink.BufferCount>ABufferCount then
        ABufferCount:=DataLink.BufferCount;
      end;

  If (FBufferCount=ABufferCount) Then
    exit;

{$ifdef dsdebug}
  Writeln('Setting buffer list size');
{$endif}

  SetBufListSize(ABufferCount);
{$ifdef dsdebug}
  Writeln('Getting next buffers');
{$endif}
  GetNextRecords;
{$Ifdef dsDebug}
  WriteLn(
    'SetBufferCount: FActiveRecord=',FActiveRecord,
    ' FCurrentRecord=',FCurrentRecord,
    ' FBufferCount= ',FBufferCount,
    ' FRecordCount=',FRecordCount);
{$Endif}
end;

Procedure TDataset.SetBookmarkStr(const Value: TBookmarkStr);

begin
  GotoBookMark(Pointer(Value))
end;

Procedure TDataset.SetBufListSize(Value: Longint);

Var I : longint;

begin
{$ifdef dsdebug}
  Writeln ('SetBufListSize: ',Value);
{$endif}
  If Value=FBufferCount Then
    exit;
  If Value>FBufferCount then
    begin
{$ifdef dsdebug}
    Writeln ('   Reallocating memory :',(Value+1)*SizeOf(PChar));
{$endif}
    ReAllocMem(FBuffers,(Value+1)*SizeOf(PChar));
{$ifdef dsdebug}
    Writeln ('   Filling memory :',(Value+1-FBufferCount)*SizeOf(PChar));
{$endif}
    FillChar(FBuffers[FBufferCount],(Value+1-FBufferCount)*SizeOF(Pchar),#0);
{$ifdef dsdebug}
    Writeln ('   Filled memory :');
{$endif}
    Try
{$ifdef dsdebug}
      Writeln ('   Assigning buffers :',(Value)*SizeOf(PChar));
{$endif}
      For I:=FBufferCount to Value do
        FBuffers[i]:=AllocRecordBuffer;
{$ifdef dsdebug}
      Writeln ('   Assigned buffers ',FBufferCount,' :',(Value)*SizeOf(PChar));
{$endif}
    except
      I:=FBufferCount;
      While (I<(Value+1)) and (FBuffers[i]<>Nil) do
        begin
        FreeRecordBuffer(FBuffers[i]);
        Inc(i);
        end;
      raise;
    end;
    end
  else
    begin
{$ifdef dsdebug}
    Writeln ('   Freeing buffers :',FBufferCount-Value);
{$endif}
    if (value > -1) and (FActiveRecord>Value-1) then
      begin
      for i := 0 to (FActiveRecord-Value) do
        shiftbuffersbackward;
      FActiverecord := Value -1;
      end;

   If Assigned(FBuffers) then
      begin
      For I:=Value+1 to FBufferCount do
        FreeRecordBuffer(FBuffers[i]);
      ReAllocMem(FBuffers,(Value+1)*SizeOf(Pchar));
      end;
    if FRecordcount > Value then FRecordcount := Value;
    end;
  If Value=-1 then
    Value:=0;
  FBufferCount:=Value;
{$ifdef dsdebug}
  Writeln ('   SetBufListSize: Final FBufferCount=',FBufferCount);
{$endif}
end;

Procedure TDataset.SetChildOrder(Component: TComponent; Order: Longint);

begin
  //!! To be implemented
end;

Procedure TDataset.SetCurrentRecord(Index: Longint);

begin
  If FCurrentRecord<>Index then
    begin
{$ifdef DSdebug}
    Writeln ('Setting current record to',index);
{$endif}
    Case GetBookMarkFlag(FBuffers[Index]) of
      bfCurrent : InternalSetToRecord(FBuffers[Index]);
      bfBOF : InternalFirst;
      bfEOF : InternalLast;
      end;
    FCurrentRecord:=index;
    end;
end;

Procedure TDataset.SetField (Index : Longint;Value : TField);

begin
  //!! To be implemented
end;

Procedure TDataset.SetFilterOptions(Value: TFilterOptions);

begin
  //!! To be implemented
end;

Procedure TDataset.SetFilterText(const Value: string);

begin
  FFilterText := value;
end;

Procedure TDataset.SetFiltered(Value: Boolean);

begin
  FFiltered := value;
end;

Procedure TDataset.SetFound(const Value: Boolean);

begin
  //!! To be implemented
end;

Procedure TDataset.SetModified(Value: Boolean);

begin
  FModified := value;
end;

Procedure TDataset.SetName(const Value: TComponentName);

begin
  //!! To be implemented
  inherited SetName(Value);
end;

Procedure TDataset.SetOnFilterRecord(const Value: TFilterRecordEvent);

begin
  //!! To be implemented
end;

Procedure TDataset.SetRecNo(Value: Longint);

begin
  //!! To be implemented
end;

Procedure TDataset.SetState(Value: TDataSetState);

begin
  If Value<>FState then
    begin
    FState:=Value;
    DataEvent(deUpdateState,0);
    end;
end;

Function TDataset.TempBuffer: PChar;

begin
  //!! To be implemented
end;

Procedure TDataset.UpdateIndexDefs;

begin
  //!! To be implemented
end;



Function TDataset.ControlsDisabled: Boolean;


begin
  //!! To be implemented
end;

Function TDataset.ActiveBuffer: PChar;


begin
{$ifdef dsdebug}
  Writeln ('Active buffer requested. Returning:',ActiveRecord);
{$endif}
  Result:=FBuffers[FActiveRecord];
end;

Procedure TDataset.Append;

begin
  DoInsertAppend(True);
end;

Procedure TDataset.AppendRecord(const Values: array of const);

begin
  //!! To be implemented
end;

Function TDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
{
  Should be overridden by descendant objects.
}
begin
  Result:=False
end;

Procedure TDataset.Cancel;

begin
  If State in [dsEdit,dsInsert] then
    begin
    DataEvent(deCheckBrowseMode,0);
    DoBeforeCancel;
    UpdateCursorPos;
    InternalCancel;
    FreeFieldBuffers;
    if (state = dsInsert) and (FRecordcount = 1) then
      begin
      FEOF := true;
      FBOF := true;
      FRecordcount := 0;
      SetState(dsBrowse);
      DataEvent(deDatasetChange,0);
      end
    else
      begin
      SetState(dsBrowse);
      SetCurrentRecord(FActiverecord);
      resync([]);
      end;
    DoAfterCancel;
    end;
end;

Procedure TDataset.CheckBrowseMode;

begin
  CheckActive;
  DataEvent(deCheckBrowseMode,0);
  If State In [dsedit,dsinsert] then
    begin
    UpdateRecord;
    If Modified then
      Post
    else
      Cancel;
    end;
end;

Procedure TDataset.ClearFields;


begin
  //!! To be implemented
end;

Procedure TDataset.Close;

begin
  Active:=False;
end;

Function TDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;

begin
  Result:=0;
end;

Function TDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;


begin
  Result:=Nil;
end;

Procedure TDataset.CursorPosChanged;


begin
  FCurrentRecord:=-1;
end;

Procedure TDataset.Delete;


begin
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  if State in [dsInsert] then
  begin
    Cancel;
  end else begin
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Delete: checking required fields');
{$endif}
    DoBeforeDelete;
    DoBeforeScroll;
    If Not TryDoing(@InternalDelete,OnPostError) then exit;
{$ifdef dsdebug}
    writeln ('Delete: Internaldelete succeeded');
{$endif}
    FreeFieldBuffers;
    SetState(dsBrowse);
{$ifdef dsdebug}
    writeln ('Delete: Browse mode set');
{$endif}
    SetCurrentRecord(FActiverecord);
    Resync([]);
    DoAfterDelete;
    DoAfterScroll;
  end;
end;

Procedure TDataset.DisableControls;


begin
  If FDisableControlsCount=0 then
    begin
    { Save current state,
      needed to detect change of state when enabling controls.
    }
    FDisableControlsState:=FState;
    FEnableControlsEvent:=deDatasetChange;
    end;
  Inc(FDisableControlsCount);
end;

Procedure TDataset.DoInsertAppend(DoAppend : Boolean);


  procedure DoInsert;

  Var BookBeforeInsert : TBookmarkStr;
      TempBuf : pointer;

  begin
  // need to scroll up al buffers after current one,
  // but copy current bookmark to insert buffer.
  If FRecordcount > 0 then BookBeforeInsert:=Bookmark;

  if FActiveRecord < FRecordCount-1 then
    begin
    TempBuf := FBuffers[FBuffercount];
    move(FBuffers[FActiveRecord],FBuffers[FActiveRecord+1],(Fbuffercount-FActiveRecord)*sizeof(FBuffers[0]));
    FBuffers[FActiveRecord]:=TempBuf;
    end
  else
    inc(FActiveRecord);

  // Active buffer is now edit buffer. Initialize.
  InitRecord(FBuffers[FActiveRecord]);
  cursorposchanged;

  // Put bookmark in edit buffer.
  if FRecordCount=0 then
    begin
    fEOF := false;
    SetBookmarkFlag(ActiveBuffer,bfBOF)
    end
  else
    begin
    fBOF := false;
    if FRecordcount > 0 then SetBookMarkData(ActiveBuffer,pointer(BookBeforeInsert));
    end;
  // update buffer count.
  If FRecordCount<FBufferCount then
    Inc(FRecordCount);
  end;
  
begin
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  CheckBrowseMode;
  DoBeforeInsert;
  DoBeforeScroll;
  If Not DoAppend then
    begin
{$ifdef dsdebug}
    Writeln ('going to insert mode');
{$endif}

    DoInsert;
    end
  else
    begin
{$ifdef dsdebug}
    Writeln ('going to append mode');
{$endif}
    ClearBuffers;
    InternalLast;
    GetPriorRecords;
    FActiveRecord:=FRecordCount-1;
    DoInsert;
    SetBookmarkFlag(ActiveBuffer,bfEOF);
    FEOF := true;
    end;
  SetState(dsInsert);
  try
    DoOnNewRecord;
  except
    SetCurrentRecord(FActiverecord);
    resync([]);
    raise;
  end;
  // mark as not modified.
  FModified:=False;
  // Final events.
  DataEvent(deDatasetChange,0);
  DoAfterInsert;
  DoAfterScroll;
{$ifdef dsdebug}
  Writeln ('Done with append');
{$endif}
end;

Procedure TDataset.Edit;

begin
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  If State in [dsedit,dsinsert] then exit;
  If FRecordCount = 0 then
    begin
    Append;
    Exit;
    end;
  CheckBrowseMode;
  DoBeforeEdit;
  If Not TryDoing(@InternalEdit,OnEditError) then
    exit;
  SetState(dsedit);
  DataEvent(deRecordChange,0);
  DoAfterEdit;
end;

Procedure TDataset.EnableControls;


begin
  If FDisableControlsCount>0 then
    begin
    Dec(FDisableControlsCount);
    If FDisableControlsCount=0 then
      begin
      // State changed since disablecontrols ?
      If FDisableControlsState<>FState then
        DataEvent(deUpdateState,0);
      If (FDisableControlsState<>dsInactive) and (FState<>dsInactive) then
        DataEvent(FEnableControlsEvent,0);
      end;
    end;
end;

Function TDataset.FieldByName(const FieldName: string): TField;


begin
  Result:=FindField(FieldName);
  If Result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[FieldName],Self);
end;

Function TDataset.FindField(const FieldName: string): TField;


begin
  Result:=FFieldList.FindField(FieldName);
end;

Function TDataset.FindFirst: Boolean;


begin
  //!! To be implemented
end;

Function TDataset.FindLast: Boolean;


begin
  //!! To be implemented
end;

Function TDataset.FindNext: Boolean;


begin
  //!! To be implemented
end;

Function TDataset.FindPrior: Boolean;


begin
  //!! To be implemented
end;

Procedure TDataset.First;


begin
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalFirst;
    GetNextRecords;
  finally
    FBOF:=True;
    DataEvent(deDatasetChange,0);
    DoAfterScroll;
  end;
end;

Procedure TDataset.FreeBookmark(ABookmark: TBookmark);


begin
  FreeMem(ABookMark,FBookMarkSize);
end;

Function TDataset.GetBookmark: TBookmark;


begin
  if BookmarkAvailable then
    begin
    GetMem (Result,FBookMarkSize);
    GetBookMarkdata(ActiveBuffer,Result);
    end
  else
    Result:=Nil;
end;

Function TDataset.GetCurrentRecord(Buffer: PChar): Boolean;


begin
  Result:=False;
end;

Procedure TDataset.GetFieldList(List: TList; const FieldNames: string);

  Function NextName(Var S : String) : String;
  
  Var
    P : integer;
  
  begin
    P:=Pos(';',S);
    If (P=0) then
      P:=Length(S)+1;
    Result:=Copy(S,1,P-1);   
    system.Delete(S,1,P);
  end;

var
  F: TField;
  Names,N : String;
  
begin
  Names:=FieldNames;
  N:=Nextname(Names);
  while (N<>'') do
    begin
    F:=FieldByName(N);
    If Assigned(List) then
      List.Add(F);
    N:=NextName(Names);
    end;
end;

Procedure TDataset.GetFieldNames(List: TStrings);


begin
  FFieldList.GetFieldNames(List);
end;

Procedure TDataset.GotoBookmark(ABookmark: TBookmark);


begin
  If Assigned(ABookMark) then
    begin
    CheckBrowseMode;
    DoBeforeScroll;
    InternalGotoBookMark(ABookMark);
    Resync([rmExact,rmCenter]);
    DoAfterScroll;
    end;
end;

Procedure TDataset.Insert;

begin
  DoInsertAppend(False);
end;

Procedure TDataset.InsertRecord(const Values: array of const);


begin
  //!! To be implemented
end;

Function TDataset.IsEmpty: Boolean;

begin
  Result:=(Bof and Eof);
end;

Function TDataset.IsSequenced: Boolean;

begin
  //!! To be implemented
end;

Procedure TDataset.Last;

begin
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalLast;
    GetPriorRecords;
    FActiveRecord:=FRecordCount-1;
  finally
    FEOF:=true;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

Function TDataset.MoveBy(Distance: Longint): Longint;
Var
  TheResult: Integer;

  Function Scrollforward : Integer;

  begin
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling forward :',Distance);
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCount   : ',FRecordCount);
    WriteLn('BufferCount   : ',FBufferCount);
{$endif}
    FBOF:=False;
    While (Distance>0) and not FEOF do
      begin
      If FActiveRecord<FRecordCount-1 then
        begin
        Inc(FActiveRecord);
        Dec(Distance);
        Inc(TheResult); //Inc(Result);
        end
      else
        begin
{$ifdef dsdebug}
       Writeln('Moveby : need next record');
{$endif}
        If GetNextRecord then
          begin
          Dec(Distance);
          Dec(Result);
          Inc(TheResult); //Inc(Result);
          end
        else
          FEOF:=true;
        end;
      end
  end;
  Function ScrollBackward : Integer;

  begin
    if FIsUniDirectional then DatabaseError(SUniDirectional);
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling backward:',Abs(Distance));
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
    WriteLn('BufferCount   : ',FBufferCount);
{$endif}
    FEOF:=False;
    While (Distance<0) and not FBOF do
      begin
      If FActiveRecord>0 then
        begin
        Dec(FActiveRecord);
        Inc(Distance);
        Dec(TheResult); //Dec(Result);
        end
      else
        begin
       {$ifdef dsdebug}
       Writeln('Moveby : need next record');
       {$endif}
        If GetPriorRecord then
          begin
          Inc(Distance);
          Inc(Result);
          Dec(TheResult); //Dec(Result);
          end
        else
          FBOF:=true;
        end;
      end
  end;

Var
  PrevRecordCount : Integer;
  Scrolled : Integer;

begin
  CheckBrowseMode;
  Result:=0; TheResult:=0;
  PrevRecordCount:=FRecordCount;
  If ((Distance>0) and FEOF) or
     ((Distance<0) and FBOF) then
    exit;
  DoBeforeScroll;
  Try
    If Distance>0 then
      Scrolled:=ScrollForward
    else
      Scrolled:=ScrollBackward;
  finally
{$ifdef dsdebug}
    WriteLn('ActiveRecord=', FActiveRecord,' FEOF=',FEOF,' FBOF=',FBOF);
{$Endif}
    If FRecordCount<>PrevRecordCount then
      DataEvent(deDatasetChange,0)
    else
      DataEvent(deDatasetScroll,Scrolled);
    DoAfterScroll;
    Result:=TheResult;
  end;
end;

Procedure TDataset.Next;

begin
  MoveBy(1);
end;

Procedure TDataset.Open;

begin
  Active:=True;
end;

Procedure TDataset.Post;

  Procedure Checkrequired;

    Var I : longint;

  begin
    For I:=0 to FFieldList.Count-1 do
      With FFieldList[i] do
        // Required fields that are NOT autoinc !! Autoinc cannot be set !!
        if Required and not ReadOnly and
           (FieldKind=fkData) and Not (DataType=ftAutoInc) then
          DatabaseErrorFmt(SNeedField,[DisplayName],Self);
  end;

begin
  if State in [dsEdit,dsInsert] then
    begin
    DataEvent(deUpdateRecord,0);
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Post: checking required fields');
{$endif}
    CheckRequired;
    DoBeforePost;
    If Not TryDoing(@InternalPost,OnPostError) then exit;
    cursorposchanged;
{$ifdef dsdebug}
    writeln ('Post: Internalpost succeeded');
{$endif}
    FreeFieldBuffers;
//    SetCurrentRecord(FActiverecord);
    Resync([]);
    SetState(dsBrowse);
{$ifdef dsdebug}
    writeln ('Post: Browse mode set');
{$endif}
    DoAfterPost;
    end;
end;

Procedure TDataset.Prior;

begin
  MoveBy(-1);
end;

Procedure TDataset.Refresh;

begin
  CheckbrowseMode;
  UpdateCursorPos;
  InternalRefresh;
  SetCurrentRecord(FActiverecord);
  Resync([]);
end;

Procedure TDataset.RegisterDataSource(ADatasource : TDataSource);

begin
  FDatasources.Add(ADataSource);
  RecalcBufListSize;
end;


Procedure TDataset.Resync(Mode: TResyncMode);

var i,count : integer;

begin
  // See if we can find the requested record.
{$ifdef dsdebug}
    Writeln ('Resync called');
{$endif}

// place the cursor of the underlying dataset to the active record
//  SetCurrentRecord(FActiverecord);

// Now look if the data on the current cursor of the underlying dataset is still available
  If GetRecord(Fbuffers[0],gmcurrent,False)<>grOk Then
// If that fails and rmExact is set, then raise an exception
    If rmExact in Mode then
      DatabaseError(SNoSuchRecord,Self)
// else, if rmexact is not set, try to fetch the next  or prior record in the underlying dataset
    else if (GetRecord(Fbuffers[0],gmnext,True)<>grOk) and
            (GetRecord(Fbuffers[0],gmprior,True)<>grOk) then
      begin
{$ifdef dsdebug}
      Writeln ('Resync: fuzzy resync');
{$endif}
      // nothing found, invalidate buffer and bail out.
      ClearBuffers;
      DataEvent(deDatasetChange,0);
      exit;
      end;
  FCurrentRecord := 0;
  FEOF := false;
  FBOF := false;

// If we've arrived here, FBuffer[0] is the current record
  If (rmCenter in Mode) then
    count := (FRecordCount div 2)
  else
    count := FActiveRecord;
  i := 0;
  FRecordcount := 1;
  FActiveRecord := 0;

// Fill the buffers before the active record
  while (i < count) and GetPriorRecord do
    inc(i);
  FActiveRecord := i;
// Fill the rest of the buffer
  getnextrecords;
// If the buffer is not full yet, try to fetch some more prior records
  if FRecordcount < FBuffercount then inc(FActiverecord,getpriorrecords);
// That's all folks!
  DataEvent(deDatasetChange,0);
end;

Procedure TDataset.SetFields(const Values: array of const);

Var I  : longint;

begin
  For I:=0 to high(Values) do
    Case Values[I].vtype of
      vtInteger : FieldByNumber(i).AsLongInt:=Values[I].VInteger;
      // needs Completion..
    end;
end;

Function TDataset.Translate(Src, Dest: PChar; ToOem: Boolean): Integer;

begin
  //!! To be implemented
end;

Function Tdataset.TryDoing (P : TDataOperation; Ev : TDatasetErrorEvent) : Boolean;

Var Retry : TDataAction;

begin
{$ifdef dsdebug}
  Writeln ('Trying to do');
  If P=Nil then writeln ('Procedure to call is nil !!!');
{$endif dsdebug}
  Result:=True;
  Retry:=daRetry;
  while Retry=daRetry do
    Try
{$ifdef dsdebug}
      Writeln ('Trying : updatecursorpos');
{$endif dsdebug}
      UpdateCursorPos;
{$ifdef dsdebug}
      Writeln ('Trying to do it');
{$endif dsdebug}
      P;
      exit;
    except
      On E : EDatabaseError do
        begin
        retry:=daFail;
        If Assigned(Ev) then
          Ev(Self,E,Retry);
        Case Retry of
          daFail : Raise;
          daAbort : Result:=False;
        end;
        end;
    else
      Raise;
    end;
{$ifdef dsdebug}
  Writeln ('Exit Trying to do');
{$endif dsdebug}
end;

Procedure TDataset.UpdateCursorPos;

begin
  If FRecordCount>0 then
    SetCurrentRecord(FactiveRecord);
end;

Procedure TDataset.UpdateRecord;

begin
  if not (State in dsEditModes) then
    DatabaseError(SNotInEditState, Self);
  DataEvent(deUpdateRecord, 0);
end;

Procedure TDataset.RemoveField (Field : TField);

begin
  //!! To be implemented
end;

Function TDataset.Getfieldcount : Longint;

begin
  Result:=FFieldList.Count;
end;

Procedure TDataset.ShiftBuffersBackward;

var TempBuf : pointer;

begin
  TempBuf := FBuffers[0];
  move(FBuffers[1],FBuffers[0],(fbuffercount)*sizeof(FBuffers[0]));
  FBuffers[buffercount]:=TempBuf;
end;

Procedure TDataset.ShiftBuffersForward;

var TempBuf : pointer;

begin
  TempBuf := FBuffers[FBufferCount];
  move(FBuffers[0],FBuffers[1],(fbuffercount)*sizeof(FBuffers[0]));
  FBuffers[0]:=TempBuf;
end;

function TDataset.GetFieldValues(Fieldname : string) : string;

begin
  result := findfield(Fieldname).asstring;
end;

procedure TDataset.SetFieldValues(Fieldname : string;value : string);

begin
  findfield(Fieldname).asstring := value;
end;


Procedure TDataset.UnRegisterDataSource(ADatasource : TDatasource);

begin
  FDataSources.Remove(ADataSource);
end;


{
  $Log$
  Revision 1.28  2004-11-05 08:32:02  michael
  TBufDataset.inc:
    - replaced Freemem by Reallocmem, Free by FreeAndNil

  Database.inc:
    - Moved Active property from TSQLTransaction to TDBTransaction
    - Gives an error if the database of an active transaction is changed

  Dataset.inc
    - Don't distribute events if FDisableControlsCount > 0
    - Replaced FActive by FState<>dsInactive
    - Set EOF after append

  db.pp:
    - Removed duplicate definition of TAlignment
    - Moved Active property from TSQLTransaction to TDBTransaction
    - Replaced FActive by FState<>dsInactive
    - Gives an error if the database of an active transaction is changed

  sqldb:
    - Moved Active property from TSQLTransaction to TDBTransaction
    - replaced Freemem by Reallocmem, Free by FreeAndNil

  IBConnection:
    - Moved FSQLDAAllocated to the cursor

  PQConnection:
    - Don't try to free the statement if a fatal error occured

  Revision 1.27  2004/10/27 07:23:13  michael
  + Patch from Joost Van der Sluis to fix transactions

  Revision 1.26  2004/10/16 09:27:23  michael
  + Fixed GotoBookMark (as suggested by Americo Luiz)

  Revision 1.25  2004/10/10 14:25:21  michael
  + Small fix for close so it does not check browsemode

  Revision 1.24  2004/09/26 16:55:24  michael
  * big patch from Joost van der Sluis
   bufdataset.inc:
    fix getrecord (prior)
    getcanmodify default false
  database.inc / db.inc:
    Added transactions
  dataset.inc:
    raise error if trying to insert into an readonly dataset
  db.inc:
    remove published properties from bufdataset
    changed ancestor of tbufdataset to tdbdataset

  Revision 1.23  2004/09/15 12:22:33  michael
  Suggested fix from Luiz Americo to .resync method

  Revision 1.22  2004/08/30 12:02:17  michael
  + Patch from Joost van der Sluis for Insert/Resync

  Revision 1.21  2004/08/23 07:30:19  michael
  + Fixes from joost van der sluis: tfieldsdefs.tdatafield and size, cancel of only record and dataset.fieldvalyes

  Revision 1.20  2004/08/21 21:10:00  michael
  * Patch from Joost van der Sluis
    - Empty recordsets don't show any bogus data anymore
    - Floatfiels.gettext fix
    - SetBufListsize fix forTDBGrid

  Revision 1.19  2004/08/14 12:46:36  michael
  + Patch from Joost van der Sluis to implement Modified and UpdateRecord event

  Revision 1.18  2004/08/13 07:06:02  michael
  + Rework of buffer management by Joost Van der Sluis

  Revision 1.17  2004/08/03 19:08:48  michael
  + Latest patch from Micha Nelissen

  Revision 1.16  2004/08/02 15:13:42  michael
  + Patch from Micha Nelissen to implement Delete method

  Revision 1.15  2004/07/25 11:32:40  michael
  * Patches from Joost van der Sluis
    interbase.pp:
        * Removed unused Fprepared
        * Changed the error message 'database connect string not filled
          in' to 'database connect string (databasename) not filled in'
        * Preparestatement and execute now checks if transaction is
          assigned (in stead of crashing if it isn't) and if the
          transaction isn't started, it calls starttransaction.

     dataset.inc:
        * In DoInternalOpen the buffers are now initialised before the
          dataset is set into browse-state

     database.inc and db.pp:
        * If the dataset is created from a stream, the database is opened
          after the dataset is read completely

  Revision 1.13  2004/05/02 21:23:18  peter
    * use ptrint

  Revision 1.12  2004/03/25 20:43:39  michael
  Some compatibility additions

  Revision 1.11  2004/01/05 21:21:38  michael
  + Fix in setbuflistsize for when Value=-1

  Revision 1.10  2003/11/09 21:23:10  michael
  + Patch from Micha Nelissen, fixing some Delphi compatibility issues

  Revision 1.9  2003/10/06 17:04:28  florian
    * small step towards calculated fields

  Revision 1.8  2003/05/06 12:08:52  michael
  + fixed dataset opening buffer issues

  Revision 1.7  2003/02/20 19:25:19  michael
  + Fixes from Jesus Reyes

  Revision 1.6  2002/09/07 15:15:22  peter
    * old logs removed and tabs fixed

}
