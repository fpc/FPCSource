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


Procedure TDataset.ActivateBuffers;

begin
  FBOF:=False;
  FEOF:=False;
  FRecordCount:=1;
  FActiveRecord:=0;
end;

Procedure TDataset.UpdateFieldDefs;

begin
  //!! To be implemented
end;

Procedure TDataset.BindFields(Binding: Boolean);

Var I : longint;

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
  //!! To be implemented
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

Procedure TDataset.DataEvent(Event: TDataEvent; Info: Longint);

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
  FBufferCount:=0;
  FDefaultFields:=FieldCount=0;
  DoBeforeOpen;
  Try
    {$ifdef dsdebug}
    Writeln ('Calling internal open');
    {$endif}
    InternalOpen;
    FBOF:=True;
    {$ifdef dsdebug}
    Writeln ('Setting state to browse');
    {$endif}
    SetState(dsBrowse);
    {$ifdef dsdebug}
    Writeln ('Setting buffer size');
    {$endif}
    SetBufListSize(DefaultBufferCount);
    {$ifdef dsdebug}
    Writeln ('Getting next records');
    {$endif}
    GetNextRecords;
    DoAfterOpen;
    DoAfterScroll;
  except
    SetState(dsInactive);
    DoInternalClose;
    raise;
  end;
end;

Function TDataset.RequiredBuffers : longint;
{
  If later some datasource requires more buffers (grids etc)
  then it should be taken into account here...
}

begin
  Result:=0;
end;

Procedure TDataset.DoInternalClose;

begin
  FreeFieldBuffers;
  ClearBuffers;
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
  Result:=True;
end;

Procedure TDataset.GetChildren(Proc: TGetChildProc; Root: TComponent);

begin
  //!! To be implemented
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

Var Shifted : Boolean;

begin
{$ifdef dsdebug}
  Writeln ('Getting next record. Internal RecordCount : ',FRecordCount);
{$endif}
  Shifted:=FRecordCount=FBufferCount;
  If Shifted then
    begin
    ShiftBuffers(0,1);
    Dec(FRecordCount);
    end;
{$ifdef dsdebug}
  Writeln ('Getting data into buffer : ',FRecordCount);
{$endif}
  Result:=GetRecord(FBuffers[FRecordCount],gmNext,True)=grOK;
  If Result then
    begin
    If FRecordCount=0 then
      ActivateBuffers
    else
      If FRecordCount<FBufferCount then
        Inc(FRecordCount);
    FCurrentRecord:=FRecordCount;
    end
  else
    begin
    if shifted then
      begin
      ShiftBuffers(0,-1);
      inc(FRecordCount);
      end;
    CursorPosChanged;
    end;
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
  Writeln ('Result Getting next record(s), GOT :',RESULT);
{$endif}
end;

Function TDataset.GetPriorRecord: Boolean;

Var Shifted : boolean;

begin
{$ifdef dsdebug}
  Writeln ('Getting previous record');
{$endif}
  Shifted:=FRecordCount>0;
  If Shifted Then
    begin
    SetCurrentRecord(0);
    ShiftBuffers(0,-1);
    end;
  Result:=GetRecord(FBuffers[0],gmPrior,True)=grOK;
  If Result then
    begin
    If FRecordCount=0 then
      ActivateBuffers
    else
      begin
      If FrecordCount<FBufferCount then
        Inc(FRecordCount);
      end;
    FCurrentRecord:=0;
    end
  else
    begin
    If Shifted then
      begin
      ShiftBuffers(0,1);
      end;
    CursorPosChanged;
    end;
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

Procedure TDataset.Loaded;

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

Procedure TDataset.RestoreState(const Value: TDataSetState);

begin
  //!! To be implemented
end;

Procedure TDataset.SetActive (Value : Boolean);

begin
  If Value<>Factive then
    If Value then
      DoInternalOpen
    else
      DoInternalClose;
  FActive:=Value;
end;

Procedure TDataset.SetBookmarkStr(const Value: TBookmarkStr);

begin
  GotoBookMark(Pointer(Value))
end;

Procedure TDataset.SetBufListSize(Value: Longint);

Var I : longint;

begin
  If Value=FBufferCount Then exit;
  I:=RequiredBuffers; // Save 1 call.
  If Value<I Then
    Value:=I;
  If Value>FBufferCount then
    begin
   {$ifdef dsdebug}
    Writeln ('Reallocating memory :',(Value+1)*SizeOf(PChar));
   {$endif}
    ReAllocMem(FBuffers,(Value+1)*SizeOf(PChar));
   {$ifdef dsdebug}
    Writeln ('Filling memory :',(Value-FBufferCount)*SizeOf(PChar));
   {$endif}
    FillChar(FBuffers[FBufferCount],(Value+1-FBufferCount)*SizeOF(Pchar),#0);
   {$ifdef dsdebug}
    Writeln ('Filled memory :');
   {$endif}
    Try
     {$ifdef dsdebug}
      Writeln ('Assigning buffers :',(Value+1)*SizeOf(PChar));
     {$endif}
      For I:=FBufferCount to Value do
        FBuffers[i]:=AllocRecordBuffer;
     {$ifdef dsdebug}
      Writeln ('Assigned buffers :',(Value+1)*SizeOf(PChar));
     {$endif}
    except
      I:=FBufferCount;
      While (I<=Value) and (FBuffers[i]<>Nil) do
        begin
        FreeRecordBuffer(FBuffers[i]);
        Inc(i);
        end;
      raise;
    end;
    end
  else
    begin
    For I:=Value+1 to FBufferCount do
      FreeRecordBuffer(FBuffers[i]);
    ReAllocMem(FBuffers,Value*SizeOf(Pchar));
    end;
  FBufferCount:=Value;
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
  //!! To be implemented
end;

Procedure TDataset.SetFiltered(Value: Boolean);

begin
  //!! To be implemented
end;

Procedure TDataset.SetFound(const Value: Boolean);

begin
  //!! To be implemented
end;

Procedure TDataset.SetModified(Value: Boolean);

begin
  //!! To be implemented
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

Function TDataset.SetTempState(const Value: TDataSetState): TDataSetState;

begin
  //!! To be implemented
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
//  Writeln ('Active buffer requested. Returning:',ActiveRecord);
{$endif}
  Result:=FBuffers[ActiveRecord];
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
    SetState(dsBrowse);
    Resync([]);
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
  //!! To be implemented
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

Var Buffer : PChar;
    BookBeforeInsert : TBookmarkStr;

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
    // need to scroll up al buffers after current one,
    // but copy current bookmark to insert buffer.
    BookBeforeInsert:=Bookmark;
    ShiftBuffers(1,FActiveRecord);
    // Active buffer is now edit buffer. Initialize.
    InitRecord(ActiveBuffer);
    // Put bookmark in edit buffer.
    if FRecordCount=0 then
      SetBookmarkFlag(ActiveBuffer,bfBOF)
    else
      SetBookMarkData(ActiveBuffer,Pointer(BookBeforeInsert));
    // update buffer count.
    If FRecordCount<FBufferCount then
      Inc(FRecordCount);
    end
  else
    // Tricky, need to get last record and scroll down.
    begin
    {$ifdef dsdebug}
    Writeln ('going to append mode');
    {$endif}
    Buffer:=FBuffers[0];
    InitRecord(Buffer);
    // just mark buffer as last. GetPreviousrecords will do an internallast
    // Because of this...
    SetBookMarkFlag(Buffer,bfEOF);
    FRecordCount:=1;
    {$ifdef dsdebug}
    Writeln ('getting prior records');
    {$endif}
    GetPriorRecords;
    // update active record.
    FactiveRecord:=FRecordCount-1;
    end;
  SetState(dsInsert);
  try
    DoOnNewRecord;
  except
    UpdateCursorPos;
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
    Insert;
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


begin

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

  Function Scrollforward : Integer;

  begin
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling forward :',Distance);
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
{$endif}
    While (Distance>0) and not FEOF do
      begin
      If FActiveRecord<FRecordCount-1 then
        begin
        Inc(FActiveRecord);
        Dec(Distance);
        Inc(Result);
        end
      else
        begin
       {$ifdef dsdebug}
       Writeln('Moveby : need next record');
       {$endif}
        If GetNextRecord then
          begin
          Dec(Distance);
          Inc(result);
          end
        else
          FEOF:=true;
        end;
      end
  end;

  Function ScrollBackward : Integer;

  begin
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling backward:',Abs(Distance));
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
{$endif}
    While (Distance<0) and not FBOF do
      begin
      If FActiveRecord>0 then
        begin
        Dec(FActiveRecord);
        Inc(Distance);
        Dec(Result);
        end
      else
        begin
       {$ifdef dsdebug}
       Writeln('Moveby : need next record');
       {$endif}
        If GetPriorRecord then
          begin
          Inc(Distance);
          Dec(Result);
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
  Result:=0;
  PrevRecordCount:=0;
  DoBeforeScroll;
  If ((Distance>0) and FEOF) or
     ((Distance<0) and FBOF) then
    exit;
  Try
    If Distance>0 then
      Scrolled:=ScrollForward
    else
      Scrolled:=ScrollBackward;
  finally
     If FRecordCount<>PrevRecordCount then
       DataEvent(deDatasetChange,0)
     else
       DataEvent(deDatasetScroll,Scrolled);
     DoAfterScroll;
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
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Post: checking required fields');
{$endif}
    CheckRequired;
    DoBeforePost;
    If Not TryDoing(@InternalPost,OnPostError) then exit;
{$ifdef dsdebug}
    writeln ('Post: Internalpost succeeded');
{$endif}
    FreeFieldBuffers;
{$ifdef dsdebug}
    writeln ('Post: Freeing field buffers');
{$endif}
    SetState(dsBrowse);
{$ifdef dsdebug}
    writeln ('Post: Browse mode set');
{$endif}
    Resync([]);
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
  Resync([]);
end;

procedure TDataSet.RecalcBufListSize;
var
  i, j, MaxValue: Integer;
  DataLink: TDataLink;
begin
  MaxValue := 0;
  for i := 0 to FDataSources.Count - 1 do
    for j := 0 to TDataSource(FDataSources[i]).DataLinks.Count - 1 do
    begin
      DataLink := TDataLink(TDataSource(FDataSources[i]).DataLinks[j]);
      if DataLink.BufferCount > MaxValue then
        MaxValue := DataLink.BufferCount;
    end;
  SetBufListSize(MaxValue);
end;

Procedure TDataset.RegisterDataSource(ADatasource : TDataSource);

begin
  FDatasources.Add(ADataSource);
  RecalcBufListSize;
end;


Procedure TDataset.Resync(Mode: TResyncMode);

Var Count,ShiftCount : Longint;

begin
  // See if we can find the requested record.
  If rmExact in Mode then
    begin
    { throw an exception if not found.
      Normally the descendant should do this if DoCheck is true. }
    If GetRecord(Fbuffers[FRecordCount-1],gmcurrent,True)<>grOk Then
      DatabaseError(SNoSuchRecord,Self);
    end
  else
    { Can we find a record in the neighbourhood ?
      Use Shortcut evaluation for this, or we'll have some funny results. }
    If (GetRecord(Fbuffers[FRecordCount-1],gmcurrent,True)<>grOk) and
       (GetRecord(Fbuffers[FRecordCount-1],gmprior,True)<>grOk) and
       (GetRecord(Fbuffers[FRecordCount-1],gmprior,True)<>grOk) then
       begin
       // nothing found, invalidate buffer and bail out.
       ClearBuffers;
       DataEvent(deDatasetChange,0);
       Exit;
       end;
  If (rmCenter in Mode) then
    ShiftCount:=FbufferCount div 2
  else
    // keep current position.
    ShiftCount:=FActiveRecord;
  // Reposition on 0
  ShiftBuffers(0,FRecordCount-1);
  ActivateBuffers;
  try
    Count:=0;
    {$ifdef dsdebug}
    Writeln ('Getting previous',ShiftCount,' records');
    {$endif}
    While (Count<ShiftCount) and GetPriorRecord do Inc(Count);
    FActiveRecord:=Count;
    // fill rest of buffers, adjust ActiveBuffer.
    SetCurrentRecord(FRecordCount-1);
    GetNextRecords;
    Inc(FActiveRecord,GetPriorRecords);
  finally
    // Notify Everyone
    DataEvent(deDatasetChange,0);
  end;
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

Procedure TDataset.Translate(Src, Dest: PChar; ToOem: Boolean);

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
  if not (State in dsEditModes) then DatabaseError(SNotInEditState, Self);
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

Procedure TDataset.ShiftBuffers (Offset, Distance : longint);

Var Temp : Pointer;
    MoveSize : Longint;

  Procedure ShiftBuffersUp;
  begin
    {$ifdef DSDEBUG}
    writeln ('Shifting buffers up from ',OffSet,' with distance :',Distance);
    writeln ('Moving ',(FBufferCount-Distance), ' Buffers at ',Distance);
    {$endif}
    Move(FBuffers[Offset],Temp^,MoveSize);
    Move(FBuffers[Offset+Distance],FBuffers[Offset],(FBufferCount-Distance-Offset)*SizeOf(Pchar));
    Move(Temp^,FBuffers[FBufferCount-Distance-Offset],MoveSize);
  end;

  Procedure ShiftBuffersDown;

  begin
    // Distance is NEGATIVE
    {$ifdef DSDEBUG}
    writeln ('Shifting buffers down with distance :',Abs(Distance));
    writeln ('Moving ',Movesize div 4,' Buffers at ',FBufferCount+Distance);
    {$endif}
    Move(FBuffers[FbufferCount+Distance],Temp^ ,MoveSize);
    Move(FBuffers[0],FBuffers[Abs(Distance)],(FBufferCount+Distance)*SizeOf(Pchar));
    Move(Temp^ ,FBuffers[0],MoveSize);
  end;

begin
  If Abs(Distance)>=BufferCount then Exit;
  try
    MoveSize:=SizeOf(Pchar)*Abs(Distance);
    GetMem(Temp,MoveSize);
    If Distance<0 Then
      ShiftBuffersDown
    else If Distance>0 then
      ShiftBuffersUp;
  Finally
    FreeMem(temp);
  end;
end;

Procedure TDataset.UnRegisterDataSource(ADatasource : TDatasource);

begin
  FDataSources.Remove(ADataSource);
end;


{
  $Log$
  Revision 1.6  2002-09-07 15:15:22  peter
    * old logs removed and tabs fixed

}
