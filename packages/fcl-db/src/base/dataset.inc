{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Michael Van Canneyt, member of the
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
  FFlatFieldDefList:=TFieldDefList.Create(Self);
  FFlatFieldList:=TFieldList.Create(Self);
  FFieldDefs:=FieldDefsClass.Create(Self);
  FFieldList:=FieldsClass.Create(Self);
  FDataSources:=TFPList.Create;
  FConstraints:=TCheckConstraints.Create(Self);

// FBuffer must be allocated on create, to make Activebuffer return nil
  ReAllocMem(FBuffers,SizeOf(TRecordBuffer));
//  pointer(FBuffers^) := nil;
  FBuffers[0] := nil;
  FActiveRecord := 0;
  FBufferCount := -1;
  FEOF := True;
  FBOF := True;
  FIsUniDirectional := False;
  FAutoCalcFields := True;
  FNestedDataSetClass := TDataSetClass(ClassType);
end;



destructor TDataSet.Destroy;

var
  i: Integer;

begin
  if Assigned(FDataSetField) then
    DataSetField:=nil;
  Active:=False;
  FFlatFieldDefList.Locked:=True;
  FFlatFieldList.Locked:=True;
  FreeAndNil(FFlatFieldDefList);
  FreeAndNil(FFlatFieldList);
  FFieldDefs.Free;
  FFieldList.Free;
  While MyDatasourceCount>0 do
    MyDataSources[MyDatasourceCount - 1].DataSet:=Nil;
  FDatasources.Free;
  for i := 0 to FBufferCount do
    FreeRecordBuffer(FBuffers[i]);
  FConstraints.Free;
  FreeMem(FBuffers);
  Inherited Destroy;
  FreeAndNil(FNestedDataSets);
end;


function TDataSet.GetNestedDataSets: TFPList;

begin
  if FNestedDataSets=nil then
    FNestedDataSets:=TFPList.Create;
  Result:=FNestedDataSets;
end;


procedure TDataSet.SetDataSetField(const AValue: TDataSetField);

begin
  if AValue=FDataSetField then
    Exit;
  if Assigned(AValue) and ((AValue.DataSet=Self) or
     (Assigned(AValue.DataSet.GetDataSource) and (AValue.DataSet.GetDataSource.DataSet=Self))) then
    DatabaseError(SCircularDataLink,Self);
  if Assigned(AValue) and Assigned(AValue.DataSet) and not InheritsFrom(AValue.DataSet.FNestedDataSetClass) then
    DatabaseErrorFmt(SNestedDataSetClass,[AValue.DataSet.FNestedDataSetClass.ClassName],Self);
  if Active then
    Close;
  if Assigned(FDataSetField) then
    FDataSetField.AssignNestedDataSet(nil);
  FDataSetField:=AValue;
  if Assigned(AValue) then
    begin
    AValue.AssignNestedDataSet(Self);
    if Assigned(AValue.DataSet) and AValue.DataSet.Active then
      Open;
    end;
end;


function TDataSet.CreateNestedDataSet(aDataSetField: TDataSetField): TDataSet;

begin
  Result:=NestedDataSetClass.Create(aDataSetField);
  try
    Result.ObjectView:=True;
    Result.DataSetField:=aDataSetField;
  except
    Result.Free;
    raise;
  end;
end;


procedure TDataSet.CheckParentState;

begin
  if Assigned(FDataSetField) and Assigned(FDataSetField.DataSet) then
    FDataSetField.DataSet.Edit;
end;


procedure TDataSet.BeginParentUpdate;

begin
  Inc(FParentUpdateCount);
end;


procedure TDataSet.EndParentUpdate;

begin
  if FParentUpdateCount>0 then
    Dec(FParentUpdateCount);
end;


// Opens the nested datasets that are not open yet
procedure TDataSet.OpenNestedDataSets;

var
  i: Integer;

begin
  if Assigned(FNestedDataSets) then
    for i:=0 to FNestedDataSets.Count-1 do
      if not TDataSet(FNestedDataSets[i]).Active then
        TDataSet(FNestedDataSets[i]).Open;
end;


// Closes the open nested datasets
procedure TDataSet.CloseNestedDataSets;

var
  i: Integer;

begin
  if Assigned(FNestedDataSets) then
    for i:=0 to FNestedDataSets.Count-1 do
      TDataSet(FNestedDataSets[i]).Close;
end;

// This procedure must be called when the first record is made/read
procedure TDataSet.ActivateBuffers;

begin
  FBOF:=False;
  FEOF:=False;
  FActiveRecord:=0;
end;

procedure TDataSet.UpdateFieldDefs;

begin
  //!! To be implemented
end;

procedure TDataSet.BindFields(Binding: Boolean);

begin
  { FieldNo is set to -1 for calculated/lookup fields, to 0 for unbound field
    and for bound fields it is set to FieldDef.FieldNo of the top-level definition }
  FCalcFieldsSize := 0;
  FBlobFieldCount := 0;
  FInternalCalcFields := False;
  BindFieldList(Fields, Binding);
end;


procedure TDataSet.BindFieldList(aFields: TFields; aBinding: Boolean);

var i, FieldIndex, lListIndex: Integer;
    FieldDef, lRoot: TFieldDef;
    Field: TField;

begin
  for i := 0 to aFields.Count - 1 do
    begin
    Field := aFields[i];
    Field.FFieldDef := Nil;
    Field.FFieldDefListIndex := -1;
    if not aBinding then
      Field.FFieldNo := 0
    else if Field.FieldKind in [fkCalculated, fkLookup] then
      begin
      Field.FFieldNo := -1;
      Field.FOffset := FCalcFieldsSize;
      Inc(FCalcFieldsSize, Field.DataSize + 1);
      end
    else
      begin
      if Assigned(Field.ParentField) then
        FieldIndex := -1
      else
        FieldIndex := FieldDefs.IndexOf(Field.FieldName);
      if FieldIndex >= 0 then
        begin
        FieldDef := FieldDefs[FieldIndex];
        Field.FFieldNo := FieldDef.FieldNo;
        end
      else
        begin
        lListIndex := FieldDefList.IndexOf(Field.FullName);
        if lListIndex = -1 then
          DatabaseErrorFmt(SFieldNotFound,[Field.FullName],Self);
        FieldDef := FieldDefList[lListIndex];
        lRoot := FieldDef;
        while Assigned(lRoot.ParentDef) do
          lRoot := lRoot.ParentDef;
        Field.FFieldNo := lRoot.FieldNo;
        if lRoot <> FieldDef then
          Field.FFieldDefListIndex := lListIndex;
        end;
      Field.FFieldDef := FieldDef;
      if FieldDef.InternalCalcField then
        FInternalCalcFields := True;
      if Field.IsBlob and (Field.FFieldDefListIndex < 0) then
        begin
        Field.FSize := FieldDef.Size;
        Field.FOffset := FBlobFieldCount;
        Inc(FBlobFieldCount);
        end;
      // synchronize CodePage between TFieldDef and TField
      // character data in record buffer and field buffer should have same CodePage
      if Field is TStringField then
        TStringField(Field).FCodePage := FieldDef.FCodePage
      else if Field is TMemoField then
        TMemoField(Field).FCodePage := FieldDef.FCodePage;
      end;
    Field.Bind(aBinding);
    if (Field is TObjectField) and not (Field is TDataSetField) then
      BindFieldList(TObjectField(Field).Fields, aBinding);
    end;
end;

function TDataSet.BookmarkAvailable: Boolean;

Const BookmarkStates = [dsBrowse,dsEdit,dsInsert];

begin
  Result:=(Not IsEmpty) and  not FIsUniDirectional and (State in BookmarkStates)
          and (getBookMarkFlag(ActiveBuffer)=bfCurrent);
end;

procedure TDataSet.CalculateFields(Buffer: TRecordBuffer);
var
  i: Integer;
  OldState: TDatasetState;
begin
  FCalcBuffer := Buffer;
  if FState = dsInternalCalc then
    DoOnCalcFields
  else
  begin
    OldState := FState;
    FState := dsCalcFields;
    try
      ClearCalcFields(FCalcBuffer);
      if not IsUniDirectional then
        for i := 0 to FFieldList.Count - 1 do
          if FFieldList[i].FieldKind = fkLookup then
            FFieldList[i].CalcLookupValue;
    finally
      DoOnCalcFields;
      FState := OldState;
    end;
  end;
end;

procedure TDataSet.CheckActive;

begin
  If Not Active then
    DataBaseError(SInactiveDataset);
end;

procedure TDataSet.CheckInactive;

begin
  If Active then
    DataBaseError(SActiveDataset);
end;

procedure TDataSet.ClearBuffers;

begin
  FRecordCount:=0;
  FActiveRecord:=0;
  FCurrentRecord:=-1;
  FBOF:=True;
  FEOF:=True;
end;

procedure TDataSet.ClearCalcFields(Buffer: TRecordBuffer);

begin
  // Empty
end;

procedure TDataSet.CloseBlob(Field: TField);

begin
  //!! To be implemented
end;

procedure TDataSet.CloseCursor;

begin
  FreeFieldBuffers;
  ClearBuffers;
  SetBufListSize(0);
  Fields.ClearFieldDefs;
  InternalClose;
  FInternalOpenComplete := False;
end;

procedure TDataSet.CreateFields;

Var I : longint;
    lFlat : Boolean;

begin
{$ifdef DSDebug}
  Writeln ('Creating fields');
  Writeln ('Count : ',fielddefs.Count);
  For I:=0 to FieldDefs.Count-1 do
    Writeln('Def ',I,' : ',Fielddefs.items[i].Name,'(',Fielddefs.items[i].FieldNo,')');
{$endif}
  lFlat:=False;
  if not ObjectView then
    For I:=0 to FieldDefs.Count-1 do
      lFlat:=lFlat or (FieldDefs[I].HasChildDefs and (FieldDefs[I].DataType<>ftDataSet));
  if lFlat then
    begin
    For I:=0 to FieldDefList.Count-1 do
      With FieldDefList[I] do
        If (DataType<>ftUnknown) and ((DataType=ftDataSet) or not (DataType in ObjectFieldTypes)) then
          CreateField(self,nil,FieldDefList.Strings[I]);
    end
  else
    For I:=0 to FieldDefs.Count-1 do
      With FieldDefs.Items[I] do
        If DataType<>ftUnknown then
          begin
          {$ifdef DSDebug}
          Writeln('About to create field ',FieldDefs.Items[i].Name);
          {$endif}
          CreateField(self);
          end;
end;

procedure TDataSet.DataEvent(Event: TDataEvent; Info: Ptrint);

  procedure HandleFieldChange(aField: TField);
  begin
    if aField.FieldKind in [fkData, fkInternalCalc] then
      SetModified(True);

    if State <> dsSetKey then begin
      if aField.FieldKind = fkData then begin
        if FInternalCalcFields then
          RefreshInternalCalcFields(ActiveBuffer);
        if FAutoCalcFields and (FCalcFieldsSize <> 0) then
          CalculateFields(ActiveBuffer);
      end;

      aField.Change;
    end;
  end;

  procedure HandleScrollOrChange;
  var
    i: Integer;
  begin
    if State <> dsInsert then
      UpdateCursorPos;
    if Assigned(FNestedDataSets) then
      for i := 0 to FNestedDataSets.Count - 1 do
        if TDataSet(FNestedDataSets[i]).Active and (TDataSet(FNestedDataSets[i]).FParentUpdateCount = 0) then
          TDataSet(FNestedDataSets[i]).DataEvent(deParentScroll, 0);
  end;

  procedure CheckNestedBrowseMode;
  var
    i: Integer;
  begin
    if Assigned(FNestedDataSets) then
      for i := 0 to FNestedDataSets.Count - 1 do
        if TDataSet(FNestedDataSets[i]).Active and (TDataSet(FNestedDataSets[i]).FParentUpdateCount = 0) then
          TDataSet(FNestedDataSets[i]).CheckBrowseMode;
  end;

var
  i: Integer;
begin
  case Event of
    deFieldChange   : HandleFieldChange(TField(Info));
    deCheckBrowseMode : CheckNestedBrowseMode;
    deDataSetChange,
    deDataSetScroll : HandleScrollOrChange;
    deLayoutChange  : FEnableControlsEvent:=deLayoutChange;
  end;

  if not ControlsDisabled and (FState <> dsBlockRead) then begin
    for i := 0 to MyDataSourceCount - 1 do
      MyDataSources[i].ProcessEvent(Event, Info);
  end;
end;

procedure TDataSet.DestroyFields;

begin
  FFieldList.Clear;
end;

procedure TDataSet.DoAfterCancel;

begin
 If assigned(FAfterCancel) then
   FAfterCancel(Self);
end;

procedure TDataSet.DoAfterClose;

begin
 If assigned(FAfterClose) and not (csDestroying in ComponentState) then
   FAfterClose(Self);
end;

procedure TDataSet.DoAfterDelete;

begin
 If assigned(FAfterDelete) then
   FAfterDelete(Self);
end;

procedure TDataSet.DoAfterEdit;

begin
 If assigned(FAfterEdit) then
   FAfterEdit(Self);
end;

procedure TDataSet.DoAfterInsert;

begin
 If assigned(FAfterInsert) then
   FAfterInsert(Self);
end;

procedure TDataSet.DoAfterOpen;

begin
 If assigned(FAfterOpen) then
   FAfterOpen(Self);
end;

procedure TDataSet.DoAfterPost;

begin
 If assigned(FAfterPost) then
   FAfterPost(Self);
end;

procedure TDataSet.DoAfterScroll;

begin
 If assigned(FAfterScroll) then
   FAfterScroll(Self);
end;

procedure TDataSet.DoAfterRefresh;

begin
 If assigned(FAfterRefresh) then
   FAfterRefresh(Self);
end;

procedure TDataSet.DoBeforeCancel;

begin
 If assigned(FBeforeCancel) then
   FBeforeCancel(Self);
end;

procedure TDataSet.DoBeforeClose;

begin
 If assigned(FBeforeClose) and not (csDestroying in ComponentState) then
   FBeforeClose(Self);
end;

procedure TDataSet.DoBeforeDelete;

begin
 If assigned(FBeforeDelete) then
   FBeforeDelete(Self);
end;

procedure TDataSet.DoBeforeEdit;

begin
 If assigned(FBeforeEdit) then
   FBeforeEdit(Self);
end;

procedure TDataSet.DoBeforeInsert;

begin
 If assigned(FBeforeInsert) then
   FBeforeInsert(Self);
end;

procedure TDataSet.DoBeforeOpen;

begin
 If assigned(FBeforeOpen) then
   FBeforeOpen(Self);
end;

procedure TDataSet.DoBeforePost;

begin
 If assigned(FBeforePost) then
   FBeforePost(Self);
end;

procedure TDataSet.DoBeforeScroll;

begin
 If assigned(FBeforeScroll) then
   FBeforeScroll(Self);
end;

procedure TDataSet.DoBeforeRefresh;

begin
 If assigned(FBeforeRefresh) then
   FBeforeRefresh(Self);
end;

procedure TDataSet.DoInternalOpen;

begin
  InternalOpen;
  FInternalOpenComplete := True;
{$ifdef dsdebug}
  Writeln ('Calling internal open');
{$endif}
{$ifdef dsdebug}
  Writeln ('Calling RecalcBufListSize');
{$endif}
  FRecordCount := 0;
  RecalcBufListSize;
  FBOF := True;
  FEOF := (FRecordCount = 0);
end;

procedure TDataSet.DoOnCalcFields;

begin
 If Assigned(FOnCalcfields) then
   FOnCalcFields(Self);
end;

procedure TDataSet.DoOnNewRecord;

begin
 If assigned(FOnNewRecord) then
   FOnNewRecord(Self);
end;

function TDataSet.FieldByNumber(FieldNo: Longint): TField;

begin
  Result:=FFieldList.FieldByNumber(FieldNo);
end;

function TDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;

begin
  //!! To be implemented
end;

procedure TDataSet.FreeFieldBuffers;

Var I : longint;

begin
  For I:=0 to FFieldList.Count-1 do
    FFieldList[i].FreeBuffers;
end;

function TDataSet.GetBookmarkStr: TBookmarkStr;

begin
  Result:='';
  If BookMarkAvailable then
    begin
    SetLength(Result,FBookMarkSize);
    GetBookMarkData(ActiveBuffer,Pointer(Result));
    end
end;

function TDataSet.GetBuffer(Index: longint): TRecordBuffer;

begin
  Result:=FBuffers[Index];
end;

function TDataSet.GetDatasourceCount: Integer;
begin
  Result:=FDataSources.Count;
end;

function TDataSet.GetDatasources(aIndex : integer): TDatasource;
begin
  Result:=TDatasource(FDataSources[aIndex]);
end;

procedure TDataSet.GetCalcFields(Buffer: TRecordBuffer);

begin
  if (FCalcFieldsSize > 0) or FInternalCalcFields then
    CalculateFields(Buffer);
end;

function TDataSet.GetCanModify: Boolean;

begin
  Result:= not FIsUnidirectional;
end;

procedure TDataSet.GetChildren(Proc: TGetChildProc; Root: TComponent);

var
 I: Integer;
 Field: TField;

begin
 for I := 0 to Fields.Count - 1 do begin
   Field := Fields[I];
   if (Field.Owner = Root) then
     Proc(Field);
 end;
end;

function TDataSet.GetDataSource: TDataSource;
begin
  Result:=nil;
end;

function TDataSet.GetRecordSize: Word;
begin
  Result := 0;
end;

procedure TDataSet.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // empty stub
end;

procedure TDataSet.InternalDelete;
begin
  // empty stub
end;

procedure TDataSet.InternalFirst;
begin
  // empty stub
end;

procedure TDataSet.InternalGotoBookmark(ABookmark: Pointer);
begin
  // empty stub
end;

function TDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

begin
  Result := False;
end;

procedure TDataSet.DataConvert(aField: TField; aSource, aDest: Pointer;
  aToNative: Boolean);

var
  DT : TFieldType;

begin
  DT := aField.DataType;
  if aToNative then
    begin
    case DT of
      ftDate, ftTime, ftDateTime: TDateTimeRec(aDest^) := DateTimeToDateTimeRec(DT, TDateTime(aSource^));
      ftTimeStamp               : TTimeStamp(aDest^) := TTimeStamp(aSource^);
      ftBCD                     : TBCD(aDest^) := CurrToBCD(Currency(aSource^));
      ftFMTBCD                  : TBcd(aDest^) := TBcd(aSource^);
  // See notes from mantis bug-report 8204 for more information
  //    ftBytes                   : ;
  //    ftVarBytes                : ;
      ftWideString, ftFixedWideChar: StrLCopy(PWideChar(aDest), PWideChar(aSource), aField.Size);
      end
    end
  else
    begin
    case DT of
      ftDate, ftTime, ftDateTime: TDateTime(aDest^) := DateTimeRecToDateTime(DT, TDateTimeRec(aSource^));
      ftTimeStamp               : TTimeStamp(aDest^) := TTimeStamp(aSource^);
      ftBCD                     : BCDToCurr(TBCD(aSource^),Currency(aDest^));
      ftFMTBCD                  : TBcd(aDest^) := TBcd(aSource^);
  //    ftBytes                   : ;
  //    ftVarBytes                : ;
      ftWideString, ftFixedWideChar: StrLCopy(PWideChar(aDest), PWideChar(aSource), aField.Size);
      end
    end
end;

function TDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;

Var
  AStatBuffer : Array[0..dsMaxStringSize] of AnsiChar;
  ADynBuffer : PAnsiChar;

begin
  If NativeFormat then
    Result:=GetFieldData(Field, Buffer)
  else
    begin
    if Field.DataSize <= dsMaxStringSize then
      begin
      Result := GetfieldData(Field, @AStatBuffer);
      if Result then DataConvert(Field,@AStatBuffer,Buffer,False);
      end
    else
      begin
      GetMem(ADynBuffer,Field.DataSize);
      try
        Result := GetfieldData(Field, ADynBuffer);
        if Result then DataConvert(Field,ADynBuffer,Buffer,False);
      finally
        FreeMem(ADynBuffer);
        end;
      end;
    end;
end;

Function DateTimeRecToDateTime(DT: TFieldType; Data: TDateTimeRec): TDateTime;

var
  TS: TTimeStamp;

begin
  TS.Date:=0;
  TS.Time:=0;
  case DT of
    ftDate: TS.Date := Data.Date;
    ftTime: With TS do
              begin
              Time := Data.Time;
              Date := DateDelta;
              end;
  else
    try
      TS:=MSecsToTimeStamp(trunc(Data.DateTime));
    except
    end;
  end;
  Result:=TimeStampToDateTime(TS);
end;

Function DateTimeToDateTimeRec(DT: TFieldType; Data: TDateTime): TDateTimeRec;

var
  TS : TTimeStamp;

begin
  TS:=DateTimeToTimeStamp(Data);
  With Result do
    case DT of
      ftDate:
        Date:=TS.Date;
      ftTime:
        Time:=TS.Time;
    else
      DateTime:=TimeStampToMSecs(TS);
    end;
end;

procedure TDataSet.SetFieldData(Field: TField; Buffer: Pointer);

begin
// empty procedure
end;

procedure TDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);

Var
  AStatBuffer : Array[0..dsMaxStringSize] of AnsiChar;
  ADynBuffer : PAnsiChar;

begin
  if NativeFormat then
    SetFieldData(Field, Buffer)
  else
    begin
    if Field.DataSize <= dsMaxStringSize then
      begin
      DataConvert(Field,Buffer,@AStatBuffer,True);
      SetfieldData(Field, @AStatBuffer);
      end
    else
      begin
      GetMem(ADynBuffer,Field.DataSize);
      try
        DataConvert(Field,Buffer,ADynBuffer,True);
        SetfieldData(Field, ADynBuffer);
      finally
        FreeMem(ADynBuffer);
        end;
      end;
    end;
end;

function TDataSet.GetField(Index: Longint): TField;

begin
  Result:=FFIeldList[index];
end;

function TDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;

begin
  Result := DefaultFieldClasses[FieldType];
end;

function TDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;

begin
  Result := GetFieldClass(FieldDef.DataType);
end;

function TDataSet.GetIsIndexField(Field: TField): Boolean;

begin
  Result:=False;
end;

function TDataSet.GetIndexDefs(IndexDefs: TIndexDefs; IndexTypes: TIndexOptions
  ): TIndexDefs;

var i,f : integer;
    IndexFields : TStrings;

begin
  IndexDefs.Update;
  Result := TIndexDefs.Create(Self);
  Result.Assign(IndexDefs);
  i := 0;
  IndexFields := TStringList.Create;
  while i < result.Count do
    begin
    if (not ((IndexTypes = []) and (result[i].Options = []))) and
       ((IndexTypes * result[i].Options) = []) then
      begin
      result.Delete(i);
      dec(i);
      end
    else
      begin
      ExtractStrings([';'],[' '],PAnsiChar(result[i].Fields),Indexfields);
      for f := 0 to IndexFields.Count-1 do if FindField(Indexfields[f]) = nil then
        begin
        result.Delete(i);
        dec(i);
        break;
        end;
      end;
    inc(i);
    end;
  IndexFields.Free;
end;

function TDataSet.GetNextRecord: Boolean;

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
  Result:=GetRecord(FBuffers[FBufferCount],gmNext,True)=grOK;

  if Result then
    begin
      If FRecordCount=0 then ActivateBuffers;
      if FRecordCount=FBufferCount then
        ShiftBuffersBackward
      else
        begin
          Inc(FRecordCount);
          FCurrentRecord:=FRecordCount - 1;
          ExchangeBuffers(FBuffers[FCurrentRecord],FBuffers[FBufferCount]);
        end;
    end
  else
    CursorPosChanged;
{$ifdef dsdebug}
  Writeln ('Result getting next record : ',Result);
{$endif}
end;

function TDataSet.GetNextRecords: Longint;

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

function TDataSet.GetPriorRecord: Boolean;

begin
{$ifdef dsdebug}
  Writeln ('GetPriorRecord: Getting previous record');
{$endif}
  CheckBiDirectional;
  If FRecordCount>0 Then SetCurrentRecord(0);
  Result:=GetRecord(FBuffers[FBufferCount],gmPrior,True)=grOK;
  if Result then
    begin
      If FRecordCount=0 then ActivateBuffers;
      ShiftBuffersForward;

      if FRecordCount<FBufferCount then
        Inc(FRecordCount);
    end
  else
    CursorPosChanged;
{$ifdef dsdebug}
  Writeln ('Result getting prior record : ',Result);
{$endif}
end;

function TDataSet.GetPriorRecords: Longint;

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting previous record(s), need :',FBufferCount);
{$endif}
  While (FRecordCount<FBufferCount) and GetPriorRecord do
    Inc(Result);
end;

function TDataSet.GetRecNo: Longint;

begin
  Result := -1;
end;

function TDataSet.GetRecordCount: Longint;

begin
  Result := -1;
end;

procedure TDataSet.InitFieldDefs;

begin
  if IsCursorOpen then
    InternalInitFieldDefs
  else
    begin
    try
      OpenCursor(True);
    finally
      CloseCursor;
      end;
    end;
end;

procedure TDataSet.SetBlockReadSize(AValue: Integer);
begin
  // the state is changed even when setting the same BlockReadSize (follows Delphi behavior)
  // e.g., state is dsBrowse and BlockReadSize is 1. Setting BlockReadSize to 1 will change state to dsBlockRead
  FBlockReadSize := AValue;
  if AValue > 0 then
  begin
    CheckActive;
    SetState(dsBlockRead);
  end
  else
  begin
    //update state only when in dsBlockRead
    if FState = dsBlockRead then
      SetState(dsBrowse);
  end;
end;

procedure TDataSet.SetFieldDefs(AFieldDefs: TFieldDefs);

begin
  Fields.ClearFieldDefs;
  FFieldDefs.Assign(AFieldDefs);
end;

procedure TDataSet.SetSparseArrays(AValue: Boolean);
begin
  // http://docwiki.embarcadero.com/Libraries/Sydney/en/Data.DB.TDataSet.SparseArrays
  FSparseArrays := AValue;
end;

procedure TDataSet.SetObjectView(AValue: Boolean);

begin
  CheckInactive;
  FObjectView := AValue;
end;


procedure TDataSet.DoInsertAppendRecord(const Values: array of const; DoAppend : boolean);
var i : integer;
    ValuesSize : integer;
begin
  ValuesSize:=Length(Values);
  if ValuesSize>FieldCount then DatabaseError(STooManyFields,self);
  if DoAppend then
    Append
  else
    Insert;

  for i := 0 to ValuesSize-1 do
    Fields[i].AssignValue(Values[i]);
  Post;

end;

// Adds to aDefs a field definition for aField named aName, with child definitions for its child fields.
function AddFieldDefFromField(aDefs: TFieldDefs; aField: TField; const aName: string): TFieldDef;

var
  i, lSize: Integer;
  lObject: TObjectField;

begin
  lSize:=aField.Size;
  if aField is TArrayField then
    lSize:=TArrayField(aField).FieldCount;
  Result:=aDefs.FieldDefClass.Create(aDefs,aName,aField.DataType,lSize,aField.Required,aDefs.Count+1);
  with Result do
    begin
    InternalCalcField := aField.FieldKind = fkInternalCalc;
    if aField.Required then Attributes := Attributes + [faRequired];
    if aField.ReadOnly then Attributes := Attributes + [faReadOnly];
    if DataType = ftBCD then Precision := (aField as TBCDField).Precision
    else if DataType = ftFMTBcd then Precision := (aField as TFMTBCDField).Precision;
    end;
  if aField is TObjectField then
    begin
    lObject:=TObjectField(aField);
    if lObject.DataType=ftArray then
      begin
      if lObject.FieldCount>0 then
        AddFieldDefFromField(Result.ChildDefs,lObject.Fields[0],'');
      end
    else
      for i:=0 to lObject.FieldCount-1 do
        AddFieldDefFromField(Result.ChildDefs,lObject.Fields[i],lObject.Fields[i].FieldName);
    end;
end;


procedure TDataSet.InitFieldDefsFromFields;
var i : integer;

begin
  if FieldDefs.Count = 0 then
    begin
    FieldDefs.BeginUpdate;
    try
      for i := 0 to Fields.Count-1 do with Fields[i] do
        if not (FieldKind in [fkCalculated,fkLookup]) then // Do not add fielddefs for calculated/lookup fields.
          FFieldDef:=AddFieldDefFromField(FieldDefs,Fields[i],FieldName);
    finally
      FieldDefs.EndUpdate;
      end;
    end;
end;

procedure TDataSet.InitRecord(Buffer: TRecordBuffer);

begin
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
end;

procedure TDataSet.InternalCancel;

begin
  //!! To be implemented
end;

procedure TDataSet.InternalEdit;

begin
  //!! To be implemented
end;

procedure TDataSet.InternalRefresh;

begin
  //!! To be implemented
end;

procedure TDataSet.OpenCursor(InfoQuery: Boolean);

begin
  if InfoQuery then
    InternalInitFieldDefs
  else if State <> dsOpening then
    DoInternalOpen;
end;

procedure TDataSet.OpenCursorcomplete;
begin
  try
    if FState = dsOpening then DoInternalOpen
  finally
    if FInternalOpenComplete then
      begin
      SetState(dsBrowse);
      DoAfterOpen;
      if not IsEmpty then
        DoAfterScroll;
      end
    else
      begin
      SetState(dsInactive);
      CloseCursor;
      end;
  end;
end;

procedure TDataSet.RefreshInternalCalcFields(Buffer: TRecordBuffer);

var
  lOldState: TDataSetState;

begin
  lOldState := SetTempState(dsInternalCalc);
  try
    CalculateFields(Buffer);
  finally
    RestoreState(lOldState);
  end;
end;

function TDataSet.SetTempState(const Value: TDataSetState): TDataSetState;

begin
  result := FState;
  FState := value;
  inc(FDisableControlsCount);
end;

procedure TDataSet.RestoreState(const Value: TDataSetState);

begin
  FState := value;
  dec(FDisableControlsCount);
end;

function TDataSet.GetActive: boolean;

begin
  result := (FState <> dsInactive) and (FState <> dsOpening);
end;

procedure TDataSet.InternalHandleException;

begin
  if assigned({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Classes.ApplicationHandleException) then
    {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}classes.ApplicationHandleException(self)
  else
    ShowException(ExceptObject,ExceptAddr);
end;

procedure TDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  // empty stub
end;

procedure TDataSet.InternalLast;
begin
  // empty stub
end;

procedure TDataSet.InternalPost;

  Procedure CheckRequiredFields;

  Var I : longint;

  begin
    For I:=0 to FFieldList.Count-1 do
      With FFieldList[i] do
        // Required fields whose value is not generated by the database
        if Required and not ReadOnly and
           (FieldKind=fkData) and Not (DataType=ftAutoInc) and
           (AutoGenerateValue=arNone) and IsNull then
          DatabaseErrorFmt(SNeedField,[DisplayName],Self);
  end;

begin
  CheckRequiredFields;
end;

procedure TDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  // empty stub
end;

procedure TDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  // empty stub
end;

procedure TDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  // empty stub
end;

procedure TDataSet.SetUniDirectional(const Value: Boolean);
begin
  FIsUniDirectional := Value;
end;

class function TDataSet.FieldDefsClass: TFieldDefsClass;
begin
  Result:=TFieldDefs;
end;

class function TDataSet.FieldsClass: TFieldsClass;
begin
  Result:=TFields;
end;

procedure TDataSet.SetActive(Value: Boolean);

begin
  if value and (Fstate = dsInactive) then
    begin
    if csLoading in ComponentState then
      begin
      FOpenAfterRead := true;
      exit;
      end
    else
      begin
      DoBeforeOpen;
      FEnableControlsEvent:=deLayoutChange;
      try
        FDefaultFields:=FieldCount=0;
        OpenCursor(False);
      finally
        if FState <> dsOpening then OpenCursorComplete;
        end;
      if FState <> dsInactive then
        OpenNestedDataSets;
      end;
    FModified:=False;
    end
  else if not value and (Fstate <> dsinactive) then
    begin
    DoBeforeClose;
    CloseNestedDataSets;
    SetState(dsInactive);
    CloseCursor;
    DoAfterClose;
    FModified:=False;
    end
end;

procedure TDataSet.Loaded;

begin
  inherited;
  try
    if FOpenAfterRead then SetActive(true);
  except
    if csDesigning in Componentstate then
      InternalHandleException
    else
      raise;
  end;
end;

function TDataset.GetDefaultBufferCount : Cardinal;

begin
  Result:=DefaultBufferCount;
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
  if IsUniDirectional then
    ABufferCount := 1
  else
    begin
    ABufferCount := GetDefaultBufferCount;
    if aBufferCount<2 then
      aBufferCount:=2;
    end;

  for i := 0 to MyDataSourceCount - 1 do
    for j := 0 to MyDataSources[i].DataLinkCount - 1 do
      begin
      DataLink:=MyDataSources[i].DataLink[j];
      if ABufferCount<DataLink.BufferCount then
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
  if (FRecordCount < FBufferCount) and not IsUniDirectional then
    begin
    FActiveRecord := FActiveRecord + GetPriorRecords;
    CursorPosChanged;
    end;
{$Ifdef dsDebug}
  WriteLn(
    'SetBufferCount: FActiveRecord=',FActiveRecord,
    ' FCurrentRecord=',FCurrentRecord,
    ' FBufferCount= ',FBufferCount,
    ' FRecordCount=',FRecordCount);
{$Endif}
  for i := 0 to MyDataSourceCount - 1 do
    for j := 0 to MyDataSources[i].DataLinkCount - 1 do
       MyDataSources[i].DataLink[j].CalcRange;
end;

procedure TDataSet.SetBookmarkStr(const Value: TBookmarkStr);

begin
  GotoBookMark(Pointer(Value))
end;

procedure TDataSet.SetBufListSize(Value: Longint);

Var I : longint;

begin
  if Value = 0 then Value := -1;
{$ifdef dsdebug}
  Writeln ('SetBufListSize: ',Value);
{$endif}
  If Value=FBufferCount Then
    exit;
  If Value>FBufferCount then
    begin
{$ifdef dsdebug}
    Writeln ('   Reallocating memory :',(Value+1)*SizeOf(TRecordBuffer));
{$endif}
    ReAllocMem(FBuffers,(Value+1)*SizeOf(TRecordBuffer));
{$ifdef dsdebug}
    Writeln ('   Filling memory :',(Value+1-FBufferCount)*SizeOf(TRecordBuffer));
{$endif}
    Inc(FBufferCount); // Cause FBuffers[FBufferCount] is already allocated
    FillChar(FBuffers[FBufferCount],(Value+1-FBufferCount)*SizeOf(TRecordBuffer),#0);
{$ifdef dsdebug}
    Writeln ('   Filled memory');
{$endif}
    Try
{$ifdef dsdebug}
      Writeln ('   Assigning buffers : ',(Value)*SizeOf(TRecordBuffer));
{$endif}
      For I:=FBufferCount to Value do
        FBuffers[i]:=AllocRecordBuffer;
{$ifdef dsdebug}
      Writeln ('   Assigned buffers (FBufferCount:',FBufferCount,') : ',(Value)*SizeOf(TRecordBuffer));
{$endif}
    except
      I:=FBufferCount;
      While (I<(Value+1)) do
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
        ShiftBuffersBackward;
      FActiveRecord := Value -1;
      end;

    If Assigned(FBuffers) then
      begin
      For I:=Value+1 to FBufferCount do
        FreeRecordBuffer(FBuffers[i]);
      // FBuffer must stay allocated, to make sure that Activebuffer returns nil
      if Value = -1 then
        begin
        ReAllocMem(FBuffers,SizeOf(TRecordBuffer));
        FBuffers[0] := nil;
        end
      else
        ReAllocMem(FBuffers,(Value+1)*SizeOf(TRecordBuffer));
      end;
    end;
  FBufferCount:=Value;
  If Value=-1 then
    Value:=0;
  if FRecordCount > Value then FRecordCount := Value;
{$ifdef dsdebug}
  Writeln ('   SetBufListSize: Final FBufferCount=',FBufferCount);
{$endif}
end;

procedure TDataSet.SetChildOrder(Component: TComponent; Order: Longint);

var
  Field: TField;
begin
  Field := Component as TField;
  if Fields.IndexOf(Field) >= 0 then
    Field.Index := Order;
end;

procedure TDataSet.SetCurrentRecord(Index: Longint);

begin
  If FCurrentRecord<>Index then
    begin
{$ifdef DSdebug}
    Writeln ('Setting current record to: ',index);
{$endif}
    if not FIsUniDirectional then Case GetBookMarkFlag(FBuffers[Index]) of
      bfCurrent : InternalSetToRecord(FBuffers[Index]);
      bfBOF : InternalFirst;
      bfEOF : InternalLast;
      end;
    FCurrentRecord:=Index;
    end;
end;

procedure TDataSet.SetDefaultFields(const Value: Boolean);
begin
  FDefaultFields := Value;
end;

procedure TDataSet.SetField(Index: Longint; Value: TField);

begin
  //!! To be implemented
end;

procedure TDataSet.CheckBiDirectional;

begin
  if FIsUniDirectional then DataBaseError(SUniDirectional);
end;

procedure TDataSet.SetFilterOptions(Value: TFilterOptions);

begin
  CheckBiDirectional;
  FFilterOptions := Value;
end;

procedure TDataSet.SetFilterText(const Value: string);

begin
  FFilterText := value;
end;

procedure TDataSet.SetFiltered(Value: Boolean);

begin
  if Value then CheckBiDirectional;
  FFiltered := value;
end;

procedure TDataSet.SetFound(const Value: Boolean);
begin
  FFound := Value;
end;

procedure TDataSet.SetModified(Value: Boolean);

begin
  FModified := value;
end;

procedure TDataSet.SetName(const Value: TComponentName);

  function CheckName(const FieldName: string): string;

  var i,j: integer;

  begin
    Result := FieldName;
    i := 0;
    j := 0;
    // Check if fieldname exists.
    while (i < Fields.Count) do
      if Not SameText(Result,Fields[i].Name) then
        inc(i)
      else
        begin
        inc(j);
        Result := FieldName + IntToStr(j);
        i := 0;
        end;
    // Check if component with the same name exists.
    if Assigned(Owner) then
      While Owner.FindComponent(Result)<>Nil do
        begin
        Inc(J);
        Result := FieldName + IntToStr(j);
        end;
  end;

var
  i: integer;
  OldName, OldFieldName: string;

begin
  if Self.Name = Value then Exit;
  OldName := Self.Name;
  inherited SetName(Value);
  if (csDesigning in ComponentState) then
    for i := 0 to Fields.Count - 1 do begin
      OldFieldName := OldName + Fields[i].FieldName;
      if Copy(Fields[i].Name, 1, Length(OldFieldName)) = OldFieldName then
        Fields[i].Name := CheckName(Value + Fields[i].FieldName);
    end;
end;

procedure TDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);

begin
  CheckBiDirectional;
  FOnFilterRecord := Value;
end;

procedure TDataSet.SetRecNo(Value: Longint);

begin
  //!! To be implemented
end;

procedure TDataSet.SetState(Value: TDataSetState);

begin
  If Value<>FState then
    begin
    FState:=Value;
    if Value=dsBrowse then
      FModified:=false;
    DataEvent(deUpdateState,0);
    end;
end;

function TDataSet.TempBuffer: TRecordBuffer;

begin
  Result := FBuffers[FRecordCount];
end;

procedure TDataSet.UpdateIndexDefs;

begin
  // Empty Abstract
end;

function TDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := nil;
end;

procedure TDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  // empty stub
end;

procedure TDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  // empty stub
end;

function TDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := bfCurrent;
end;

function TDataSet.ControlsDisabled: Boolean;

begin
  Result := (FDisableControlsCount > 0);
end;

function TDataSet.ActiveBuffer: TRecordBuffer;

begin
{$ifdef dsdebug}
  Writeln ('Active buffer requested. Returning record number:',ActiveRecord);
{$endif}
  Result:=FBuffers[FActiveRecord];
end;

procedure TDataSet.Append;

begin
  DoInsertAppend(True);
end;

procedure TDataSet.InternalInsert;

begin
  //!! To be implemented
end;

procedure TDataSet.AppendRecord(const Values: array of const);

begin
  DoInsertAppendRecord(Values,True);
end;

function TDataSet.BookmarkValid(ABookmark: TBookmark): Boolean;
{
  Should be overridden by descendant objects.
}
begin
  Result:=False
end;

procedure TDataSet.Cancel;

begin
  If State in [dsEdit,dsInsert] then
    begin
    DataEvent(deCheckBrowseMode,0);
    DoBeforeCancel;
    UpdateCursorPos;
    InternalCancel;
    FreeFieldBuffers;
    if (State = dsInsert) and (FRecordCount = 1) then
      begin
      FEOF := true;
      FBOF := true;
      FRecordCount := 0;
      InitRecord(ActiveBuffer);
      SetState(dsBrowse);
      DataEvent(deDatasetChange,0);
      end
    else
      begin
      SetState(dsBrowse);
      SetCurrentRecord(FActiveRecord);
      resync([]);
      end;
    DoAfterCancel;
    end;
end;

procedure TDataSet.CheckBrowseMode;

begin
  CheckActive;
  DataEvent(deCheckBrowseMode,0);
  Case State of
    dsEdit,dsInsert: begin
      UpdateRecord;
      If Modified then Post else Cancel;
    end;
    dsSetKey: Post;
  end;
end;

procedure TDataSet.ClearFields;


begin
  if not (State in dsEditModes) then
    DatabaseError(SNotEditing, Self);
  DataEvent(deCheckBrowseMode, 0);
  FreeFieldBuffers;
  InternalInitRecord(ActiveBuffer);
  if State <> dsSetKey then GetCalcFields(ActiveBuffer);
  DataEvent(deRecordChange, 0);
end;

procedure TDataSet.Close;

begin
  Active:=False;
end;

function TDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;

begin
  Result:=0;
end;

function TDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode
  ): TStream;


begin
  Result:=Nil;
end;

procedure TDataSet.CursorPosChanged;


begin
  FCurrentRecord:=-1;
end;

procedure TDataSet.Delete;

begin
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  If IsEmpty then
    DatabaseError(SDatasetEmpty,Self);
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
    If Not TryDoing(@InternalDelete,OnDeleteError) then exit;
{$ifdef dsdebug}
    writeln ('Delete: Internaldelete succeeded');
{$endif}
    FreeFieldBuffers;
    SetState(dsBrowse);
{$ifdef dsdebug}
    writeln ('Delete: Browse mode set');
{$endif}
    SetCurrentRecord(FActiveRecord);
    Resync([]);
    DoAfterDelete;
    DoAfterScroll;
  end;
end;

procedure TDataSet.DisableControls;


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

procedure TDataSet.DoInsertAppend(DoAppend: Boolean);


  procedure DoInsert(DoAppend : Boolean);

  Var BookBeforeInsert : TBookmark;
      TempBuf : pointer;

  begin
  // need to scroll up al buffers after current one,
  // but copy current bookmark to insert buffer.
  If FRecordCount > 0 then
    BookBeforeInsert:=Bookmark;

  if not DoAppend then
    begin
    if FRecordCount > 0 then
      begin
      TempBuf := FBuffers[FBufferCount];
      move(FBuffers[FActiveRecord],FBuffers[FActiveRecord+1],(FBufferCount-FActiveRecord)*sizeof(FBuffers[0]));
      FBuffers[FActiveRecord]:=TempBuf;
      end;
    end
  else if FRecordCount=FBufferCount then
    ShiftBuffersBackward
  else
    begin
    if FRecordCount>0 then
      inc(FActiveRecord);
    end;

  // Active buffer is now edit buffer. Initialize.
  InitRecord(FBuffers[FActiveRecord]);
  cursorposchanged;

  // Put bookmark in edit buffer.
  if FRecordCount=0 then
    SetBookmarkFlag(ActiveBuffer,bfEOF)
  else
    begin
    fBOF := false;
    // 29:01:05, JvdS: Why is this here?!? It can result in records with the same bookmark-data?
    // I would say that the 'internalinsert' should do this. But I don't know how Tdbf handles it

    // 1-apr-06, JvdS: It just sets the bookmark of the newly inserted record to the place
    // where the record should be inserted. So it is ok.
    if FRecordCount > 0 then
      begin
      SetBookMarkData(ActiveBuffer,pointer(BookBeforeInsert));
      FreeBookmark(BookBeforeInsert);
      end;
    end;

  InternalInsert;

  // update buffer count.
  If FRecordCount<FBufferCount then
    Inc(FRecordCount);
  end;

begin
  CheckBrowseMode;
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  DoBeforeInsert;
  CheckParentState;
  DoBeforeScroll;
  If Not DoAppend then
    begin
{$ifdef dsdebug}
    Writeln ('going to insert mode');
{$endif}
    DoInsert(false);
    end
  else
    begin
{$ifdef dsdebug}
    Writeln ('going to append mode');
{$endif}
    ClearBuffers;
    InternalLast;
    GetPriorRecords;
    if FRecordCount>0 then
      FActiveRecord:=FRecordCount-1;
    DoInsert(True);
    SetBookmarkFlag(ActiveBuffer,bfEOF);
    FBOF :=False;
    FEOF := true;
    end;
  SetState(dsInsert);
  try
    DoOnNewRecord;
  except
    SetCurrentRecord(FActiveRecord);
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

procedure TDataSet.Edit;

begin
  If State in [dsEdit,dsInsert] then exit;
  CheckBrowseMode;
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  If FRecordCount = 0 then
    begin
    Append;
    Exit;
    end;
  DoBeforeEdit;
  CheckParentState;
  If Not TryDoing(@InternalEdit,OnEditError) then exit;
  GetCalcFields(ActiveBuffer);
  SetState(dsEdit);
  DataEvent(deRecordChange,0);
  DoAfterEdit;
end;

procedure TDataSet.EnableControls;


begin
  if FDisableControlsCount > 0 then
    Dec(FDisableControlsCount);

  if FDisableControlsCount = 0 then begin
    if FState <> FDisableControlsState then
      DataEvent(deUpdateState, 0);

    if (FState <> dsInactive) and (FDisableControlsState <> dsInactive) then
      DataEvent(FEnableControlsEvent, 0);
  end;
end;

function TDataSet.FieldByName(const FieldName: string): TField;


begin
  Result:=FindField(FieldName);
  If Result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[FieldName],Self);
end;

function TDataSet.FindField(const FieldName: string): TField;


begin
  Result:=FFieldList.FindField(FieldName);
  if (Result=nil) and ObjectView then
    Result:=FieldList.Find(FieldName);
end;

function TDataSet.FindFirst: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindLast: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindNext: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindPrior: Boolean;


begin
  Result:=False;
end;

procedure TDataSet.First;


begin
  CheckBrowseMode;
  DoBeforeScroll;
  if not FIsUniDirectional then
    ClearBuffers
  else if not FBof then
    begin
    Active := False;
    Active := True;
    end;
  try
    InternalFirst;
    if not FIsUniDirectional then GetNextRecords;
  finally
    FBOF:=True;
    DataEvent(deDatasetChange,0);
    DoAfterScroll;
    end;
end;

procedure TDataSet.FreeBookmark(ABookmark: TBookmark);


begin
  {$ifdef noautomatedbookmark}
   FreeMem(ABookMark,FBookMarkSize);
  {$endif}
end;

function TDataSet.GetBookmark: TBookmark;


begin
  if BookmarkAvailable then
    begin
    {$ifdef noautomatedbookmark}
      GetMem (Result,FBookMarkSize);
    {$else}
      setlength(Result,FBookMarkSize);
    {$endif}
    GetBookMarkdata(ActiveBuffer,pointer(Result));
    end
  else
    Result:=Nil;
end;

function TDataSet.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;


begin
  Result:=False;
end;

procedure TDataSet.GetFieldList(List: TList; const FieldNames: string);

var
  F: TField;
  N: String;
  StrPos: Integer;

begin
  if (FieldNames = '') or (List = nil) then
    Exit;
  StrPos := 1;
  repeat
    N := ExtractFieldName(FieldNames, StrPos);
    F := FieldByName(N);
    List.Add(F);
  until StrPos > Length(FieldNames);
end;

procedure TDataSet.GetFieldNames(List: TStrings);


begin
  FFieldList.GetFieldNames(List);
end;

procedure TDataSet.GotoBookmark(const ABookmark: TBookmark);


begin
  If Assigned(ABookMark) then
    begin
    CheckBrowseMode;
    DoBeforeScroll;
    InternalGotoBookMark(pointer(ABookMark));
    Resync([rmExact,rmCenter]);
    DoAfterScroll;
    end;
end;

procedure TDataSet.Insert;

begin
  DoInsertAppend(False);
end;

procedure TDataSet.InsertRecord(const Values: array of const);

begin
  DoInsertAppendRecord(Values,False);
end;

function TDataSet.IsEmpty: Boolean;

begin
  Result:=(fBof and fEof) and
          (not (State = dsInsert)); // After an insert on an empty dataset, both fBof and fEof are true
end;

function TDataSet.IsLinkedTo(ADataSource: TDataSource): Boolean;

var
  lDataSource: TDataSource;
  lDataSet: TDataSet;

begin
  Result := True;
  lDataSource := ADataSource;
  while Assigned(lDataSource) do
    begin
    lDataSet := lDataSource.DataSet;
    if lDataSet = nil then
      Break;
    if lDataSet = Self then
      Exit;
    if Assigned(lDataSet.DataSetField) and (lDataSet.DataSetField.DataSet = Self) then
      Exit;
    lDataSource := lDataSet.DataSource;
    end;
  Result := False;
end;

function TDataSet.IsSequenced: Boolean;

begin
  Result := True;
end;

procedure TDataSet.Last;

begin
  CheckBiDirectional;
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalLast;
    GetPriorRecords;
    if FRecordCount>0 then
      FActiveRecord:=FRecordCount-1
  finally
    FEOF:=true;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
    end;
end;

function TDataSet.MoveBy(Distance: Longint): Longint;
Var
  TheResult: Integer;

  Function ScrollForward : Integer;
  begin
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling forward : ',Distance);
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
    CheckBiDirectional;
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling backward : ',Abs(Distance));
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
  Scrolled : Integer;

begin
  CheckBrowseMode;
  Result:=0; TheResult:=0;
  DoBeforeScroll;
  If (Distance = 0) or
     ((Distance>0) and FEOF) or
     ((Distance<0) and FBOF) then
    exit;
  Try
    Scrolled := 0;
    If Distance>0 then
      Scrolled:=ScrollForward
    else
      Scrolled:=ScrollBackward;
  finally
{$ifdef dsdebug}
    WriteLn('ActiveRecord=', FActiveRecord,' FEOF=',FEOF,' FBOF=',FBOF);
{$Endif}
    DataEvent(deDatasetScroll,Scrolled);
    DoAfterScroll;
    Result:=TheResult;
  end;
end;

procedure TDataSet.Next;

begin
  if BlockReadSize>0 then
    BlockReadNext
  else
    MoveBy(1);
end;

procedure TDataSet.BlockReadNext;
begin
  MoveBy(1);
end;

procedure TDataSet.Open;

begin
  Active:=True;
end;

procedure TDataSet.Post;

begin
  UpdateRecord;
  if State in [dsEdit,dsInsert] then
    begin
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Post: checking required fields');
{$endif}
    DoBeforePost;
    If Not TryDoing(@InternalPost,OnPostError) then exit;
    cursorposchanged;
{$ifdef dsdebug}
    writeln ('Post: Internalpost succeeded');
{$endif}
    FreeFieldBuffers;
// First set the state to dsBrowse, then the Resync, to prevent the calling of
// the deDatasetChange event, while the state is still 'editable', while the db isn't
    SetState(dsBrowse);
    Resync([]);
{$ifdef dsdebug}
    writeln ('Post: Browse mode set');
{$endif}
    DoAfterPost;
    end
  else if State<>dsSetKey then
    DatabaseErrorFmt(SNotEditing, [Name], Self);
end;

procedure TDataSet.Prior;

begin
  MoveBy(-1);
end;

procedure TDataSet.Refresh;

begin
  CheckbrowseMode;
  DoBeforeRefresh;
  UpdateCursorPos;
  InternalRefresh;
{ SetCurrentRecord is called by UpdateCursorPos already, so as long as
  InternalRefresh doesn't do strange things this should be ok. }
//  SetCurrentRecord(FActiveRecord);
  Resync([]);
  DoAfterRefresh;
end;

procedure TDataSet.RegisterDataSource(ADataSource: TDataSource);

begin
  FDataSources.Add(ADataSource);
  RecalcBufListSize;
end;


procedure TDataSet.Resync(Mode: TResyncMode);

var i,count : integer;

begin
  // See if we can find the requested record.
{$ifdef dsdebug}
    Writeln ('Resync called');
{$endif}
  if FIsUnidirectional then Exit;
// place the cursor of the underlying dataset to the active record
//  SetCurrentRecord(FActiveRecord);

// Now look if the data on the current cursor of the underlying dataset is still available
  If GetRecord(FBuffers[0],gmCurrent,False)<>grOk Then
// If that fails and rmExact is set, then raise an exception
    If rmExact in Mode then
      DatabaseError(SNoSuchRecord,Self)
// else, if rmexact is not set, try to fetch the next  or prior record in the underlying dataset
    else if (GetRecord(FBuffers[0],gmNext,True)<>grOk) and
            (GetRecord(FBuffers[0],gmPrior,True)<>grOk) then
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
  FRecordCount := 1;
  FActiveRecord := 0;

// Fill the buffers before the active record
  while (i < count) and GetPriorRecord do
    inc(i);
  FActiveRecord := i;
// Fill the rest of the buffer
  GetNextRecords;
// If the buffer is not full yet, try to fetch some more prior records
  if FRecordCount < FBufferCount then inc(FActiveRecord,getpriorrecords);
// That's all folks!
  DataEvent(deDatasetChange,0);
end;

procedure TDataSet.SetFields(const Values: array of const);

Var I  : longint;
begin
  For I:=0 to high(Values) do
    Fields[I].AssignValue(Values[I]);
end;

function TDataSet.Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer;

begin
  strcopy(dest,src);
  Result:=StrLen(dest);
end;

function TDataSet.TryDoing(P: TDataOperation; Ev: TDatasetErrorEvent): Boolean;

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
          daAbort : Abort;
        end;
        end;
    else
      Raise;
    end;
{$ifdef dsdebug}
  Writeln ('Exit Trying to do');
{$endif dsdebug}
end;

procedure TDataSet.UpdateCursorPos;

begin
  If FRecordCount>0 then
    SetCurrentRecord(FActiveRecord);
end;

procedure TDataSet.UpdateRecord;

begin
  if not (State in dsEditModes) then
    DatabaseErrorFmt(SNotEditing, [Name], Self);
  DataEvent(deUpdateRecord, 0);
end;

function TDataSet.UpdateStatus: TUpdateStatus;

begin
  Result:=usUnmodified;
end;

procedure TDataSet.RemoveField(Field: TField);

begin
  //!! To be implemented
end;

procedure TDataSet.SetConstraints(Value: TCheckConstraints);
begin
  FConstraints.Assign(Value);
end;

function TDataSet.GetfieldCount: Integer;

begin
  Result:=FFieldList.Count;
end;

procedure TDataSet.ShiftBuffersBackward;

var TempBuf : pointer;

begin
  TempBuf := FBuffers[0];
  move(FBuffers[1],FBuffers[0],(FBufferCount)*sizeof(FBuffers[0]));
  FBuffers[BufferCount]:=TempBuf;
end;

procedure TDataSet.ShiftBuffersForward;

var TempBuf : pointer;

begin
  TempBuf := FBuffers[FBufferCount];
  move(FBuffers[0],FBuffers[1],(FBufferCount)*sizeof(FBuffers[0]));
  FBuffers[0]:=TempBuf;
end;

function TDataSet.GetFieldValues(const FieldName: string): Variant;

var i: Integer;
    lFields: TList;
begin
  lFields := TList.Create;
  try
    GetFieldList(lFields, FieldName);
    if lFields.Count>1 then begin
      Result := VarArrayCreate([0, lFields.Count - 1], varVariant);
      for i := 0 to lFields.Count - 1 do
        Result[i] := TField(lFields[i]).Value;
    end else
      Result := FieldByName(FieldName).Value;
  finally
    lFields.Free;
  end;
end;

procedure TDataSet.SetFieldValues(const FieldName: string; Value: Variant);

var
  i, l, h : Integer;
  lFields: TList;
begin
  if VarIsArray(Value) then begin
    lFields := TList.Create;
    try
      GetFieldList(lFields, FieldName);
      l := VarArrayLowBound(Value, 1);
      h := VarArrayHighBound(Value, 1);
      if (lFields.Count = 1) and (l < h) then
        // Allow for a field type that can deal with an array
        FieldByName(FieldName).Value := Value
      else
        for i := 0 to lFields.Count - 1 do
          TField(lFields[i]).Value := Value[l+i];
    finally
      lFields.Free;
    end;
  end else
    FieldByName(FieldName).Value := Value;
end;

function TDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): boolean;

begin
  CheckBiDirectional;
  Result := False;
end;

function TDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;

begin
  CheckBiDirectional;
  Result := Null;
end;


procedure TDataSet.UnRegisterDataSource(ADataSource: TDataSource);

begin
  FDataSources.Remove(ADataSource);
end;

{------------------------------------------------------------------------------}
{ IProviderSupport methods}

procedure TDataSet.PSEndTransaction(Commit: Boolean);
begin
  DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSExecute;
begin
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  Result := 0;
  DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSGetAttributes(List: TList);
begin
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetCommandText: string;
begin
  Result := '';
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetCommandType: TPSCommandType;
begin
  Result := ctUnknown;
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetDefaultOrder: TIndexDef;
begin
  Result := nil;
  //DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := nil;
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetKeyFields: string;
begin
  Result := '';
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetParams: TParams;
begin
  Result := nil;
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetQuoteChar: string;
begin
  Result := '';
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetTableName: string;
begin
  Result := '';
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError
  ): EUpdateError;
begin
  if Prev <> nil then
    Result := EUpdateError.Create(E.Message, '', 0, Prev.ErrorCode, E)
  else
    Result := EUpdateError.Create(E.Message, '', 0, 0, E)
end;

function TDataSet.PSInTransaction: Boolean;
begin
  Result := False;
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSIsSQLBased: Boolean;
begin
  Result := False;
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSIsSQLSupported: Boolean;
begin
  Result := False;
  DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSReset;
begin
  //DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSSetCommandText(const CommandText: string);
begin
  DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSSetParams(AParams: TParams);
begin
  DatabaseError('Provider support not available', Self);
end;

procedure TDataSet.PSStartTransaction;
begin
  DatabaseError('Provider support not available', Self);
end;

function TDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet
  ): Boolean;
begin
  Result := False;
  DatabaseError('Provider support not available', Self);
end;

{------------------------------------------------------------------------------}

operator Enumerator(ADataSet: TDataSet): TDataSetEnumerator;
begin
 Result:=TDataSetEnumerator.Create(ADataSet);
end;

constructor TDataSetEnumerator.Create(ADataSet: TDataSet);
begin
  inherited Create;
  FDataSet:=ADataSet;
  FBOF:=True;
  FDataSet.First;
end;

function TDataSetEnumerator.GetCurrent: TFields;
begin
  Result := FDataSet.Fields;
end;

function TDataSetEnumerator.MoveNext: Boolean;

begin
  if FBOF then
    FBOF:=False
  else
    FDataSet.Next;
  Result:=not FDataSet.EOF;
end;
