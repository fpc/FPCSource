{ ---------------------------------------------------------------------
    TDatalink
  ---------------------------------------------------------------------}

Constructor TDataLink.Create;

begin
  Inherited Create;
  FBufferCount:=1;
  FDatasourceFixed:=False;
end;


Destructor TDataLink.Destroy;

begin
  Factive:=False;
  FEditing:=False;
  FDataSourceFixed:=False;
  DataSource:=Nil;
  Inherited Destroy;
end;


Procedure TDataLink.ActiveChanged;

begin
end;

Procedure TDataLink.CheckActiveAndEditing;

Var
  B : Boolean;

begin
  B:=Assigned(DataSource) and (DataSource.State<>dsInactive);
  If B<>FActive then
    begin
    FActive:=B;
    ActiveChanged;
    end;
  B:=Assigned(DataSource) and (DataSource.State in dsEditModes) and Not FReadOnly;
  If B<>FEditing Then
    begin
    FEditing:=B;
    EditingChanged;
    end;
end;


Procedure TDataLink.CheckBrowseMode;

begin
end;

Function TDataLink.CalcFirstRecord(Index : Integer) : Integer;

{ recalculates index of first record in buffer,
  and returns number of record scrolled.}

Var
   A,              // Index of active record in buffer
   F,              // Index of current first record in buffer
   L : Integer;    // index of Last (for us) record in buffer;

begin
  A:=DataSource.DataSet.FActiveRecord;
  F:=FFirstRecord+Index;
  L:=F+FBufferCount-1;
  If A>L Then
    Result:=A-L
  else If A<F Then
    Result:=A-F;
  FFirstRecord:=F+Result;
end;


Procedure TDataLink.DataEvent(Event: TDataEvent; Info: Longint);


begin
  Case Event of
    deFieldChange,
    deRecordChange :
      If Not FUpdatingRecord then
        RecordChanged(TField(Info));
    deDataSetChange:
      begin
      SetActive(DataSource.DataSet.Active);
      CalcFirstRecord(Info);
      DatasetChanged;
      end;
    deDataSetScroll:
      DatasetScrolled(CalcFirstRecord(Info));
    deLayoutChange:
      begin
      CalcFirstRecord(Info);
      LayoutChanged;
      end;
    deUpdateRecord:
      UpdateRecord;
    deUpdateState:
      CheckActiveAndEditing;
    deCheckBrowseMode:
      CheckBrowseMode;
    deFocusControl:
      FocusControl(TFieldRef(Info));
  end;
end;


Procedure TDataLink.DataSetChanged;

begin
  RecordChanged(Nil);
end;


Procedure TDataLink.DataSetScrolled(Distance: Integer);

begin
  DataSetChanged;
end;


Procedure TDataLink.EditingChanged;

begin
end;


Procedure TDataLink.FocusControl(Field: TFieldRef);

begin
end;


Function TDataLink.GetActiveRecord: Integer;

begin
  Result:=Dataset.FActiveRecord;
end;

Function TDatalink.GetDataSet : TDataset;

begin
  Result:=FDataSource.DataSet
end;


Function TDataLink.GetBOF: Boolean;

begin
  Result:=DataSet.BOF
end;


Function TDataLink.GetBufferCount: Integer;

begin
  Result:=FBufferCount;
end;


Function TDataLink.GetEOF: Boolean;

begin
  Result:=DataSet.EOF
end;


Function TDataLink.GetRecordCount: Integer;

Var D : TDataSet;

begin
  Result:=Dataset.RecordCount;
  If Result>BufferCount then
    Result:=BufferCount;
end;


Procedure TDataLink.LayoutChanged;

begin
  DataSetChanged;
end;


Function TDataLink.MoveBy(Distance: Integer): Integer;

begin
  DataSet.MoveBy(Distance);
end;


Procedure TDataLink.RecordChanged(Field: TField);

begin
end;


Procedure TDataLink.SetActiveRecord(Value: Integer);

begin
  Dataset.FActiveRecord:=Value;
end;


Procedure TDataLink.SetBufferCount(Value: Integer);

begin
  If FBUfferCount<>Value then
    begin
    FBufferCount:=Value;
    Dataset.SetBufListSize(Value);
    end;
end;

procedure TDataLink.SetActive(AActive: Boolean);
begin
  if Active <> AActive then
  begin
    FActive := AActive;
    // !!!: Set internal state
    ActiveChanged;
  end;
end;

Procedure TDataLink.SetDataSource(Value : TDatasource);

begin
  if not FDataSourceFixed then
  begin
    if Assigned(DataSource) then
      DataSource.UnregisterDatalink(Self);
    FDataSource := Value;
    if Assigned(DataSource) then
      DataSource.RegisterDatalink(Self);
  end;
end;

Procedure TDatalink.SetReadOnly(Value : Boolean);

begin
  If FReadOnly<>Value then
    begin
    FReadOnly:=Value;
    CheckActiveAndEditing;
    end;
end;

Procedure TDataLink.UpdateData;

begin
end;



Function TDataLink.Edit: Boolean;

begin
  If Not FReadOnly then
    DataSource.Edit;
  // Triggered event will set FEditing
  Result:=FEditing;
end;


Procedure TDataLink.UpdateRecord;

begin
  FUpdatingRecord:=True;
  Try
    UpdateData;
  finally
   FUpdatingRecord:=False;
  end;
end;



{ ---------------------------------------------------------------------
    TDetailDataLink
  ---------------------------------------------------------------------}

Function TDetailDataLink.GetDetailDataSet: TDataSet;

begin
end;


{ ---------------------------------------------------------------------
    TMasterDataLink
  ---------------------------------------------------------------------}

constructor TMasterDataLink.Create(ADataSet: TDataSet);

begin
end;


destructor TMasterDataLink.Destroy;

begin
  inherited Destroy;
end;


Procedure TMasterDataLink.ActiveChanged;

begin
end;


Procedure TMasterDataLink.CheckBrowseMode;

begin
end;


Function TMasterDataLink.GetDetailDataSet: TDataSet;

begin
end;


Procedure TMasterDataLink.LayoutChanged;

begin
end;


Procedure TMasterDataLink.RecordChanged(Field: TField);

begin
end;

procedure TMasterDatalink.SetFieldNames(const Value: string);

begin
end;


{ ---------------------------------------------------------------------
    TDatasource
  ---------------------------------------------------------------------}

Constructor TDataSource.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FDatalinks:=TList.Create;
  FEnabled:=True;
  FAutoEdit:=True;
end;


Destructor TDataSource.Destroy;

begin
  FOnStateCHange:=Nil;
  Dataset:=Nil;
  With FDataLinks do
    While Count>0 do
      TDatalink(Items[Count - 1]).DataSource:=Nil;
  FDatalinks.Free;
  inherited Destroy;
end;


Procedure TDatasource.Edit;

begin
  If (State=dsBrowse) and AutoEdit Then
    Dataset.Edit;
end;


Function TDataSource.IsLinkedTo(ADataSet: TDataSet): Boolean;

begin
  Result:=False;
end;


procedure TDatasource.DistributeEvent(Event: TDataEvent; Info: Longint);


Var
  i : Longint;

begin
  With FDatalinks do
    begin
    For I:=0 to Count-1 do
      With TDatalink(Items[i]) do
        If Not VisualControl Then
          DataEvent(Event,Info);
    For I:=0 to Count-1 do
      With TDatalink(Items[i]) do
        If VisualControl Then
          DataEvent(Event,Info);
    end;
end;

procedure TDatasource.RegisterDataLink(DataLink: TDataLink);

begin
  FDatalinks.Add(DataLink);
  if Assigned(DataSet) then
    DataSet.RecalcBufListSize;
end;


procedure TDatasource.SetDataSet(ADataSet: TDataSet);

begin
  If FDataset<>Nil Then
    FDataset.UnRegisterDataSource(Self);
  If ADataset<>Nil Then
    ADataset.RegisterDatasource(Self);
  FDataSet:=ADataset;
  ProcessEvent(deUpdateState,0);
end;


procedure TDatasource.SetEnabled(Value: Boolean);

begin
  FEnabled:=Value;
end;


Procedure TDatasource.DoDataChange (Info : Pointer);

begin
  If Assigned(OnDataChange) Then
    OnDataChange(Self,TField(Info));
end;

Procedure TDatasource.DoStateChange;

begin
  If Assigned(OnStateChange) Then
    OnStateChange(Self);
end;


Procedure TDatasource.DoUpdateData;

begin
  If Assigned(OnUpdateData) Then
    OnUpdateData(Self);
end;


procedure TDatasource.UnregisterDataLink(DataLink: TDataLink);

begin
  FDatalinks.Remove(Datalink);
  If Dataset<>Nil then
    Dataset.SetBufListSize(DataLink.BufferCount);
end;


procedure TDataSource.ProcessEvent(Event : TDataEvent; Info : longint);

Const
  OnDataChangeEvents = [deRecordChange, deDataSetChange,
                           deDataSetScroll,
                           deLayoutChange,deUpdateState];

Var
  NeedDataChange : Boolean;
  FLastState : TdataSetState;

begin
  // Special UpdateState handling.
  If Event=deUpdateState then
    begin
    NeedDataChange:=(FState=dsInactive);
    FLastState:=FState;
    If Assigned(Dataset) then
      FState:=Dataset.State
    else
      FState:=dsInactive;
    // Don't do events if nothing changed.
    If FState<>FlastState then
      exit;
    end
  else
    NeedDataChange:=True;
  DistributeEvent(Event,Info);
  // Extra handlers
  If Not (csDestroying in ComponentState) then
    begin
    If (Event=deUpdateState) then
      DoStateChange;
    If (Event in OnDataChangeEvents) and
       NeedDataChange Then
      DoDataChange(Nil);
    If (Event = deFieldChange) Then
      DoDataCHange(Pointer(Info));
    If (Event=deUpdateRecord) then
      DoUpdateData;
    end;
 end;
