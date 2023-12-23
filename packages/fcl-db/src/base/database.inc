{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    TDatabase and related objects implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ ---------------------------------------------------------------------
    TDatabase
  ---------------------------------------------------------------------}

Procedure TDatabase.CheckConnected;

begin
  If Not Connected Then
    DatabaseError(SNotConnected,Self);
end;


Procedure TDatabase.CheckDisConnected;
begin
  If Connected Then
    DatabaseError(SConnected,Self);
end;

procedure TDatabase.DoConnect;
begin
  DoInternalConnect;
  FConnected := True;
end;

procedure TDatabase.DoDisconnect;
begin
  CloseDatasets;
  CloseTransactions;
  DoInternalDisConnect;
  if csLoading in ComponentState then
    FOpenAfterRead := false;
  FConnected := False;
end;

function TDatabase.GetConnected: boolean;
begin
  Result:= FConnected;
end;

constructor TDatabase.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FParams:=TStringlist.Create;
  FDatasets:=TThreadList.Create;
  FTransactions:=TThreadList.Create;
  FConnected:=False;
end;

destructor TDatabase.Destroy;

begin
  CloseForDestroy;
  RemoveDatasets;
  RemoveTransactions;
  FDatasets.Free;
  FTransactions.Free;
  FParams.Free;
  Inherited Destroy;
end;

procedure TDatabase.CloseDataSets;

Var
  I : longint;
  L : TList;

begin
  If Assigned(FDatasets) then
    begin
    L:=FDatasets.LockList;
    try
      For I:=L.Count-1 downto 0 do
        TDataset(L[i]).Close;
    finally
      FDatasets.UnlockList;
    end;
    end;
end;

procedure TDatabase.CloseTransactions;

Var
  I : longint;
  L : TList;

begin
  If Assigned(FTransactions) then
    begin
    L:=FTransactions.LockList;
    try
      For I:=L.Count-1 downto 0 do
        try
          TDBTransaction(L[i]).EndTransaction;
        except
          if not ForcedClose then
            Raise;
        end;
    finally
      FTransactions.UnlockList
    end;
    end;
end;

procedure TDatabase.RemoveDataSets;

Var
  I : longint;
  L : TList;
begin
  If Assigned(FDatasets) then
    begin
    L:=FDatasets.LockList;
    try
      For I:=L.Count-1 downto 0 do
        TDBDataset(L[i]).Database:=Nil;
    finally
      FDatasets.UnlockList;
    end;
    end;
end;

procedure TDatabase.RemoveTransactions;

Var
  I : longint;
  L : TList;
begin
  If Assigned(FTransactions) then
    begin
    L:=FTransactions.LockList;
    try
      For I:=L.Count-1 downto 0 do
        TDBTransaction(L[i]).Database:=Nil;
    finally
      FTransactions.UnlockList
    end;
    end;
end;

procedure TDatabase.SetParams(AValue: TStrings);
begin
  if AValue<>nil then
    FParams.Assign(AValue);
end;

Function TDatabase.GetDataSetCount : Longint;

Var
  L : TList;

begin
  Result:=0;
  If Assigned(FDatasets) Then
    begin
    L:=FDatasets.LockList;
    try
      Result:=L.Count;
    finally
      FDatasets.Unlocklist;
    end;
    end;
end;

Function TDatabase.GetTransactionCount : Longint;

Var
  L : TList;

begin
  Result:=0;
  If Assigned(FTransactions) Then
    begin
    L:=FTransactions.LockList;
    try
      Result:=L.Count;
    finally
      FTransactions.UnlockList;
    end;
    end;
end;

Function TDatabase.GetDataset(Index : longint) : TDataset;

Var
  L : TList;

begin
  If Not Assigned(FDatasets) then
    begin
    result := nil;
    DatabaseError(SNoDatasets);
    end
  else
    begin
    L:=FDatasets.LockList;
    try
      Result:=TDataset(L[Index])
    finally
      FDatasets.UnlockList;
    end;
    end;
end;

Function TDatabase.GetTransaction(Index : longint) : TDBtransaction;

Var
  L : TList;

begin
  If Not Assigned(FTransactions) then
    begin
    result := nil;
    DatabaseError(SNoTransactions);
    end
  else
    begin
    L:=FTransactions.LockList;
    try
      Result:=TDBTransaction(L[Index])
    finally
      FTransactions.UnlockList;
    end;
    end;
end;

procedure TDatabase.RegisterDataset (DS : TDBDataset);

Var
  I : longint;
  L : TList;
begin
  L:=FDatasets.LockList;
  try
    I:=L.IndexOf(DS);
    If I=-1 then
      L.Add(DS)
    else
      DatabaseErrorFmt(SDatasetRegistered,[DS.Name]);
  finally
    FDatasets.UnlockList;
  end;
end;

procedure TDatabase.RegisterTransaction (TA : TDBTransaction);

Var
  I : longint;
  L : TList;

begin
  L:=FTransactions.LockList;
  try
    I:=L.IndexOf(TA);
    If I=-1 then
      L.Add(TA)
    else
      DatabaseErrorFmt(STransactionRegistered,[TA.Name]);
  finally
    FTransactions.UnlockList;
  end;
end;

procedure TDatabase.UnRegisterDataset (DS : TDBDataset);

Var
  I : longint;
  L : TList;

begin
  L:=FDatasets.LockList;
  try
    I:=L.IndexOf(DS);
    If I<>-1 then
      L.Delete(I)
    else
      DatabaseErrorFmt(SNoDatasetRegistered,[DS.Name]);
  finally
    FDatasets.UnlockList;
  end;
end;

procedure TDatabase.UnRegisterTransaction (TA : TDBTransaction);

Var
  I : longint;
  L : TList;

begin
  L:=FTransactions.LockList;
  try
    I:=L.IndexOf(TA);
    If I<>-1 then
      L.Delete(I)
    else
      DatabaseErrorFmt(SNoTransactionRegistered,[TA.Name]);
  finally
    FTransactions.UnlockList;
  end;
end;


{ ---------------------------------------------------------------------
    TDBDataset
  ---------------------------------------------------------------------}

Procedure TDBDataset.SetDatabase (Value : TDatabase);

begin
  If Value<>FDatabase then
    begin
    CheckInactive;
    If Assigned(FDatabase) then
      FDatabase.UnregisterDataset(Self);
    If Value<>Nil Then
      Value.RegisterDataset(Self);
    FDatabase:=Value;
    end;
end;

Procedure TDBDataset.SetTransaction (Value : TDBTransaction);

begin
  CheckInactive;
  If Value<>FTransaction then
    begin
    If Assigned(FTransaction) then
      FTransaction.UnregisterDataset(Self);
    If Value<>Nil Then
      Value.RegisterDataset(Self);
    FTransaction:=Value;
    end;
end;

Procedure TDBDataset.CheckDatabase;

begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

Destructor TDBDataset.Destroy;

begin
  Database:=Nil;
  Transaction:=Nil;
  Inherited;
end;

{ ---------------------------------------------------------------------
    TDBTransaction
  ---------------------------------------------------------------------}
procedure TDBTransaction.SetActive(Value : boolean);
begin
  if FActive and (not Value) then
    EndTransaction
  else if (not FActive) and Value then
    if csLoading in ComponentState then
      begin
      FOpenAfterRead := true;
      exit;
      end
    else
      StartTransaction;
end;

procedure TDBTransaction.Loaded;

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

procedure TDBTransaction.InternalHandleException;

begin
  if assigned(classes.ApplicationHandleException) then
    classes.ApplicationHandleException(self)
  else
    ShowException(ExceptObject,ExceptAddr);
end;

procedure TDBTransaction.CheckActive;

begin
  If not FActive Then
    DatabaseError(STransNotActive,Self);
end;

procedure TDBTransaction.CheckInactive;

begin
  If FActive Then
    DatabaseError(STransActive,Self);
end;

procedure TDBTransaction.Commit;
begin
  EndTransaction;
end;

procedure TDBTransaction.CommitRetaining;
begin
  Commit;
  StartTransaction;
end;

procedure TDBTransaction.Rollback;
begin
  EndTransaction;
end;

procedure TDBTransaction.RollbackRetaining;
begin
  RollBack;
  StartTransaction;
end;

procedure TDBTransaction.CloseTrans;

begin
  FActive := false;
end;

procedure TDBTransaction.OpenTrans;

begin
  FActive := true;
end;

procedure TDBTransaction.SetDatabase(Value: TDatabase);

begin
  If Value=FDatabase then
    exit;
  CheckInactive;
  If Assigned(FDatabase) then
    FDatabase.UnregisterTransaction(Self);
  If Value<>Nil Then
    Value.RegisterTransaction(Self);
  FDatabase:=Value;
end;

constructor TDBTransaction.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FDatasets:=TThreadList.Create;
end;

procedure TDBTransaction.CheckDatabase;

begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

Function TDBTransaction.AllowClose(DS : TDBDataset) : Boolean;

begin
  Result:=Assigned(DS);
end;

procedure TDBTransaction.CloseDataset(DS: TDBDataset; InCommit : Boolean);
begin
  DS.Close;
end;

procedure TDBTransaction.CloseDataSets(InCommit: Boolean);

Var
  I : longint;
  L : TList;
  DS : TDBDataset;

begin
  If Assigned(FDatasets) then
    begin
    L:=FDatasets.LockList;
    try
      For I:=L.Count-1 downto 0 do
        begin
        DS:=TDBDataset(L[i]);
        If AllowClose(DS) then
          CloseDataset(DS,InCommit);
        end;
    finally
      FDatasets.UnlockList;
    end;
    end;
end;

procedure TDBTransaction.CloseDataSets;

begin
  CloseDatasets(Active);
end;

destructor TDBTransaction.Destroy;

begin
  CloseDataSets;
  RemoveDatasets;
  Database:=Nil;
  FDatasets.Free;
  Inherited;
end;

procedure TDBTransaction.RemoveDataSets;

Var
  I : longint;
  L : TList;

begin
  If Not Assigned(FDatasets) then
    exit;
  L:=FDatasets.LockList;
  try
    For I:=L.Count-1 downto 0 do
      TDBDataset(L[i]).Transaction:=Nil;
  finally
    FDatasets.unlockList;
  end;
end;

function TDBTransaction.GetDataset(Index: longint): TDBDataset;

Var
  L : TList;


begin
  If Not Assigned(FDatasets) then
    DatabaseError(SNoDatasets);
  L:=FDatasets.LockList;
  try
    Result:=TDBDataset(L[Index])
  finally
    FDatasets.UnlockList;
  end;
end;

function TDBTransaction.GetDataSetCount: Longint;

Var
  L : TList;

begin
  Result:=0;
  If Not Assigned(FDatasets) Then
    exit;
  L:=FDatasets.lockList;
  try
    Result:=L.Count
  finally
    FDatasets.UnlockList;
  end;
end;

procedure TDBTransaction.RegisterDataset (DS : TDBDataset);

Var
  I : longint;
  L : TList;
begin
  L:=FDatasets.LockList;
  try
    I:=L.IndexOf(DS);
    If I=-1 then
      L.Add(DS)
    else
      DatabaseErrorFmt(SDatasetRegistered,[DS.Name]);
  finally
    FDatasets.UnlockList;
  end;
end;

procedure TDBTransaction.UnRegisterDataset (DS : TDBDataset);

Var
  I : longint;
  L : TList;

begin
  L:=FDatasets.LockList;
  try
    I:=L.IndexOf(DS);
    If I<>-1 then
      L.Delete(I)
    else
      DatabaseErrorFmt(SNoDatasetRegistered,[DS.Name]);
  finally
    FDatasets.UnlockList;
  end;
end;

{ ---------------------------------------------------------------------
    TCustomConnection
  ---------------------------------------------------------------------}

function TCustomConnection.GetDataSet(Index: Longint): TDataSet;
begin
  Result := nil;
end;

function TCustomConnection.GetDataSetCount: Longint;
begin
  Result := 0;
end;

procedure TCustomConnection.InternalHandleException;
begin
  if assigned(classes.ApplicationHandleException) then
    classes.ApplicationHandleException(self)
  else
    ShowException(ExceptObject,ExceptAddr);
end;

procedure TCustomConnection.SetAfterConnect(const AValue: TNotifyEvent);
begin
  FAfterConnect:=AValue;
end;

procedure TCustomConnection.SetAfterDisconnect(const AValue: TNotifyEvent);
begin
  FAfterDisconnect:=AValue;
end;

procedure TCustomConnection.SetBeforeConnect(const AValue: TNotifyEvent);
begin
  FBeforeConnect:=AValue;
end;

procedure TCustomConnection.SetBeforeDisconnect(const AValue: TNotifyEvent);
begin
  FBeforeDisconnect:=AValue;
end;

procedure TCustomConnection.SetForcedClose(AValue: Boolean);
begin
  if FForcedClose=AValue then Exit;
  FForcedClose:=AValue;
end;

procedure TCustomConnection.DoCloseError(aError: Exception);
begin
  if Assigned(FOnCloseError) then
    FOnCloseError(Self,aError);
end;

procedure TCustomConnection.DoLoginPrompt;

var
  ADatabaseName, AUserName, APassword: string;

begin
  if FLoginPrompt then
    begin
    GetLoginParams(ADatabaseName, AUserName, APassword);
    if Assigned(FOnLogin) then
      FOnLogin(Self, AUserName, APassword) // by value
    else if Assigned(LoginDialogExProc) then
      begin
      LoginDialogExProc(ADatabaseName, AUserName, APassword, False); // by reference
      SetLoginParams(ADatabaseName, AUserName, APassword);
      end;
    end;
end;

procedure TCustomConnection.SetConnected(Value: boolean);

begin
  If Value<>Connected then
    begin
    If Value then
      begin
      if csReading in ComponentState then
        begin
        FStreamedConnected := true;
        exit;
        end
      else
        begin
        if Assigned(BeforeConnect) then
          BeforeConnect(self);
        DoLoginPrompt;
        DoConnect;
        if Assigned(AfterConnect) then
          AfterConnect(self);
        end;
      end
    else
      begin
      if Assigned(BeforeDisconnect) then
        BeforeDisconnect(self);
      DoDisconnect;
      if Assigned(AfterDisconnect) then
        AfterDisconnect(self);
      end;
    end;
end;

procedure TCustomConnection.GetLoginParams(out ADatabaseName, AUserName, APassword: string);
begin
  if IsPublishedProp(Self,'DatabaseName') then
    ADatabaseName := GetStrProp(Self,'DatabaseName');
  if IsPublishedProp(Self,'UserName') then
    AUserName := GetStrProp(Self,'UserName');
  if IsPublishedProp(Self,'Password') then
    APassword := GetStrProp(Self,'Password');
end;

procedure TCustomConnection.SetLoginParams(const ADatabaseName, AUserName, APassword: string);
begin
  if IsPublishedProp(Self,'DatabaseName') then
    SetStrProp(Self,'DatabaseName',ADatabaseName);
  if IsPublishedProp(Self,'UserName') then
    SetStrProp(Self,'UserName',AUserName);
  if IsPublishedProp(Self,'Password') then
    SetStrProp(Self,'Password',APassword);
end;

procedure TCustomConnection.DoConnect;

begin
  // Do nothing yet
end;

procedure TCustomConnection.DoDisconnect;

begin
  // Do nothing yet
end;

function TCustomConnection.GetConnected: boolean;

begin
  Result := False;
end;

procedure TCustomConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then
      SetConnected(true);
  except
    if csDesigning in Componentstate then
      InternalHandleException
    else
      raise;
  end;
end;

procedure TCustomConnection.Close(ForceClose : Boolean = False);
begin
  try
    ForcedClose:=ForceClose;
    Connected := False;
  finally
    ForcedClose:=false;
  end;
end;

procedure TCustomConnection.CloseForDestroy;

Const
  MaxCount = 2;

var
  Force : Boolean;
  aCount : Integer;
begin
  Force:=False;
  aCount:=0;
  While Connected and (aCount<MaxCount) do
    try
      Inc(aCount);
      // Will set connected to false
      Close(Force);
    except
      On E : Exception do
        begin
        Force:=True;
        DoCloseError(E);
        end;
    end;
end;

destructor TCustomConnection.Destroy;
begin
  CloseForDestroy;
  Inherited Destroy;
end;

procedure TCustomConnection.Open;
begin
  Connected := True;
end;

