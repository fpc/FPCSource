{
    $Id: database.inc,v 1.12 2005/04/26 16:37:44 michael Exp $
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

procedure TDataBase.Loaded;

begin
  inherited;
  if FOpenAfterRead then
    SetConnected(true);
end;

procedure TDataBase.SetConnected (Value : boolean);

begin
  If Value<>FConnected then
    begin
    If Value then
      begin
      if csReading in ComponentState then
        begin
        FOpenAfterRead := true;
        exit;
        end
      else
//        try
          DoInternalConnect;
//        except
//          on e: EDatabaseError do DoInternalDisconnect;
      //    raise;
//        end; {try}
      end
    else
      begin
      Closedatasets;
      Closetransactions;
      DoInternalDisConnect;
      if csloading in ComponentState then
        FOpenAfterRead := false;
      end;
    FConnected:=Value;
    end;
end;


procedure TDatabase.Notification(AComponent: TComponent; Operation: TOperation);

begin
  //!! To be implemented.
end;

constructor TDatabase.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FParams:=TStringlist.Create;
  FDatasets:=TList.Create;
  FTransactions:=TList.Create;
end;

destructor TDatabase.Destroy;

begin
  Connected:=False;
  RemoveDatasets;
  RemoveTransactions;
  FDatasets.Free;
  FTransactions.Free;
  FParams.Free;
  Inherited Destroy;
end;

procedure TDatabase.Close;

begin
  Connected:=False;
end;

procedure TDatabase.CloseDataSets;

Var I : longint;

begin
  If Assigned(FDatasets) then
    begin
    For I:=FDatasets.Count-1 downto 0 do
      TDBDataset(FDatasets[i]).Close;
    end;
end;

procedure TDatabase.CloseTransactions;

Var I : longint;

begin
  If Assigned(FTransactions) then
    begin
    For I:=FTransactions.Count-1 downto 0 do
      TDBTransaction(FTransactions[i]).EndTransaction;
    end;
end;

procedure TDatabase.RemoveDataSets;

Var I : longint;

begin
  If Assigned(FDatasets) then
    For I:=FDataSets.Count-1 downto 0 do
      TDBDataset(FDataSets[i]).Database:=Nil;
end;

procedure TDatabase.RemoveTransactions;

Var I : longint;

begin
  If Assigned(FTransactions) then
    For I:=FTransactions.Count-1 downto 0 do
      TDBTransaction(FTransactions[i]).Database:=Nil;
end;

procedure TDatabase.Open;

begin
  Connected:=True;
end;


Function TDatabase.GetDataSetCount : Longint;

begin
  If Assigned(FDatasets) Then
    Result:=FDatasets.Count
  else
    Result:=0;
end;

Function TDatabase.GetTransactionCount : Longint;

begin
  If Assigned(FTransactions) Then
    Result:=FTransactions.Count
  else
    Result:=0;
end;

Function TDatabase.GetDataset(Index : longint) : TDBDataset;

begin
  If Assigned(FDatasets) then
    Result:=TDBDataset(FDatasets[Index])
  else
    DatabaseError(SNoDatasets);
end;

Function TDatabase.GetTransaction(Index : longint) : TDBtransaction;

begin
  If Assigned(FTransactions) then
    Result:=TDBTransaction(FTransactions[Index])
  else
    DatabaseError(SNoTransactions);
end;

procedure TDatabase.RegisterDataset (DS : TDBDataset);

Var I : longint;

begin
  I:=FDatasets.IndexOf(DS);
  If I=-1 then
    FDatasets.Add(DS)
  else
    DatabaseErrorFmt(SDatasetRegistered,[DS.Name]);
end;

procedure TDatabase.RegisterTransaction (TA : TDBTransaction);

Var I : longint;

begin
  I:=FTransactions.IndexOf(TA);
  If I=-1 then
    FTransactions.Add(TA)
  else
    DatabaseErrorFmt(STransactionRegistered,[TA.Name]);
end;

procedure TDatabase.UnRegisterDataset (DS : TDBDataset);

Var I : longint;

begin
  I:=FDatasets.IndexOf(DS);
  If I<>-1 then
    FDatasets.Delete(I)
  else
    DatabaseErrorFmt(SNoDatasetRegistered,[DS.Name]);
end;

procedure TDatabase.UnRegisterTransaction (TA : TDBTransaction);

Var I : longint;

begin
  I:=FTransactions.IndexOf(TA);
  If I<>-1 then
    FTransactions.Delete(I)
  else
    DatabaseErrorFmt(SNoTransactionRegistered,[TA.Name]);
end;


{ ---------------------------------------------------------------------
    TDBdataset
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
  if FOpenAfterRead then SetActive(true);
end;


Procedure TDBTransaction.CheckActive;

begin
  If not FActive Then
    DatabaseError(STransNotActive,Self);
end;

Procedure TDBTransaction.CheckInActive;

begin
  If FActive Then
    DatabaseError(STransActive,Self);
end;

Procedure TDBTransaction.CloseTrans;

begin
  FActive := false;
end;

Procedure TDBTransaction.OpenTrans;

begin
  FActive := true;
end;

Procedure TDBTransaction.SetDatabase (Value : TDatabase);

begin
  If Value<>FDatabase then
    begin
    CheckInactive;
    If Assigned(FDatabase) then
      FDatabase.UnregisterTransaction(Self);
    If Value<>Nil Then
      Value.RegisterTransaction(Self);
    FDatabase:=Value;
    end;
end;

constructor TDBTransaction.create(AOwner : TComponent);

begin
  inherited create(AOwner);
  FDatasets:=TList.Create;
end;

Procedure TDBTransaction.CheckDatabase;

begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

procedure TDBTransaction.CloseDataSets;

Var I : longint;

begin
  If Assigned(FDatasets) then
    begin
    For I:=FDatasets.Count-1 downto 0 do
      TDBDataset(FDatasets[i]).Close;
    end;
end;

Destructor TDBTransaction.Destroy;

begin
  Database:=Nil;
  RemoveDatasets;
  FDatasets.Free;
  Inherited;
end;

procedure TDBTransaction.RemoveDataSets;

Var I : longint;

begin
  If Assigned(FDatasets) then
    For I:=FDataSets.Count-1 downto 0 do
      TDBDataset(FDataSets[i]).Transaction:=Nil;
end;

Function TDBTransaction.GetDataSetCount : Longint;

begin
  If Assigned(FDatasets) Then
    Result:=FDatasets.Count
  else
    Result:=0;
end;

procedure TDBTransaction.UnRegisterDataset (DS : TDBDataset);

Var I : longint;

begin
  I:=FDatasets.IndexOf(DS);
  If I<>-1 then
    FDatasets.Delete(I)
  else
    DatabaseErrorFmt(SNoDatasetRegistered,[DS.Name]);
end;

procedure TDBTransaction.RegisterDataset (DS : TDBDataset);

Var I : longint;

begin
  I:=FDatasets.IndexOf(DS);
  If I=-1 then
    FDatasets.Add(DS)
  else
    DatabaseErrorFmt(SDatasetRegistered,[DS.Name]);
end;

Function TDBTransaction.GetDataset(Index : longint) : TDBDataset;

begin
  If Assigned(FDatasets) then
    Result:=TDBDataset(FDatasets[Index])
  else
    DatabaseError(SNoDatasets);
end;

{ ---------------------------------------------------------------------
    TCustomConnection
  ---------------------------------------------------------------------}

procedure TCustomConnection.SetAfterConnect(const AValue: TNotifyEvent);
begin
  if FAfterConnect=AValue then exit;
  FAfterConnect:=AValue;
end;

procedure TCustomConnection.SetAfterDisconnect(const AValue: TNotifyEvent);
begin
  if FAfterDisconnect=AValue then exit;
  FAfterDisconnect:=AValue;
end;

procedure TCustomConnection.SetBeforeConnect(const AValue: TNotifyEvent);
begin
  if FBeforeConnect=AValue then exit;
  FBeforeConnect:=AValue;
end;

procedure TCustomConnection.SetBeforeDisconnect(const AValue: TNotifyEvent);
begin
  if FBeforeDisconnect=AValue then exit;
  FBeforeDisconnect:=AValue;
end;

procedure TCustomConnection.DoInternalConnect;
begin
  if Assigned(BeforeConnect) then
    BeforeConnect(self);
  DoConnect;
  if Assigned(AfterConnect) then
    AfterConnect(self);
end;

procedure TCustomConnection.DoInternalDisconnect;
begin
  if Assigned(BeforeDisconnect) then
    BeforeDisconnect(self);
  DoDisconnect;
  if Assigned(AfterDisconnect) then
    AfterDisconnect(self);
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
  Result := Connected;
end;

procedure TCustomConnection.StartTransaction;

begin
  // Do nothing yet
end;

procedure TCustomConnection.EndTransaction;

begin
  // Do nothing yet
end;


{
  $Log: database.inc,v $
  Revision 1.12  2005/04/26 16:37:44  michael
  + Added TCustomConnection by Uberto Barbini

  Revision 1.11  2005/04/24 19:21:28  joost
  - some fixes in assignment of transactions and databases

  Revision 1.10  2005/02/14 17:13:12  peter
    * truncate log

}
