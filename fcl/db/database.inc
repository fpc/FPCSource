{
    $Id$
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
  if FOpenAfterRead then SetConnected(true);
end;

procedure TDataBase.SetConnected (Value : boolean);

begin
  If Value<>FConnected then
    begin
    If Value then
      begin
      if csLoading in ComponentState then
        begin
        FOpenAfterRead := true;
        exit;
        end
      else
        DoInternalConnect;
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
  CheckInactive;
  If Value<>FDatabase then
    begin
    If Assigned(FDatabase) then
      FDatabase.UnregisterDataset(Self);
    If Value<>Nil Then
      Value.RegisterDataset(Self);
    FDatabase:=Value;
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
  Inherited;
end;

{ ---------------------------------------------------------------------
    TDBTransaction
  ---------------------------------------------------------------------}

Procedure TDBTransaction.SetDatabase (Value : TDatabase);

begin
//  CheckInactive;
  If Value<>FDatabase then
    begin
    If Assigned(FDatabase) then
      FDatabase.UnregisterTransaction(Self);
    If Value<>Nil Then
      Value.RegisterTransaction(Self);
    FDatabase:=Value;
    end;
end;

Procedure TDBTransaction.CheckDatabase;

begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

Destructor TDBTransaction.Destroy;

begin
  Database:=Nil;
  Inherited;
end;


{
  $Log$
  Revision 1.6  2004-09-26 16:55:24  michael
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

  Revision 1.5  2004/07/25 11:32:40  michael
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

  Revision 1.4  2003/08/16 16:42:21  michael
  + Fixes in TDBDataset etc. Changed MySQLDb to use database as well

  Revision 1.3  2002/09/07 15:15:22  peter
    * old logs removed and tabs fixed

}
