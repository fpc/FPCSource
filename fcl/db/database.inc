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
  //!! To be implemented.
end;

procedure TDataBase.SetConnected (Value : boolean);

begin
  If Value<>FConnected then
    begin
    If Value then
      DoInternalConnect
    else
      begin
      Closedatasets;
      DoInternalDisConnect;
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
end;

destructor TDatabase.Destroy;

begin
  Connected:=False;
  RemoveDatasets;
  FDatasets.Free;
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

procedure TDatabase.RemoveDataSets;

Var I : longint;

begin
  If Assigned(FDatasets) then
    For I:=FDataSets.Count-1 downto 0 do
      TDBDataset(FDataSets[i]).Database:=Nil;
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


Function TDatabase.GetDataset(Index : longint) : TDBDataset;

begin
  If Assigned(FDatasets) then
    Result:=TDBDataset(FDatasets[Index])
  else
    DatabaseError(SNoDatasets);
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

procedure TDatabase.UnRegisterDataset (DS : TDBDataset);

Var I : longint;

begin
  I:=FDatasets.IndexOf(DS);
  If I<>-1 then
    FDatasets.Delete(I)
  else
    DatabaseErrorFmt(SNoDatasetRegistered,[DS.Name]);
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

{
  $Log$
  Revision 1.3  2002-09-07 15:15:22  peter
    * old logs removed and tabs fixed

}
