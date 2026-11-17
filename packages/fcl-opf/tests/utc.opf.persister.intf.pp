unit utc.opf.persister.intf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, db, sqldb,
  sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Persister.Intf;

type

  { TTestIntfContact — fixture class implementing IOPFieldProvider }

  TTestIntfContact = class(TObject, IOPFieldProvider)
  private
    FID: Integer;
    FFirstName: UTF8String;
    FLastName: UTF8String;
    FAge: Integer;
  public
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
    property ID: Integer read FID write FID;
    property FirstName: UTF8String read FFirstName write FFirstName;
    property LastName: UTF8String read FLastName write FLastName;
    property Age: Integer read FAge write FAge;
  end;

  { TTestNoIntfClass — class WITHOUT IOPFieldProvider for error test }

  TTestNoIntfClass = class(TObject)
  private
    FDummy: Integer;
  public
    property Dummy: Integer read FDummy write FDummy;
  end;

  { TTestInterfacePersister — unit tests for TInterfacePersister }

  TTestInterfacePersister = class(TTestCase)
  private
    FPersister: TInterfacePersister;
    FMap: TOPFieldMapList;
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure CreateTestTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
    procedure PopulateIntfContact;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPopulateFieldMap;
    procedure TestPopulateFieldMapPrimaryKey;
    procedure TestPersistPropertyInteger;
    procedure TestPersistPropertyString;
    procedure TestApplyPropertyInteger;
    procedure TestApplyPropertyString;
    procedure TestApplyPropertyNull;
    procedure TestPopulateFieldMapNoInterface;
    procedure TestPersistAndApplyRoundTrip;
  end;

implementation

{ TTestIntfContact }

procedure TTestIntfContact.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := F.Options + [foInKey];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('FirstName', 'FirstName');
  F.PropertyTypeInfo := TypeInfo(UTF8String);

  F := aMap.AddField('LastName', 'LastName');
  F.PropertyTypeInfo := TypeInfo(UTF8String);

  F := aMap.AddField('Age', 'Age');
  F.PropertyTypeInfo := TypeInfo(Integer);
end;

function TTestIntfContact.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then
      FID := 0
    else if aFieldMap.PropertyName = 'FirstName' then
      FFirstName := ''
    else if aFieldMap.PropertyName = 'LastName' then
      FLastName := ''
    else if aFieldMap.PropertyName = 'Age' then
      FAge := 0
    else
      Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then
    FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'FirstName' then
    FFirstName := aField.AsString
  else if aFieldMap.PropertyName = 'LastName' then
    FLastName := aField.AsString
  else if aFieldMap.PropertyName = 'Age' then
    FAge := aField.AsInteger
  else
    Result := False;
end;

function TTestIntfContact.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then
    aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'FirstName' then
    aParam.AsString := FFirstName
  else if aFieldMap.PropertyName = 'LastName' then
    aParam.AsString := FLastName
  else if aFieldMap.PropertyName = 'Age' then
    aParam.AsInteger := FAge
  else
    Result := False;
end;

{ TTestInterfacePersister }

procedure TTestInterfacePersister.SetUp;
begin
  inherited SetUp;
  FPersister := TInterfacePersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
  FMap.DefaultFieldOptions := [foInSelect, foInInsert, foInUpdate];
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
end;

procedure TTestInterfacePersister.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  if Assigned(FConn) and FConn.Connected then
    FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestInterfacePersister.CreateTestTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestIntfContact (ID INTEGER PRIMARY KEY, FirstName TEXT, LastName TEXT, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestInterfacePersister.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestInterfacePersister.PopulateIntfContact;
begin
  FPersister.PopulateFieldMap(TTestIntfContact, FMap);
end;

procedure TTestInterfacePersister.TestPopulateFieldMap;
begin
  PopulateIntfContact;
  AssertEquals('TTestIntfContact should have 4 fields', 4, FMap.Count);
  AssertNotNull('ID field exists', FMap.FindByPropertyName('ID'));
  AssertNotNull('FirstName field exists', FMap.FindByPropertyName('FirstName'));
  AssertNotNull('LastName field exists', FMap.FindByPropertyName('LastName'));
  AssertNotNull('Age field exists', FMap.FindByPropertyName('Age'));
end;

procedure TTestInterfacePersister.TestPopulateFieldMapPrimaryKey;
var
  F: TOPFieldMap;
begin
  PopulateIntfContact;
  F := FMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertTrue('ID has foInKey', foInKey in F.Options);
  AssertTrue('ID kind is tkInteger', F.PropertyKind = tkInteger);
end;

procedure TTestInterfacePersister.TestPersistPropertyInteger;
var
  Obj: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateIntfContact;
  CreateTestTable;
  Obj := TTestIntfContact.Create;
  try
    Obj.Age := 42;
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('INSERT INTO TestIntfContact (ID, Age) VALUES (:ID, :Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Age'));
      AssertEquals('Param value', 42, Q.Params.ParamByName('Age').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestInterfacePersister.TestPersistPropertyString;
var
  Obj: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateIntfContact;
  CreateTestTable;
  Obj := TTestIntfContact.Create;
  try
    Obj.FirstName := 'Alice';
    FM := FMap.FindByPropertyName('FirstName');
    Q := MakeQuery('INSERT INTO TestIntfContact (ID, FirstName) VALUES (:ID, :FirstName)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('FirstName'));
      AssertEquals('Param value', 'Alice', Q.Params.ParamByName('FirstName').AsString);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestInterfacePersister.TestApplyPropertyInteger;
var
  Obj: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateIntfContact;
  CreateTestTable;
  Q := MakeQuery('INSERT INTO TestIntfContact (ID, Age) VALUES (1, 25)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestIntfContact.Create;
  try
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestIntfContact WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Age'));
      AssertEquals('Age applied', 25, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestInterfacePersister.TestApplyPropertyString;
var
  Obj: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateIntfContact;
  CreateTestTable;
  Q := MakeQuery('INSERT INTO TestIntfContact (ID, FirstName) VALUES (1, ''Bob'')');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestIntfContact.Create;
  try
    FM := FMap.FindByPropertyName('FirstName');
    Q := MakeQuery('SELECT FirstName FROM TestIntfContact WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('FirstName'));
      AssertEquals('FirstName applied', 'Bob', Obj.FirstName);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestInterfacePersister.TestApplyPropertyNull;
var
  Obj: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateIntfContact;
  CreateTestTable;
  Q := MakeQuery('INSERT INTO TestIntfContact (ID, FirstName) VALUES (1, NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestIntfContact.Create;
  try
    Obj.FirstName := 'Original';
    FM := FMap.FindByPropertyName('FirstName');
    Q := MakeQuery('SELECT FirstName FROM TestIntfContact WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('FirstName'));
      AssertEquals('Null sets default empty', '', Obj.FirstName);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestInterfacePersister.TestPopulateFieldMapNoInterface;
begin
  try
    FPersister.PopulateFieldMap(TTestNoIntfClass, FMap);
    Fail('Expected EOPFPersisterIntf for class without IOPFieldProvider');
  except
    on E: EOPFPersisterIntf do
      AssertTrue('Error mentions class name',
        Pos('TTestNoIntfClass', E.Message) > 0);
  end;
end;

procedure TTestInterfacePersister.TestPersistAndApplyRoundTrip;
var
  Src, Dst: TTestIntfContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  I: Integer;
begin
  PopulateIntfContact;
  CreateTestTable;
  Src := TTestIntfContact.Create;
  try
    Src.ID := 99;
    Src.FirstName := 'Charlie';
    Src.LastName := 'Brown';
    Src.Age := 30;
    { Persist all fields via INSERT }
    Q := MakeQuery('INSERT INTO TestIntfContact (ID, FirstName, LastName, Age) VALUES (:ID, :FirstName, :LastName, :Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      for I := 0 to FMap.Count - 1 do
        FPersister.PersistProperty(Src, FMap[I], Q.Params.ParamByName(FMap[I].FieldName));
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply all fields from SELECT }
  Dst := TTestIntfContact.Create;
  try
    Q := MakeQuery('SELECT ID, FirstName, LastName, Age FROM TestIntfContact WHERE ID=99');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      AssertFalse('Row found', Q.EOF);
      for I := 0 to FMap.Count - 1 do
      begin
        FM := FMap[I];
        FPersister.ApplyProperty(Dst, FM, Q.FieldByName(FM.FieldName));
      end;
      Q.Close;
    finally
      Q.Free;
    end;
    AssertEquals('ID round-trip', 99, Dst.ID);
    AssertEquals('FirstName round-trip', 'Charlie', Dst.FirstName);
    AssertEquals('LastName round-trip', 'Brown', Dst.LastName);
    AssertEquals('Age round-trip', 30, Dst.Age);
  finally
    Dst.Free;
  end;
end;

initialization
  RegisterTest(TTestInterfacePersister);

end.
