{ demo_json_mapping - Table and column mapping from a JSON file
  Step 1 writes the default mapping to default_mapping.json; step 2 loads
  mapping.json, an edited copy, creates the schema and saves and loads objects.
  Works on FPC 3.2.2: no attributes or extended RTTI are used.
  Compile: fpc demo_json_mapping.lpr -Fu../../src }
program demo_json_mapping;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,
  Opf.Persister.Rtti,
  Opf.Mapping.Json;

const
  DB_FILE = 'demo_json_mapping.db';
  DEFAULT_MAPPING = 'default_mapping.json';
  MAPPING = 'mapping.json';

type
  { TCustomer: the key is Code, which no naming convention recognizes. }
  TCustomer = class(TPersistent, IOPPersistenceState)
  private
    FCode: String;
    FName: String;
    FEmail: String;
    FNotes: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property Code: String read FCode write FCode;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
    property Notes: String read FNotes write FNotes;
  end;

  { TOrder: CustomerCode refers to TCustomer. }
  TOrder = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FCustomerCode: String;
    FOrderDate: TDateTime;
    FAmount: Double;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property CustomerCode: String read FCustomerCode write FCustomerCode;
    property OrderDate: TDateTime read FOrderDate write FOrderDate;
    property Amount: Double read FAmount write FAmount;
  end;


function TCustomer.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TOrder.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TOrder.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


// Returns the name of a file next to the executable.
function LocalFile(const aName: String): String;

begin
  Result:=ExtractFilePath(ParamStr(0))+aName;
end;


// Writes the contents of a text file to the console.
procedure ShowFile(const aFileName: String);

var
  lLines: TStringList;

begin
  lLines:=TStringList.Create;
  try
    lLines.LoadFromFile(aFileName);
    Write(lLines.Text);
  finally
    lLines.Free;
  end;
end;


// Writes the table, columns and key of a registered class.
procedure ShowMapping(aClass: TClass);

var
  lDef: TPersisterDef;
  lField: TOPFieldMap;
  I: Integer;

begin
  lDef:=OPRegistry.FindPersister(aClass);
  WriteLn(aClass.ClassName, ' -> table ', lDef.TableName, ' (auto-created: ', lDef.AutoCreated, ')');
  for I:=0 to lDef.FieldMap.Count-1 do
  begin
    lField:=lDef.FieldMap[I];
    Write('  ', lField.PropertyName:14, ' -> ', lField.FieldName);
    if foInKey in lField.Options then
      Write('  [primary key]');
    WriteLn;
  end;
end;


// Writes the default mapping of the classes, the starting point for mapping.json.
procedure WriteDefaultMapping;

begin
  WriteLn('=== Step 1: write the default mapping ===');
  // TCustomer has no ID property: its first field, Code, is marked as key
  SaveDefaultMapping(LocalFile(DEFAULT_MAPPING), [TCustomer, TOrder], TRTTIPersister);
  WriteLn('Written to ', DEFAULT_MAPPING, ':');
  ShowFile(LocalFile(DEFAULT_MAPPING));
  WriteLn;
end;


// Writes the SQL of the tables and indexes in the SQLite database.
procedure ShowTables(aConn: TSQLConnection; aTrans: TSQLTransaction);

var
  lQuery: TSQLQuery;

begin
  lQuery:=TSQLQuery.Create(Nil);
  try
    lQuery.Database:=aConn;
    lQuery.Transaction:=aTrans;
    lQuery.SQL.Text:='SELECT sql FROM sqlite_master WHERE sql IS NOT NULL AND substr(name, 1, 5)<>''_opf_''';
    lQuery.Open;
    while not lQuery.EOF do
    begin
      WriteLn(lQuery.Fields[0].AsString);
      lQuery.Next;
    end;
    lQuery.Close;
    aTrans.Commit;
  finally
    lQuery.Free;
  end;
end;


// Saves a customer with two orders and loads them back.
procedure UseMapping;

var
  lCustomer: TCustomer;
  lOrder: TOrder;
  lOrders: TObjectList;
  I: Integer;

begin
  lCustomer:=TCustomer.Create;
  try
    lCustomer.Code:='C001';
    lCustomer.Name:='Acme Ltd';
    lCustomer.Email:='sales@acme.example';
    lCustomer.Notes:='Not stored: skipped in mapping.json';
    lCustomer.SetPersistenceState(opsNew);
    OPManager.Save(lCustomer);
  finally
    lCustomer.Free;
  end;
  for I:=1 to 2 do
  begin
    lOrder:=TOrder.Create;
    try
      lOrder.ID:=I;
      lOrder.CustomerCode:='C001';
      lOrder.OrderDate:=EncodeDate(2026, 11, 20+I);
      lOrder.Amount:=100*I;
      lOrder.SetPersistenceState(opsNew);
      OPManager.Save(lOrder);
    finally
      lOrder.Free;
    end;
  end;
  lCustomer:=TCustomer.Create;
  try
    lCustomer.Code:='C001';
    OPManager.Load(lCustomer);
    WriteLn('Loaded customer ', lCustomer.Code, ': ', lCustomer.Name, ' <', lCustomer.Email, '> notes="', lCustomer.Notes, '"');
  finally
    lCustomer.Free;
  end;
  lOrders:=TObjectList.Create(True);
  try
    OPManager.Load(lOrders, TOrder);
    for I:=0 to lOrders.Count-1 do
    begin
      lOrder:=TOrder(lOrders[I]);
      WriteLn('Loaded order ', lOrder.ID, ' for ', lOrder.CustomerCode, ' on ', DateToStr(lOrder.OrderDate), ': ', lOrder.Amount:0:2);
    end;
  finally
    lOrders.Free;
  end;
end;


var
  lConn: TSQLite3Connection;
  lTrans: TSQLTransaction;
  lOPConn: TOPConnection;
  lRelation: TOPRelationDef;

begin
  // LoadMapping finds the classes of the file with Classes.GetClass
  RegisterClasses([TCustomer, TOrder]);
  WriteDefaultMapping;

  WriteLn('=== Step 2: load the edited mapping ===');
  if not FileExists(LocalFile(MAPPING)) then
  begin
    WriteLn(MAPPING, ' not found next to the executable.');
    Halt(1);
  end;
  // No RegisterPersister calls: the classes of the file are registered with TRTTIPersister
  LoadMapping(LocalFile(MAPPING), TRTTIPersister);
  ShowMapping(TCustomer);
  ShowMapping(TOrder);
  lRelation:=OPRelationRegistry.FindParentRelation(TOrder);
  WriteLn('Relation: ', lRelation.ChildClass.ClassName, '.', lRelation.ForeignKeyFieldName, ' -> ', lRelation.ParentClass.ClassName, ' (constraint ', lRelation.ConstraintName, ')');
  WriteLn;

  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  lConn:=TSQLite3Connection.Create(Nil);
  lTrans:=TSQLTransaction.Create(Nil);
  try
    lConn.DatabaseName:=DB_FILE;
    lConn.Transaction:=lTrans;
    lConn.Open;
    OPSQLBuilder.SQLDialect:=dSQLite;
    lOPConn:=OPManager.Connections.Add as TOPConnection;
    lOPConn.SingleConnection:=lConn;
    lOPConn.IsDefault:=True;

    WriteLn('=== Schema created from the mapping ===');
    OPManager.CreateSchema(lConn);
    ShowTables(lConn, lTrans);
    WriteLn;

    WriteLn('=== Save and load ===');
    UseMapping;
    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TOPManager.ClearInstance;
    ClearMapping;
    TOPRelationRegistry.ClearInstance;
    TOPRegistry.ClearInstance;
    if lConn.Connected then
      lConn.Close;
    lTrans.Free;
    lConn.Free;
    if FileExists(DB_FILE) then
      DeleteFile(DB_FILE);
  end;
end.
