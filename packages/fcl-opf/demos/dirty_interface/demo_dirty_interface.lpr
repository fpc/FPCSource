{ demo_dirty_interface - Dirty tracking via IOPDirtyTracking interface
  =====================================================================
  Demonstrates per-class dirty tracking where the class itself stores
  the snapshot by implementing IOPDirtyTracking. A TOPSelfTracker is
  attached to the manager and asks each object to keep its own snapshot.

  This approach gives the class full control over snapshot storage and
  supports explicit MarkDirty for forcing columns into the UPDATE.

  TEmployee is a plain TObject, so the tracker cannot observe it being
  freed: each employee is purged from the tracker before it is freed.

  Compile:  lazbuild demo_dirty_interface.lpi
  Run:      ./demo_dirty_interface
}
program demo_dirty_interface;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.DDL,
  Opf.Persister.Rtti,
  Opf.SnapshotHooks,
  Opf.Snapshot,
  Opf.Logging;

const
  DB_FILE = 'demo_dirty_interface.db';

type
  { TEmployee - implements both IOPPersistenceState and IOPDirtyTracking.
    The class stores its own snapshot of property values. The framework
    captures the snapshot after Load and compares at Save time. }

  TEmployee = class(TObject, IOPPersistenceState, IOPDirtyTracking)
  private
    FID: Integer;
    FName: String;
    FDepartment: String;
    FSalary: Double;
    FState: TObjectPersistenceState;
    { Snapshot storage }
    FSnapshot: TOPVariantArray;
    FHasSnapshot: Boolean;
    FMarkedDirty: TStringList;
    { IOPPersistenceState }
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    { IOPDirtyTracking }
    procedure StoreSnapshot(const aValues: TOPVariantArray);
    function RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
    procedure MarkDirty(const aPropertyName: UTF8String);
    function GetMarkedDirty: TUTF8StringArray;
    procedure ClearSnapshot;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Department: String read FDepartment write FDepartment;
    property Salary: Double read FSalary write FSalary;
  end;

constructor TEmployee.Create;
begin
  inherited Create;
  FHasSnapshot := False;
  FMarkedDirty := TStringList.Create;
  FMarkedDirty.Sorted := True;
  FMarkedDirty.Duplicates := dupIgnore;
end;

destructor TEmployee.Destroy;
begin
  FMarkedDirty.Free;
  inherited Destroy;
end;

function TEmployee.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TEmployee.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

procedure TEmployee.StoreSnapshot(const aValues: TOPVariantArray);
begin
  FSnapshot := Copy(aValues);
  FHasSnapshot := True;
  FMarkedDirty.Clear;
end;

function TEmployee.RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
begin
  Result := FHasSnapshot;
  if Result then
    aValues := FSnapshot;
end;

procedure TEmployee.MarkDirty(const aPropertyName: UTF8String);
begin
  FMarkedDirty.Add(aPropertyName);
end;

function TEmployee.GetMarkedDirty: TUTF8StringArray;
var
  I: Integer;
begin
  SetLength(Result, FMarkedDirty.Count);
  for I := 0 to FMarkedDirty.Count - 1 do
    Result[I] := FMarkedDirty[I];
end;

procedure TEmployee.ClearSnapshot;
begin
  SetLength(FSnapshot, 0);
  FHasSnapshot := False;
  FMarkedDirty.Clear;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;
  Tracker: TOPSelfTracker;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  Def := OPRegistry.RegisterPersister(TEmployee, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;
  ExecSQL(OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray));

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  OPManager.Logger := TOPLogger.Instance;
  TOPLogger(TOPLogger.Instance).LogKinds := [lkSQL];
  TOPLogger(TOPLogger.Instance).LogToConsole := True;

  { The tracker attaches itself to the default manager and asks every
    object for its IOPDirtyTracking interface. }
  Tracker := TOPSelfTracker.Create(nil);

  WriteLn('Database setup complete. Dirty tracking: IOPDirtyTracking INTERFACE on TEmployee.');
  WriteLn('(TOPSelfTracker attached - class stores its own snapshot.)');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  FreeAndNil(Tracker);
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  SetupTrans.Free;
  if Conn.Connected then
    Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

var
  Emp: TEmployee;
begin
  SetupDatabase;
  try
    { INSERT }
    WriteLn('--- INSERT ---');
    Emp := TEmployee.Create;
    try
      Emp.ID := 1;
      Emp.Name := 'Alice';
      Emp.Department := 'Engineering';
      Emp.Salary := 85000;
      Emp.FState := opsNew;
      OPManager.Save(Emp);
      WriteLn('Saved: ID=', Emp.ID, ' Name="', Emp.Name,
        '" Dept="', Emp.Department, '" Salary=', Emp.Salary:0:0);
    finally
      Tracker.Purge(Emp);
      Emp.Free;
    end;
    WriteLn;

    { Partial UPDATE - change only Salary }
    WriteLn('--- PARTIAL UPDATE (change Salary only) ---');
    Emp := TEmployee.Create;
    try
      Emp.ID := 1;
      OPManager.Load(Emp);
      WriteLn('Loaded:  Salary=', Emp.Salary:0:0);
      Emp.Salary := 92000;
      Emp.FState := opsUpdate;
      WriteLn('Changed: Salary=', Emp.Salary:0:0);
      WriteLn('SQL generated (observe only Salary in SET clause):');
      OPManager.Save(Emp);
    finally
      Tracker.Purge(Emp);
      Emp.Free;
    end;
    WriteLn;

    { MarkDirty - force a column into the UPDATE even though value unchanged }
    WriteLn('--- MARKDIRTY (force Department into UPDATE) ---');
    Emp := TEmployee.Create;
    try
      Emp.ID := 1;
      OPManager.Load(Emp);
      WriteLn('Loaded:  Dept="', Emp.Department, '" (not changing the value)');
      Emp.MarkDirty('Department');
      Emp.FState := opsUpdate;
      WriteLn('Called MarkDirty("Department") - forcing it into UPDATE:');
      OPManager.Save(Emp);
    finally
      Tracker.Purge(Emp);
      Emp.Free;
    end;
    WriteLn;

    { No-change save - skipped }
    WriteLn('--- NO-CHANGE SAVE (should skip UPDATE) ---');
    Emp := TEmployee.Create;
    try
      Emp.ID := 1;
      OPManager.Load(Emp);
      Emp.FState := opsUpdate;
      WriteLn('Saving without changes...');
      OPManager.Save(Emp);
      WriteLn('No SQL generated - UPDATE skipped.');
    finally
      Tracker.Purge(Emp);
      Emp.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
