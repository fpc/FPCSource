{ demo_nullable - Three levels of nullable field support
  ======================================================
  This demo shows three different approaches to handling NULL database
  values in fcl-opf. Each level offers different tradeoffs:

  1. Level 0 - Stored-based: Uses Pascal's "stored" property specifier
     to track null state. Zero framework dependencies; pure RTTI.
     Use when: you have a few optional fields and want zero coupling.

  2. Level 1 - IOPFNullable interface: Uses an explicit interface to
     track null state per property. The framework provides a base class.
     Use when: you need per-property null tracking with clean API.

  3. Level 2 - TNullable<T> extended RTTI: Uses generic record fields
     with type-safe HasValue/Value semantics. The ORM discovers them
     automatically via extended RTTI.
     Use when: you want type-safe nullable fields with zero boilerplate.

  Compile:  fpc demo_nullable.lpr -Fu../../src
  Run:      ./demo_nullable
}
program demo_nullable;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, typinfo,
  { ORM units }
  Opf.Manager,         { TOPManager -- central facade }
  Opf.Registry,        { TOPRegistry -- class registration }
  Opf.Fields,          { TOPFieldMap -- field mapping }
  Opf.Sql,             { TOPSQLBuilder -- SQL dialect }
  Opf.Driver,
  Opf.Persister.Rtti,     { TRTTIPersister -- automatic RTTI-based mapping }
  Opf.Persister.ExtRTTI,  { TExtRTTIPersister -- extended RTTI for TNullable<T> }
  Opf.Nullable;            { IOPFNullable, TOPFNullableObject, TNullable types }

const
  DB_FILE = 'demo_nullable.db';

type

  { Part 1: Level 0 - Stored-based nullable
    The simplest approach. Uses Pascal's "stored" property specifier to
    indicate whether a property has a value or is NULL.

    How it works:
    - A Boolean field (e.g., FAgeStored) controls the "stored" state.
    - When FAgeStored = True, the ORM persists the Age value normally.
    - When FAgeStored = False, the ORM persists SQL NULL for Age.
    - On load, the ORM sets the stored field to True (non-null) or
      False (null) based on the database column value.

    The ORM detects stored specifiers via PPropInfo^.PropProcs bits.
    Only field-based stored specifiers (type 0) get full read/write
    support. This is pure classic RTTI - no extra units needed.

    Pros: Zero framework coupling. Standard Pascal feature.
    Cons: Requires a Boolean field per nullable property. Verbose. }

  TEmployee = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
    FAgeStored: Boolean;
    FDepartment: String;
    FDepartmentStored: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    { "stored FAgeStored" tells both the compiler and the ORM that this
      property's presence is controlled by the FAgeStored Boolean field. }
    property Age: Integer read FAge write FAge stored FAgeStored;
    property Department: String read FDepartment write FDepartment stored FDepartmentStored;
  end;

  { Part 2: Level 1 - IOPFNullable interface
    Uses the IOPFNullable interface for explicit per-property null
    tracking. The framework provides TOPFNullableObject as a convenient
    base class that implements the interface using a sorted TStringList.

    How it works:
    - Call SetNull('Phone') to mark Phone as NULL before saving.
    - Call IsNull('Phone') to check if Phone is NULL after loading.
    - ClearNull('Phone') marks it as non-null again.
    - The ORM detects IOPFNullable via GetInterfaceEntryByStr and
      automatically calls IsNull/SetNull/ClearNull during persist/apply.

    Level 1 takes precedence over Level 0. If a class implements
    IOPFNullable, the interface is checked FIRST. The stored specifier
    is only checked if IOPFNullable is not implemented.

    When a class implements IOPFNullable, ALL its published properties
    are marked with foNullable (any property can be null).

    Pros: Clean API. Any property can be nullable. No extra fields.
    Cons: Requires inheriting from TOPFNullableObject (or manual impl). }

  TPerson = class(TOPFNullableObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FEmail: String;
    FPhone: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
    property Phone: String read FPhone write FPhone;
  end;

  (* Part 3: Level 2 - TNullable<T> extended RTTI 
    Uses FPC's generic TNullable<T> record type. Each field carries its
    own HasValue/Value state - no separate Boolean fields, no interface.

    How it works:
    - Declare private fields of type TNullableDouble, TNullableString, etc.
    - The {$RTTI EXPLICIT FIELDS([vcPrivate])} directive tells the compiler
      to emit extended RTTI for private fields of the following class.
    - The ORM's second RTTI pass (via TRttiContext.GetFields) discovers
      TNullable fields by checking: tkRecord + name starts with
      "TNullable$" + has FValue and FHasValue inner fields.
    - Column naming: the ORM strips the leading "F" from the field name.
      So FNullPrice becomes column "NullPrice".

    TNullable<T> records CANNOT be published properties in FPC (records
    with management operators are not publishable). They must be private
    or public fields discovered via extended RTTI.

    Assigning a value (e.g., FNullPrice := 19.99) automatically sets
    HasValue to True. The default state is HasValue = False (null).

    Pros: Type-safe. Zero boilerplate. Self-documenting.
    Cons: Requires {$RTTI} directive. FPC trunk only. Cannot be published. 
  *)

  {$RTTI EXPLICIT FIELDS([vcPrivate])}
  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FNullPrice: TNullableDouble;
    FNullDescription: TNullableString;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    { ID and Name are regular published properties - handled by classic RTTI.
      Both classic RTTI properties and TNullable fields coexist in the same
      field map. The ORM runs two passes: classic RTTI first, then extended. }
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

{ IOPPersistenceState implementations:
  Every class used with TOPManager.Save/Load must implement this
  interface so the ORM can track object lifecycle state. }

function TEmployee.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TEmployee.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TEmployee.Create;
begin
  inherited Create;
  { Level 0: stored fields default to True (non-null).
    Set to False explicitly to mark a property as NULL. }
  FAgeStored := True;
  FDepartmentStored := True;
end;

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TProduct.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TProduct.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

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
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  OPSQLBuilder.SQLDialect := dSQLite;

  { Register all three model classes with the RTTI persister.
    The ORM automatically discovers published properties (classic RTTI)
    and TNullable record fields (extended RTTI). }
  OPRegistry.RegisterPersister(TEmployee, TRTTIPersister);
  OPRegistry.RegisterPersister(TPerson, TRTTIPersister);
  OPRegistry.RegisterPersister(TProduct, TExtRTTIPersister);

  { Create tables. Note how Level 2 columns use stripped names:
    FNullPrice -> NullPrice, FNullDescription -> NullDescription }
  ExecSQL('CREATE TABLE Employee (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER, Department TEXT)');
  ExecSQL('CREATE TABLE Person (ID INTEGER PRIMARY KEY, Name TEXT, Email TEXT, Phone TEXT)');
  ExecSQL('CREATE TABLE Product (ID INTEGER PRIMARY KEY, Name TEXT, NullPrice REAL, NullDescription TEXT)');

  { Configure ORM manager with the SQLite connection }
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  WriteLn('Database setup complete.');
  WriteLn('  Tables: Employee, Person, Product');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  { IMPORTANT: Free SetupTrans BEFORE Conn to avoid SQLite lock issues }
  if Assigned(SetupTrans) then
  begin
    SetupTrans.Free;
    SetupTrans := nil;
  end;
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    Conn.Free;
    Conn := nil;
  end;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ ===================================================================
  Part 1: Level 0 - Stored-based nullable demo
  =================================================================== }
procedure DemoPart1_StoredBased;
var
  Emp, Loaded: TEmployee;
begin
  WriteLn('=== Part 1: Level 0 - Stored-based nullable ===');
  WriteLn('Uses Pascal''s "stored" property specifier to control null state.');
  WriteLn('Each nullable property has a companion Boolean field.');
  WriteLn;

  { Step 1: Create an employee with Age set, but Department as NULL }
  WriteLn('Step 1: Save an employee (Age=30, Department=NULL)');
  Emp := TEmployee.Create;
  try
    Emp.FID := 1;
    Emp.FName := 'Alice';
    Emp.FAge := 30;
    Emp.FAgeStored := True;        { Age has a value - will be persisted as 30 }
    Emp.FDepartment := '';
    Emp.FDepartmentStored := False; { Department is NULL - will be persisted as SQL NULL }
    Emp.FState := opsNew;           { Mark as new object for first Save }
    OPManager.Save(Emp);
    WriteLn('  Saved: ID=', Emp.FID, ' Name="', Emp.FName, '"');
    WriteLn('  Age stored (non-null): ', Emp.FAgeStored, ' → value=', Emp.FAge);
    WriteLn('  Department stored (non-null): ', Emp.FDepartmentStored, ' → NULL in DB');
  finally
    Emp.Free;
  end;
  WriteLn;

  { Step 2: Load the employee back and check null state }
  WriteLn('Step 2: Load the employee back and inspect null state');
  Loaded := TEmployee.Create;
  try
    Loaded.FID := 1;
    OPManager.Load(Loaded);
    WriteLn('  Loaded: ID=', Loaded.FID, ' Name="', Loaded.FName, '"');
    WriteLn('  Age stored (non-null): ', Loaded.FAgeStored, ' → value=', Loaded.FAge);
    WriteLn('  Department stored (non-null): ', Loaded.FDepartmentStored);
    { You can also check using IsStoredProp from the typinfo unit: }
    WriteLn('  IsStoredProp(Age): ', IsStoredProp(Loaded, GetPropInfo(Loaded, 'Age')));
    WriteLn('  IsStoredProp(Department): ', IsStoredProp(Loaded, GetPropInfo(Loaded, 'Department')));
  finally
    Loaded.Free;
  end;
  WriteLn;
end;

{ ===================================================================
  Part 2: Level 1 - IOPFNullable interface demo
  =================================================================== }
procedure DemoPart2_InterfaceBased;
var
  Person, Loaded: TPerson;
begin
  WriteLn('=== Part 2: Level 1 - IOPFNullable interface ===');
  WriteLn('Uses TOPFNullableObject base class for per-property null tracking.');
  WriteLn('Call SetNull/ClearNull/IsNull to manage null state.');
  WriteLn;

  { Step 1: Create a person with Email set, but Phone as NULL }
  WriteLn('Step 1: Save a person (Email="alice@example.com", Phone=NULL)');
  Person := TPerson.Create;
  try
    Person.FID := 1;
    Person.FName := 'Alice';
    Person.FEmail := 'alice@example.com';
    Person.FPhone := '';
    { Mark Phone as NULL using the IOPFNullable interface.
      All properties default to non-null. Call SetNull explicitly. }
    Person.SetNull('Phone');
    Person.FState := opsNew;
    OPManager.Save(Person);
    WriteLn('  Saved: ID=', Person.FID, ' Name="', Person.FName, '"');
    WriteLn('  Email is null: ', Person.IsNull('Email'), ' → value="', Person.FEmail, '"');
    WriteLn('  Phone is null: ', Person.IsNull('Phone'), ' → NULL in DB');
  finally
    Person.Free;
  end;
  WriteLn;

  { Step 2: Load the person back and check null state }
  WriteLn('Step 2: Load the person back and inspect null state');
  Loaded := TPerson.Create;
  try
    Loaded.FID := 1;
    OPManager.Load(Loaded);
    WriteLn('  Loaded: ID=', Loaded.FID, ' Name="', Loaded.FName, '"');
    WriteLn('  Email is null: ', Loaded.IsNull('Email'), ' → value="', Loaded.FEmail, '"');
    WriteLn('  Phone is null: ', Loaded.IsNull('Phone'));
    { The ORM called SetNull('Phone') automatically because the DB column was NULL.
      It called ClearNull('Email') because the DB column had a value. }
  finally
    Loaded.Free;
  end;
  WriteLn;
end;

{ ===================================================================
  Part 3: Level 2 - TNullable<T> extended RTTI demo
  =================================================================== }
procedure DemoPart3_TNullable;
var
  Prod, Loaded: TProduct;
begin
  WriteLn('=== Part 3: Level 2 - TNullable<T> extended RTTI ===');
  WriteLn('Uses generic TNullable<T> record fields with HasValue/Value.');
  WriteLn('The ORM discovers these fields via extended RTTI (second pass).');
  WriteLn;

  { Step 1: Create a product with Price set, but Description as NULL }
  WriteLn('Step 1: Save a product (NullPrice=19.99, NullDescription=NULL)');
  Prod := TProduct.Create;
  try
    Prod.FID := 1;
    Prod.FName := 'Widget';
    { Assigning a value to a TNullable field automatically sets HasValue=True.
      This is handled by the TNullable record's implicit operator. }
    Prod.FNullPrice := 19.99;
    { FNullDescription is left at its default: HasValue=False (NULL).
      TNullable records auto-initialize to "no value" via their
      management operator (class operator Initialize). }
    Prod.FState := opsNew;
    OPManager.Save(Prod);
    WriteLn('  Saved: ID=', Prod.FID, ' Name="', Prod.FName, '"');
    WriteLn('  NullPrice.HasValue: ', Prod.FNullPrice.HasValue, ' → value=', Prod.FNullPrice.Value:0:2);
    WriteLn('  NullDescription.HasValue: ', Prod.FNullDescription.HasValue, ' → NULL in DB');
  finally
    Prod.Free;
  end;
  WriteLn;

  { Step 2: Load the product back and check HasValue/Value }
  WriteLn('Step 2: Load the product back and inspect HasValue/Value');
  Loaded := TProduct.Create;
  try
    Loaded.FID := 1;
    OPManager.Load(Loaded);
    WriteLn('  Loaded: ID=', Loaded.FID, ' Name="', Loaded.FName, '"');
    WriteLn('  NullPrice.HasValue: ', Loaded.FNullPrice.HasValue, ' → value=', Loaded.FNullPrice.Value:0:2);
    WriteLn('  NullDescription.HasValue: ', Loaded.FNullDescription.HasValue);
    { The ORM used the read-modify-write cycle via extended RTTI:
      1. GetValue(Instance) → get record as TValue
      2. SetValue on inner FValue/FHasValue fields
      3. SetValue(Instance, record) → write record back to object }
  finally
    Loaded.Free;
  end;
  WriteLn;
end;

{ ===================================================================
  Summary: Comparison of the three levels
  =================================================================== }
procedure PrintSummary;
begin
  WriteLn('=== Summary: Choosing the Right Nullable Level ===');
  WriteLn;
  WriteLn('  Level | Mechanism          | Null Detection        | Framework Coupling');
  WriteLn('  ------+--------------------+-----------------------+-------------------');
  WriteLn('    0   | stored specifier   | IsStoredProp / field  | None (pure Pascal)');
  WriteLn('    1   | IOPFNullable intf  | IsNull(''PropName'')    | TOPFNullableObject');
  WriteLn('    2   | TNullable<T> RTTI  | Field.HasValue        | Opf.Nullable unit');
  WriteLn;
  WriteLn('  Level | Pros                                 | Cons');
  WriteLn('  ------+--------------------------------------+-----------------------------------');
  WriteLn('    0   | Zero coupling, standard Pascal       | Boolean field per nullable prop');
  WriteLn('    1   | Clean API, any prop nullable         | Requires base class or manual impl');
  WriteLn('    2   | Type-safe, zero boilerplate          | FPC trunk only, {$RTTI} directive');
  WriteLn;
  WriteLn('  Which level should I use?');
  WriteLn('  -------------------------');
  WriteLn('  - Level 0: Best for simple cases with 1-2 optional fields.');
  WriteLn('             No framework dependency. Works with any FPC version.');
  WriteLn('  - Level 1: Best when you need many nullable properties and');
  WriteLn('             want a clean IsNull/SetNull API. Good for forms/UI.');
  WriteLn('  - Level 2: Best for new projects on FPC trunk. Type-safe,');
  WriteLn('             self-documenting, and requires the least code.');
  WriteLn;
  WriteLn('  All three levels can coexist in the same project.');
  WriteLn('  Precedence: Level 2 (checked first) > Level 1 > Level 0.');
end;

begin
  SetupDatabase;
  try
    DemoPart1_StoredBased;
    DemoPart2_InterfaceBased;
    DemoPart3_TNullable;
    PrintSummary;
    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
