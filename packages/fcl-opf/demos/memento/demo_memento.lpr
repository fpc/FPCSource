{ demo_memento - Change tracking with a hand-written memento
  ==========================================================
  TCustomer writes its own TOPMemento and hands it out through IOPMementoSource.
  A TOPMementoTracker takes that same route for the reference state it keeps, so
  saving a customer with one changed property writes a partial UPDATE. The
  second case takes a memento of its own, edits the customer, restores the
  memento and saves: nothing is left to write and no statement is built.
  TCustomer descends from TPersistent so the tracker can observe it and drop its
  entry when it is freed; a plain TObject descendant cannot be observed and its
  entry must be removed with Purge before the object is freed.
  A memento names the class it handles and the match is exact, so a descendant
  of TCustomer needs a CreateMemento and a memento of its own.
  The SELECT lines below are logged before their WHERE clause is appended, so a
  load prints a statement without one.

  Compile:  lazbuild demo_memento.lpi
  Run:      ./demo_memento
}
program demo_memento;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, sqldb, sqlite3conn,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.DDL,
  Opf.Persister.Rtti,
  Opf.ChangeTracking,
  Opf.Memento,
  Opf.MementoTracker,
  Opf.Logging;

const
  DB_FILE = 'demo_memento.db';

type
  { TCustomer - subject that hands out a memento of its own state }

  TCustomer = class(TPersistent, IOPPersistenceState, IOPMementoSource)
  private
    FCity: String;
    FCountry: String;
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    // Hand out a memento holding the current state; the caller owns it.
    function CreateMemento: TOPMemento;
  published
    // Primary key of the customer.
    property ID: Integer read FID write FID;
    // Name of the customer.
    property Name: String read FName write FName;
    // City the customer lives in.
    property City: String read FCity write FCity;
    // Country the customer lives in.
    property Country: String read FCountry write FCountry;
  end;

  { TCustomerMemento - hand-written memento for TCustomer }

  TCustomerMemento = class(TOPMemento)
  private
    FCity: String;
    FCountry: String;
    FName: String;
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  end;

  { TSQLWatcher - prints the SQL the logger reports and counts the statements }

  TSQLWatcher = class(TObject)
  private
    FCount: Integer;
  public
    // Print an SQL line the logger reports and count it.
    procedure LogEvent(aSender: TObject; aLevel: TLogKind; const aMsg: string);
    // Start counting from zero again.
    procedure Reset;
    // Statements counted since the last Reset.
    property Count: Integer read FCount;
  end;

function TCustomer.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TCustomer.CreateMemento: TOPMemento;

begin
  Result:=TCustomerMemento.Create(Self);
end;


{ TCustomerMemento }

{ ID is left out: the key is not part of the state an edit can change. }

function TCustomerMemento.GetSubjectClass: TClass;

begin
  Result:=TCustomer;
end;


procedure TCustomerMemento.Capture(aSubject: TObject);

var
  lCustomer: TCustomer;

begin
  lCustomer:=TCustomer(aSubject);
  FName:=lCustomer.FName;
  FCity:=lCustomer.FCity;
  FCountry:=lCustomer.FCountry;
end;


procedure TCustomerMemento.DoRestore(aSubject: TObject);

var
  lCustomer: TCustomer;

begin
  lCustomer:=TCustomer(aSubject);
  lCustomer.FName:=FName;
  lCustomer.FCity:=FCity;
  lCustomer.FCountry:=FCountry;
end;


procedure TCustomerMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

var
  lCustomer: TCustomer;

begin
  lCustomer:=TCustomer(aSubject);
  aNames.AddIf(FName<>lCustomer.FName,'Name');
  aNames.AddIf(FCity<>lCustomer.FCity,'City');
  aNames.AddIf(FCountry<>lCustomer.FCountry,'Country');
end;


{ TSQLWatcher }

{$push}
{$warn 5024 off}
procedure TSQLWatcher.LogEvent(aSender: TObject; aLevel: TLogKind; const aMsg: string);

begin
  if aLevel<>lkSQL then
    Exit;
  Inc(FCount);
  WriteLn('  ',aMsg);
end;
{$pop}


procedure TSQLWatcher.Reset;

begin
  FCount:=0;
end;


var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;
  Tracker: TOPMementoTracker;
  Watcher: TSQLWatcher;

procedure ExecSQL(const aSQL: String);

var
  lQuery: TSQLQuery;

begin
  lQuery:=TSQLQuery.Create(nil);
  try
    lQuery.Database:=Conn;
    lQuery.Transaction:=SetupTrans;
    lQuery.SQL.Text:=aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    lQuery.ExecSQL;
    SetupTrans.Commit;
  finally
    lQuery.Free;
  end;
end;


procedure SetupDatabase;

var
  lOpConn: TOPConnection;
  lCustDef: TPersisterDef;
  lKeyField: TOPFieldMap;

begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn:=TSQLite3Connection.Create(nil);
  Conn.DatabaseName:=DB_FILE;
  SetupTrans:=TSQLTransaction.Create(Conn);
  Conn.Transaction:=SetupTrans;
  Conn.Open;

  lCustDef:=OPRegistry.RegisterPersister(TCustomer,TRTTIPersister);
  { The default field map keeps the key in the SET clause as well. Without the
    next line the partial UPDATE below reads SET ID=:ID,City=:City. }
  lKeyField:=lCustDef.FieldMap.FindByPropertyName('ID');
  lKeyField.Options:=lKeyField.Options-[foInUpdate];
  OPSQLBuilder.SQLDialect:=dSQLite;
  ExecSQL(OPDDLBuilder.GetCreateSQL(lCustDef.TableName,lCustDef.FieldMap.AsArray));

  TOPManager.ClearInstance;
  lOpConn:=OPManager.Connections.Add as TOPConnection;
  lOpConn.SingleConnection:=Conn;
  lOpConn.IsDefault:=True;

  { The SQL is printed from OnLog rather than by the logger itself, so it lands
    on the same stream as the rest of the output and keeps its place in it. }
  Watcher:=TSQLWatcher.Create;
  OPManager.Logger:=TOPLogger.Instance;
  TOPLogger(TOPLogger.Instance).LogKinds:=[lkSQL];
  TOPLogger(TOPLogger.Instance).OnLog:=@Watcher.LogEvent;

  { The constructor attaches the tracker to the default manager. }
  Tracker:=TOPMementoTracker.Create(nil);

  WriteLn('Database setup complete.');
  WriteLn('  TCustomer: tracked by TOPMementoTracker through IOPMementoSource');
  WriteLn;
end;


procedure TeardownDatabase;

begin
  Tracker.Free;
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  TOPLogger(TOPLogger.Instance).OnLog:=Nil;
  Watcher.Free;
  SetupTrans.Free;
  if Conn.Connected then
    Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;


procedure ShowPendingChanges(aObject: TObject);

var
  lFields: TStringList;

begin
  lFields:=TStringList.Create;
  try
    if not OPManager.GetDirtyFields(aObject,lFields) then
      WriteLn('  GetDirtyFields: False - the object is not tracked')
    else if lFields.Count=0 then
      WriteLn('  GetDirtyFields: True, changed properties: (none)')
    else
      WriteLn('  GetDirtyFields: True, changed properties: ',lFields.CommaText);
  finally
    lFields.Free;
  end;
end;


var
  Customer: TCustomer;
  Source: IOPMementoSource;
  Undo: TOPMemento;

begin
  SetupDatabase;
  try
    WriteLn('--- INSERT the customer ---');
    WriteLn('Expected: an INSERT naming every column');
    Customer:=TCustomer.Create;
    try
      Customer.ID:=1;
      Customer.Name:='Alice';
      Customer.City:='New York';
      Customer.Country:='USA';
      Customer.FState:=opsNew;
      OPManager.Save(Customer);
    finally
      Customer.Free;
    end;
    WriteLn;

    WriteLn('--- Case 1: change one property and save ---');
    WriteLn('Expected: a partial UPDATE with only the city column in its SET clause');
    Customer:=TCustomer.Create;
    try
      Customer.ID:=1;
      OPManager.Load(Customer);
      WriteLn('  Loaded: Name="',Customer.Name,'" City="',Customer.City,'"');
      { The load handed the tracker a memento of the customer. }
      ShowPendingChanges(Customer);
      Customer.City:='Chicago';
      ShowPendingChanges(Customer);
      Customer.FState:=opsUpdate;
      Watcher.Reset;
      OPManager.Save(Customer);
      WriteLn('  Statements logged during this save: ',Watcher.Count);
    finally
      Customer.Free;
    end;
    WriteLn;

    WriteLn('--- Case 2: take a memento, edit, then cancel the edit ---');
    WriteLn('Expected: no UPDATE at all, because nothing is left to write');
    Customer:=TCustomer.Create;
    try
      Customer.ID:=1;
      OPManager.Load(Customer);
      WriteLn('  Loaded: Name="',Customer.Name,'" City="',Customer.City,'"');
      if not Supports(Customer,IOPMementoSource,Source) then
        raise Exception.Create('TCustomer hands out no memento');
      Undo:=Source.CreateMemento;
      try
        Customer.Name:='Alice Cooper';
        Customer.City:='Boston';
        ShowPendingChanges(Customer);
        { The cancel button: the state of the customer goes back to the memento. }
        Undo.RestoreTo(Customer);
        WriteLn('  After the cancel: Name="',Customer.Name,'" City="',Customer.City,'"');
        ShowPendingChanges(Customer);
      finally
        Undo.Free;
      end;
      Customer.FState:=opsUpdate;
      Watcher.Reset;
      OPManager.Save(Customer);
      WriteLn('  Statements logged during this save: ',Watcher.Count);
    finally
      Customer.Free;
    end;

    WriteLn;
    WriteLn('--- What the row holds now ---');
    WriteLn('Expected: the city case 1 wrote, and no trace of the cancelled edit');
    Customer:=TCustomer.Create;
    try
      Customer.ID:=1;
      OPManager.Load(Customer);
      WriteLn('  Stored: Name="',Customer.Name,'" City="',Customer.City,'"');
    finally
      Customer.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
    WriteLn('Notice: case 1 wrote one column; case 2 wrote nothing at all.');
  finally
    TeardownDatabase;
  end;
end.
