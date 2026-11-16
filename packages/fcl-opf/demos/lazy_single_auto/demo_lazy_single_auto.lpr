(*
  demo_lazy_single_auto - Lazy loading of N:1 single object via automatic setup
  ==============================================================================
  This demo shows how TOPLazy fields for N:1 single-object references are
  automatically wired by the Manager on FPC 3.3.1+ using extended RTTI.
  No IOPLazyInit interface is needed.

  The Manager discovers the lazy field, matches it against the relation
  registry (FindParentRelation), reads the FK value from the child, sets
  it as PK on the parent object, and calls Load.

  Requirements:
  - FPC trunk (3.3.1+)
  - {$RTTI EXPLICIT FIELDS} on the class containing the lazy field
  - Use TOPLazyObject from Opf.Proxy (has RTTI for inner fields)

  Compile:  fpc demo_lazy_single_auto.lpr -Fu../../src
  Run:      ./demo_lazy_single_auto
*)
program demo_lazy_single_auto;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,
  Opf.Proxy,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_lazy_single_auto.db';

type
  { TCountry:
    The parent entity }
    
  TCountry = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TCity: 
    Child with lazy N:1 reference to country, auto-setup by Manager }
    
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  
  
  TCity = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FCountryID: Integer;
    FCityName: String;
    FState: TObjectPersistenceState;
    FCountry: TCountry;
    FLazyCountry: TOPLazyObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetCountry: TCountry;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property CountryID: Integer read FCountryID write FCountryID;
    property CityName: String read FCityName write FCityName;
  end;

{ TCountry implementation }

function TCountry.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCountry.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TCity implementation }

function TCity.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCity.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TCity.Create;
begin
  inherited Create;
  FCountry := TCountry.Create;
  FLazyCountry.Init(FCountry);
end;

destructor TCity.Destroy;
begin
  FCountry.Free;
  inherited Destroy;
end;

function TCity.GetCountry: TCountry;
begin
  Result := TCountry(FLazyCountry.Value);
end;

{ Database helpers }

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := aSQL;
    if not Trans.Active then
      Trans.StartTransaction;
    Q.ExecSQL;
    Trans.Commit;
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
  Trans := TSQLTransaction.Create(Conn);
  Conn.Transaction := Trans;
  Conn.Open;

  ExecSQL('CREATE TABLE Country (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE City (ID INTEGER PRIMARY KEY, CountryID INTEGER, CityName TEXT)');

  OPRegistry.RegisterPersister(TCountry, TRTTIPersister);
  OPRegistry.RegisterPersister(TCity, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TCountry, TCity, 'CountryID');

  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  Trans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Main }

var
  Country: TCountry;
  City: TCity;
begin
  SetupDatabase;
  try
    WriteLn('--- INSERT ---');
    Country := TCountry.Create;
    try
      Country.ID := 1;
      Country.Name := 'Belgium';
      Country.FState := opsNew;
      OPManager.Save(Country);
      WriteLn('Saved country: "', Country.Name, '"');
    finally
      Country.Free;
    end;

    City := TCity.Create;
    try
      City.ID := 10;
      City.CountryID := 1;
      City.CityName := 'Brussels';
      City.FState := opsNew;
      OPManager.Save(City);
      WriteLn('Saved city: "', City.CityName, '" CountryID=', City.CountryID);
    finally
      City.Free;
    end;
    WriteLn;

    WriteLn('--- AUTOMATIC LAZY LOAD N:1 (no IOPLazyInit) ---');
    City := TCity.Create;
    try
      City.ID := 10;
      City.FState := opsKey;
      OPManager.Load(City);
      WriteLn('Loaded city: "', City.CityName, '" CountryID=', City.CountryID);
      WriteLn('Country loaded yet? ', City.FLazyCountry.IsLoaded);

      WriteLn('Accessing Country...');
      Country := City.GetCountry;
      WriteLn('Country: ID=', Country.ID, ' Name="', Country.Name, '"');
      WriteLn('Country loaded now? ', City.FLazyCountry.IsLoaded);
    finally
      City.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
