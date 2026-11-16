unit opfdatamodule;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Sql, Opf.DDL, Opf.Driver,
  Opf.Persister.Intf, Opf.M2M, Opf.Relations,
  filmmodels;

type

  { TOPFDataModule }

  TOPFDataModule = class(TDataModule)
    OPManager1: TOPManager;
    OPRegistry1: TOPRegistry;
    OPSQLBuilder1: TOPSQLBuilder;
    OPM2MRegistry1: TOPManyToManyRegistry;
    OPRelationRegistry1: TOPRelationRegistry;
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
  public
    procedure SetupDatabase;
    procedure TeardownDatabase;
    procedure GetStatistics(out aFilms, aDirectors, aActors, aGenres, aLoans: Integer);
    property Conn: TSQLite3Connection read FConn;
    property Trans: TSQLTransaction read FTrans;
  end;

var
  OPFDataModuleInstance: TOPFDataModule;

function OPManager: TOPManager;

implementation

const
  DB_FILE = 'demo_film_library.db';

function OPManager: TOPManager;
begin
  Result := OPFDataModuleInstance.OPManager1;
end;

{$R *.lfm}

{ TOPFDataModule }

procedure TOPFDataModule.SetupDatabase;
var
  OpConn: TOPConnection;
begin
  { Create SQLite connection - opens existing DB or creates new one }
  FConn := TSQLite3Connection.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FConn.DatabaseName := DB_FILE;
  FConn.Transaction := FTrans;
  FConn.Open;

  { 1. Register all classes with TInterfacePersister }
  OPRegistry1.RegisterPersister(TFilm, TInterfacePersister);
  OPRegistry1.RegisterPersister(TDirector, TInterfacePersister);
  OPRegistry1.RegisterPersister(TActor, TInterfacePersister);
  OPRegistry1.RegisterPersister(TGenre, TInterfacePersister);
  OPRegistry1.RegisterPersister(TLoan, TInterfacePersister);

  { 2. Register M2M relationships }
  OPM2MRegistry1.RegisterManyToMany(
    TFilm, TDirector, 'film_director',
    'FilmID', 'DirectorID', False,
    'DirectorList', 'FilmList');
  OPM2MRegistry1.RegisterManyToMany(
    TFilm, TActor, 'film_actor',
    'FilmID', 'ActorID', False,
    'ActorList', 'FilmList');
  OPM2MRegistry1.RegisterManyToMany(
    TFilm, TGenre, 'film_genre',
    'FilmID', 'GenreID', False,
    'GenreList', '');

  { 3. Register 1:N relationship (Film->Loan with cascade delete) }
  OPRelationRegistry1.RegisterRelationship(
    TFilm, TLoan, 'FilmID', False, 'LoanList');

  { 4. Wire M2M and Relation registries to the manager }
  OPManager1.M2MRegistry := OPM2MRegistry1;
  OPManager1.RelationRegistry := OPRelationRegistry1;

  { 5. Set global builder dialects }
  Opf.Sql.OPSQLBuilder.SQLDialect := dSQLite;
  OPDDLBuilder.SQLDialect := dSQLite;

  { 6. Create complete schema - entity tables, foreign keys, junction tables }
  OPManager1.CreateSchema(FConn, [coIfNotExists]);

  { 7. Register query-only model (no table needed) }
  OPRegistry1.RegisterPersister(TReportRow, TInterfacePersister);

  { 8. Wire SQLite connection to design-time TOPConnection }
  OpConn := OPManager1.Connections[0] as TOPConnection;
  OpConn.SingleConnection := FConn;
end;

procedure TOPFDataModule.TeardownDatabase;
var
  OpConn: TOPConnection;
begin
  OpConn := OPManager1.Connections[0] as TOPConnection;
  OpConn.SingleConnection := nil;
  if Assigned(FTrans) then
  begin
    FTrans.Free;
    FTrans := nil;
  end;
  if Assigned(FConn) then
  begin
    if FConn.Connected then
      FConn.Close;
    FConn.Free;
    FConn := nil;
  end;
end;

procedure TOPFDataModule.GetStatistics(out aFilms, aDirectors, aActors, aGenres, aLoans: Integer);
var
  Q: TSQLQuery;

  function GetCount(const aTable: String): Integer;
  begin
    Q.SQL.Text := 'SELECT COUNT(*) FROM ' + aTable;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
  end;

begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    aFilms := GetCount('Film');
    aDirectors := GetCount('Director');
    aActors := GetCount('Actor');
    aGenres := GetCount('Genre');
    aLoans := GetCount('Loan');
  finally
    Q.Free;
  end;
end;

end.
