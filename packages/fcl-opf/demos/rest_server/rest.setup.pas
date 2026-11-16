{ rest.setup - OPF database setup and JSON helper functions for REST demo
  ========================================================================
  Sets up SQLite + OPF registration, creates schema, seeds sample data.
  Provides JSON conversion helpers used by all entity route units.
}
unit rest.setup;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, contnrs, db, sqldb, sqlite3conn, fpjson,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Driver,
  Opf.Persister.Intf, Opf.M2M, Opf.Relations,
  filmmodels;

{ Database lifecycle }
procedure SetupDatabase(const aDBFile: String);
procedure TeardownDatabase;

{ JSON helpers - entity to JSON }
function FilmToJSON(aFilm: TFilm; aIncludeRelations: Boolean = False): TJSONObject;
function DirectorToJSON(aDir: TDirector): TJSONObject;
function ActorToJSON(aAct: TActor): TJSONObject;
function GenreToJSON(aGenre: TGenre): TJSONObject;
function LoanToJSON(aLoan: TLoan): TJSONObject;
function ListToJSONArray(aList: TObjectList; aClass: TClass): TJSONArray;

{ JSON helpers - JSON to entity }
procedure JSONToFilm(aJSON: TJSONObject; aFilm: TFilm);
procedure JSONToDirector(aJSON: TJSONObject; aDir: TDirector);
procedure JSONToActor(aJSON: TJSONObject; aAct: TActor);
procedure JSONToGenre(aJSON: TJSONObject; aGenre: TGenre);
procedure JSONToLoan(aJSON: TJSONObject; aLoan: TLoan);

{ Response helpers }
function EnvelopeJSON(aData: TJSONData): String;
function ErrorJSON(const aMessage: String): String;

implementation

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;

{ --- Database setup --- }

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

function CountRows(const aTable: String): Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := 'SELECT COUNT(*) FROM ' + aTable;
    if not Trans.Active then
      Trans.StartTransaction;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    Trans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SeedSampleData;
begin
  if CountRows('Film') > 0 then
    Exit;

  WriteLn('Seeding sample data...');

  { Directors }
  ExecSQL('INSERT INTO Director (ID, Name, Country) VALUES (1, ''Christopher Nolan'', ''UK'')');
  ExecSQL('INSERT INTO Director (ID, Name, Country) VALUES (2, ''Ridley Scott'', ''UK'')');

  { Actors }
  ExecSQL('INSERT INTO Actor (ID, Name, BirthYear) VALUES (1, ''Leonardo DiCaprio'', 1974)');
  ExecSQL('INSERT INTO Actor (ID, Name, BirthYear) VALUES (2, ''Harrison Ford'', 1942)');
  ExecSQL('INSERT INTO Actor (ID, Name, BirthYear) VALUES (3, ''Cillian Murphy'', 1976)');

  { Genres }
  ExecSQL('INSERT INTO Genre (ID, Name) VALUES (1, ''Sci-Fi'')');
  ExecSQL('INSERT INTO Genre (ID, Name) VALUES (2, ''Thriller'')');
  ExecSQL('INSERT INTO Genre (ID, Name) VALUES (3, ''Drama'')');

  { Films }
  ExecSQL('INSERT INTO Film (ID, Title, Year, Rating) VALUES (1, ''Inception'', 2010, 88)');
  ExecSQL('INSERT INTO Film (ID, Title, Year, Rating) VALUES (2, ''Blade Runner'', 1982, 89)');
  ExecSQL('INSERT INTO Film (ID, Title, Year, Rating) VALUES (3, ''Oppenheimer'', 2023, 93)');

  { Film-Director associations }
  ExecSQL('INSERT INTO film_director (FilmID, DirectorID) VALUES (1, 1)');
  ExecSQL('INSERT INTO film_director (FilmID, DirectorID) VALUES (2, 2)');
  ExecSQL('INSERT INTO film_director (FilmID, DirectorID) VALUES (3, 1)');

  { Film-Actor associations }
  ExecSQL('INSERT INTO film_actor (FilmID, ActorID) VALUES (1, 1)');
  ExecSQL('INSERT INTO film_actor (FilmID, ActorID) VALUES (2, 2)');
  ExecSQL('INSERT INTO film_actor (FilmID, ActorID) VALUES (3, 3)');

  { Film-Genre associations }
  ExecSQL('INSERT INTO film_genre (FilmID, GenreID) VALUES (1, 1)');
  ExecSQL('INSERT INTO film_genre (FilmID, GenreID) VALUES (1, 2)');
  ExecSQL('INSERT INTO film_genre (FilmID, GenreID) VALUES (2, 1)');
  ExecSQL('INSERT INTO film_genre (FilmID, GenreID) VALUES (3, 3)');

  { Loans }
  ExecSQL('INSERT INTO Loan (ID, BorrowerName, BorrowDate, FilmID) VALUES (1, ''Bob'', ''2025-03-15'', 1)');

  WriteLn('Seeded 3 films, 2 directors, 3 actors, 3 genres, 1 loan.');
end;

procedure SetupDatabase(const aDBFile: String);
var
  OpConn: TOPConnection;
begin
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Conn.DatabaseName := aDBFile;
  Conn.Transaction := Trans;
  Conn.Open;

  { Register all entity classes }
  OPRegistry.RegisterPersister(TFilm, TInterfacePersister);
  OPRegistry.RegisterPersister(TDirector, TInterfacePersister);
  OPRegistry.RegisterPersister(TActor, TInterfacePersister);
  OPRegistry.RegisterPersister(TGenre, TInterfacePersister);
  OPRegistry.RegisterPersister(TLoan, TInterfacePersister);

  { M2M relationships }
  OPM2MRegistry.RegisterManyToMany(
    TFilm, TDirector, 'film_director',
    'FilmID', 'DirectorID', False,
    'DirectorList', 'FilmList');
  OPM2MRegistry.RegisterManyToMany(
    TFilm, TActor, 'film_actor',
    'FilmID', 'ActorID', False,
    'ActorList', 'FilmList');
  OPM2MRegistry.RegisterManyToMany(
    TFilm, TGenre, 'film_genre',
    'FilmID', 'GenreID', False,
    'GenreList', '');

  { 1:N relationship }
  OPRelationRegistry.RegisterRelationship(
    TFilm, TLoan, 'FilmID', False, 'LoanList');

  { Set dialect and create schema }
  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  { Create tables manually - CreateSchema has issues with TMediaFormats set type }
  ExecSQL('CREATE TABLE IF NOT EXISTS Film (ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'Title TEXT, IMDBId TEXT, Year INTEGER, Synopsis TEXT, Rating INTEGER, MediaFormat INTEGER)');
  ExecSQL('CREATE TABLE IF NOT EXISTS Director (ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'Name TEXT, Country TEXT)');
  ExecSQL('CREATE TABLE IF NOT EXISTS Actor (ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'Name TEXT, BirthYear INTEGER)');
  ExecSQL('CREATE TABLE IF NOT EXISTS Genre (ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'Name TEXT)');
  ExecSQL('CREATE TABLE IF NOT EXISTS Loan (ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'BorrowerName TEXT, BorrowDate TEXT, ReturnDate TEXT, FilmID INTEGER)');
  ExecSQL('CREATE TABLE IF NOT EXISTS film_director (FilmID INTEGER, DirectorID INTEGER)');
  ExecSQL('CREATE TABLE IF NOT EXISTS film_actor (FilmID INTEGER, ActorID INTEGER)');
  ExecSQL('CREATE TABLE IF NOT EXISTS film_genre (FilmID INTEGER, GenreID INTEGER)');

  SeedSampleData;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  FreeAndNil(Trans);
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    FreeAndNil(Conn);
  end;
end;

{ --- JSON conversion --- }

function DirectorToJSON(aDir: TDirector): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', aDir.ID);
  Result.Add('name', aDir.Name);
  if aDir.CountryStored then
    Result.Add('country', aDir.Country)
  else
    Result.Add('country', TJSONNull.Create);
end;

function ActorToJSON(aAct: TActor): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', aAct.ID);
  Result.Add('name', aAct.Name);
  if aAct.BirthYearStored then
    Result.Add('birthYear', aAct.BirthYear)
  else
    Result.Add('birthYear', TJSONNull.Create);
end;

function GenreToJSON(aGenre: TGenre): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', aGenre.ID);
  Result.Add('name', aGenre.Name);
end;

function LoanToJSON(aLoan: TLoan): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', aLoan.ID);
  Result.Add('borrowerName', aLoan.BorrowerName);
  Result.Add('borrowDate', aLoan.BorrowDate);
  if aLoan.ReturnDateStored then
    Result.Add('returnDate', aLoan.ReturnDate)
  else
    Result.Add('returnDate', TJSONNull.Create);
  Result.Add('filmId', aLoan.FilmID);
end;

function FilmToJSON(aFilm: TFilm; aIncludeRelations: Boolean): TJSONObject;
var
  I: Integer;
  Arr: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('id', aFilm.ID);
  Result.Add('title', aFilm.Title);
  Result.Add('year', aFilm.Year);
  if aFilm.IMDBIdStored then
    Result.Add('imdbId', aFilm.IMDBId)
  else
    Result.Add('imdbId', TJSONNull.Create);
  if aFilm.SynopsisStored then
    Result.Add('synopsis', aFilm.Synopsis)
  else
    Result.Add('synopsis', TJSONNull.Create);
  if aFilm.RatingStored then
    Result.Add('rating', aFilm.Rating)
  else
    Result.Add('rating', TJSONNull.Create);

  if aIncludeRelations then
  begin
    Arr := TJSONArray.Create;
    for I := 0 to aFilm.DirectorList.Count - 1 do
      Arr.Add(DirectorToJSON(TDirector(aFilm.DirectorList[I])));
    Result.Add('directors', Arr);

    Arr := TJSONArray.Create;
    for I := 0 to aFilm.ActorList.Count - 1 do
      Arr.Add(ActorToJSON(TActor(aFilm.ActorList[I])));
    Result.Add('actors', Arr);

    Arr := TJSONArray.Create;
    for I := 0 to aFilm.GenreList.Count - 1 do
      Arr.Add(GenreToJSON(TGenre(aFilm.GenreList[I])));
    Result.Add('genres', Arr);

    Arr := TJSONArray.Create;
    for I := 0 to aFilm.LoanList.Count - 1 do
      Arr.Add(LoanToJSON(TLoan(aFilm.LoanList[I])));
    Result.Add('loans', Arr);
  end;
end;

function ListToJSONArray(aList: TObjectList; aClass: TClass): TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to aList.Count - 1 do
  begin
    if aClass = TFilm then
      Result.Add(FilmToJSON(TFilm(aList[I])))
    else if aClass = TDirector then
      Result.Add(DirectorToJSON(TDirector(aList[I])))
    else if aClass = TActor then
      Result.Add(ActorToJSON(TActor(aList[I])))
    else if aClass = TGenre then
      Result.Add(GenreToJSON(TGenre(aList[I])))
    else if aClass = TLoan then
      Result.Add(LoanToJSON(TLoan(aList[I])));
  end;
end;

procedure JSONToFilm(aJSON: TJSONObject; aFilm: TFilm);
begin
  if aJSON.IndexOfName('title') >= 0 then
    aFilm.Title := aJSON.Get('title', '');
  if aJSON.IndexOfName('year') >= 0 then
    aFilm.Year := aJSON.Get('year', 0);
  if aJSON.IndexOfName('imdbId') >= 0 then
  begin
    if aJSON.Types['imdbId'] = jtNull then
      aFilm.IMDBIdStored := False
    else
    begin
      aFilm.IMDBId := aJSON.Get('imdbId', '');
      aFilm.IMDBIdStored := True;
    end;
  end;
  if aJSON.IndexOfName('synopsis') >= 0 then
  begin
    if aJSON.Types['synopsis'] = jtNull then
      aFilm.SynopsisStored := False
    else
    begin
      aFilm.Synopsis := aJSON.Get('synopsis', '');
      aFilm.SynopsisStored := True;
    end;
  end;
  if aJSON.IndexOfName('rating') >= 0 then
  begin
    if aJSON.Types['rating'] = jtNull then
      aFilm.RatingStored := False
    else
    begin
      aFilm.Rating := aJSON.Get('rating', 0);
      aFilm.RatingStored := True;
    end;
  end;
end;

procedure JSONToDirector(aJSON: TJSONObject; aDir: TDirector);
begin
  if aJSON.IndexOfName('name') >= 0 then
    aDir.Name := aJSON.Get('name', '');
  if aJSON.IndexOfName('country') >= 0 then
  begin
    if aJSON.Types['country'] = jtNull then
      aDir.CountryStored := False
    else
    begin
      aDir.Country := aJSON.Get('country', '');
      aDir.CountryStored := True;
    end;
  end;
end;

procedure JSONToActor(aJSON: TJSONObject; aAct: TActor);
begin
  if aJSON.IndexOfName('name') >= 0 then
    aAct.Name := aJSON.Get('name', '');
  if aJSON.IndexOfName('birthYear') >= 0 then
  begin
    if aJSON.Types['birthYear'] = jtNull then
      aAct.BirthYearStored := False
    else
    begin
      aAct.BirthYear := aJSON.Get('birthYear', 0);
      aAct.BirthYearStored := True;
    end;
  end;
end;

procedure JSONToGenre(aJSON: TJSONObject; aGenre: TGenre);
begin
  if aJSON.IndexOfName('name') >= 0 then
    aGenre.Name := aJSON.Get('name', '');
end;

procedure JSONToLoan(aJSON: TJSONObject; aLoan: TLoan);
begin
  if aJSON.IndexOfName('borrowerName') >= 0 then
    aLoan.BorrowerName := aJSON.Get('borrowerName', '');
  if aJSON.IndexOfName('borrowDate') >= 0 then
    aLoan.BorrowDate := aJSON.Get('borrowDate', '');
  if aJSON.IndexOfName('returnDate') >= 0 then
  begin
    if aJSON.Types['returnDate'] = jtNull then
      aLoan.ReturnDateStored := False
    else
    begin
      aLoan.ReturnDate := aJSON.Get('returnDate', '');
      aLoan.ReturnDateStored := True;
    end;
  end;
end;

function EnvelopeJSON(aData: TJSONData): String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('data', aData.Clone);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function ErrorJSON(const aMessage: String): String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('error', aMessage);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

end.
