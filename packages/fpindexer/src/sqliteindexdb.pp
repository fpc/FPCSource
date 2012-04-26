{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    SQLite-based index database
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit SQLiteIndexDB;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpIndexer, ctypes, sqlite3;

type
  TDatabaseID = record
    ID: int64;
    Name: string;
  end;

  { TSQLiteIndexDB }

  TSQLiteIndexDB = class(TSQLIndexDB)
  private
    db: Psqlite3;
    FFileName: string;
    Frow: integer;
    FSearchClass: TFPSearch;
    LanguageID: TDatabaseID;
    QueryResult: string;
    SearchWordID: TDatabaseID;
    URLID: TDatabaseID;
    procedure CheckSQLite(Rc: cint; pzErrMsg: PChar);
  protected
    class function AllowForeignKeyInTable: boolean; override;
    function GetFieldType(FieldType: TIndexField): string; override;
    function GetLanguageID(const ALanguage: string): int64;
    function GetURLID(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64; override;
    function GetWordID(const AWord: string): int64; virtual;
    function InsertLanguage(const ALanguage: string): int64; virtual;
    function InsertURL(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64): int64;
    function InsertWord(const AWord: string): int64; virtual;
    procedure Execute(const sql: string; ignoreErrors: boolean = True); override;
  public
    destructor Destroy; override;
    procedure AddSearchData(ASearchData: TSearchWordData); override;
    procedure BeginTrans; override;
    procedure CommitTrans; override;
    procedure CompactDB; override;
    procedure Connect; override;
    procedure CreateDB; override;
    procedure DeleteWordsFromFile(URL: string); override;
    procedure FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions); override;
  published
    property FileName: string read FFileName write FFileName;
  end;

implementation

function SearchCallback(_para1: pointer; plArgc: longint; argv: PPchar; argcol: PPchar): longint; cdecl;
var
  PVal: ^PChar;
  SearchRes: TSearchWordData;
begin
  PVal := argv;
  with SearchRes do
  begin
    Position := StrToInt64(PVal^);     Inc(PVal);
    URL := PVal^;                      Inc(PVal);
    Context := PVal^;                  Inc(PVal);
    SearchWord := PVal^;               Inc(PVal);
    FileDate := ISO8601ToDate(PVal^);  Inc(PVal);
    Language := PVal^;
  end;

  with TSQLiteIndexDB(_para1) do
  begin
    FSearchClass.AddResult(FRow, SearchRes);
    Inc(Frow);
  end;
  Result := 0;
end;

function IndexCallback(_para1: pointer; plArgc: longint; argv: PPchar; argcol: PPchar): longint; cdecl;
begin
  //store the query result
  TSQLiteIndexDB(_para1).QueryResult := argv^;
  Result := 0;
end;

{ TSQLiteIndexDB }

procedure TSQLiteIndexDB.Execute(const sql: string; ignoreErrors: boolean = True);
var
  pzErrMsg: PChar;
  rc: cint;
begin
  QueryResult := '';
  //Writeln('Executing  : ',SQL);
  rc := sqlite3_exec(db, PChar(sql), @IndexCallback, self, @pzErrMsg);
  if not ignoreErrors then
    CheckSQLite(rc, pzErrMsg);
end;

function TSQLiteIndexDB.GetURLID(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean): int64;
var
  SQL: string;
begin
  if (URL = URLID.Name) then
    Result := URLID.ID
  else
  begin
    SQL := Format(GetURLSQL(False), [QuoteString(URL)]);
    Execute(SQL, False);
    Result := StrToInT64Def(QueryResult, -1);
    if (Result = -1) and DoCreate then
      Result := InsertURL(URL, ATimeStamp, ALanguageID);
    URLID.ID := Result;
    URLID.Name := URL;
  end;
end;

function TSQLiteIndexDB.GetLanguageID(const ALanguage: string): int64;
var
  SQL: string;
begin
  if (ALanguage = LanguageID.Name) then
    Result := LanguageID.ID
  else
  begin
    SQL := Format(GetLanguageSQL(False), [QuoteString(Alanguage)]);
    Execute(SQL, False);
    Result := StrToInT64Def(QueryResult, -1);
    if (Result = -1) then
      Result := InsertLanguage(ALanguage);
    LanguageID.ID := Result;
    LanguageID.Name := ALanguage;
  end;
end;

function TSQLiteIndexDB.GetWordID(const AWord: string): int64;
var
  SQL: string;
begin
  if (AWord = SearchWordID.Name) then
    Result := SearchWordID.ID
  else
  begin
    SQL := Format(GetWordSQL(False), [QuoteString(AWord)]);
    Execute(SQL, False);
    Result := StrToInT64Def(QueryResult, -1);
    if (Result = -1) then
      Result := InsertWord(AWord);
    SearchWordID.ID := Result;
    SearchWordID.Name := AWord;
  end;
end;

function TSQLiteIndexDB.InsertWord(const AWord: string): int64;
begin
  Execute(Format(InsertSQL(itWords, False), ['Null', QuoteString(AWord)]), False);
  Result := sqlite3_last_insert_rowid(db);
end;

function TSQLiteIndexDB.InsertURL(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64): int64;
begin
  // ifFilesID,ifFilesURL,ifFilesReindex,ifFilesUpdated,ifFilesTimeStamp,ifFilesLanguageID
  Execute(Format(InsertSQL(itFiles, False), ['Null', QuoteString(URL), '0', '0', QuoteString(DateToISO8601(ATimeStamp)), IntToStr(AlanguageID)]), False);
  Result := sqlite3_last_insert_rowid(db);
end;

function TSQLiteIndexDB.InsertLanguage(const ALanguage: string): int64;
begin
  Execute(Format(InsertSQL(itLanguages, False), ['Null', QuoteString(ALanguage)]), False);
  Result := sqlite3_last_insert_rowid(db);
end;

function TSQLiteIndexDB.GetFieldType(FieldType: TIndexField): string;
begin
  Result := inherited GetFieldType(FieldType);
  if (Result = PrimaryFieldType) then
    Result := 'INTEGER PRIMARY KEY NOT NULL';
end;

class function TSQLiteIndexDB.AllowForeignKeyInTable: boolean;
begin
  Result := True;
end;

procedure TSQLiteIndexDB.DeleteWordsFromFile(URL: string);
begin
  inherited DeleteWordsFromFile(URL);

  //reset the cached URL ID
  URLID.ID := -1;
  URLID.Name := '';
end;

procedure TSQLiteIndexDB.CreateDB;
begin
  Connect;
  CreateIndexerTables;
end;

procedure TSQLiteIndexDB.Connect;
var
  rc: cint;
begin
  if (Filename = '') then
    raise EFPIndexer.Create('Error: no filename specified');
  rc := sqlite3_open(PChar(FFilename), @db);
  if rc <> SQLITE_OK then
    raise EFPIndexer.CreateFmt('Cannot open database: %s', [filename]);
end;

destructor TSQLiteIndexDB.Destroy;
begin
  sqlite3_close(db);
  inherited Destroy;
end;

procedure TSQLiteIndexDB.BeginTrans;
begin
  Execute('BEGIN IMMEDIATE TRANSACTION');
end;

procedure TSQLiteIndexDB.CommitTrans;
begin
  Execute('COMMIT TRANSACTION');
end;

procedure TSQLiteIndexDB.CompactDB;
begin
  {$note this does not work, why?}
  //Execute('VACUUM');
end;

procedure TSQLiteIndexDB.AddSearchData(ASearchData: TSearchWordData);
var
  WID, LID, FID: int64;
  SQL: string;
begin
  WID := GetWordID(ASearchData.SearchWord);
  LID := GetLanguageID(ASearchData.Language);
  FID := GetURLID(ASearchData.URL, ASearchData.FileDate, LID, True);
  SQL := InsertSQL(itMatches, False);
  // ifMatchesID,ifMatchesWordId,ifMatchesFileID,ifMatchesLanguageID,ifMatchesPosition,ifMatchesContext,
  SQL := Format(SQL, ['Null', IntToStr(WID), IntToStr(FID), IntToStr(LID), IntToStr(ASearchData.Position), QuoteString(ASearchData.Context)]);
  //add to SearchWordList
  Execute(SQL, False);
  // Result:=sqlite3_last_insert_rowid(db);
end;

procedure TSQLiteIndexDB.CheckSQLite(Rc: cint; pzErrMsg: PChar);
var
  S: string;
begin
  if (rc <> SQLITE_OK) then
  begin
    if (pzErrMsg <> nil) then
      S := strPas(pzErrMsg);
    raise EFPIndexer.CreateFmt('SQLite error: %s', [S]);
  end;
end;

procedure TSQLiteIndexDB.FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions);
var
  pzErrMsg: PChar;
  rc: cint;
  sql: string;
begin
  FSearchClass := FPSearch;
  Frow := 0;

  sql := GetMatchSQL(SearchOptions, SearchWord, False);
  //sql := Format(sql, [SearchWord]);
  rc := sqlite3_exec(db, PChar(sql), @SearchCallback, self, @pzErrMsg);
  CheckSQLite(rc, pzErrMsg);
end;

end.

