{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    SQLDB-based index database
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit SQLDBIndexDB;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpIndexer, sqldb, db;

// SQLDB Specific, cache query objects
type
  TCachedQueryType = (cqtInsertWord, cqtGetWordID, cqtInsertFile, cqtGetFileID,
    cqtInsertLanguage, cqtGetLanguageID, cqtInsertMatch);

// Interbase specific
const
  {$note @MvC, TIndexTable is defined as: itWords, itLanguages, itFiles, itMatches, below order seems to be wrong}
  DefaultGeneratorNames: array[TIndexTable] of string = ('GEN_WORDS', 'GEN_MATCHES', 'GEN_LANGUAGES', 'GEN_FILES');

type

  { TSQLDBIndexDB }

  TSQLDBIndexDB = class(TSQLIndexDB)
  private
    // SQLDB specific
    db: TSQLConnection;
    FLastURLID: int64;
    FLastURL: string;
    FLastLanguageID: int64;
    FLastLanguage: string;
    FLastWordID: int64;
    FLastWord: string;
    FQueries: array [TCachedQueryType] of TSQLQuery;
  protected
    // SQLDB Specific statements
    procedure Execute(const sql: string; ignoreErrors: boolean = True); override;
    function GetLanguageID(const ALanguage: string): int64;
    function GetWordID(const AWord: string): int64;
    function GetURLID(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64; override;
    function CreateQuery(const ASQL: string): TSQLQuery;
    function CreateCachedQuery(QueryType: TCachedQueryType; const ASQL: string): TSQLQuery;
    // Connection specific, need to be overridden
    function GetConnection: TSQLConnection; virtual; abstract;
    procedure InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData); virtual; abstract;
    function InsertWord(const AWord: string): int64; virtual; abstract;
    function InsertURL(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64): int64; virtual; abstract;
    function InsertLanguage(const ALanguage: string): int64; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure CreateDB; override;
    procedure BeginTrans; override;
    procedure CommitTrans; override;
    procedure CompactDB; override;
    procedure AddSearchData(ASearchData: TSearchWordData); override;
    procedure FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions); override;
    procedure DeleteWordsFromFile(URL: string); override;
  end;

implementation

{ TSQLDBIndexDB }

function TSQLDBIndexDB.GetLanguageID(const ALanguage: string): int64;
var
  Q: TSQLQuery;
begin
  if SameFileName(FLastLanguage, ALanguage) then
    Result := FLastLanguageID
  else
  begin
    Q := CreateCachedQuery(cqtGetLanguageID, GetLanguageSQL);
    Q.ParamByName(GetFieldName(ifLanguagesName)).AsString := ALanguage;
    Q.Open;
    try
      if (Q.EOF and Q.BOF) then
        Result := InsertLanguage(ALanguage)
      else
        Result := Q.FieldByName(GetFieldName(ifLanguagesID)).AsLargeInt;
      FLastLanguageID := Result;
      FLastLanguage := ALanguage;
    finally
      Q.Close;
    end;
  end;
end;

function TSQLDBIndexDB.GetWordID(const AWord: string): int64;
var
  Q: TSQLQuery;
begin
  if (FLastWord = AWord) then
    Result := FLastWordID
  else
  begin
    Q := CreateCachedQuery(cqtGetWordID, GetWordSQL);
    Q.ParamByName(GetFieldName(ifWordsWord)).AsString := AWord;
    Q.Open;
    try
      if (Q.EOF and Q.BOF) then
        Result := InsertWord(AWord)
      else
        Result := Q.FieldByName(GetFieldName(ifWordsID)).AsLargeInt;
      FLastWordID := Result;
      FLastWord := AWord;
    finally
      Q.Close;
    end;
  end;
end;

function TSQLDBIndexDB.CreateQuery(const ASQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(Self);
  Result.Database := Self.db;
  Result.Transaction := Self.db.Transaction;
  Result.SQL.Text := ASQL;
  //Writeln('SQL  :',ASQL);
end;

function TSQLDBIndexDB.GetURLID(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64;
var
  Q: TSQLQuery;
begin
  if SameFileName(FLastURL, URL) then
    Result := FLastURLID
  else
  begin
    Q := CreateCachedQuery(cqtGetFileID, GetSearchFileSQL);
    If Length(URL)>255 then
      Writeln('URL Length : ',Length(URL),' : ',URL);
    Q.ParamByName(GetFieldName(ifFilesURL)).AsString := URL;
    Q.Open;
    try
      if (Q.EOF and Q.BOF) then
      begin
        if DoCreate then
          Result := InsertURL(URL, ATimeStamp, ALanguageID)
        else
          Result := -1;
      end
      else
        Result := Q.FieldByName(GetFieldName(ifFilesID)).AsLargeInt;
      FLastURLID := Result;
      FLastURL := URL;
    finally
      Q.Close;
    end;
  end;
end;

function TSQLDBIndexDB.CreateCachedQuery(QueryType: TCachedQueryType; const ASQL: string): TSQLQuery;
begin
  if FQueries[QueryType] = nil then
  begin
    FQueries[QueryType] := CreateQuery(ASQL);
    FQueries[QueryType].Prepare;
  end;
  Result := FQueries[QueryType];
end;

procedure TSQLDBIndexDB.AddSearchData(ASearchData: TSearchWordData);
var
  WID, FID, LID: int64;
begin
  //check if the SearchWord already is in the list
  LID := GetLanguageID(ASearchData.Language);
  FID := GetURLID(ASearchData.URL, ASearchData.FileDate, LID);
  WID := GetWordID(ASearchData.SearchWord);
  InsertMatch(WID, FID, LID, ASearchData);
end;

procedure TSQLDBIndexDB.FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions);
var
  Q: TSQLQuery;
  FN, FP, FD, FW, FC: TField;
  Res: TSearchWordData;
  S,WW : String;
  I,L : Integer;

begin
  Q := CreateQuery(GetMatchSQL(SearchOptions,SearchWord,True));
  try
    Writeln(Q.SQL.Text);
    WW := getFieldName(ifWordsWord);
    for i := 0 to SearchWord.Count - 1 do
      If SearchWord.Token[i].TokenType=wtWord then
        begin
        S:=SearchWord.Token[i].Value;
        if (Length(S)>0) and (S[1]='''') then
          Delete(S,1,1);
        L:=Length(S);
        if (l>0) and (S[l]='''') then
          Delete(S,l,1);
        if (soContains in Searchoptions) then
          S:='%'+S+'%';
        Q.ParamByName(WW+IntToStr(i)).AsString:=S;
        end;
    Q.Open;
    FN := Q.FieldByName(GetFieldName(ifFilesURL));
    FD := Q.FieldByName(GetFieldName(ifFilesTimeStamp));
    FC := Q.FieldByName(GetFieldName(ifMatchesContext));
    FP := Q.FieldByName(GetFieldName(ifMatchesPosition));
    FW := Q.FieldByName(GetFieldName(ifWordsWord));
    while not Q.EOF do
    begin
      Res.FileDate := FD.AsDateTime;
      Res.URL := FN.AsString;
      Res.SearchWord := FW.AsString;
      Res.Position := FP.AsInteger;
      Res.Context:=FC.aSString;
      FPSearch.AddResult(Q.RecNo, Res);
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

procedure TSQLDBIndexDB.DeleteWordsFromFile(URL: string);
begin
  inherited DeleteWordsFromFile(URL);
  FLastURL := '';
end;

procedure TSQLDBIndexDB.Execute(const sql: string; ignoreErrors: boolean = True);
begin
  if SQL = '' then
    exit;
  try
    DB.ExecuteDirect(sql);
  except
    if not IgnoreErrors then
      raise;
  end;
end;

procedure TSQLDBIndexDB.Connect;
begin
  if (DB = nil) then
    db := GetConnection;
  if DB.Transaction = nil then
    DB.Transaction := TSQLTransaction.Create(db);
  DB.Connected := True;
end;

procedure TSQLDBIndexDB.Disconnect;
Var
  T : TCachedQueryType;

begin
  For T:=Low(TCachedQueryType) to High(TCachedQueryType) do
    FreeAndNil(FQueries[T]);
  FreeAndNil(DB);
end;

procedure TSQLDBIndexDB.CreateDB;
begin
  if DB = nil then
    DB := GetConnection;
  DB.CreateDB;
  Connect;
  CreateIndexerTables;
end;

destructor TSQLDBIndexDB.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TSQLDBIndexDB.BeginTrans;
begin
  DB.Transaction.StartTransaction;
end;

procedure TSQLDBIndexDB.CommitTrans;
begin
  DB.Transaction.Commit;
end;

procedure TSQLDBIndexDB.CompactDB;
begin
  //not yet implemented
end;

end.

