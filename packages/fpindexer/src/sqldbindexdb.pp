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

Const
  DefaultGeneratorNames: array[TIndexTable] of string = ('GEN_WORDS','GEN_LANGUAGES', 'GEN_FILES', 'GEN_MATCHES');

type

  { TSQLDBIndexDB }

  TSQLDBIndexDB = class(TSQLIndexDB)
  private
    // SQLDB specific
    FDB: TSQLConnection;
    FLastURLID: int64;
    FLastURL: string;
    FLastLanguageID: int64;
    FLastLanguage: string;
    FLastWordID: int64;
    FLastWord: string;
    FProps : Array [0..3] of UTF8String;
    FQueries: array [TCachedQueryType] of TSQLQuery;
    function GetS(AIndex: integer): UTF8String;
    procedure SetS(AIndex: integer; const AValue: UTF8String);
    Procedure EnsureDB;
  protected
    // SQLDB Specific statements
    procedure Execute(const sql: UTF8string; ignoreErrors: boolean = True); override;
    function GetLanguageID(const ALanguage: UTF8string): int64;
    function GetWordID(const AWord: UTF8String): int64;
    function GetURLID(const URL: UTF8String; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64; override;
    function CreateQuery(const ASQL: UTF8String): TSQLQuery;
    function CreateCachedQuery(QueryType: TCachedQueryType; const ASQL: UTF8String): TSQLQuery;
    // Connection specific, need to be overridden
    function CreateConnection: TSQLConnection; virtual; abstract;
    procedure InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData); virtual; abstract;
    function InsertWord(const AWord: UTF8String): int64; virtual; abstract;
    function InsertURL(const URL: UTF8String; ATimeStamp: TDateTime; ALanguageID: int64): int64; virtual; abstract;
    function InsertLanguage(const ALanguage: UTF8String): int64; virtual; abstract;
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
    function GetAvailableWords(out aList : TUTF8StringArray; aContaining : UTF8String; Partial : TAvailableMatch) : integer;override;
    procedure DeleteWordsFromFile(URL: UTF8String); override;
    Property NativeConnection : TSQLConnection Read FDB;
  published
    property DatabasePath: UTF8String Index 0 read GetS write SetS;
    property UserName: UTF8String Index 1 read GetS write SetS;
    property Password: UTF8String Index 2 read GetS write SetS;
    property HostName : UTF8String Index 3 read GetS write SetS;
  end;

implementation

{ TSQLDBIndexDB }

function TSQLDBIndexDB.GetS(AIndex: integer): UTF8String;
begin
  Result:=FProps[aIndex];
end;

procedure TSQLDBIndexDB.SetS(AIndex: integer; const AValue: UTF8String);
begin
  FProps[aIndex]:=aValue;
  if Assigned(FDB) then
    case Aindex of
      0: FDB.DatabaseName := AValue;
      1: FDB.UserName := AValue;
      2: FDB.Password := AValue;
      3: FDB.HostName := AValue;
    end;
end;

procedure TSQLDBIndexDB.EnsureDB;
begin
  if FDB=Nil then
    begin
    FDB:=CreateConnection;
    FDB.UserName:=UserName;
    FDB.Password:=Password;
    FDB.HostName:=HostName;
    FDB.DatabaseName:=DatabasePath;
    end;
  if FDB.Transaction = nil then
    FDB.Transaction := TSQLTransaction.Create(FDB);
  FDB.LogEvents:=LogAllEventsExtra;
end;


function TSQLDBIndexDB.GetLanguageID(const ALanguage: UTF8String): int64;
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

function TSQLDBIndexDB.GetWordID(const AWord: UTF8String): int64;
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

function TSQLDBIndexDB.CreateQuery(const ASQL: UTF8String): TSQLQuery;
begin
  Result := TSQLQuery.Create(Self);
  Result.Database := Self.FDB;
  Result.Transaction := Self.FDB.Transaction;
  Result.SQL.Text := ASQL;
  Result.UsePrimaryKeyAsKey:=False;
//  Result.UniDirectional:=True;
  //Writeln('SQL  :',ASQL);
end;

function TSQLDBIndexDB.GetURLID(const URL: UTF8String; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64;
var
  Q: TSQLQuery;
begin
  if SameFileName(FLastURL, URL) then
    Result := FLastURLID
  else
  begin
    Q := CreateCachedQuery(cqtGetFileID, GetSearchFileSQL);
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

function TSQLDBIndexDB.CreateCachedQuery(QueryType: TCachedQueryType; const ASQL: UTF8String): TSQLQuery;
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
  S,WW : UTF8String;
  I,L : Integer;

begin
  Q := CreateQuery(GetMatchSQL(SearchOptions,SearchWord,True));
  try
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
    I:=0;
    while not Q.EOF do
    begin
      Res.FileDate := FD.AsDateTime;
      Res.URL := FN.AsString;
      Res.SearchWord := FW.AsString;
      Res.Position := FP.AsInteger;
      Res.Context:=FC.aSString;
      Res.Rank:=0;
      FPSearch.AddResult(i, Res);
      Inc(I);
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

Function TSQLDBIndexDB.GetAvailableWords(out aList : TUTF8StringArray; aContaining: UTF8String; Partial: TAvailableMatch) : Integer;

Var
  Q : TSQLQuery;

begin
  Result:=0;
  Q := CreateQuery(AvailableWordsSQL(aContaining,Partial));
  try
    Q.PacketRecords:=-1;
    if (aContaining<>'') or (Partial<>amall) then
      With Q.ParamByName(SearchTermParam) do
        case Partial of
          amExact : AsString:=aContaining;
          amContains : AsString:='%'+aContaining+'%';
          amStartsWith  : AsString:=aContaining+'%';
        end;
    Q.Open;
    SetLength(aList,Q.RecordCount);
    Q.First;
    While not Q.EOF do
      begin
      If Length(aList)<=Result then
        SetLength(aList,Result+100);
      aList[Result]:=Q.Fields[0].AsUTF8String;
      Inc(Result);
      Q.Next;
      end;
    SetLength(aList,Result);
  finally
    Q.Free;
  end;
end;

procedure TSQLDBIndexDB.DeleteWordsFromFile(URL: UTF8String);
begin
  inherited DeleteWordsFromFile(URL);
  FLastURL := '';
end;

procedure TSQLDBIndexDB.Execute(const sql: UTF8String; ignoreErrors: boolean = True);
begin
  if SQL = '' then
    exit;
  try
    FDB.ExecuteDirect(sql);
  except
    on E : exception do
      if not IgnoreErrors then
        raise
      else
        // Writeln(E.ClassName,' : ',E.Message);
  end;
end;

procedure TSQLDBIndexDB.Connect;
begin
  EnsureDB;
  FDB.Connected := True;
end;

procedure TSQLDBIndexDB.Disconnect;
Var
  T : TCachedQueryType;

begin
  For T:=Low(TCachedQueryType) to High(TCachedQueryType) do
    FreeAndNil(FQueries[T]);
  FreeAndNil(FDB);
end;

procedure TSQLDBIndexDB.CreateDB;
begin
  EnsureDB;
  FDB.CreateDB;
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
  FDB.Transaction.StartTransaction;
end;

procedure TSQLDBIndexDB.CommitTrans;
begin
  FDB.Transaction.Commit;
end;

procedure TSQLDBIndexDB.CompactDB;
begin
  //not yet implemented
end;

end.

