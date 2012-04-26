{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    SQLDB Firebird based indexer
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fbIndexdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpIndexer, sqldbIndexDB ,sqldb, ibconnection;

type
  { TFBIndexDB }

  TFBIndexDB = class(TSQLDBIndexDB)
  private
    FIB: TIBConnection;
    FGenQuery: Array[TIndexTable] of TSQLQuery;
    Function GetGenQuery(ATable : TIndexTable) : TSQLQuery;
    function GetS(AIndex: integer): string;
    procedure SetS(AIndex: integer; const AValue: string);
  protected
    procedure InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData); override;
    function InsertWord(const AWord: string): int64; override;
    function InsertURL(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64): int64; override;
    function InsertLanguage(const ALanguage: string): int64; override;
    function GetConnection: TSQLConnection; override;
    function GetID(TableType: TIndexTable): int64;
    procedure FinishCreateTable(const TableType: TIndexTable); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DatabasePath: string Index 0 read GetS write SetS;
    property UserName: string Index 1 read GetS write SetS;
    property Password: string Index 2 read GetS write SetS;
  end;

implementation

{ TFBIndexDB }

function TFBIndexDB.GetID(TableType: TIndexTable): int64;

var
  Q: TSQLQuery;
  S: string;

begin
  Q := GetGenQuery(TableType);
  Q.Open;
  try
    if (Q.EOF and Q.BOF) then
      raise Exception.CreateFmt('Could not get ID for table %s', [GetTableName(TableType)]);
    Result := Q.Fields[0].AsLargeInt;
  finally
    Q.Close;
  end;
end;

function TFBIndexDB.InsertLanguage(const ALanguage: string): int64;
var
  Q: TSQLQuery;
begin
  Result := getID(itLanguages);
  Q := CreateCachedQuery(cqtInsertLanguage, InsertSQL(itLanguages));
  Q.ParamByName(GetFieldName(ifLanguagesID)).AsLargeInt := Result;
  Q.ParamByName(GetFieldName(ifLanguagesName)).AsString := ALanguage;
  Q.ExecSQL;
end;

function TFBIndexDB.InsertWord(const AWord: string): int64;
var
  Q: TSQLQuery;
begin
  Result := getID(itWords);
  Q := CreateCachedQuery(cqtInsertWord, InsertSQL(itWords));
  Q.ParamByName(GetFieldName(ifWordsID)).AsLargeInt := Result;
  Q.ParamByName(GetFieldName(ifWordsWord)).AsString := AWord;
  Q.ExecSQL;
end;

function TFBIndexDB.InsertURL(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64): int64;
var
  Q: TSQLQuery;
begin
  Result := getID(itFiles);
  Q := CreateCachedQuery(cqtInsertFile, InsertSQL(itFiles));
  Q.ParamByName(GetFieldName(ifFilesID)).AsLargeInt := Result;
  Q.ParamByName(GetFieldName(ifFilesURL)).AsString := URL;
  Q.ParamByName(GetFieldName(ifFilesTimeStamp)).AsDateTime := ATimeStamp;
  Q.ParamByName(GetFieldName(ifFilesLanguageID)).AsLargeInt := ALanguageID;
  Q.ParamByName(GetFieldName(ifFilesUpdated)).AsInteger := 0;
  Q.ParamByName(GetFieldName(ifFilesReindex)).AsInteger := 0;
  Q.ExecSQL;
end;

procedure TFBIndexDB.InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData);
var
  Q: TSQLQuery;
begin
  Q := CreateCachedQuery(cqtInsertMatch, InsertSQL(itMatches));
  Q.ParamByName(GetFieldName(ifMatchesID)).AsLargeInt := GetID(itMatches);
  Q.ParamByName(GetFieldName(ifMatchesLanguageID)).AsLargeInt := aLanguageID;
  Q.ParamByName(GetFieldName(ifMatchesWordID)).AsLargeInt := aWordID;
  Q.ParamByName(GetFieldName(ifMatchesFileID)).AsLargeInt := aFileID;
  Q.ParamByName(GetFieldName(ifMatchesPosition)).AsLargeInt := ASearchData.Position;
  Q.ParamByName(GetFieldName(ifMatchesContext)).AsString := ASearchData.Context;
  Q.ExecSQL;
end;

function TFBIndexDB.GetConnection: TSQLConnection;
begin
  Result := FiB;
end;

procedure TFBIndexDB.FinishCreateTable(const TableType: TIndexTable);
begin
  Execute('CREATE GENERATOR ' + DefaultGeneratorNames[TableType], True);
end;

constructor TFBIndexDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIB := TIBConnection.Create(nil);
end;

destructor TFBIndexDB.Destroy;
begin
  // Parent destroys FIB.
  inherited Destroy;
end;

function TFBIndexDB.GetGenQuery(ATable: TIndexTable): TSQLQuery;
begin
  If (FGenQuery[ATable]=Nil) then
    begin
    FGenQuery[ATable]:=CreateQuery(Format('SELECT GEN_ID(%S,1) FROM RDB$DATABASE', [DefaultGeneratorNames[ATable]]));
    FGenQuery[ATable].Prepare;
    end;
  Result:=FGenQuery[ATable];
end;

function TFBIndexDB.GetS(AIndex: integer): string;
begin
  case Aindex of
    0: Result := FIB.DatabaseName;
    1: Result := FIB.UserName;
    2: Result := FIB.Password;
    3: Result := FIB.HostName;
  end;
end;

procedure TFBIndexDB.SetS(AIndex: integer; const AValue: string);
begin
  case Aindex of
    0: FIB.DatabaseName := AValue;
    1: FIB.UserName := AValue;
    2: FIB.Password := AValue;
    3: FIB.HostName := AValue;
  end;
end;

end.

