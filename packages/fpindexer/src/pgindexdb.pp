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
unit pgindexdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpIndexer, sqldbIndexDB ,sqldb, pqconnection;

type
  { TPGIndexDB }

  TPGIndexDB = class(TSQLDBIndexDB)
  private
    FGenQuery: Array[TIndexTable] of TSQLQuery;
    Function GetGenQuery(ATable : TIndexTable) : TSQLQuery;
  protected
    procedure InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData); override;
    function InsertWord(const AWord: UTF8String): int64; override;
    function InsertURL(const URL: UTF8String; ATimeStamp: TDateTime; ALanguageID: int64): int64; override;
    function InsertLanguage(const ALanguage: UTF8String): int64; override;
    function CreateConnection: TSQLConnection; override;
    function GetID(TableType: TIndexTable): int64;
    procedure FinishCreateTable(const TableType: TIndexTable); override;
    procedure FinishDropTable(const TableType: TIndexTable); override;
  end;

implementation

{ TPGIndexDB }

function TPGIndexDB.GetID(TableType: TIndexTable): int64;

var
  Q: TSQLQuery;

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

function TPGIndexDB.InsertLanguage(const ALanguage: UTF8String): int64;
var
  Q: TSQLQuery;
begin
  Result := getID(itLanguages);
  Q := CreateCachedQuery(cqtInsertLanguage, InsertSQL(itLanguages));
  Q.ParamByName(GetFieldName(ifLanguagesID)).AsLargeInt := Result;
  Q.ParamByName(GetFieldName(ifLanguagesName)).AsString := ALanguage;
  Q.ExecSQL;
end;

function TPGIndexDB.InsertWord(const AWord: UTF8String): int64;
var
  Q: TSQLQuery;
begin
  Result := getID(itWords);
  Q := CreateCachedQuery(cqtInsertWord, InsertSQL(itWords));
  Q.ParamByName(GetFieldName(ifWordsID)).AsLargeInt := Result;
  Q.ParamByName(GetFieldName(ifWordsWord)).AsString := AWord;
  Q.ExecSQL;
end;

function TPGIndexDB.InsertURL(const URL: UTF8String; ATimeStamp: TDateTime; ALanguageID: int64): int64;
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

procedure TPGIndexDB.InsertMatch(AWordID, aFileID, aLanguageID: int64; const ASearchData: TSearchWordData);
var
  Q: TSQLQuery;
begin
  Q := CreateCachedQuery(cqtInsertMatch, InsertSQL(itMatches));
  Q.ParamByName(GetFieldName(ifMatchesID)).AsLargeInt := GetID(itMatches);
  Q.ParamByName(GetFieldName(ifMatchesLanguageID)).AsLargeInt := aLanguageID;
  Q.ParamByName(GetFieldName(ifMatchesWordID)).AsLargeInt := aWordID;
  Q.ParamByName(GetFieldName(ifMatchesFileID)).AsLargeInt := aFileID;
  Q.ParamByName(GetFieldName(ifMatchesPosition)).AsLargeInt := ASearchData.Position;
  Q.ParamByName(GetFieldName(ifMatchesContext)).AsString := Copy(ASearchData.Context,1,255);
  Q.ExecSQL;
end;

function TPGIndexDB.CreateConnection: TSQLConnection;

Var
  T : TIndexTable;

begin
  // So they are recreated
  for T in TIndexTable do
    FreeAndNil(FGenQuery[T]);
  Result := TPQConnection.Create(Self);
end;

procedure TPGIndexDB.FinishCreateTable(const TableType: TIndexTable);
begin
  Execute('CREATE SEQUENCE ' + DefaultGeneratorNames[TableType], True);
end;

procedure TPGIndexDB.FinishDropTable(const TableType: TIndexTable);
begin
  Execute('DROP SEQUENCE ' + DefaultGeneratorNames[TableType], True);
end;


function TPGIndexDB.GetGenQuery(ATable: TIndexTable): TSQLQuery;
begin
  If (FGenQuery[ATable]=Nil) then
    begin
    FGenQuery[ATable]:=CreateQuery(Format('select nextval(''%s'')', [DefaultGeneratorNames[ATable]]));
    FGenQuery[ATable].ParseSQL:=False;
    FGenQuery[ATable].Prepare;
    end;
  Result:=FGenQuery[ATable];
end;


end.

