{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    Database indexer
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
unit dbIndexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ireadertxt, db, sqldb, ibconnection, fpindexer;

Type

  { TDBIndexer }

  TDBIndexer = Class(TComponent)
  private
    FDatabase: TSQLConnection;
    FFieldInURL: Boolean;
    FIndexDB: TCustomIndexDB;
    FIndexer: TFPIndexer;
    FMFL: integer;
    FSKipTables: TStrings;
    function GetRecordCount(const TableName: string): Int64;
    Function IndexRecord(const TableName: String; Dataset: TDataset;
      KeyField: TField; List: TStrings) : int64;
    procedure SetDatabase(AValue: TSQLConnection);
    procedure SetIndexDB(AValue: TCustomIndexDB);
    procedure SetSkipTables(AValue: TStrings);
  Protected
    Procedure CreateIndexer;
    Procedure FreeIndexer;
    Procedure Log(Msg : String);
    Procedure Log(Fmt : String; Args : Array of const);
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    procedure GetFieldNames(const TableName : String; List: TStrings; out KeyField: String); virtual;
    Function IndexTable(const TableName: string) : int64; virtual;
    Property Indexer : TFPIndexer read FIndexer;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure IndexDatabase;
    Property Database : TSQLConnection Read FDatabase Write SetDatabase;
    Property IndexDB : TCustomIndexDB Read FIndexDB Write SetIndexDB;
    Property FieldInURL : Boolean Read FFieldInURL Write FFIeldInURl;
    Property MinFieldLength : integer Read FMFL Write FMFL;
    Property SkipTables : TStrings Read FSKipTables Write SetSkipTables;
  end;

  { TIBIndexer }

  TIBIndexer = Class(TDBIndexer)
    procedure GetFieldNames(const TableName : String; List: TStrings; out KeyField: String); override;
  end;

implementation

uses dateutils;

{ TIBIndexer }

procedure TIBIndexer.GetFieldNames(const TableName: String; List: TStrings;
  out KeyField: String);
Const
  SQLFieldNames =
'  SELECT'
+'       rel_field.rdb$field_name AS FIELDNAME'
{+'       rdb$field_type AS FIELDTYPE,'
+'       rdb$field_sub_type AS FIELDSUBTYPE,'
+'       rel_field.rdb$null_flag AS FIELDISNULL,'
+'       rdb$field_length AS FIELDSIZE,'
+'       rdb$field_scale AS FIELDSCALE,'
+'       rdb$character_length AS FIELDCHARLENGTH,'
+'       rdb$field_precision AS FIELDPRECISION,'
+'       field.rdb$default_source AS FIELDDEFAULT,'
+'       field.rdb$validation_source AS FIELDCHECK'}
+'     FROM'
+'       rdb$relations rel'
+'       JOIN rdb$relation_fields rel_field'
+'         ON rel_field.rdb$relation_name = rel.rdb$relation_name'
+'       JOIN rdb$fields field'
+'         ON rel_field.rdb$field_source = field.rdb$field_name'
+'     WHERE'
+'       (rel.rdb$relation_name = :TableName)'
+'       AND ((rdb$field_type in (14,37)'
+'             and (rdb$character_length >= :MinLength))'
+'            or ((rdb$field_type=261) AND(rdb$field_sub_type in (0,1))))'
+'     ORDER BY'
+'       rel_field.rdb$field_position,'
+'       rel_field.rdb$field_name';
SQLPrimaryKeyField =
'       SELECT'
+'            ixf.rdb$field_name as FIELDNAME'
{
  '            ix.rdb$relation_name AS TABLENAME,'
  '            ix.rdb$index_name AS INDEXNAME,'
  '            ix.rdb$unique_flag AS INDEXUNIQUE,'
  '            ix.rdb$index_type AS INDEX_TYPE'
}
+'          FROM'
+'            rdb$indices ix'
+'            JOIN rdb$relations rel'
+'              ON ix.rdb$relation_name = rel.rdb$relation_name'
+'            JOIN rdb$relation_constraints rel_con'
+'             on ((ix.rdb$relation_name = rel_con.rdb$relation_name)'
+'                 and (ix.rdb$index_name=rel_con.rdb$index_name))'
+'             JOIN rdb$index_segments ixf'
+'             on (ixf.rdb$index_name = ix.rdb$index_name)'
+'          WHERE'
+'              (rel.rdb$system_flag <> 1 OR rel.rdb$system_flag IS NULL)'
+'            AND'
+'              rel.rdb$relation_name = :TableName'
+'              AND rel_con.rdb$constraint_type=''PRIMARY KEY'''
+'          ORDER BY ix.rdb$relation_name, ix.rdb$index_name';

Var
  Q : TSQLQuery;

begin
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=Self.Database;
    Q.SQL.TExt:=SQLFieldNames;
    Q.ParamByName('TableName').AsString:=TableName;
    If MinFieldLength=0 then
      MinFieldLength:=2;
    Q.ParamByName('MinLength').AsInteger:=MinFieldLength;
    Q.Open;
    try
      While not Q.EOF do
        begin
        List.Add(Trim(Q.Fields[0].AsString));
        Q.Next;
        end;
    finally
      Q.Close;
    end;
    Q.SQL.TExt:=SQLPrimaryKeyField;
    Q.ParamByName('TableName').AsString:=TableName;
    Q.Open;
    try
      If not Q.EOF then
        KeyField:=Trim(Q.Fields[0].AsString);
      Q.Next;
      If not Q.EOF then
        Raise Exception.CreateFmt('Primary key of table "%s" has multiple fields',[TableName]);
    finally
      Q.Close;
    end;
  finally
    Q.Free;
  end;

end;

{ TDBIndexer }

procedure TDBIndexer.SetDatabase(AValue: TSQLConnection);
begin
  if FDatabase=AValue then exit;
  if Assigned(FDatabase) then
    FDatabase.RemoveFreeNotification(Self);
  FDatabase:=AValue;
  if Assigned(FDatabase) then
    FDatabase.FreeNotification(Self);
end;

procedure TDBIndexer.SetIndexDB(AValue: TCustomIndexDB);
begin
  if FIndexDB=AValue then exit;
  if Assigned(FIndexDB) then
    FIndexDB.RemoveFreeNotification(Self);
  FIndexDB:=AValue;
  if Assigned(FIndexDB) then
    FIndexDB.FreeNotification(Self);
end;

procedure TDBIndexer.SetSkipTables(AValue: TStrings);
begin
  if FSKipTables=AValue then exit;
  FSKipTables.Assign(AValue);
end;

procedure TDBIndexer.CreateIndexer;
begin
  FIndexer:=TFPIndexer.Create(Self);
  Findexer.FreeNotification(Self);
  Findexer.Database:=IndexDB;
  FIndexer.DetectLanguage:=False;
  FIndexer.CommitFiles:=True;
end;

procedure TDBIndexer.FreeIndexer;
begin
  Findexer.Free;
end;

procedure TDBIndexer.Log(Msg: String);
begin
  Writeln(Msg);
end;

procedure TDBIndexer.Log(Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;

procedure TDBIndexer.IndexDatabase;

Var
  TL : TStringList;
  I : Integer;
  Start : TDateTime;
  C,D : Int64;

begin
  if FMFL=0 then
    FMFL:=2;
  TL:=TStringList.Create;
  try
    Database.GetTableNames(TL);
    For I:=TL.Count-1 downto 0 do
      if FSkipTables.IndexOf(TL[i])<>-1 then
        TL.Delete(I);
    Log('Found %d tables.',[TL.Count]);
    if TL.Count=0 then
      exit;
    CreateIndexer;
    try
      For I:=0 to TL.Count-1 do
        begin
        Log('Indexing table %d/%d : %s',[I+1,TL.Count,TL[i]]);
        Start:=Now;
        C:=IndexTable(TL[i]);
        D:=SecondsBetween(Now,Start);
        if (D<>0) then
          D:=Round(C/D);
        Log('%d records. %d records/sec',[C,D]);
        end;
    finally
      FreeIndexer;
    end;
  finally
    TL.Free;
  end;
end;

procedure TDBIndexer.Notification(AComponent: TComponent; Operation: TOperation
  );

begin
  Inherited;
  if Operation=opRemove then
    if AComponent=FIndexer then
      FIndexer:=Nil
    else if AComponent=FDatabase then
      FDatabase:=Nil;
end;

procedure TDBIndexer.GetFieldNames(Const TableName : String; List : TStrings; Out KeyField : String);

begin
  Database.GetFieldNames(TableName,List);
  KeyField:=List[0];
end;

Function TDBIndexer.GetRecordCount(Const TableName : string) : Int64;

begin
  With TSQLQuery.Create(Self) do
    try
      Database:=Self.Database;
      SQL.Text:=Format('SELECT COUNT(*) AS THECOUNT FROM %s',[TableName]);
      Open;
      If not (EOF and BOF) then
        begin
        if Fields[0].DataType=ftLargeInt then
          Result:=(Fields[0] as TLargeIntField).AsLargeInt
        else
          Result:=Fields[0].AsInteger;
        end
      else
        Result:=0;
    finally
      Free;
    end;
end;

function TDBIndexer.IndexTable(Const TableName : string) : int64;

Var
  FL  : TStringList;
  KF  : String;
  SQL : String;
  Q   : TSQLQuery;
  I   : Integer;
  KFF : TField;
  RCount,TCount : Int64;
  BS : Integer;

begin
  Result:=0;
  FL:=TStringList.Create;
  try
    GetFieldNames(TableName,FL,KF);
    if FL.Count=0 then
      begin
      Log('Table "%s" has no indexable fields.',[TAbleName]);
      exit;
      end;
    if (KF='') then
      begin
      Log('Table "%s" has no key field.',[TableName]);
      exit;
      end;
    FL.Sorted:=True;
    SQL:=KF;
    For I:=0 to FL.Count-1 do
      begin
      if (FL[i]<>KF) then
        SQL:=SQL+', '+FL[i];
      end;
    SQL:='SELECT '+SQL+' FROM '+TableName;
    Log('SQL : %s',[SQL]);
    RCount:=0;
    Result:=0;
    TCount:=GetRecordCount(TableName);
    if TCount>10000 then
      BS:=1000
    else
      BS:=100;
    Q:=TSQLQuery.Create(Self);
    try
      Q.SQL.Text:=SQL;
      Q.UniDirectional:=True;
      Q.DataBase:=Self.Database;
      Q.Open;
      KFF:=Q.FieldByName(KF);
      For I:=0 to FL.Count-1 do
        FL.Objects[i]:=Q.FieldByName(FL[i]);
      While Not Q.EOF do
        begin
        Inc(RCount);
        If (RCount mod BS)=1 then
          Log('Indexing record %d of %d',[RCount,TCount]);
        IndexRecord(TableName,Q,KFF,FL);
        Inc(Result);
        Q.Next;
        end;
    finally
      Q.Free;
    end;
  finally
    FL.Free;
  end;
end;

constructor TDBIndexer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkipTables:=TStringList.Create;
  TStringList(FSkipTables).Sorted:=True;
end;

destructor TDBIndexer.Destroy;
begin
  FreeAndNil(FSkipTables);
  inherited Destroy;
end;

Function TDBIndexer.IndexRecord(Const TableName : String; Dataset : TDataset; KeyField : TField; List : TStrings) : Int64;

Var
  URL,FURL : String;
  F   : TField;
  SS  : TStringStream;
  I : Integer;
  R : TIReaderTxt;
  S,T : String;
  wc : Int64;

begin
  Result:=0;
  T:='';
  R:=TIReaderTXT.Create;
  try
    URL:=TableName+'/'+KeyField.AsString;
    For I:=0 to List.Count-1 do
      begin
      F:=TField(List.Objects[i]);
      if (F.DataType in [ftString,ftWideString,ftMemo]) and (F.Size>MinFieldLength) then
        begin
        If FieldInURL Then
          begin
          SS:=TStringStream.Create(F.AsString);
          try
            FURL:=URL+'/'+F.AsString;
            WC:=Indexer.IndexStream(FURL,Now,SS,R);
            Result:=Result+WC;
  //        Writeln(URL,' : ',F.FieldName,' (',F.Size,')');
          finally
            SS.Free;
          end;
          end
        else
          begin
          S:=Trim(F.AsString);
          if (S<>'') then
            T:=T+' '+F.AsString;
          end;
        end;
      end;
    if (not FieldInURL) and (T<>'') then
      begin
      SS:=TStringStream.Create(T);
      try
        WC:=Indexer.IndexStream(URL,Now,SS,R);
        Result:=WC;
//        Writeln(URL,' : "',T,'" : ',wc);
      finally
        SS.Free;
      end;
      end
  finally
    R.Free;
  end;
end;

end.

