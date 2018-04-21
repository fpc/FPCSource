{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report Designer Data connector for SQLDB based data.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdatasqldb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, sqldb, db, fpjson, fpreportdata;

Const
  keyConnection   = 'connection';
  keySQL          = 'sql';
  keyType         = 'dbtype';
  keyHostName     = 'host';
  keyDatabaseName = 'database';
  keyUserName     = 'user';
  keyPassword     = 'pwd';
  keyRole         = 'role';
  keyParams       = 'params';
  KeyCharSet      = 'charset';
  keyHash         = 'FPCRulez';

Resourcestring
  SErrNoConnectionData = 'No connection data available';
  SErrNoSQL = 'No SQL statement set';

Type

  { TFPReportConnector }

  TFPReportConnector = Class(TSQLConnector)
  Private
    FRefCount: Integer;
    Class procedure init;
    class procedure done;
    Class var
      FPool : TStringList;
  Public
    Procedure LoadFromConfig(aConfig : TJSONObject);
    class function CreateConnection(aConfig: TJSONObject): TFPReportConnector; virtual;
    Class Function TestConnection (aConfig : TJSONObject) : string; virtual;
    class function CreateDataset(aOwner: TComponent; aConfig: TJSONObject): TSQLQuery;
    class function CreateConfigHash(aConfig: TJSONObject): String;
    Class Procedure StartRender(ADataset : TDataset); virtual;
    Class Procedure EndRender(ADataset : TDataset); virtual;
    Class procedure CheckDBRelease;
    Property RefCount : Integer Read FRefCount;
  end;

  { TFPReportQuery }

  TFPReportQuery = class(TSQLQuery)
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;

  { TReportSQLtransaction }

  TFPReportSQLtransaction = Class(TSQLTransaction)
  private
    FStartRefCount: Integer;
  Public
    Procedure StartRender;
    Procedure EndRender;
    Property StartRefCount : Integer Read FStartRefCount;
  end;
  { TSQLDBReportDataHandler }

  TSQLDBReportDataHandler = Class(TFPReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Class Procedure StartRender(ADataset : TDataset); override;
    Class Procedure EndRender(ADataset : TDataset); override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
    Class Function AllowMasterDetail: Boolean; override;
    Class Procedure SetMasterDataset(ADetail, AMaster: TDataset); override;
  end;


implementation

{ TFPReportSQLtransaction }

procedure TFPReportSQLtransaction.StartRender;

Var
  Start : Boolean;

begin
  Start:=(FStartRefCount=0);
  Inc(FStartRefCount);
  if Start and not Active then
    StartTransaction;
end;

procedure TFPReportSQLtransaction.EndRender;
begin
  if FStartRefCount>0 then
    begin
    Dec(FStartRefCount);
    If FStartRefCount=0 then
      RollBack;
    end;
end;

{ TFPReportQuery }

constructor TFPReportQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly:=True;
end;

destructor TFPReportQuery.Destroy;

begin
  If Database is TFPReportConnector then
    Dec(TFPReportConnector(Database).FRefCount);
  inherited Destroy;
  TFPReportConnector.CheckDBRelease;
end;

{ TFPReportConnector }

class procedure TFPReportConnector.init;
begin
  FPool:=TStringList.Create;
  FPool.OwnsObjects:=True;
  FPool.Sorted:=True;
  FPool.Duplicates:=dupError;
end;

class procedure TFPReportConnector.done;
begin
  FreeAndNil(FPool);
end;

Class Function TFPReportConnector.CreateConfigHash(aConfig : TJSONObject) : String;

  Procedure AH(N,V : String);

  begin
    if (V<>'') then
      Result:=Result+';'+N+'='+V;
  end;

  Procedure AH(N : String);


  begin
    AH(N,aConfig.get(N,''));
  end;

Var
  A : TJSONArray;
  I : Integer;

begin
  AH(keyType);
  AH(keyHostName);
  AH(keyDatabaseName);
  AH(keyUserName);
  AH(keyPassword);
  AH(keyRole);
  A:=aConfig.get(keyParams,TJSONArray(Nil));
  If Assigned(A) then
    For I:=0 to A.Count-1 do
      AH(IntToStr(I),A.Strings[i]);
end;


class procedure TFPReportConnector.StartRender(ADataset: TDataset);

var
  Q : TFPReportQuery;
  T : TFPReportSQLTransaction;

begin
  if (aDataset is TFPReportQuery) then
    begin
    Q:=aDataset as TFPReportQuery;
    if Q.Transaction is TFPReportSQLTransaction then
      begin
      T:=Q.Transaction as TFPReportSQLTransaction;
      T.StartRender;
      end;
    end;
end;

class procedure TFPReportConnector.EndRender(ADataset: TDataset);

var
  Q : TFPReportQuery;
  T : TFPReportSQLTransaction;

begin
  if (aDataset is TFPReportQuery) then
    begin
    Q:=aDataset as TFPReportQuery;
    if Q.Transaction is TFPReportSQLTransaction then
      begin
      T:=Q.Transaction as TFPReportSQLTransaction;
      T.EndRender;
      end;
    end;
end;

class procedure TFPReportConnector.CheckDBRelease;

Var
  I : Integer;

begin
  For I:=FPool.Count-1 downto 0 do
    begin
    // Writeln('Connection count for ',FPool[i], ' : ',TFPReportConnector(FPool.Objects[i]).FRefCount);
    if TFPReportConnector(FPool.Objects[i]).FRefCount=0 then
      FPool.Delete(I);
    end;
end;

procedure TFPReportConnector.LoadFromConfig(aConfig: TJSONObject);

Var
  S : String;
  A : TJSONArray;
  I : Integer;

begin
  ConnectorType:=aConfig.get(keyType,'');
  HostName:=aConfig.get(keyHostName,'');
  DatabaseName:=aConfig.get(keyDatabaseName,'');
  UserName:=aConfig.get(keyUserName,'');
  S:=aConfig.get(keyPassword,'');
  if (S<>'') then
    Password:=XORDecode(keyHash,S);
  Role:=aConfig.get(keyRole,'');
  Params.Clear;
  A:=aConfig.get(keyParams,TJSONArray(Nil));
  If Assigned(A) then
    For I:=0 to A.Count-1 do
      Params.Add(A.Strings[i]);
end;

class function TFPReportConnector.CreateConnection(aConfig: TJSONObject): TFPReportConnector;

begin
  Result:=Self.Create(Nil);
  Result.LoadFromConfig(aConfig);
  Result.LogEvents:=LogAllEventsExtra;
  Result.Transaction:=TFPReportSQLtransaction.Create(Result);
end;

class function TFPReportConnector.TestConnection(aConfig: TJSONObject): string;

Var
  C : TFPReportConnector;

begin
  Result:='';
  C:=CreateConnection(aConfig);
  try
    C.Connected:=True;
  except
    On E : Exception do
      Result:=E.Message;
  end;
  C.free;
end;

class function TFPReportConnector.CreateDataset(aOwner: TComponent; aConfig: TJSONObject): TSQLQuery;

Var
  S : String;
  C : TFPReportConnector;
  I : integer;
  O : TJSONObject;

begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if O=Nil then
    Raise EDatabaseError.Create(SErrNoConnectionData);
  S:=CreateConfigHash(o);
  i:=FPool.IndexOf(S);
  if (I<>-1) then
    C:=FPool.Objects[i] as TFPReportConnector
  else
    begin
    C:=CreateConnection(o);
    FPool.AddObject(S,C);
    end;
  Result:=TFPReportQuery.Create(aOwner);
  Result.Database:=C;
  Result.SQL.Text:=aConfig.get(keySQL,'');
//  Result.UniDirectional:=True;
  Result.PacketRecords:=-1;
  Result.UsePrimaryKeyAsKey:=False;
  Inc(C.FRefCount);
end;

{ TSQLDBReportDataHandler }

function TSQLDBReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;
begin
  Result:=TFPReportConnector.CreateDataset(aOwner,aConfig);
end;

class procedure TSQLDBReportDataHandler.StartRender(ADataset: TDataset);
begin
  TFPReportConnector.StartRender(aDataset);
end;

class procedure TSQLDBReportDataHandler.EndRender(ADataset: TDataset);
begin
  TFPReportConnector.EndRender(aDataset);
end;

class function TSQLDBReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  O : TJSONObject;

begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if (O=Nil) or (O.Count=0) then
    Result:=SErrNoConnectionData
  else if Trim(aConfig.Get(keySQL,''))='' then
    Result:=SErrNoSQL
end;

class function TSQLDBReportDataHandler.DataType: String;
begin
  Result:='SQLDB';
end;

class function TSQLDBReportDataHandler.DataTypeDescription: String;
begin
  Result:='SQL Database server';
end;

class function TSQLDBReportDataHandler.AllowMasterDetail: Boolean;
begin
  Result:=True;
end;

class procedure TSQLDBReportDataHandler.SetMasterDataset(ADetail, AMaster: TDataset);

Var
  Q : TSQLQuery;
  DS : TDatasource;

begin
  Q:=(ADetail as TSQLQuery);
  DS:=Q.DataSource;
  if DS=Nil then
    begin
    DS:=TDatasource.Create(Q);
    Q.Datasource:=DS;
    end;
  DS.Dataset:=AMaster;
end;

initialization
  TSQLDBReportDataHandler.RegisterHandler;
  TFPReportConnector.Init;
Finalization
  TFPReportConnector.Done;
end.

