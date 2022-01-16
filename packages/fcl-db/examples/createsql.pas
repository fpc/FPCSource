program createsql;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  typinfo, Classes, SysUtils, CustApp, db, sqldb, fpdatadict,
  fpddfb,fpddpq,fpddOracle,fpddsqlite3,fpddmysql40,fpddmysql41,fpddmysql50, fpddodbc,
  strutils;


type

  { TGenSQLApplication }

  TGenSQLApplication = class(TCustomApplication)
  private
    function CreateSQLEngine(AType: String): TFPDDSQLEngine;
    procedure ConnectToDatabase(const AType, ADatabaseName,AUserName,APassword: String);
    procedure DoConvertQuery(const S, T, KF: String; ST: TSTatementType);
  protected
    FConn : TSQLConnector;
    FDD : TFPDataDictionary;
    FENG  : TFPDDSQLEngine;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const AMsg : string); virtual;
  end;

{ TGenSQLApplication }

procedure TGenSQLApplication.ConnectToDatabase(Const AType,ADatabaseName,AUSerName,APassword : String);
begin
  FConn:=TSQLConnector.Create(Self);
  FConn.ConnectorType:=AType;
  FConn.DatabaseName:=ADatabaseName;
  FConn.UserName:=AUserName;
  FConn.Password:=APassword;
  FConn.Transaction:=TSQLTransaction.Create(Self);
  FConn.Connected:=True;
  FDD:=TFPDataDictionary.Create;
  FENG:=CreateSQLEngine(AType);
end;

Function TGenSQLApplication.CreateSQLEngine(AType : String): TFPDDSQLEngine;

begin
  Case lowercase(AType) of
    'firebird' :  Result:=TFPDDFBSQLEngine.Create;
  else
    Result:=TFPDDSQLEngine.Create;
  end;
end;

procedure TGenSQLApplication.DoConvertQuery(Const S,T,KF : String; ST : TSTatementType);

Var
  Q  : TSQLQuery;
  TD : TDDTableDef;
  Fields,KeyFields : TFPDDFieldList;
  I : Integer;
  F : TDDFieldDef;
  FN,SQL : String;

begin
  TD:=FDD.Tables.AddTable(T);
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FConn;
    Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:=S;
    Q.Open;
    TD.ImportFromDataset(Q);
  finally
    Q.Free;
  end;
  if (KF<>'') then
    begin
    KeyFields:=TFPDDFieldList.Create(False);
    For I:=1 to WordCount(KF,[',']) do
      begin
      FN:=ExtractWord(I,KF,[',']);
      F:=TD.Fields.FieldByName(FN);
      if (F=nil) then
        Writeln('Warning: Field ',FN,' does not exist.')
      else
        KeyFields.Add(F);
      end;
    end;
  Fields:=TFPDDFieldList.CreateFromTableDef(TD);
  try
    FEng.TableDef:=TD;
    Case ST of
      stDDL    : SQL:=FEng.CreateCreateSQL(KeyFields);
      stSelect : SQL:=FEng.CreateSelectSQL(Fields,KeyFields);
      stInsert : SQL:=FEng.CreateInsertSQL(Fields);
      stUpdate : SQL:=FEng.CreateUpdateSQL(Fields,KeyFields);
      stDelete : SQL:=FEng.CreateDeleteSQL(KeyFields);
    end;
    Writeln(SQL);
  finally
    KeyFields.Free;
  end;
end;
procedure TGenSQLApplication.DoRun;

var
  ErrorMsg: String;
  S,T,KF : String;
  I : Integer;
  ST : TStatementType;

begin

  // quick check parameters
  ErrorMsg:=CheckOptions('hc:d:s:t:y:k:u:p:', 'help connection-type: database: sql: table: type: keyfields: user: password:');
  if ErrorMsg<>'' then
    WriteHelp(ErrorMsg);
  if HasOption('h', 'help') then
    WriteHelp('');
  S:=GetOptionValue('c','connection-type');
  T:=GetOptionValue('d','database');
  if (S='') or (t='') then
    Writehelp('Need database and connectiontype');
  ConnectToDatabase(S,T,GetOptionValue('u','user'),GetOptionValue('p','password'));
  S:=GetOptionValue('s','sql');
  T:=GetOptionValue('t','table');
  if (t='') then
    Writehelp('Need table name');
  i:=GetEnumValue(TypeInfo(TStatementType),'st'+GetOptionValue('y','type'));
  if I=-1 then
    Writehelp(Format('Unknown statement type : %s',[GetOptionValue('y','type')]));
  ST:=TStatementType(i);
  KF:=GetOptionValue('k','keyfields');
  if (KF='') and  (st in [stselect, stupdate, stdelete]) then
    Writehelp('Need key fields for delete, select and update');
  if (S='') then
    S:='SELECT * FROM '+T+' WHERE 0=1';
  DoConvertQuery(S,T,KF,ST);
  // stop program loop
  Terminate;
end;

constructor TGenSQLApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGenSQLApplication.Destroy;
begin
  FreeAndNil(FConn);
  FreeAndNil(FDD);
  FreeAndNil(FENG);
  inherited Destroy;
end;

procedure TGenSQLApplication.WriteHelp(Const AMsg : string);

Var
  S : String;
  L : TStrings;
begin
  if AMsg<>'' then
    Writeln('Error : ',AMsg);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h  --help              this help message');
  Writeln('-c  --connection-type=ctype   Set connection type (required)' );
  Writeln('-d  --database=db       database connection name (required)');
  Writeln('-s  --sql=sql           SQL to execute (optional)');
  Writeln('-t  --table=tablename   tablename to use for statement (required)');
  Writeln('-y  --type=stype        Statement type (required) one of ddl, select, insert, update, delete)');
  Writeln('-k  --keyfields=fields  Comma-separated list of key fields (required for delete, update, optional for select,ddl)');
  Writeln('-u  --user=username     User name to connect to database');
  Writeln('-p  --password=password Password of user to connect to database with');
  Writeln('Where ctype is one of : ');
  L:=TStringList.Create;
  try
    GetConnectionList(L);
    for S in L do
      Writeln('  ',lowercase(S));

  finally
    L.Free;
  end;

  Halt(Ord(AMsg<>''));
end;

var
  Application: TGenSQLApplication;
begin
  Application:=TGenSQLApplication.Create(nil);
  Application.Title:='Generate SQL Demo';
  Application.Run;
  Application.Free;
end.

