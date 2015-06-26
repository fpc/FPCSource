program logsqldemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  typinfo, Classes, SysUtils, CustApp, db, sqldb,
  ibconnection, sqlite3conn, oracleconnection, mysql40conn,mysql41conn, mssqlconn,
  mysql50conn, mysql55conn, mysql56conn, odbcconn, pqconnection, strutils;


type

  { TGenSQLApplication }

  TGenSQLApplication = class(TCustomApplication)
    procedure DoSQLLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
  private
    procedure ConnectToDatabase(const AType, ADatabaseName,AUserName,APassword: String);
    procedure RunQuery(SQL: String; ParamValues: TStrings);
  protected
    FConn : TSQLConnector;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const AMsg : string); virtual;
  end;

{ TGenSQLApplication }

procedure TGenSQLApplication.DoSQLLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  Writeln(stderr,'[',EventType,'] : ',Msg);
end;

procedure TGenSQLApplication.ConnectToDatabase(const AType, ADatabaseName,
  AUserName, APassword: String);
begin
  FConn:=TSQLConnector.Create(Self);
  FConn.ConnectorType:=AType;
  FConn.DatabaseName:=ADatabaseName;
  FConn.UserName:=AUserName;
  FConn.Password:=APassword;
  FConn.Transaction:=TSQLTransaction.Create(Self);
  FConn.OnLog:=@DoSQLLog;
  FConn.LogEvents:=LogAllEventsExtra;
  FConn.Connected:=True;
end;

procedure TGenSQLApplication.RunQuery(SQL : String; ParamValues : TStrings);

Var
  S,PT,V : String;
  I : Integer;
  P : TParam;
  Q : TSQLQuery;
  F : TField;

begin
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FConn;
    Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:=SQL;
    For P in Q.Params do
      begin
      S:=ParamValues.Values[P.Name];
      PT:=ExtractWord(1,S,[':']);
      V:=ExtractWord(2,S,[':']);
      Case lowercase(PT) of
        's' : P.AsString:=V;
        'i'    : P.AsInteger:=StrToInt(V);
        'i64'  : P.AsLargeInt:=StrToInt64(V);
        'dt'   : P.AsDateTime:=StrToDateTime(V);
        'd'    : P.AsDateTime:=StrToDate(V);
        't'    : P.AsDateTime:=StrToTime(V);
        'f'    : P.AsFloat:=StrToFloat(V);
        'c'    : P.AsCurrency:=StrToCurr(V);
      else
        Raise Exception.CreateFmt('unknown parameter type for %s : %s (value: %s)',[P.Name,PT,V]);
      end
      end;
    Q.Open;
    I:=0;
    While not Q.EOF do
      begin
      Inc(I);
      Writeln('Record ',I,':');
      For F in Q.Fields do
        if F.IsNull then
          writeln(F.FieldName,'=<Null>')
        else
          writeln(F.FieldName,'=',F.AsString);
      Q.Next;
      end;
  finally
    Q.Free;
  end;
end;

procedure TGenSQLApplication.DoRun;

var
  ErrorMsg: String;
  S,T,KF : String;
  I : Integer;
  ST : TStatementType;
  P : TStrings;

begin

  // quick check parameters
  ErrorMsg:=CheckOptions('hc:d:s:u:p:P:', 'help connection-type: database: sql: user: password: param:');
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
  P:=TStringList.Create;
  try
    P.AddStrings(GetOptionValues('P','param'));
    RunQuery(S,P);
  finally
    P.Free;
  end;
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
  inherited Destroy;
end;

procedure TGenSQLApplication.WriteHelp(const AMsg: string);

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
  Writeln('Where ctype is one of : ');
  L:=TStringList.Create;
  try
    GetConnectionList(L);
    for S in L do
      Writeln('  ',lowercase(S));

  finally
    L.Free;
  end;
  Writeln('-d  --database=db       database connection name (required)');
  Writeln('-s  --sql=sql           SQL to execute (required), can contain parameters');
  Writeln('-u  --user=username     User name to connect to database');
  Writeln('-p  --password=password Password of user to connect to database with');
  Writeln('-P  --param=name=value  Parameter values encoded as ptype:value');
  Writeln('Where ptype is one of : ');
  Writeln('  s  : string');
  Writeln('  dt : datetime');
  Writeln('  d  : date');
  Writeln('  t  : time');
  Writeln('  i  : integer');
  Writeln('  i64  : int64');
  Writeln('  f  : float');
  Writeln('  c  : currency');

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

