{$mode objfpc}
{$h+}
uses
  custapp, sysutils, strutils, classes, db, sqldb, bufdataset, XMLDatapacketReader,
  sqlite3conn, pqconnection, ibconnection, mssqlconn, oracleconnection,mysql55conn,mysql40conn,mysql51conn,mysql50conn;

Const
  CmdSep = [' ',#9,#10,#13,#12];

type

  { TSQLShellApplication }

  TSQLShellApplication = class(TCustomApplication)
  Private
    FConn : TSQLConnection;
    FTR : TSQLTransaction;
    FQuery : TSQLQuery;
    FConnType : String;
    FCharset : String;
    FDatabaseName: String;
    FHostName : string;
    FUserName : String;
    FPassword : String;
    FPort : INteger;
    FAutoCommit : Boolean;
    procedure ConnectToDatabase;
    procedure DisconnectFromDatabase;
    procedure ExecuteCommand(const ASQL: UTF8String);
    procedure ExecuteSystemCommand(const S : UTF8String);
    procedure MaybeCommit;
    procedure MaybeRollBack;
    function ParseArgs: Boolean;
    procedure RunCommandLoop;
    procedure SaveLast(FN: String);
    procedure Usage(const Err: String);
    procedure WriteHelp;
  Protected
    procedure DoRun; override;
    Property Conn : TSQLConnection Read FConn;
    Property AutoCommit : Boolean Read FAutoCommit;
  end;


Procedure TSQLShellApplication.ConnectToDatabase;

begin
  FConn:=TSQLConnector.Create(Self);
  TSQLConnector(FConn).ConnectorType:=FConnType;
  FTR:=TSQLTransaction.Create(Self);
  Conn.Transaction:=FTR;
  Conn.DatabaseName:=FDatabaseName;
  Conn.HostName:=FHostName;
  Conn.UserName:=FUserName;
  Conn.Password:=FPassword;
  Conn.Connected:=True;
  if FCharset<>'' then
    Conn.CharSet:=FCharset;
end;


Procedure TSQLShellApplication.DisconnectFromDatabase;

begin
  FreeAndNil(FTr);
  FreeAndNil(FConn);
end;

Procedure TSQLShellApplication.ExecuteCommand(Const ASQL : UTF8String);

Var
  Q : TSQLQuery;
  F : TField;
  
begin
  FreeAndNil(FQuery);
  Q:=TSQLQuery.Create(Conn);
  Q.Database:=Conn;
  Q.Transaction:=FTr;
  if not FTR.Active then
    FTR.StartTransaction;
  Q.SQL.Text:=aSQL;
  Q.Prepare;
  if Q.StatementType<>stSelect then
    begin
    Q.ExecSQL;
    Writeln('Rows affected : ',Q.RowsAffected);
    if AutoCommit then
      (Q.Transaction as TSQLTransaction).Commit;
    Q.Free;
    end
  else
    begin
    Q.Open;
    Write('|');
    For F in Q.Fields do
      Write(' ',F.FieldName,' |');
    Writeln;
    While not Q.EOF do
      begin
      Write('|');
      For F in Q.Fields do
        Write(F.AsString,' |');
      Writeln;
      Q.Next;
      end;
    FQuery:=Q;
    end;
end;

Procedure TSQLShellApplication.SaveLast(FN : String);

begin
  FN:=Trim(FN);
  if FN='' then
    begin
    Write('Type filename to save data: ');
    Readln(fn);
    end;
  if (FN<>'') then
    FQuery.SaveToFile(FN,dfXML);
end;

Procedure TSQLShellApplication.MaybeCommit;
begin
  if FTR.Active then
    FTR.Commit;
end;

Procedure TSQLShellApplication.MaybeRollBack;
begin
  if FTR.Active then
    FTR.Commit;
end;

Procedure TSQLShellApplication.ExecuteSystemCommand(Const S : UTF8String);

Var
  Cmd,Args : String;

begin
  Cmd:=ExtractWord(1,S,CmdSep);
  Args:=S;
  Delete(Args,1,Length(Cmd)+Pos(Cmd,Args)-1);
  While (Length(Args)>0) and (Args[1] in CmdSep) do
    Delete(Args,1,1);
  case Cmd of
   'a','autocommit' :
      FAutoCommit:=Not FAutoCommit;
   'q','quit' :
      begin
      MaybeCommit;
      Terminate;
      end;
   'x','exit' :
      begin
      MaybeRollBack;
      Terminate;
      end;
   'c','commit' :
      MaybeCommit;
   'r','collback':
      MaybeRollBack;
   's',
   'save' : SaveLast(Args);
   '?','h','help' : WriteHelp;
  end;
end;

Procedure TSQLShellApplication.WriteHelp;

begin
  Writeln('Commands : ');
  Writeln('\a \autocommit  Toggle autocommit (Current autocommit :',FAutoCommit,')');
  Writeln('\c \commit      commit');
  Writeln('\h \help        this help');
  Writeln('\q \quit        commit and quit');
  Writeln('\r \rollback    commit');
  Writeln('\x \exit        RollBack and quit');
  Writeln('\s \save [FN]   Save result of last select to XML file');
end;

Procedure TSQLShellApplication.RunCommandLoop;

Var
  S : UTF8String;

begin
  Writeln('Enter commands, end with \q. \?, \h or \help for help.');
  Repeat
    Write('SQL > ');
    Readln(S);
    try
      While (Length(S)>0) and (S[1] in CmdSep) do
        Delete(S,1,1);
      if Copy(S,1,1)='\' then
        begin
        Delete(S,1,1);
        ExecuteSystemCommand(S)
        end
      else
        ExecuteCommand(S)
    except
      On E : Exception do
        Writeln(Format('Error %s executing command : %s',[E.ClassName,E.Message]));
    end;
  until Terminated;
  Terminate;
end;

Procedure  TSQLShellApplication.Usage(Const Err : String);

Var
  L : TStrings;
  S : String;

begin
  if (Err<>'') then
    Writeln('Error : ',Err);
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help           This help text.');
  Writeln('-t --type=TYPE      Set connection type.');
  Writeln('-d --database=DB    Set database name.');
  Writeln('-H --hostname=DB    Set database hostname.');
  Writeln('-u --username=NAME  Set database user name.');
  Writeln('-p --password=PWD   Set database user password.');
  Writeln('-c --charset=SET    Set database character set.');
  Writeln('-P --port=N         Set database connection port.');
  Writeln('Known connection types for this binary:');
  L:=TStringList.Create;
  try
    GetConnectionList(L);
    for S in L do
      Writeln('  ',S);
  finally
    L.Free;
  end;
end;

Function TSQLShellApplication.ParseArgs : Boolean;

Var
  S : String;

begin
  Result:=False;
  S:=CheckOptions('hH:d:t:u:p:c:P:',['help','hostname:','database:','type:','username:','password:','c:charset','port']);
  if (S<>'') or (HasOption('h','help')) then
    begin
    Usage(S);
    exit;
    end;
  FConnType:=GetOptionValue('t','type');
  FHostName:=GetOptionValue('H','hostname');
  FDatabaseName:=GetOptionValue('d','database');
  FUserName:=GetOptionValue('u','user');
  FPassword:=GetOptionValue('p','password');
  FCharset:=GetOptionValue('c','charset');
  if HasOption('P','port') then
    begin
    FPort:=StrToIntDef(GetOptionValue('P','port'),-1);
    if FPort=-1 then
      Usage('Databasename not supplied');
    exit;
    end;
  Result:=(FDatabaseName<>'');
  if not Result then
    Usage('Databasename not supplied');
end;

Procedure TSQLShellApplication.DoRun;

begin
  StopOnException:=True;
  if Not ParseArgs then
    begin
    terminate;
    exit;
    end;
  ConnectToDatabase;
  RunCommandLoop;
  DisconnectFromDatabase;
end;

begin
  With TSQLShellApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.


