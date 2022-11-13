{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST rest bridge demo applocation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program demorestbridge;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, sqldbrestbridge, fphttpapp, IBConnection, odbcconn, mysql55conn, mysql56conn, pqconnection,
  mssqlconn, oracleconnection, sqldbrestxml, sqldbrestio, sqldbrestschema, sqldbrestdata, sqldbrestjson, sqldbrestcsv, sqldbrestcds,
  sqldbrestado,  sqldbrestconst, sqldbrestauth, sqldbrestini, sqldb, sqldbrestauthini;

type
  { TXMLSQLDBRestDispatcher }

  TXMLSQLDBRestDispatcher = class(TSQLDBRestDispatcher)
    Function CreateOutputStreamer(IO: TRestIO): TRestOutputStreamer; override;
  end;

  { TRestServerDemoApplication }

  TRestServerDemoApplication = class(THTTPApplication)
  private
    procedure DoAfterRequest(Sender: TObject; aConn: TSQLConnection; aResource: TSQLDBRestResource);
  Protected
    FAuth : TRestBasicAuthenticator;
    FDisp : TSQLDBRestDispatcher;
    FRequestCount,
    FMaxRequests : integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TXMLSQLDBRestDispatcher }

function TXMLSQLDBRestDispatcher.CreateOutputStreamer(IO: TRestIO): TRestOutputStreamer;
begin
  io.Response.ContentStream:=TMemoryStream.Create;
  io.Response.FreeContentStream:=True;
  Result:=TXMLOutputStreamer.Create(IO.Response.ContentStream,Strings,Statuses, @IO.DoGetVariable);
end;

{ TRestServerDemoApplication }

procedure TRestServerDemoApplication.DoAfterRequest(Sender: TObject; aConn: TSQLConnection; aResource: TSQLDBRestResource);
begin
  inc(FRequestCount);
  if (FMaxRequests>0) and (FRequestCount>=FMaxRequests) then
    begin
    DoLog(etInfo,'Maximum requests reached');
    Terminate;
    end;
end;

procedure TRestServerDemoApplication.DoRun;
var
  UN,PWD,
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hc:s:m:p:u:w:', ['help','config:','save-config:','max-requests:','port:','user:','password:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  Port:=3000;
  if HasOption('p','port') then
    Port:=StrToIntDef(GetOptionValue('p','port'),Port);
  if HasOption('x','xml-only') then
    FDisp:=TXMLSQLDBRestDispatcher.Create(Self)
  else
    FDisp:=TSQLDBRestDispatcher.Create(Self);
  if HasOption('c', 'config') then
    FDisp.LoadFromFile(GetOptionValue('c', 'config'),[{dioSkipReadSchemas}])
  else
    begin
    // create a Default setup
    FAuth:=TRestBasicAuthenticator.Create(Self);
    // This is not the DB user !
    FAuth.DefaultUserName:='me';
    FAuth.DefaultPassword:='secret';
    FAuth.AuthenticateUserSQL.Text:='select uID from users where (uLogin=:UserName) and (uPassword=:Password)';
    FDisp.DispatchOptions:=FDisp.DispatchOptions+[rdoCustomView,rdoHandleCORS];
    UN:=GetOptionValue('u','user');
    if UN='' then
      UN:='You';
    PWD:=GetOPtionValue('w','password');
    if PWD='' then
      PWD:='Secret';
    FDisp.ExposeDatabase(TPQConnectionDef.TypeName,'localhost','expensetracker',UN,PWD,Nil,[foFilter,foInInsert,foInUpdate,foOrderByDesc]);
    With FDisp.Schemas[0].Schema.Resources do
      begin
      FindResourceByName('users').Fields.FindByFieldName('uID').GeneratorName:='seqUsersID';
      FindResourceByName('projects').Fields.FindByFieldName('pID').GeneratorName:='seqProjectsID';
      FindResourceByName('expensetypes').Fields.FindByFieldName('etID').GeneratorName:='seqExpenseTypesID';
      FindResourceByName('expenses').Fields.FindByFieldName('eID').GeneratorName:='seqExpenseID';
      end;
    FDisp.Authenticator:=Fauth;
    if HasOption('s','save-config') then
      FDisp.SaveToFile(GetOptionValue('s','save-config'));
    end;
  // Mostly for debug purposes, to get e.g. a heap trace
  if HasOption('m','max-requests') then
    FMaxRequests:=StrToIntDef(GetOptionValue('m','max-requests'),0);
  FDisp.AfterGet:=@DoAfterRequest;
  FDisp.AfterPost:=@DoAfterRequest;
  FDisp.AfterPut:=@DoAfterRequest;
  FDisp.AfterDelete:=@DoAfterRequest;
  FDisp.Active:=True;
  Inherited DoRun;
end;

constructor TRestServerDemoApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRestServerDemoApplication.Destroy;
begin
  FreeAndNil(FDisp);
  FreeAndNil(FAuth);
  inherited Destroy;
end;

procedure TRestServerDemoApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help             this message');
  Writeln('-c --config=File      Read config from .ini file');
  Writeln('-m --max-requests=N   Server at most N requests, then quit.');
  Writeln('-p --port=N           TCP/IP Port to listen on (default 3000)');
  Writeln('-s --saveconfig=File  Write config to .ini file (ignored when -c or --config is used)');
  Writeln('-u --user=USER        Database connection username');
  Writeln('-w --password=PWD     Password for database connection user');
  Writeln('-x --xml-only         Only allow XML requests)');
end;

var
  Application: TRestServerDemoApplication;

begin
  Application:=TRestServerDemoApplication.Create(nil);
  Application.Title:='SQLDB REST bridge Application';
  Application.Run;
  Application.Free;
end.

