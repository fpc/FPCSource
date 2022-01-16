program macrotest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, db, sqldb, ibconnection;

type

  { TTestMacroApp }

  TTestMacroApp = class(TCustomApplication)
    DB : TIBConnection;
    TR : TSQLTransaction;
    Q : TSQLQuery;
  protected
    Procedure SetupDatabase;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(aMsg : String); virtual;
  end;

{ TTestMacroApp }

procedure TTestMacroApp.SetupDatabase;

begin
  DB:=TIBConnection.Create(Self);
  TR:=TSQLTransaction.Create(Self);
  With DB do
    begin
    Hostname:='localhost';
    DatabaseName:=GetOptionValue('d','database');
    if DatabaseName='' then
      DatabaseName:='employees'; // Alias
    UserName:=GetOptionValue('u','username');
    if UserName='' then
      UserName:='SYSDBA';
    Password:=GetOptionValue('p','password');
    if Password='' then
      Password:='masterkey';
    Charset:='UTF8';
    DB.Transaction:=TR;
    end;
  Q:=TSQLQuery.Create(Self);
  Q.Database:=DB;
  Q.Transaction:=TR;
  Q.SQL.Text:='Select * from ('+sLineBreak+
      '  Select 1 as id from rdb$database'+sLineBreak+
      '  union all'+sLineBreak+
      '  Select 2 as id from rdb$database'+sLineBreak+
      '  )'+sLineBreak+
      '%WHERE_CL' +sLineBreak+
      '%ORDER_CL' +sLineBreak;
  Q.MacroCheck:=true;
  Q.MacroByName('WHERE_CL').AsString:='where 1=1';
  Q.MacroByName('ORDER_CL').AsString:='order by 1';
end;

procedure TTestMacroApp.DoRun;
var
  ErrorMsg: String;

begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions('hd:u:p:', ['help','database:','user:','password:']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    WriteHelp(ErrorMsg);
    Exit;
    end;
  SetupDatabase;
  With Q do
    begin
    WriteLn( 'Execution of SQL Statement :' + LineEnding+LineEnding+SQl.Text );
    Writeln('Initial macro values:');
    WriteLn( '%WHERE_CL = "'+MacroByName('WHERE_CL').AsString+'"');
    WriteLn( '%ORDER_CL = "'+MacroByName('ORDER_CL').AsString+'"');
    Writeln;
    Open;
    Writeln( 'First field value (expect "1") using default macro order (with default order by clause): '+Fields[0].AsString);
    Writeln;
    Close;
    MacroByName('ORDER_CL').AsString := 'Order by 1 DESC';
    WriteLn('Set new value to %ORDER_CL = "'+MacroByName('ORDER_CL').AsString+'"');
    Writeln;
    Open;
    WriteLn('First field value (expect "2") using new macro order (after new order by clause): '+Fields[0].AsString);
    Writeln;
    Close;
    end;
  // stop program loop
  Terminate;
end;

constructor TTestMacroApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestMacroApp.Destroy;
begin
  inherited Destroy;
end;

procedure TTestMacroApp.WriteHelp(aMsg : string);
begin
  if AMsg<>'' then
    Writeln('Error: ',aMsg);
  { add your help code here }
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help         this text');
  Writeln('-d --database=DB  Name of firebird database to connect to');
  Writeln('-u --user=Name    Name of user to connect with');
  Writeln('-p --password=PW  Password of user to connect with');
end;

var
  Application: TTestMacroApp;
begin
  Application:=TTestMacroApp.Create(nil);
  Application.Title:='Macro test application';
  Application.Run;
  Application.Free;
end.

