program fbeventstest;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,sysutils,
  FBEventMonitor,ibconnection,sqldb;

const
  MAXEVENTS=35;
  NUMTESTS=100;

type

  { TMyEventAlert }

  TMyEventAlert=class
    class procedure OnFBEvent(Sender: TObject; EventName: string; EventCount: longint;
      var CancelAlerts: boolean);
  end;

var
  EvSent,EvReceived:Array [1..MAXEVENTS] of integer;
  TotalRecieved:integer;

function testNEvents(IBConnection:TIBConnection;n:integer):boolean;
var
  EventsM:TFBEventMonitor;
  i,j,k:integer;
  IBConnection2:TIBConnection;
  bExpectEvents:boolean;
begin
  IBConnection.Close;
  //Create second connection to listen on. Events are not sent to current connection.
  IBConnection2:=TIBConnection.Create(nil);
  IBConnection2.HostName:=IBConnection.HostName;
  IBConnection2.DatabaseName:=IBConnection.DatabaseName;
  IBConnection2.UserName:=IBConnection.UserName;
  IBConnection2.Password:=IBConnection.Password;
  for i:=1 to MAXEVENTS do
    begin
    EvSent[i]:=0;
    EvReceived[i]:=0;
    end;
  EventsM:=TFBEventMonitor.create(nil);
  EventsM.Connection:=IBConnection2;
  for i:=1 to n do
    EventsM.Events.Add('E'+IntToStr(i));
  EventsM.OnEventAlert:=TMyEventAlert.OnFBEvent;
  EventsM.RegisterEvents;
  i:=NUMTESTS;
  TotalRecieved:=0;
  bExpectEvents:=false;
  Randomize;
  IBConnection.Open;
  IBConnection.ExecuteDirect('RECREATE PROCEDURE send_custom(event_name varchar(127)) '+
     'AS '+
     'BEGIN '+
     'POST_EVENT event_name; '+
     'END ');
  IBConnection.Transaction.Commit;
  j:=1+random(i);  //random number of events per transaction
  while i>0 do
    begin
    k:=1+random(n);
    IBConnection.ExecuteDirect('execute PROCEDURE send_custom(''E'+IntTostr(k)+''');');
    EvSent[k]:=EvSent[k]+1;
    if i<j then
      begin
      IBConnection.Transaction.Commit;
      bExpectEvents:=true;
      j:=1+random(i);
      end;
    if bExpectEvents then
      CheckSynchronize;
    i:=i-1;
    end;
  IBConnection.Transaction.Commit;
  for i:=1 to 300 do  //3 secs max
    begin
    Sleep(10); //wait until everything received
    CheckSynchronize;
    if TotalRecieved=NUMTESTS then
      break;
    end;
  result:=true;
  for i:=1 to n do
    begin
    result:=result and (EvSent[i]=EvReceived[i]);
    end;
  EventsM.Free;
  IBConnection2.Free;
  IBConnection.Close;
end;

{ TMyEventAlert }

class procedure TMyEventAlert.OnFBEvent(Sender: TObject; EventName: string;
  EventCount: longint; var CancelAlerts: boolean);
var i:integer;
begin
  i:=StrToInt(copy(EventName,2,2));
  EvReceived[i]:=EvReceived[i]+EventCount;
  TotalRecieved:=TotalRecieved+EventCount;
end;

var
  IBConnection1:TIBConnection;
  SQLTransaction1: TSQLTransaction;
  i:integer;

begin
  if paramcount=0 then
    begin
    WriteLn('Usage:');
    WriteLn('  '+Paramstr(0) +' database [hostname] [username] [password]');
    WriteLn('    database : database name.');
    WriteLn('    hostname : default localhost');
    WriteLn('    username : default SYSDBA.');
    WriteLn('    password : default masterkey');
    exit;
    end;
  IBConnection1:=TIBConnection.Create(nil);
  SQLTransaction1:= TSQLTransaction.Create(nil);
  IBConnection1.Transaction:=SQLTransaction1;
  SQLTransaction1.DataBase:=IBConnection1;
  IBConnection1.Password:='masterkey';
  IBConnection1.UserName:='SYSDBA';
  IBConnection1.HostName:='';
  if paramcount=4 then
    IBConnection1.Password:=paramstr(4);
  if paramcount>=3 then
    IBConnection1.UserName:=paramstr(3);
  if paramcount>=2 then
    IBConnection1.HostName:=paramstr(2);
  IBConnection1.DatabaseName:=paramstr(1);
  for i:=1 to 16 do
    begin
    if testNEvents(IBConnection1,i) then
      WriteLn(inttostr(i)+' succeeded')
    else
      WriteLn(inttostr(i)+' failed. Missed '+ IntToStr(NUMTESTS-TotalRecieved)+' Events');
    end;
  SQLTransaction1.Free;
  IBConnection1.Free;
  WriteLn('Tests finished.');
end.

