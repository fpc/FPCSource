program PQEventsTest;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,sysutils,
  PQEventMonitor,pqconnection,sqldb;

const
  MAXEVENTS=35;
  NUMTESTS=100;

type

  { TMyEventAlert }

  TMyEventAlert=class
    class procedure OnPQEvent(Sender: TObject; EventName: string; EventCount: longint;
      var CancelAlerts: boolean);
  end;

var
  EvSent,EvReceived:Array [1..MAXEVENTS] of integer;
  TotalRecieved:integer;

function testNEvents(PQConnection:TPQConnection;n:integer):boolean;
var
  EventsM:TPQEventMonitor;
  i,j,k:integer;
begin
  for i:=1 to MAXEVENTS do
    begin
    EvSent[i]:=0;
    EvReceived[i]:=0;
    end;
  EventsM:=TPQEventMonitor.create(nil);
  EventsM.Connection:=PQConnection;
  for i:=1 to n do
    EventsM.Events.Add('E'+IntToStr(i));
  EventsM.OnEventAlert:=TMyEventAlert.OnPQEvent;
  EventsM.RegisterEvents;
  i:=NUMTESTS;
  TotalRecieved:=0;
  Randomize;
  while i>0 do
    begin
    k:=1+random(n);
    PQConnection.ExecuteDirect('NOTIFY E'+IntTostr(k));
    PQConnection.Transaction.Commit;
    EvSent[k]:=EvSent[k]+1;
    EventsM.Poll;
    i:=i-1;
    end;
  for i:=1 to 300 do  //3 secs max
    begin
    Sleep(10); //wait until everything received
    EventsM.Poll;
    if TotalRecieved=NUMTESTS then
      break;
    end;
  result:=true;
  for i:=1 to n do
    begin
    result:=result and (EvSent[i]=EvReceived[i]);
    end;
  EventsM.Free;
end;

{ TMyEventAlert }

class procedure TMyEventAlert.OnPQEvent(Sender: TObject; EventName: string;
  EventCount: longint; var CancelAlerts: boolean);
var i:integer;
begin
  i:=StrToInt(copy(EventName,2,2));
  EvReceived[i]:=EvReceived[i]+EventCount;
  TotalRecieved:=TotalRecieved+EventCount;
end;

var
  PQConnection1:TPQConnection;
  SQLTransaction1: TSQLTransaction;
  i:integer;

begin
  if paramcount<4 then
    begin
    WriteLn('Usage:');
    WriteLn('  '+Paramstr(0) +' database hostname username password');
    exit;
    end;
  PQConnection1:=TPQConnection.Create(nil);
  SQLTransaction1:= TSQLTransaction.Create(nil);
  PQConnection1.Transaction:=SQLTransaction1;
  SQLTransaction1.DataBase:=PQConnection1;
  PQConnection1.Password:=paramstr(4);
  PQConnection1.UserName:=paramstr(3);
  PQConnection1.HostName:=paramstr(2);
  PQConnection1.DatabaseName:=paramstr(1);
  for i:=1 to 16 do
    begin
    if testNEvents(PQConnection1,i) then
      WriteLn(inttostr(i)+' succeeded')
    else
      WriteLn(inttostr(i)+' failed. Missed '+ IntToStr(NUMTESTS-TotalRecieved)+' Events');
    end;
  SQLTransaction1.Free;
  PQConnection1.Free;
  WriteLn('Tests finished.');
end.

