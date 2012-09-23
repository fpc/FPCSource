unit PQEventMonitor;

{ PostGresql notification monitor

  Copyright (C) 2012 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}
{$Define LinkDynamically}

interface

uses
  Classes, SysUtils,pqconnection,db,dbconst,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}


type
  TEventAlert = procedure(Sender: TObject; EventName: string; EventCount: longint;
    var CancelAlerts: boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: integer) of object;

{ TPQEventMonitor }

  TPQEventMonitor=class (TComponent)
  private
    FConnection: TPQConnection;
    FDBHandle: PPGconn;
    FErrorMsg: string;
    FEvents: TStrings;
    FOnError: TErrorEvent;
    FOnEventAlert: TEventAlert;
    FRegistered: Boolean;
    function GetNativeHandle: pointer;
    procedure SetConnection(AValue: TPQConnection);
    procedure SetEvents(AValue: TStrings);
    procedure SetRegistered(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Poll;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    property ErrorMsg:string read FErrorMsg;
    property NativeHandle: pointer read GetNativeHandle;
  published
    property Connection: TPQConnection read FConnection write SetConnection;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;


implementation

ResourceString
  SErrConnectionFailed = 'Connection to database failed';
  SErrExecuteFailed = 'Execution of query failed';

{ TPQEventMonitor }

function TPQEventMonitor.GetNativeHandle: pointer;
begin
  result:=FDBHandle;
end;


procedure TPQEventMonitor.SetConnection(AValue: TPQConnection);
begin
  if FConnection=AValue then Exit;
  If not (csDesigning in ComponentState) and FRegistered then
    begin
    if assigned(FConnection) then
      FConnection.RemoveFreeNotification(self); // remove us from the old connection
    UnRegisterEvents;
    FConnection:=AValue;
    if assigned(FConnection) then
      begin
      RegisterEvents;
      end;
    end
  else
    FConnection:=AValue;
  if assigned(FConnection) then
    FConnection.FreeNotification(Self); //in case Connection is destroyed before we are
end;

procedure TPQEventMonitor.SetEvents(AValue: TStrings);
begin
  FEvents.Assign(AValue);
end;

procedure TPQEventMonitor.SetRegistered(AValue: Boolean);
begin
  if not (csDesigning in ComponentState) then
    if AValue then
      RegisterEvents
    else
      UnRegisterEvents;
end;

constructor TPQEventMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents:=TStringList.Create;
  {$IfDef LinkDynamically}
  InitialisePostgres3;             // stick to library in case connection closes before us
  {$EndIf}
end;

destructor TPQEventMonitor.Destroy;
begin
  if FRegistered then
    UnRegisterEvents;
  if assigned(FConnection) then
    FConnection.RemoveFreeNotification(self);
  FEvents.Free;
  {$IfDef LinkDynamically}
  ReleasePostgres3;
  {$EndIf}
  inherited Destroy;
end;

procedure TPQEventMonitor.Poll;
var
  notify:PpgNotify;
  CancelAlerts:boolean;
begin
  if FConnection.Connected and FRegistered and (PQconsumeInput(FDBHandle)=1) then
    begin
    CancelAlerts:=false;
    repeat
      notify:=PQnotifies(FDBHandle);
      if assigned(notify) then
        begin
        if assigned(OnEventAlert) then
          OnEventAlert(Self,notify^.relname,1,CancelAlerts);
        PQfreemem(notify);
        end;
    until not assigned(notify) or CancelAlerts;
    if CancelAlerts then
      UnRegisterEvents;
    end;
end;

procedure TPQEventMonitor.RegisterEvents;
var
  i:Integer;
  sConn: String;
  res: PPGresult;
  msg:string;
  notify:PpgNotify;
  CancelAlerts:boolean;
begin
  If not assigned(FConnection) then
    DatabaseError(SErrNoDatabaseAvailable,Self);
  if not(csDesigning in ComponentState) and not FRegistered and (Events.Count>0) then
    begin
    sConn := '';
    if (FConnection.UserName <> '') then sConn := sConn + ' user=''' + FConnection.UserName + '''';
    if (FConnection.Password <> '') then sConn := sConn + ' password=''' + FConnection.Password + '''';
    if (FConnection.HostName <> '') then sConn := sConn + ' host=''' + FConnection.HostName + '''';
    if (FConnection.DatabaseName <> '') then sConn := sConn + ' dbname=''' + FConnection.DatabaseName + '''';
    if (FConnection.Params.Text <> '') then sConn := sConn + ' '+FConnection.Params.Text;

    FDBHandle := PQconnectdb(pchar(sConn));
    if (PQstatus(FDBHandle) <> CONNECTION_OK) then
      begin
      msg := PQerrorMessage(FDBHandle);
      PQFinish(FDBHandle);
      DatabaseError(sErrConnectionFailed + ' (TPQEventMonitor: ' + Msg + ')',self);
      end;
    for i:=0 to Events.Count-1 do
      begin
      res := PQexec(FDBHandle,pchar('LISTEN '+ Events[i]));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        msg := PQerrorMessage(FDBHandle);
        PQclear(res);
        PQFinish(FDBHandle);
        FDBHandle:=nil;
        DatabaseError(SErrExecuteFailed + ' (TPQEventMonitor: ' + Msg + ')',self);
        end
      else
        PQclear(res);
      end;
    FRegistered :=true;
    end;
end;

procedure TPQEventMonitor.UnRegisterEvents;
var
  i: Integer;
  res: PPGresult;
  msg:string;
begin
  if not (csDesigning in ComponentState) and FRegistered then
    begin
    for i:=0 to Events.Count-1 do
      begin
      res := PQexec(FDBHandle,pchar('unlisten '+ Events[i]));
      if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
        begin
        msg := PQerrorMessage(FDBHandle);
        PQclear(res);
        PQFinish(FDBHandle);
        FDBHandle:=nil;
        DatabaseError(SErrExecuteFailed + ' (TPQEventMonitor: ' + Msg + ')',self);
        end
      else
        PQclear(res);
      end;
    PQFinish(FDBHandle);
    FDBHandle:=nil;
    FRegistered :=false;
    end;
end;

end.

