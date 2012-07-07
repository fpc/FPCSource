unit FBEventMonitor;

{ Interbase/Firebird Event monitor

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
  Classes, SysUtils,
{$IfDef LinkDynamically}
  ibase60dyn,
{$Else}
  ibase60,
{$EndIf}
  IBConnection,syncobjs,db,dbconst;

type
  TEventAlert = procedure(Sender: TObject; EventName: string; EventCount: longint;
    var CancelAlerts: boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: integer) of object;

  { TFBEventMonitor }

  TFBEventMonitor=class (TComponent)
  private
    FConnection: TIBConnection;
    FErrorMsg: string;
    FEvents: TStrings;
    FEventsThread: TThread;
    FOnError: TErrorEvent;
    FOnEventAlert: TEventAlert;
    FRegistered: Boolean;
    function GetNativeHandle: PISC_DB_HANDLE;
    procedure SetConnection(AValue: TIBConnection);
    procedure SetEvents(AValue: TStrings);
    procedure SetRegistered(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    property ErrorMsg:string read FErrorMsg;
    property NativeHandle: PISC_DB_HANDLE read GetNativeHandle;
  published
    property Connection: TIBConnection read FConnection write SetConnection;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation
const
  MAXEVENTSPEREPB=15;      //isc_event_block limitated to 15 events.
type
  TEPBpair=record
    EventBuf:PChar;        //isc_event_block event block
    ResultBuf:PChar;       //isc_event_block result block
    Signal:pointer;        //pointer to TFBEventsThread.FSignal
    Signaled:Boolean;      //this event block is signaled
    Len:ISC_LONG;          //lenght returned by isc_event_block
    Count:integer;         //number of events in this event block
    EventID:ISC_LONG;      //handle of current event returned by isc_que_events
    bStartup:boolean;      //results are not valid yet, don't generate event
  end;
  PEPBpair=^TEPBpair;

  { TFBEventsThread }

  TFBEventsThread=class(TThread)
  protected
    FCancelAlerts: Boolean;
    FCounts: array [0..19] of longint; //FB manual says ISC_STATUS but FB code int32
    FEPBs: array of TEPBpair;
    FErrorCode: integer;
    FEventCount: integer;
    FEventNumber: integer;
    FParent:TFBEventMonitor;
    FSignal:TSimpleEvent;
    FStatus: array [0..19] of ISC_STATUS;
    procedure CheckError(Status: PISC_STATUS);
    procedure DoErrorEvent;
    procedure DoEventAlert;
    procedure Execute; override;
    procedure QueueEvents(DBHandle: pointer; block: integer);
  public
    constructor Create(Parent:TFBEventMonitor);
    procedure DoTerminate;
  end;

  { THandleIBConnection }
  // descendant to access protected GetHandle
  THandleIBConnection=class(TIBConnection)
  private
    function GetDBHandle: pointer;
  public
    property Handle:pointer read GetDBHandle;
  end;

procedure event_function(ptr:pointer;len:ushort;updated: pchar);cdecl;
begin
  Move(updated^, PEPBpair(ptr)^.ResultBuf^, len);
  PEPBpair(ptr)^.signaled:=true;
  TEventObject(PEPBpair(ptr)^.signal^).SetEvent;
end;

function THandleIBConnection.GetDBHandle: pointer;
begin
  result:=GetHandle;
end;

{ TFBEventsThread }

procedure TFBEventsThread.CheckError(Status: PISC_STATUS);
var
  buf : array [0..1023] of char;

begin
  if ((Status[0] = 1) and (Status[1] <> 0)) then
  begin
    FErrorCode := Status[1];
    FParent.FErrorMsg := '';
    while isc_interprete(Buf, @Status) > 0 do
      FParent.FErrorMsg := FParent.FErrorMsg + LineEnding +' -' + StrPas(Buf);
    if Assigned(FParent.FOnError) then
      synchronize(@DoErrorEvent);
  end;
end;

procedure TFBEventsThread.DoErrorEvent;
begin
  FParent.FOnError(FParent,FErrorCode);
end;

procedure TFBEventsThread.DoEventAlert;
begin
  FParent.FOnEventAlert(FParent, FParent.FEvents[FEventNumber],FEventCount, FCancelAlerts);
end;

procedure TFBEventsThread.Execute;
var
  i,j:integer;
  DBHandle : pointer;
  bStartup:boolean;

  function P(num:integer):pchar;
  begin
    num:=num+i*MAXEVENTSPEREPB;
    if num<FParent.FEvents.Count then
      result:=pchar(FParent.FEvents[num])
    else
      result:=nil;
  end;

begin
  // create event blocks and register events
  DBHandle:=THandleIBConnection(FParent.Connection).GetDBHandle;
  SetLength(FEPBs,1+(FParent.FEvents.Count -1) div MAXEVENTSPEREPB);
  for i:=0 to high(FEPBs) do
    begin
    FEPBs[i].Signal:=@FSignal;
    FEPBs[i].bStartup:=true;
    FEPBs[i].Count:=FParent.FEvents.Count-i*MAXEVENTSPEREPB;
    if FEPBs[i].Count > MAXEVENTSPEREPB then
      FEPBs[i].Count:=MAXEVENTSPEREPB;
    FEPBs[i].Len := isc_event_block(@FEPBs[i].EventBuf, @FEPBs[i].ResultBuf,
        word(FEPBs[i].Count),[
        P(0), P(1),P(2), P(3), P(4),P(5), P(6), P(7),
        P(8), P(9), P(10), P(11), P(12), P(13), P(14)]);
    QueueEvents(DBHandle,i);
    end;
  FCancelAlerts:=false;
  bStartup:=true;
  while not Terminated do
    begin
    if FSignal.WaitFor(100)=wrSignaled then
      begin
      FSignal.ResetEvent;
      for i:=0 to high(FEPBs) do
        if FEPBs[i].Signaled then
          begin
          FEPBs[i].Signaled:=false;
          isc_event_counts(@FCounts[0], Short(FEPBs[i].Len), FEPBs[i].EventBuf, FEPBs[i].ResultBuf);
          if not FEPBs[i].bStartup and Assigned(FParent.FOnEventAlert) then
            begin
            for j:=0 to FEPBs[i].Count-1 do
              if (FCounts[j]<>0) and not FCancelAlerts then
                begin
                FEventNumber:=(i-low(FEPBs))*MAXEVENTSPEREPB+j;
                FEventCount:=FCounts[j];
                Synchronize(@DoEventAlert);
                end;
            end;
          FEPBs[i].bStartup:=false;
          QueueEvents(DBHandle,i);
          end;
      end;
    if terminated or FCancelAlerts or not FParent.Connection.Connected then
      break;
  end;
  // unregister events
  if FParent.Connection.Connected then   // Don't do this if connection is gone
    for i:=0 to high(FEPBs) do
      begin
      isc_cancel_events(@FStatus[0],@DBHandle,@FEPBs[i].EventID);
      isc_free(FEPBs[i].EventBuf);   //points to unreachable memory if connection gone
      isc_free(FEPBs[i].ResultBuf);
      CheckError(FStatus);
      end;
  SetLength(FEPBs,0);
  FSignal.destroy;
  FParent.FRegistered :=false;
end;

procedure TFBEventsThread.QueueEvents(DBHandle:pointer;block: integer);
begin
  isc_que_events(@FStatus[0], @DBHandle, @FEPBs[block].EventID, Short(FEPBs[block].Len),
    FEPBs[block].EventBuf,TISC_CALLBACK(@event_function), @FEPBs[block]);
  CheckError(FStatus);
end;

constructor TFBEventsThread.Create(Parent: TFBEventMonitor);
begin
  FParent:=Parent;
  FSignal:=TSimpleEvent.Create();
  inherited create(false);
end;

procedure TFBEventsThread.DoTerminate;
begin
  if not Terminated then
    begin
    Terminate;
    FSignal.SetEvent;
    end;
end;

{ TFBEventMonitor }


function TFBEventMonitor.GetNativeHandle: PISC_DB_HANDLE;
begin
  result:=THandleIBConnection(FConnection).GetDBHandle;
end;


procedure TFBEventMonitor.SetConnection(AValue: TIBConnection);
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

procedure TFBEventMonitor.SetEvents(AValue: TStrings);
begin
  FEvents.Assign(AValue);
end;

procedure TFBEventMonitor.SetRegistered(AValue: Boolean);
begin
  FRegistered := AValue;
  if not (csDesigning in ComponentState) then
    if AValue then
      RegisterEvents
    else
      UnRegisterEvents;
end;

constructor TFBEventMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents:=TStringList.Create;
  {$IfDef LinkDynamically}
  InitialiseIBase60;             // stick to library in case connection closes before us
  {$EndIf}
end;

destructor TFBEventMonitor.Destroy;
begin
  if FRegistered then
    UnRegisterEvents;
  if assigned(FConnection) then
    FConnection.RemoveFreeNotification(self);
  FEvents.Free;
  {$IfDef LinkDynamically}
  ReleaseIBase60;
  {$EndIf}
  inherited Destroy;
end;

procedure TFBEventMonitor.RegisterEvents;
begin
  If not assigned(FConnection) then
    DatabaseError(SErrNoDatabaseAvailable,Self);
  if not(csDesigning in ComponentState) and not FRegistered and (Events.Count>0) then
    begin
    if not Connection.Connected then
       Connection.Connected:=true;
    if Connection.Connected then
      begin
      FEventsThread:=TFBEventsThread.Create(Self);
      FEventsThread.FreeOnTerminate:=true;
      FRegistered :=assigned(FEventsThread);
      end;
    end;
end;

procedure TFBEventMonitor.UnRegisterEvents;
begin
  if not (csDesigning in ComponentState) and FRegistered then
    begin
    TFBEventsThread(FEventsThread).DoTerminate;
    FEventsThread.WaitFor;
    FRegistered :=false;
    end;
end;

end.

