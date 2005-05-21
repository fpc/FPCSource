{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Unix implementation of event mechanism

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$linklib c}

const
  // OpenLog options
  LOG_PID    = $01;
  LOG_CONS   = $02;
  LOG_ODELAY = $04;
  LOG_NDELAY = $08;
  LOG_NOWAIT = $10;
  LOG_PERROR = $20;

  // Priority levels
  LOG_EMERG   = 0;
  LOG_ALERT   = 1;
  LOG_CRIT    = 2;
  LOG_ERR     = 3;
  LOG_WARNING = 4;
  LOG_NOTICE  = 5;
  LOG_INFO    = 6;
  LOG_DEBUG   = 7;
  LOG_PRIMASK = $07;

  // facility
  LOG_KERN     = 0 shl 3;
  LOG_USER     = 1 shl 3;
  LOG_MAIL     = 2 shl 3;
  LOG_DAEMON   = 3 shl 3;
  LOG_AUTH     = 4 shl 3;
  LOG_SYSLOG   = 5 shl 3;
  LOG_LPR      = 6 shl 3;
  LOG_NEWS     = 7 shl 3;
  LOG_UUCP     = 8 shl 3;
  LOG_CRON     = 9 shl 3;
  LOG_AUTHPRIV = 10 shl 3;

procedure closelog;cdecl;external;
procedure openlog(__ident:pchar; __option:longint; __facilit:longint);cdecl;external;
function setlogmask(__mask:longint):longint;cdecl;external;
procedure syslog(__pri:longint; __fmt:pchar; args:array of const);cdecl;external;

Function TEventLog.DefaultFileName : String;

begin
  Result:='/tmp/'+ChangeFileExt(ExtractFileName(Paramstr(0)),'.log');
end;

Resourcestring
  SErrNoSysLog = 'Could not open system log (error %d)';
  SErrLogFailed = 'Failed to log entry (error %d)';

Procedure TEventLog.ActivateSystemLog;

begin
  CheckIdentification;
  OpenLog(Pchar(Identification),LOG_NOWAIT,LOG_USER);
end;

Procedure TEventLog.DeActivateSystemLog;

begin
  CloseLog;
end;

procedure TEventLog.WriteSystemLog(EventType : TEventType; Msg : String);

Var
  P,PT : PChar;
  T : String;

begin
  P:=PChar(Msg);
  T:=EventTypeToString(EventType);
  PT:=PChar(T);
  syslog(MapTypeToEvent(EventType),'[%s] %s',[PT,P]);
end;

Function TEventLog.RegisterMessageFile(AFileName : String) : Boolean;

begin
  Result:=True;
end;

function TEventLog.MapTypeToCategory(EventType: TEventType): Word;
begin
  Result:=0;
  If (EventType=ETCustom) then
    DoGetCustomEventCategory(Result);
end;

function TEventLog.MapTypeToEventID(EventType: TEventType): DWord;

begin
  Result:=0;
  If (EventType=ETCustom) then
    DoGetCustomEventID(Result);
end;

function TEventLog.MapTypeToEvent(EventType: TEventType): DWord;

Const
  WinET : Array[TEventType] of word = (LOG_NOTICE,
     LOG_INFO,LOG_WARNING,LOG_ERR,LOG_DEBUG);

begin
  If EventType=etCustom Then
    begin
    If CustomLogType=0 then
      CustomLogType:=LOG_NOTICE;
    Result:=CustomLogType;
    DoGetCustomEvent(Result);
    end
  else
    Result:=WinET[EventType];
end;
