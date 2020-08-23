{
    This file is part of the Free Component library.
    Copyright (c) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team

    Debugserver client interface, based on SimpleIPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit dbugintf;

interface

uses dbugmsg;

Type
  TDebugLevel = (dlInformation,dlWarning,dlError);
  TErrorLevel = Array[TDebugLevel] of integer;

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendInteger(const Identifier: string; const Value: Integer; HexNotation: Boolean = False);
procedure SendPointer(const Identifier: string; const Value: Pointer);
procedure SendDebugEx(const Msg: string; MType: TDebugLevel);
procedure SendDebug(const Msg: string);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel);

procedure SetDebuggingEnabled(const AValue : boolean);
function GetDebuggingEnabled : Boolean;

{ low-level routines }

Function StartDebugServer(const aLogFilename : String = '') : integer;
Function InitDebugClient : Boolean;
function InitDebugClient(const ShowPID: Boolean; const ServerLogFilename: String = ''): Boolean;
procedure FreeDebugClient;

ResourceString
  SProcessID = '%d Process %s (PID=%d)';
  SEntering = '> Entering ';
  SExiting  = '< Exiting ';
  SSeparator = '>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-<';
  SServerStartFailed = 'Failed to start debugserver. (%s)';

Var
  DebugServerExe     : String = ''; { We can override this global var. in our compiled IPC client, with DefaultDebugServer a.k.a. dbugmsg.DebugServerID, or something else  }
  DefaultDebugServer : String = DebugServerID ; { A "last ressort" simplier compiled IPC server's name, called in command line by your client a.k.a. the compiler's target file "-o" }
  SendError          : String = '';

implementation

Uses 
  SysUtils, classes, process, simpleipc, strutils;

Const
  DmtInformation = lctInformation;
  DmtWarning     = lctWarning;
  DmtError       = lctError;
  ErrorLevel     : TErrorLevel
                 = (dmtInformation,dmtWarning,dmtError);
  IndentChars    = 2;
  
var
  DebugClient : TSimpleIPCClient = nil;
  MsgBuffer : TMemoryStream = Nil;
  AlwaysDisplayPID : Boolean = False;
  ServerID : Integer;
  DebugDisabled : Boolean = False;
  Indent : Integer = 0;
  
Procedure WriteMessage(Const Msg : TDebugMessage);

begin
  MsgBuffer.Seek(0,soFrombeginning);
  WriteDebugMessageToStream(MsgBuffer,Msg);
  DebugClient.SendMessage(mtUnknown,MsgBuffer);
end;


procedure SendDebugMessage(Var Msg : TDebugMessage);

begin
  if DebugDisabled then exit;
  try
    If (DebugClient=Nil) then
      if InitDebugClient = false then exit;
    if (Indent>0) then
      Msg.Msg:=StringOfChar(' ',Indent)+Msg.Msg;
    WriteMessage(Msg);
  except
    On E : Exception do
      SendError:=E.Message;
  end;
end;

procedure SendBoolean(const Identifier: string; const Value: Boolean);

Const
  Booleans : Array[Boolean] of string = ('False','True');

begin
  SendDebugFmt('%s = %s',[Identifier,Booleans[value]]);
end;

procedure SendDateTime(const Identifier: string; const Value: TDateTime);

begin
  SendDebugFmt('%s = %s',[Identifier,DateTimeToStr(Value)]);
end;

procedure SendInteger(const Identifier: string; const Value: Integer; HexNotation: Boolean = False);

Const
  Msgs : Array[Boolean] of string = ('%s = %d','%s = %x');

begin
  SendDebugFmt(Msgs[HexNotation],[Identifier,Value]);
end;

procedure SendPointer(const Identifier: string; const Value: Pointer);

begin
  SendDebugFmt('%s = %p',[Identifier,Value]);
end;

procedure SendDebugEx(const Msg: string; MType: TDebugLevel);

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[MTYpe];
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Msg
  else
    Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendDebug(const Msg: string);

Var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=dmtInformation;
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Msg
  else
    Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendMethodEnter(const MethodName: string);

begin
  SendDebug(SEntering+MethodName);
  inc(Indent,IndentChars);
end;

procedure SendMethodExit(const MethodName: string);

begin
  Dec(Indent,IndentChars);
  If (Indent<0) then
    Indent:=0;
  SendDebug(SExiting+MethodName);
end;

procedure SendSeparator;

begin
  SendDebug(SSeparator);
end;

procedure SendDebugFmt(const Msg: string; const Args: array of const);

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=dmtInformation;
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Format(Msg,Args)
  else
    Mesg.Msg:=Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel);

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[mType];
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Format(Msg,Args)
  else
    Mesg.Msg:=Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

procedure SetDebuggingEnabled(const AValue: boolean);
begin
  DebugDisabled := not AValue;
end;

function GetDebuggingEnabled: Boolean;
begin
  Result := not DebugDisabled;
end;

function StartDebugServer(Const aLogFileName : string = '') : Integer;

Var
  Cmd : string;

begin
  Cmd := DebugServerExe;
  if Cmd='' then
    Cmd := DefaultDebugServer;
  With TProcess.Create(Nil) do
    begin
    Try
      Executable := Cmd;
      If aLogFileName<>'' Then
        Parameters.Add(aLogFileName);
      Execute;
      Result := ProcessID;
    Except On E: Exception do
      begin
      SendError := Format(SServerStartFailed,[E.Message]);
      Result := 0;
      end;
    end;
    Free;
    end;
end;

procedure FreeDebugClient;

Var
  msg : TDebugMessage;

begin
  try
    If (DebugClient<>Nil) and
       (DebugClient.ServerRunning) then
      begin
      Msg.MsgType:=lctStop;
      Msg.MsgTimeStamp:=Now;
      Msg.Msg:=Format(SProcessID,[GetProcessID, ApplicationName, GetProcessID]);
      WriteMessage(Msg);
      end;
    if assigned(MsgBuffer) then FreeAndNil(MsgBuffer);
    if assigned(DebugClient) then FreeAndNil(DebugClient);
  except
  end;
end;

Function InitDebugClient : Boolean;

begin
  InitDebugClient(False,'');
end;


function InitDebugClient(const ShowPID: Boolean; const ServerLogFilename: String = ''): Boolean;

Var
  msg : TDebugMessage;
  I : Integer;

begin
  Result := False;
  AlwaysDisplayPID:= ShowPID;
  DebugClient:=TSimpleIPCClient.Create(Nil);
  DebugClient.ServerID:=DebugServerID;
  If not DebugClient.ServerRunning then
    begin
    ServerID:=StartDebugServer(ServerLogFileName);
    if ServerID = 0 then
      begin
      DebugDisabled := True;
      FreeAndNil(DebugClient);
      Exit;
      end
    else
      DebugDisabled := False;
    I:=0;
    While (I<10) and not DebugClient.ServerRunning do
      begin
      Inc(I);
      Sleep(100);
      end;
    end;
  try
    DebugClient.Connect;
  except
    FreeAndNil(DebugClient);
    DebugDisabled:=True;
    Raise;
  end;
  MsgBuffer:=TMemoryStream.Create;
  Msg.MsgType:=lctIdentify;
  Msg.MsgTimeStamp:=Now;
  Msg.Msg:=Format(SProcessID,[GetProcessID, ApplicationName, GetProcessID]);
  WriteMessage(Msg);
  Result := True;
end;

Finalization
  FreeDebugClient;
end.
