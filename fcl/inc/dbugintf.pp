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

uses
   simpleipc,
   classes;

Type
  TDebugLevel = (dlInformation,dlWarning,dlError);

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendDebugEx(const Msg: string; MType: TDebugLevel);
procedure SendDebug(const Msg: string);
procedure SendInteger(const Identifier: string; const Value: Integer);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel);

{ low-level routines }

Function  StartDebugServer : integer;
Procedure InitDebugClient;

Const
  SendError       : String = '';

ResourceString
  SProcessID = 'Process %s';
  SEntering = '> Entering ';
  SExiting  = '< Exiting ';
  SSeparator = '>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-<';

implementation

Uses SysUtils, msgintf, process;

Const
  DmtInformation = lctInformation;
  DmtWarning     = lctWarning;
  DmtError       = lctError;
  ErrorLevel     : Array[TDebugLevel] of integer
                 = (dmtInformation,dmtWarning,dmtError);

var
  DebugClient : TSimpleIPCClient = nil;
  MsgBuffer : TMemoryStream = Nil;
  ServerID : Integer;

  
Procedure WriteMessage(Const Msg : TDebugMessage);

begin
  MsgBuffer.Seek(0,soFrombeginning);
  WriteDebugMessageToStream(MsgBuffer,Msg);
  DebugClient.SendMessage(mtUnknown,MsgBuffer);
end;


procedure SendDebugMessage(Const Msg : TDebugMessage);

begin
  try
    If (DebugClient=Nil) then
      InitDebugClient;
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

procedure SendDebugEx(const Msg: string; MType: TDebugLevel);

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[MTYpe];
  Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendDebug(const Msg: string);

Var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=dmtInformation;
  Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendInteger(const Identifier: string; const Value: Integer);

begin
  SendDebugFmt('%s = %d',[identifier,Value]);
end;

procedure SendMethodEnter(const MethodName: string);

begin
  SendDebug(SEntering+MethodName);
end;

procedure SendMethodExit(const MethodName: string);

begin
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
  Mesg.Msg:=Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel);

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[mType];
  Mesg.Msg:=Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

function StartDebugServer : Integer;

begin
  With TProcess.Create(Nil) do
    Try
      CommandLine:='debugserver';
      Execute;
      Result:=ProcessID;
    Finally
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
      Msg.Msg:=Format(SProcessID,[ApplicationName]);
      WriteMessage(Msg);
      end;
    FreeAndNil(MsgBuffer);
    FreeAndNil(DebugClient);
  except
  end;
end;

Procedure InitDebugClient;

Var
  msg : TDebugMessage;
  I : Integer;
  
begin
  DebugClient:=TSimpleIPCClient.Create(Nil);
  DebugClient.ServerID:=DebugServerID;
  If not DebugClient.ServerRunning then
    begin
    ServerID:=StartDebugServer;
    I:=0;
    While (I<10) and not DebugClient.ServerRunning do
      begin
      Inc(I);
      Sleep(100);
      end;
    end;
  DebugClient.Connect;
  MsgBuffer:=TMemoryStream.Create;
  Msg.MsgType:=lctIdentify;
  Msg.MsgTimeStamp:=Now;
  Msg.Msg:=Format(SProcessID,[ApplicationName]);
  WriteMessage(Msg);
end;

Initialization

Finalization
  FreeDebugClient;
end.
