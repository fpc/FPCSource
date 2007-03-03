{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    User interface for debug server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}
{$mode objfpc}
{$h+}
{$endif}
unit dbugintf;


interface

uses
{$ifdef fpc}
   baseunix,
{$else}
   Libc,
{$endif}
   msgintf,
   classes,
   ssockets;

Type
  TDebugLevel = (dlInformation,dlWarning,dlError);

{$ifdef fpc}
  pid_t = longint;
{$endif}

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

procedure SendDebugMessage(Const Msg : TDebugMessage);
function  CreateDebugStream : TStream;
function  StartDebugServer : pid_t;
Procedure InitDebugStream;

Const
  SendError       : String = '';

ResourceString
  SProcessID = 'Process %d: %s';
  SEntering = '> Entering ';
  SExiting  = '< Exiting ';
  SSeparator = '>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-<';

implementation

Uses SysUtils,process;
//     UnixProcessUtils;

Const
  DmtInformation = lctInformation;
  DmtWarning     = lctWarning;
  DmtError       = lctError;
  ErrorLevel     : Array[TDebugLevel] of integer
                 = (dmtInformation,dmtWarning,dmtError);

Const
  DebugStream : TStream = nil;

Procedure WriteMessage(S : TStream; Const Msg : TDebugMessage);

Var
  MsgSize : Integer;

begin
  S.WriteBuffer(Msg.MsgType,SizeOf(Integer));
  S.WriteBuffer(Msg.MsgTimeStamp,SizeOf(TDateTime));
  MsgSize:=Length(Msg.Msg);
  S.WriteBuffer(MsgSize,SizeOf(Integer));
  S.WriteBuffer(Msg.msg[1],MsgSize);
end;

procedure SendDebugMessage(Const Msg : TDebugMessage);

begin
  try
    If DebugStream=Nil then
      begin
      InitDebugStream;
      end;
    WriteMessage(debugStream,Msg);
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

function StartDebugServer : pid_t;

begin
  With TProcess.Create(Nil) do
    Try
      CommandLine:='debugserver';
      Execute;
    Finally
      Free;
    end;
end;

function CreateUnixDebugStream(SocketFile : String) : TStream;

{$ifdef fpc}
Var
  tv,tr : timespec;
{$endif}

begin
    If Not FileExists(DebugSocket) then
      begin
      StartDebugServer;
{$ifndef fpc}
      sleep(1000);
{$else}
      tv.tv_sec:=1;
      tv.tv_nsec:=0;
      fpnanosleep(@tv,@tr);
{$endif}
      end;
{$ifdef fpc}
  Result:=TUnixSocket.Create(SocketFile);
{$else}
  Result:=TUnixSocket.CreateFromFile(SocketFile);
{$endif}
end;

Function CreateInetDebugStream (HostName : String; Port : Word) : TStream;

begin
  Result:=TInetSocket.Create(HostName,Port);
end;


function CreateDebugStream : TStream;

Var
  Msg : TDebugMessage;

begin
  Case DebugConnection of
    dcUnix : Result:=CreateUnixDebugStream(DebugSocket);
    dcInet : Result:=CreateInetDebugStream(DebugHostName,DebugPort);
  end;
  Msg.MsgType:=lctIdentify;
  Msg.MsgTimeStamp:=Now;
  Msg.Msg:=Format(SProcessID,[fpgetPID,ExtractFileName(Paramstr(0))]);
  WriteMessage(REsult,Msg);
end;

procedure FreeDebugStream;

Var i : Integer;

begin
  If (DebugStream<>Nil) then
    try
      i:=-1;
      DebugStream.WriteBuffer(I,SizeOf(I));
      DebugStream.Free;
    except
    end;
end;

Procedure InitDebugStream;

begin
  debugstream:=CreateDebugStream;
end;

Initialization

Finalization
  FreeDebugStream;
end.
