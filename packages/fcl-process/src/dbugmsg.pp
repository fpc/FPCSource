{
    This file is part of the Free Component library.
    Copyright (c) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team

    Debugserver Client/Server common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit dbugmsg;

interface

uses Classes;

Const
  DebugServerID = 'fpcdebugserver'; { compiled IPC server's IDentifiant-name. Should be the same as the compiled IPC client dbugintf.DefaultDebugServer }

  lctStop        = -1;
  lctInformation = 0;
  lctWarning     = 1;
  lctError       = 2;
  lctIdentify    = 3;

Type
  TDebugMessage = Record
    MsgType      : Integer;
    MsgTimeStamp : TDateTime;
    Msg          : String;
  end;

Procedure ReadDebugMessageFromStream(AStream : TStream; Var Msg : TDebugMessage);
Procedure WriteDebugMessageToStream(AStream : TStream; Const Msg : TDebugMessage);
Function DebugMessageName(msgType : Integer) : String;


implementation

resourcestring
  SStop        = 'Stop';
  SInformation = 'Information';
  SWarning     = 'Warning';
  SError       = 'Error';
  SIdentify    = 'Identify';
  SUnknown     = 'Unknown';

procedure ReadDebugMessageFromStream(AStream : TStream; Var Msg : TDebugMessage);

Var
  MsgSize : Integer;

begin
  With AStream do
    begin
    ReadBuffer(Msg.MsgType,SizeOf(Integer));
    ReadBuffer(Msg.MsgTimeStamp,SizeOf(TDateTime));
    ReadBuffer(MsgSize,SizeOf(Integer));
    SetLength(Msg.Msg,MsgSize);
    If (MsgSize<>0) then
      ReadBuffer(Msg.msg[1],MsgSize);
    end;
end;

procedure WriteDebugMessageToStream(AStream : TStream; Const Msg : TDebugMessage);

Var
  MsgSize : Integer;

begin
  With AStream do
    begin
    WriteBuffer(Msg.MsgType,SizeOf(Integer));
    WriteBuffer(Msg.MsgTimeStamp,SizeOf(TDateTime));
    MsgSize:=Length(Msg.Msg);
    WriteBuffer(MsgSize,SizeOf(Integer));
    WriteBuffer(Msg.msg[1],MsgSize);
    end;
end;

Function DebugMessageName(msgType : Integer) : String;

begin
  Case MsgType of
    lctStop        : Result:=SStop;
    lctInformation : Result:=SInformation;
    lctWarning     : Result:=SWarning;
    lctError       : Result:=SError;
    lctIdentify    : Result:=SIdentify;
  else
    Result:=SUnknown;
  end;
end;

end.
