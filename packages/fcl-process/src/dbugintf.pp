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
{$IFNDEF FPC_DOTTEDUNITS}
unit dbugintf;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Dbugmsg;
{$ELSE FPC_DOTTEDUNITS}
uses dbugmsg;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TDebugLevel = (dlInformation,dlWarning,dlError);
  TErrorLevel = Array[TDebugLevel] of integer;

//Result is true on success. See RaiseExceptionOnSendError.
function SendBoolean    (const Identifier: string; const Value: Boolean) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendDateTime   (const Identifier: string; const Value: TDateTime) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendInteger    (const Identifier: string; const Value: Integer;
                         HexNotation: Boolean = False) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendPointer    (const Identifier: string; const Value: Pointer) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendDebugEx    (const Msg: string; MType: TDebugLevel) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendDebug      (const Msg: string) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendMethodEnter(const MethodName: string) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendMethodExit (const MethodName: string) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendSeparator : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendDebugFmt   (const Msg: string; const Args: array of const) : Boolean;
//Result is true on success. See RaiseExceptionOnSendError.
function SendDebugFmtEx (const Msg: string; const Args: array of const;
                         MType: TDebugLevel) : Boolean;

procedure SetDebuggingEnabled(const AValue : boolean);
function GetDebuggingEnabled : Boolean;

{ low-level routines }

//Start the debug server and return its ProcessID.
function StartDebugServer(const ADebugServerExe : String = '';
                          const ARaiseExceptionOnSendError : Boolean = False;
                          const aLogFilename : String = '') : integer;
//Initialize the debug client and start the server.
function InitDebugClient : Boolean;
//Initialize the debug client and start the server.
function InitDebugClient(const ShowPID: Boolean; const ADebugServerExe : String = '';
                         const ARaiseExceptionOnSendError : Boolean = False;
                         const ServerLogFilename: String = ''): Boolean;
procedure FreeDebugClient;

ResourceString
  SProcessID = '%d Process %s (PID=%d)';
  SEntering = '> Entering ';
  SExiting  = '< Exiting ';
  SSeparator = '>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-<';
  SServerStartFailed = 'Failed to start debugserver (%s). (%s)';

Var
  DebugServerExe            : String = ''; { We can override this global var. in our compiled IPC client, with DefaultDebugServer a.k.a. dbugmsg.DebugServerID, or something else  }
  DefaultDebugServer        : String = DebugServerID ; { A "last ressort" simplier compiled IPC server's name, called in command line by your client a.k.a. the compiler's target file "-o" }
  //Last error message of a Send... function. Not cleared on a new call!
  SendError                 : String = '';
  //Raise an exception if a Send... function fails.
  //Otherwise the Send... functions will return false without an exception in case of an error.
  RaiseExceptionOnSendError : Boolean = false;

implementation

{$IFDEF FPC_DOTTEDUNITS}
Uses 
  System.SysUtils, System.Classes, System.Process, System.Simpleipc;
{$ELSE FPC_DOTTEDUNITS}
Uses 
  SysUtils, classes, process, simpleipc;
{$ENDIF FPC_DOTTEDUNITS}

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
  if not Assigned(MsgBuffer) then
    exit;
  MsgBuffer.Seek(0,soFrombeginning);
  WriteDebugMessageToStream(MsgBuffer,Msg);
  DebugClient.SendMessage(mtUnknown,MsgBuffer);
end;


function SendDebugMessage(Var Msg : TDebugMessage) : Boolean;
begin
  Result:=False;
  if DebugDisabled then exit(True);
  try
    If (DebugClient=Nil) then
      if InitDebugClient = false then exit;
    If (Indent>0) then
      Msg.Msg:=StringOfChar(' ',Indent)+Msg.Msg;
    WriteMessage(Msg);
    Result:=True;
  except
    On E : Exception do
    begin
      SendError:=E.Message;
      if RaiseExceptionOnSendError then
        raise;
    end;
  end;
end;

function SendBoolean(const Identifier: string; const Value: Boolean) : Boolean;

Const
  Booleans : Array[Boolean] of string = ('False','True');

begin
  Result:=SendDebugFmt('%s = %s',[Identifier,Booleans[value]]);
end;

function SendDateTime(const Identifier: string; const Value: TDateTime) : Boolean;

begin
  Result:=SendDebugFmt('%s = %s',[Identifier,DateTimeToStr(Value)]);
end;

function SendInteger(const Identifier: string; const Value: Integer;
                     HexNotation: Boolean = False) : Boolean;

Const
  Msgs : Array[Boolean] of string = ('%s = %d','%s = %x');

begin
  Result:=SendDebugFmt(Msgs[HexNotation],[Identifier,Value]);
end;

function SendPointer(const Identifier: string; const Value: Pointer) : Boolean;

begin
  Result:=SendDebugFmt('%s = %p',[Identifier,Value]);
end;

function SendDebugEx(const Msg: string; MType: TDebugLevel) : Boolean;

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[MTYpe];
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Msg
  else
    Mesg.Msg:=Msg;
  Result:=SendDebugMessage(Mesg);
end;

function SendDebug(const Msg: string) : Boolean;

Var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=dmtInformation;
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Msg
  else
    Mesg.Msg:=Msg;
  Result:=SendDebugMessage(Mesg);
end;

function SendMethodEnter(const MethodName: string) : Boolean;

begin
  Result:=SendDebug(SEntering+MethodName);
  inc(Indent,IndentChars);
end;

function SendMethodExit(const MethodName: string) : Boolean;

begin
  Dec(Indent,IndentChars);
  If (Indent<0) then
    Indent:=0;
  Result:=SendDebug(SExiting+MethodName);
end;

function SendSeparator: Boolean;

begin
  Result:=SendDebug(SSeparator);
end;

function SendDebugFmt(const Msg: string; const Args: array of const) : Boolean;

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=dmtInformation;
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Format(Msg,Args)
  else
    Mesg.Msg:=Format(Msg,Args);
  Result:=SendDebugMessage(Mesg);
end;

function SendDebugFmtEx(const Msg: string; const Args: array of const;
                        MType: TDebugLevel) : Boolean;

Var
  Mesg : TDebugMessage;

begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=ErrorLevel[mType];
  if AlwaysDisplayPID then
    Mesg.Msg:=IntToStr(GetProcessID)+' '+Format(Msg,Args)
  else
    Mesg.Msg:=Format(Msg,Args);
  Result:=SendDebugMessage(Mesg);
end;

procedure SetDebuggingEnabled(const AValue: boolean);
begin
  DebugDisabled := not AValue;
end;

function GetDebuggingEnabled: Boolean;
begin
  Result := not DebugDisabled;
end;

function StartDebugServer(const ADebugServerExe : String = '';
                          const ARaiseExceptionOnSendError : Boolean = False;
                          Const aLogFileName : string = '') : Integer;
Var
  Cmd : string;
begin
  Result := 0;
  if ADebugServerExe<>'' then
    DebugServerExe:=ADebugServerExe;
  RaiseExceptionOnSendError:=ARaiseExceptionOnSendError;

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
      E.Message:=Format(SServerStartFailed,[cmd,E.Message]);
      Free;
      raise;
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
  Result:=InitDebugClient(False,'',RaiseExceptionOnSendError,'');
end;


function InitDebugClient(const ShowPID: Boolean;
                         const ADebugServerExe : String = '';                      // Start the debug server and return its ProcessID.
                         const ARaiseExceptionOnSendError : Boolean = False;
                         const ServerLogFilename: String = ''): Boolean;

Var
  msg : TDebugMessage;
  I : Integer;

begin
  Result := False;
  AlwaysDisplayPID:= ShowPID;
  DebugClient:=TSimpleIPCClient.Create(Nil);
  DebugClient.ServerID:=DebugServerID;
  try
    If not DebugClient.ServerRunning then
      begin
      ServerID:=StartDebugServer(ADebugServerExe,ARaiseExceptionOnSendError,ServerLogFileName);
      if ServerID = 0 then
        begin
        DebugDisabled := True;
        FreeAndNil(DebugClient);
        Exit;
        end
      else
        DebugDisabled := False;
      I:=0;
      While (I<100) and not DebugClient.ServerRunning do
        begin
        Inc(I);
        Sleep(100);
        end;
      end;
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
