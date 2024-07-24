program syncipcserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  syncipc;

const
  TEST_SERVER_NAME = 'TestSyncIPCServer';
  MSG_TEST_STOP = 101;
  RES_TEST_STOPPED = $0CACA; //:-( A message for you in Italian...

type

  { TTestSyncIPCServer }

  TTestSyncIPCServer=class(TSyncIPCServer)
  protected
    function MessageReceived(AMsgID:Integer):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; override; overload;
  end;

  { TTestSyncIPCServerApp }

  TTestSyncIPCServerApp = class(TCustomApplication)
  protected
    CommServer : TTestSyncIPCServer;

    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  DoStop : Boolean=False;


{ TTwain32SyncIPCServer }

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
var
   resBuf:array of TRect;

begin
    if (resultClient = nil)
    then writeln('MessageReceived Async (mtData_Null) : '+IntToStr(AMsgID))
    else writeln('MessageReceived (mtData_Null) : '+IntToStr(AMsgID));

    Case AMsgID of
    MSG_TEST_STOP: begin
                     if (resultClient <> nil)
                     then Writeln('   Result=$0CACA');
                     Result:= MessageResult(RES_TEST_STOPPED);
                     DoStop:=True;
                   end;
    end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer; IntegerSize:Byte): Boolean;
begin
  if (resultClient = nil)
  then Writeln('MessageReceived '+IntToStr(AMsgID)+' Async (mtData_Integer '+IntToStr(IntegerSize)+' bytes) :'+IntToHex(AInteger))
  else Writeln('MessageReceived '+IntToStr(AMsgID)+' (mtData_Integer '+IntToStr(IntegerSize)+' bytes) :'+IntToHex(AInteger));

  Case AMsgID of
  3: begin
       if (resultClient <> nil)
       then Writeln('   Result=$ABCDEF0');
       Result:= MessageResult($ABCDEF0);
     end;
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
  if (resultClient = nil)
  then writeln('MessageReceived '+IntToStr(AMsgID)+' Async (mtData_Stream '+IntToStr(AStream.Size)+' bytes) :')
  else writeln('MessageReceived '+IntToStr(AMsgID)+' (mtData_Stream '+IntToStr(AStream.Size)+' bytes) :');

  Case AMsgID of
  4: begin
       if (resultClient <> nil)
       then Writeln('   Result=Reply to SyncMessage 4 as Stream');
       AStream.WriteAnsiString('Reply to SyncMessage 4 as Stream');
       Result :=MessageResult(AStream);
     end;
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  if (resultClient = nil)
  then writeln('MessageReceived '+IntToStr(AMsgID)+' Async (mtData_String) :'+Msg)
  else writeln('MessageReceived '+IntToStr(AMsgID)+' (mtData_String) :'+Msg);

  Case AMsgID of
  1: begin
       if (resultClient <> nil)
       then Writeln('   Result=Ciao son Sync Result for 1');
       Result :=MessageResult('Ciao son Sync Result for 1');
     end;
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
var
   resRect: TRect;

begin
  if (resultClient = nil)
  then writeln('MessageReceived '+IntToStr(AMsgID)+' Async (mtData_Var '+IntToStr(Count)+' bytes):')
  else writeln('MessageReceived '+IntToStr(AMsgID)+' (mtData_Var '+IntToStr(Count)+' bytes):');

  Case AMsgID of
  2: begin
       resRect:= TRect(Buffer);
       Writeln('   '+IntToStr(resRect.Top)+'-'+IntToStr(resRect.Left)+'-'+IntToStr(resRect.Bottom)+'-'+IntToStr(resRect.Right));
       resRect.Top:=resRect.Top+33;
       resRect.Left:=resRect.Left+66;
       resRect.Bottom:=resRect.Bottom+100;
       resRect.Right:=resRect.Right+200;
       if (resultClient <> nil)
       then Writeln('   Result='+IntToStr(resRect.Top)+'-'+IntToStr(resRect.Left)+'-'+IntToStr(resRect.Bottom)+'-'+IntToStr(resRect.Right));
       Result :=MessageResult(resRect, sizeof(TRect));
     end;
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
type PRect=^TRect;
begin
  if (resultClient = nil)
  then writeln('MessageReceived '+IntToStr(AMsgID)+' Async (mtData_Pointer '+IntToStr(Count)+' bytes) :')
  else writeln('MessageReceived '+IntToStr(AMsgID)+' (mtData_Pointer '+IntToStr(Count)+' bytes) :');

  Case AMsgID of
  5: begin
       Writeln('   '+IntToStr(PRect(APointer)^.Top)+'-'+IntToStr(PRect(APointer)^.Left)+'-'+IntToStr(PRect(APointer)^.Bottom)+'-'+IntToStr(PRect(APointer)^.Right));
       PRect(APointer)^.Top:=PRect(APointer)^.Top+33;
       PRect(APointer)^.Left:=PRect(APointer)^.Left+66;
       PRect(APointer)^.Bottom:=PRect(APointer)^.Bottom+100;
       PRect(APointer)^.Right:=PRect(APointer)^.Right+200;
       if (resultClient <> nil)
       then Writeln('   Result='+IntToStr(PRect(APointer)^.Top)+'-'+IntToStr(PRect(APointer)^.Left)+'-'+IntToStr(PRect(APointer)^.Bottom)+'-'+IntToStr(PRect(APointer)^.Right));
       Result :=MessageResult(APointer, sizeof(TRect));
     end;
  end;
end;

{ TDigIt_Twain32Comm }

procedure TTestSyncIPCServerApp.DoRun;
var
  ErrorMsg: String;
  stopClient: TSyncIPCClient;
  recSize, recBuf:Longint;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h s', 'help stop');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse help parameter
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse stop parameter
  if HasOption('s', 'stop') then
    try
       stopClient :=TSyncIPCClient.Create(nil);
       stopClient.ServerID:=TEST_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
       stopClient.Connect;
       if stopClient.ServerRunning
       then stopClient.SendSyncMessage(10000, MSG_TEST_STOP, mtData_Null, recBuf, 0, recBuf, recSize);

       stopClient.Free;
       Terminate;
       Exit;
    except
      On E:Exception do begin
        ShowException(E);
        stopClient.Free;
        Terminate;
        Exit;
      end;
    end;

  try
     CommServer  := TTestSyncIPCServer.Create(Nil);
     CommServer.ServerID:=TEST_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     CommServer.StartServer(True);  // start listening, threaded

     if CommServer.Active then
     begin
       writeln('Start listening, threaded on : '+CommServer.ServerID);
       repeat
         Sleep(10);
         CheckSynchronize;
       until DoStop;
       writeln('Stop listening, threaded on : '+CommServer.ServerID);
     end;

  finally
     CommServer.Free;
     Terminate;
  end;
end;

constructor TTestSyncIPCServerApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestSyncIPCServerApp.Destroy;
begin
  inherited Destroy;
end;

procedure TTestSyncIPCServerApp.WriteHelp;
begin
  writeln('Usage: ', ExtractFileName(ExeName), ' options');
  writeln(' options:');
  writeln('         -h [--help] ', 'Show This Help');
  writeln('         -s [--stop] ', 'Stop Server');
end;

var
  Application: TTestSyncIPCServerApp;
begin
  Application:=TTestSyncIPCServerApp.Create(nil);
  Application.Title:='Test SyncIPC Server';
  Application.Run;
  Application.Free;
end.

