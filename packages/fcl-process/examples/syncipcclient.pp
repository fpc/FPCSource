program syncipcclient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  simpleipc, syncipc;

const
  TEST_SERVER_NAME = 'TestSyncIPCServer';
  MSG_TEST_STOP = 101;

type
  { TTestSyncIPCClientApp }

  TTestSyncIPCClientApp = class(TCustomApplication)
  protected
    CommsClient: TSyncIPCClient;

    procedure DoRun; override;

    procedure btStopClick;
    procedure btStringClick;
    procedure btRectVarClick;
    procedure btIntClick;
    procedure btStreamClick;
    procedure btPRectClick;

    procedure btStopAClick;
    procedure btStringAClick;
    procedure btRectVarAClick;
    procedure btIntAClick;
    procedure btStreamAClick;
    procedure btPRectAClick;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTestSyncIPCServerApp }

procedure TTestSyncIPCClientApp.DoRun;
var
   uSel:Char;

begin
  try
     CommsClient :=TSyncIPCClient.Create(nil);
     CommsClient.ServerID:=TEST_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     CommsClient.Connect;
     if CommsClient.ServerRunning then
     begin
          repeat
            Writeln; Writeln('Select what to send :');
            Writeln('   0 (String)');
            Writeln('   1 (TRect)');
            Writeln('   2 (Int)');
            Writeln('   3 (Stream)');
            Writeln('   4 (PRect)');
            Writeln('   5 (Async String)');
            Writeln('   6 (Async TRect)');
            Writeln('   7 (Async Int)');
            Writeln('   8 (Async Stream)');
            Writeln('   9 (Async PRect)');
            Writeln('   x STOP(null)');
            Writeln('   y STOP(Async null)');
            Readln(uSel);

            Case uSel of
            '0': btStringClick;
            '1': btRectVarClick;
            '2': btIntClick;
            '3': btStreamClick;
            '4': btPRectClick;
            '5': btStringAClick;
            '6': btRectVarAClick;
            '7': btIntAClick;
            '8': btStreamAClick;
            '9': btPRectAClick;
            'x': btStopClick;
            'y': btStopAClick;
            end;
          until (uSel='x') or (uSel='y');

          CommsClient.Free;
          Terminate;
          Exit;
     end;
  except
    On E:Exception do begin
      ShowException(E);
      CommsClient.Free;
      Terminate;
      Exit;
    end;

  end;
end;

procedure TTestSyncIPCClientApp.btStopClick;
Var
   recSize, recBuf:Longint;
   resType:TMessageType;

begin
  Writeln('SendSyncMessage STOP (mtData_Null):');
  resType :=CommsClient.SendSyncMessage(30000, MSG_TEST_STOP, mtData_Null, recBuf, 0, recBuf, recSize);
  Writeln('SendSyncMessage STOP Return ('+TMessageTypeToStr(resType)+' '+IntToStr(recSize)+' bytes)');
  if (resType=mtData_Integer) then
  begin
    Writeln('  :'+IntToHex(recBuf));
  end;
end;

procedure TTestSyncIPCClientApp.btStringClick;
Var
   recStr:String;
   resType:TMessageType;

begin
  Writeln('SendSyncMessage 1 (mtData_String):Ciao SyncMsg1');
  resType :=CommsClient.SendSyncMessage(30000, 1, 'Ciao SyncMsg1', recStr);
  Writeln('SendSyncMessage 1 Return ('+TMessageTypeToStr(resType)+'):'+recStr);
end;

procedure TTestSyncIPCClientApp.btRectVarClick;
Var
   recBuf:TRect;
   recSize:Integer;
   resType:TMessageType;

begin
  recBuf.Top:=666;
  recBuf.Left:=999;
  recBuf.Bottom:=789;
  recBuf.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendSyncMessage 2 (mtData_Var '+IntToStr(recSize)+' bytes):'+
        IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  resType :=CommsClient.SendSyncMessage(30000, 2, mtData_Var, recBuf, recSize, recBuf, recSize);
  Writeln('SendSyncMessage 2 Return ('+TMessageTypeToStr(resType)+' '+IntToStr(recSize)+' bytes)');
  if (resType=mtData_Var) then
  begin
    Writeln('  :'+IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  end;
end;

procedure TTestSyncIPCClientApp.btIntClick;
Var
   recSize, recBuf, msg:Longint;
   resType:TMessageType;

begin
  msg:=$1BCDEF23;
  Writeln('SendSyncMessage 3 (mtData_Integer):'+IntToHex(msg));
  resType :=CommsClient.SendSyncMessage(30000, 3, mtData_Integer, msg, 0, recBuf, recSize);
  Writeln('SendSyncMessage 3 Return ('+TMessageTypeToStr(resType)+' '+IntToStr(recSize)+' bytes)');
  if (resType=mtData_Integer) then
  begin
    Writeln('  :'+IntToHex(recBuf));
  end;
end;

procedure TTestSyncIPCClientApp.btStreamClick;
Var
   recSize:Integer;
   recBuf:TMemoryStream;
   res:TMemoryStream=nil;
   resType:TMessageType;
   retStr:String;

begin
  recBuf:=TMemoryStream.Create;
  recBuf.WriteAnsiString('SyncMessage 4 as Stream25');
  recSize:=recBuf.Size;
  Writeln('SendSyncMessage 4 (mtData_Stream '+IntToStr(recSize)+' bytes):SyncMessage 4 as Stream25');
  (*  //Test with Result on a new Stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtData_Stream, recBuf, 0, res, recSize);
  if (resType=mtData_Stream) then
  begin
    res.Position:=0;
    retStr:=res.ReadAnsiString;
    Writeln('SendSyncMessage 4 Return ('+IntToStr(resType)+' - '+IntToStr(recSize)+'):'+retStr+' - '+IntToStr(Integer(res.Size)));
  end;
  *)
  //Test with Result on the same stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtData_Stream, recBuf, 0, recBuf, recSize);
  Writeln('SendSyncMessage 4 Return ('+TMessageTypeToStr(resType)+' '+IntToStr(recSize)+' bytes)');
  if (resType=mtData_Stream) then
  begin
    retStr:=recBuf.ReadAnsiString;
    retStr:=recBuf.ReadAnsiString;
    Writeln('  :'+retStr);
  end;
  recBuf.Free;
  if res<>nil then res.Free;
end;

procedure TTestSyncIPCClientApp.btPRectClick;
Var
   recBuf:^TRect;
   recSize, msg:Integer;
   resType:TMessageType;

begin
  GetMem(recBuf, SizeOf(TRect));
  recBuf^.Top:=666;
  recBuf^.Left:=999;
  recBuf^.Bottom:=789;
  recBuf^.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendSyncMessage 5 (mtData_Pointer '+IntToStr(recSize)+' bytes):'+
        IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  resType :=CommsClient.SendSyncMessage(30000, 5, mtData_Pointer, recBuf, recSize, recBuf, recSize);
  Writeln('SendSyncMessage 5 Return ('+TMessageTypeToStr(resType)+' '+IntToStr(recSize)+' bytes)');
  if (resType=mtData_Pointer) then
  begin
    Writeln('  :'+IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  end;
  FreeMem(recBuf, recSize);
end;

procedure TTestSyncIPCClientApp.btStopAClick;
Var
   recBuf:Longint;

begin
  Writeln('SendMessage Async STOP (mtData_Null):');
  CommsClient.SendMessage(MSG_TEST_STOP, mtData_Null, recBuf);
end;

procedure TTestSyncIPCClientApp.btStringAClick;
begin
  Writeln('SendMessage Async 1 (mtData_String):Ciao SyncMsg1');
  CommsClient.SendMessage(1, 'Ciao SyncMsg1');
end;

procedure TTestSyncIPCClientApp.btRectVarAClick;
Var
   recBuf:TRect;
   recSize:Integer;

begin
  recBuf.Top:=666;
  recBuf.Left:=999;
  recBuf.Bottom:=789;
  recBuf.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendMessage Async 2 (mtData_Var '+IntToStr(recSize)+' bytes):'+
        IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  CommsClient.SendMessage(2, mtData_Var, recBuf, recSize);
end;

procedure TTestSyncIPCClientApp.btIntAClick;
Var
   msg:Longint;

begin
  msg:=$1BCDEF23;
  Writeln('SendMessage Async 3 (mtData_Integer):'+IntToHex(msg));
  CommsClient.SendMessage(3, mtData_Integer, msg);
end;

procedure TTestSyncIPCClientApp.btStreamAClick;
Var
   recSize:Integer;
   recBuf:TMemoryStream;

begin
  recBuf:=TMemoryStream.Create;
  recBuf.WriteAnsiString('SyncMessage 4 as Stream25');
  recSize:=recBuf.Size;
  Writeln('SendMessage Async 4 (mtData_Stream '+IntToStr(recSize)+' bytes):SyncMessage 4 as Stream25');
  CommsClient.SendMessage(4, mtData_Stream, recBuf);
  recBuf.Free;
end;

procedure TTestSyncIPCClientApp.btPRectAClick;
Var
   recBuf:^TRect;
   recSize, msg:Integer;
   resType:TMessageType;

begin
  GetMem(recBuf, SizeOf(TRect));
  recBuf^.Top:=666;
  recBuf^.Left:=999;
  recBuf^.Bottom:=789;
  recBuf^.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendMessage Async 5 (mtData_Pointer '+IntToStr(recSize)+' bytes):'+
        IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  CommsClient.SendMessage(5, mtData_Pointer, recBuf, recSize);
  FreeMem(recBuf, recSize);
end;

constructor TTestSyncIPCClientApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestSyncIPCClientApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TTestSyncIPCClientApp;

begin
  Application:=TTestSyncIPCClientApp.Create(nil);
  Application.Title:='Test SyncIPC Client';
  Application.Run;
  Application.Free;
end.

