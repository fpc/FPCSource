program threadedhttpserver;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver, fpmimetypes, testhttpserver, syncobjs;

Type
  TServerThread = class(TThread)
  private
    FCSWriteln: TCriticalSection;
    FServ : TTestHTTPServer;
    procedure ServOnIdle(Sender: TObject);
    procedure WriteInfo(S: string);
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

{ THTTPServer }

function THTTPServer.CreateConnection(Data: TSocketStream): TFPHTTPConnection;
begin
  Result := inherited CreateConnection(Data);
  Result.Socket.IOTimeout := 15*1000;
  Result.EnableKeepAlive := True;
  Result.KeepAliveTimeout := 60*1000;
end;

{ TServerThread }

constructor TServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited;

  FCSWriteln := TCriticalSection.Create;

  FServ:=TTestHTTPServer.Create(Nil);
  FServ.BaseDir:=ExtractFilePath(ParamStr(0));
{$ifdef unix}
  FServ.MimeTypesFile:='/etc/mime.types';
{$endif}
  FServ.Threaded:=True;
  FServ.KeepAliveEnabled:=True;
  FServ.KeepAliveTimeout:=60*1000;
  FServ.Port:=8080;
  FServ.WriteInfo := @WriteInfo;
  FServ.AcceptIdleTimeout := 500;
  FServ.OnAcceptIdle := @ServOnIdle;
end;

destructor TServerThread.Destroy;
begin
  FCSWriteln.Free;
  FServ.Free;
  inherited Destroy;
end;

procedure TServerThread.Execute;
begin
  FServ.Active:=True;
end;

procedure TServerThread.ServOnIdle(Sender: TObject);
begin
  WriteInfo('Active connections: '+IntToStr(FServ.ConnectionCount));
  if Terminated then
    FServ.Active := False;
end;

procedure TServerThread.WriteInfo(S: string);
begin
  FCSWriteln.Enter;
  WriteLn(S);
  FCSWriteln.Leave;
end;

var
  T: TServerThread;
begin
  T := TServerThread.Create(True);
  T.FreeOnTerminate := False;
  T.Start;
  WriteLn('Press enter to close server.');
  ReadLn;
  T.Terminate;
  T.WaitFor;
  T.Free;
end.

