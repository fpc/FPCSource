program threadedhttpserver;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver, fpmimetypes, testhttpserver, syncobjs, ssockets;

Type
  THTTPServer = class(TTestHTTPServer)
  protected
    function CreateConnection(Data: TSocketStream): TFPHTTPConnection; override;
  public
    Property ConnectionCount;
  end;

  TServerThread = class(TThread)
  private
    FCSWriteln: TCriticalSection;
    FServ : THTTPServer;
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
  Result.Socket.IOTimeout := 180*1000;
end;

{ TServerThread }

constructor TServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited;

  FCSWriteln := TCriticalSection.Create;

  FServ:=THTTPServer.Create(Nil);
  FServ.BaseDir:=ExtractFilePath(ParamStr(0));
{$ifdef unix}
  FServ.MimeTypesFile:='/etc/mime.types';
{$endif}
  FServ.KeepConnections := True;
  FServ.KeepConnectionTimeout := 60*1000;
  FServ.ThreadMode:=tmThread;
  FServ.Port:=8080;
  FServ.WriteInfo := @WriteInfo;
  FServ.AcceptIdleTimeout := 1000;
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

