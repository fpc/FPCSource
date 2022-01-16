program checkipcserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, simpleipc
  { you can add units after this };

type

  { TSimpleIPCClientApp }

  TSimpleIPCClientApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TSimpleIPCClientApp }

procedure TSimpleIPCClientApp.DoRun;
var
  IPCClient: TSimpleIPCClient;
begin
  IPCClient := TSimpleIPCClient.Create(nil);
  IPCClient.ServerID:= 'ipc_test_crash';

  if IPCClient.ServerRunning then
    WriteLn('Server is runnning')
  else
    WriteLn('Server is NOT runnning');

  IPCClient.Destroy;
  Terminate;
end;

constructor TSimpleIPCClientApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

var
  Application: TSimpleIPCClientApp;
begin
  Application:=TSimpleIPCClientApp.Create(nil);
  Application.Title:='IPC Client';
  Application.Run;
  Application.Free;
end.

