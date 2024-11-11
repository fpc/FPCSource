program hostsimpletest;

{$mode objfpc}

uses
  BrowserConsole, JS, Types, Classes, SysUtils, Web, WasiEnv, WasiHostApp, JOB_Browser, JOB_Shared;

Type

  { TMyApplication }

  TMyApplication = class(TBrowserWASIHostApplication)
  Private
    FWADomBridge : TJSObjectBridge;
  Public
    constructor Create(aOwner : TComponent); override;
    procedure DoRun; override;
  end;

{ TMyApplication }

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FWADomBridge:=TJSObjectBridge.Create(WasiEnvironment);
  RunEntryFunction:='_initialize';
end;

procedure TMyApplication.DoRun;
begin
  // Your code here
  Terminate;
  StartWebAssembly('promisedemo.wasm',true);
end;

var
  Application : TMyApplication;
begin
  ConsoleStyle:=DefaultCRTConsoleStyle;
  HookConsole;
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
