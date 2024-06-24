program webhost;

{$mode objfpc}

uses
  BrowserConsole, JS, Types, Classes, SysUtils, Web, WasiEnv, WasiHostApp, JOB_Browser, JOB_Shared;


var
  wasmFilename : string; external name 'wasmFilename';

Type

  { TMyApplication }

  TMyApplication = class(TBrowserWASIHostApplication)
  Private
    FJOB : TJSObjectBridge;
    function GetWasmModuleName: String;
  Public
    constructor Create(aOwner : TComponent); override;
    procedure DoRun; override;
  end;

{ TMyApplication }

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FJOB:=TJSObjectBridge.Create(WasiEnvironment);
  RunEntryFunction:='_initialize';
end;

function TMyApplication.GetWasmModuleName : String;
{ Determine webassembly module to run
  1. from external variable wasmFilename
  2. from first part of hash:  #moduleName/
  3. from query varable wasmmodule: ?wasmmodule=x
  4. Hardcoded 'demo.wasm';
}

begin
  Result:='';
  if IsString(wasmFilename) then
    Result:=wasmFilename;
  if (Result='') then
    Result:=ParamStr(1);
  if (Result='') then
    Result:=GetEnvironmentVar('wasmmodule');
  if Result='' then
    Result:='demo.wasm';
end;

procedure TMyApplication.DoRun;

var
  WasmModule : String;

begin
  Terminate;
  WasmModule:=GetWasmModuleName;
  Writeln('Loading & starting webassembly module :' ,WasmModule);
  StartWebAssembly(WasmModule,true);
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
