{
    This file is part of the Free Component Library

    Webassembly Timer API - imported functions and structures.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.timer.api;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  wasm.logger.api, wasm.timer.shared;

Type
  TWasmTimerTickEvent = Procedure (aTimerID : TWasmTimerID; userdata : pointer; var aContinue : Boolean);

function __wasm_timer_allocate(ainterval : longint; userdata: pointer) : TWasmTimerID; external TimerExportName name TimerFN_allocate;

procedure __wasm_timer_deallocate(timerid: TWasmTimerID); external TimerExportName name TimerFN_Deallocate;

function __wasm_timer_tick(timerid: TWasmTimerID; userdata : pointer) : boolean;

procedure __wasmtimer_log(level : TWasmLogLevel; const Msg : String);
procedure __wasmtimer_log(level : TWasmLogLevel; const Fmt : String; Args : Array of const);

var
  OnWasmTimerTick : TWasmTimerTickEvent;
  WasmTimerLogEnabled : Boolean;

implementation

function __wasm_timer_tick(timerid: TWasmTimerID; userdata : pointer) : boolean;

begin
  Result:=True;
  if assigned(OnWasmTimerTick) then
    OnWasmTimerTick(timerid,userdata,Result)
  else
    Result:=False;
end;

procedure __wasmtimer_log(level : TWasmLogLevel; const Msg : String);
begin
  if not WasmTimerLogEnabled then
    exit;
  __wasm_log(level,'timer',msg);
end;

procedure __wasmtimer_log(level : TWasmLogLevel; const Fmt : String; Args : Array of const);
begin
  if not WasmTimerLogEnabled then
    exit;
  __wasm_log(level,'timer',fmt,args);
end;

exports __wasm_timer_tick;

end.

