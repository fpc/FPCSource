{
    This file is part of the Free Component Library

    Webassembly centralized utility logging API.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.logger.api;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

Type
  TWasmLogLevel = (wllTrace, wllDebug, wllInfo, wllWarning, wllError, wllCritical);
  TWasmLogLevels = set of TWasmLogLevel;

  TWasmLogHook = procedure (Level : TWasmLogLevel; const Msg : string) of object;

Const
  AllLogLevels = [Low(TWasmLogLevel)..High(TWasmLogLevel)];

var
  OnWasmLog : TWasmLogHook;
  WasmLogLevels : TWasmLogLevels = AllLogLevels;

procedure __wasm_log(level : TWasmLogLevel; const Module, Msg : String);
procedure __wasm_log(level : TWasmLogLevel; const Module, Fmt : String; Args : Array of const);

implementation

procedure __wasm_log(level : TWasmLogLevel; const Module, Msg : String);

begin
  if not (level in WasmLogLevels) then
    exit;
  if not Assigned(OnWasmLog) then
    exit;
  OnWasmLog(level,'['+Module+'] '+Msg);
end;

procedure __wasm_log(level : TWasmLogLevel; const Module, Fmt : String; Args : Array of const);

begin
  if not (level in WasmLogLevels) then
    exit;
  __wasm_log(level,Module,SafeFormat(Fmt,Args));
end;

end.

