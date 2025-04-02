{
    This file is part of the Free Component Library

    Webassembly memory utils.
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.memutils;

{$mode ObjFPC}{$H+}

interface

Type
  TWasmGrowMemoryEvent = procedure(aPages : longint) of object;

var
  MemGrowNotifyCallBack : TWasmGrowMemoryCallBack;
  MemGrowNotifyEvent : TWasmGrowMemoryEvent;

implementation

procedure __wasm_memory_grow_notification(aPages : Longint); external 'wasmmem' name 'wasm_memory_grow_notification' ;

procedure MemNotify(aPages : longint);

begin
  __wasm_memory_grow_notification(aPages);
  if assigned(MemGrowNotifyCallBack) then
    MemGrowNotifyCallBack(aPages);
  if assigned(MemGrowNotifyEvent) then
    MemGrowNotifyEvent(aPages);
end;

initialization
  WasmGrowMemoryCallback:=@MemNotify;
end.

