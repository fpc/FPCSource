{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                              Heap Management
*****************************************************************************}

const
  WasmMemoryPageSize=65536;

var
  InitialHeapBlockStart: Pointer;
  InitialHeapBlockEnd: Pointer;

procedure SetInitialHeapBlockStart(p: Pointer);[Public, Alias : 'FPC_WASM_SETINITIALHEAPBLOCKSTART'];
begin
  InitialHeapBlockStart:=p;
end;

procedure InitInitialHeapBlock;
begin
  InitialHeapBlockEnd:=Pointer(PtrUInt(fpc_wasm32_memory_size)*WasmMemoryPageSize);
end;

function SysOSAlloc(size: ptruint): pointer;
const
  err = high(longword);
var
  res: ptruint;
begin
  if (PtrUInt(InitialHeapBlockEnd)-PtrUInt(InitialHeapBlockStart))>=size then
    begin
      SysOSAlloc:=InitialHeapBlockStart;
      Inc(InitialHeapBlockStart,size);
    end
  else
    begin
      res:=fpc_wasm32_memory_grow((size + WasmMemoryPageSize - 1) div WasmMemoryPageSize);
      if res<>err then
        SysOSAlloc:=pointer(res*WasmMemoryPageSize)
      else
        SysOSAlloc:=nil;
    end;
end;

procedure SysOSFree(p: pointer; size: ptruint);
begin
end;
