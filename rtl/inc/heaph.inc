{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Heap manager interface section

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Memorymanager }
type
  TFPCHeapStatus = record
    MaxHeapSize,
    MaxHeapUsed,
    CurrHeapSize,
    CurrHeapUsed,
    CurrHeapFree  : ptrint;
  end;
  THeapStatus = record
    TotalAddrSpace: Cardinal;
    TotalUncommitted: Cardinal;
    TotalCommitted: Cardinal;
    TotalAllocated: Cardinal;
    TotalFree: Cardinal;
    FreeSmall: Cardinal;
    FreeBig: Cardinal;
    Unused: Cardinal;
    Overhead: Cardinal;
    HeapErrorCode: Cardinal;
  end;

  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    NeedLock            : boolean;
    Getmem              : Function(Size:ptrint):Pointer;
    Freemem             : Function(p:pointer):ptrint;
    FreememSize         : Function(p:pointer;Size:ptrint):ptrint;
    AllocMem            : Function(Size:ptrint):Pointer;
    ReAllocMem          : Function(var p:pointer;Size:ptrint):Pointer;
    MemSize             : function(p:pointer):ptrint;
    GetHeapStatus       : function :THeapStatus;
    GetFPCHeapStatus    : function :TFPCHeapStatus;
  end;

  TMemoryMutexManager = record
    MutexInit : procedure;
    MutexDone : procedure;
    MutexLock : procedure;
    MutexUnlock : procedure;
  end;

procedure GetMemoryManager(var MemMgr: TMemoryManager);
procedure SetMemoryManager(const MemMgr: TMemoryManager);
function  IsMemoryManagerSet: Boolean;
procedure SetMemoryMutexManager(var MutexMgr: TMemoryMutexManager);

{ Variables }
const
  MaxKeptOSChunks: DWord = 3; { if more than MaxKeptOSChunks are free, the heap manager will release
                              chunks back to the OS }
  growheapsizesmall : ptrint=32*1024; { fixed-size small blocks will grow with 32k }
  growheapsize1 : ptrint=256*1024;  { < 256k will grow with 256k }
  growheapsize2 : ptrint=1024*1024; { > 256k will grow with 1m }
var
  ReturnNilIfGrowHeapFails : boolean;

{ Default MemoryManager functions }
Function  SysGetmem(Size:ptrint):Pointer;
Function  SysFreemem(p:pointer):ptrint;
Function  SysFreememSize(p:pointer;Size:ptrint):ptrint;
Function  SysMemSize(p:pointer):ptrint;
Function  SysAllocMem(size:ptrint):Pointer;
function  SysTryResizeMem(var p:pointer;size : ptrint):boolean;
Function  SysReAllocMem(var p:pointer;size:ptrint):Pointer;
function  SysGetHeapStatus:THeapStatus;
function  SysGetFPCHeapStatus:TFPCHeapStatus;

{ Tp7 functions }
Procedure Getmem(Var p:pointer;Size:ptrint);
Procedure Getmemory(Var p:pointer;Size:ptrint);
Procedure Freemem(p:pointer;Size:ptrint);
Procedure Freememory(p:pointer;Size:ptrint);

{ FPC additions }
Function  MemSize(p:pointer):ptrint;

{ Delphi functions }
function GetMem(size:ptrint):pointer;
function GetMemory(size:ptrint):pointer;
function Freemem(p:pointer):ptrint;
function Freememory(p:pointer):ptrint;
function AllocMem(Size:ptrint):pointer;
function ReAllocMem(var p:pointer;Size:ptrint):pointer;
function ReAllocMemory(var p:pointer;Size:ptrint):pointer;
function GetHeapStatus:THeapStatus;
function GetFPCHeapStatus:TFPCHeapStatus;

{ Bootstrapping }

