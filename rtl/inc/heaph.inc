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
{$ifdef HASGETFPCHEAPSTATUS}
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
{$else HASGETFPCHEAPSTATUS}
  THeapStatus = record
    MaxHeapSize,
    MaxHeapUsed,
    CurrHeapSize,
    CurrHeapUsed,
    CurrHeapFree  : ptrint;
  end;
{$endif HASGETFPCHEAPSTATUS}

  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    NeedLock            : boolean;
    Getmem              : Function(Size:ptrint):Pointer;
    Freemem             : Function(p:pointer):ptrint;
    FreememSize         : Function(p:pointer;Size:ptrint):ptrint;
    AllocMem            : Function(Size:ptrint):Pointer;
    ReAllocMem          : Function(var p:pointer;Size:ptrint):Pointer;
    MemSize             : function(p:pointer):ptrint;
{$ifdef HASGETFPCHEAPSTATUS}
    GetHeapStatus       : function :THeapStatus;
    GetFPCHeapStatus    : function :TFPCHeapStatus;
{$else HASGETFPCHEAPSTATUS}
    GetHeapStatus : procedure(var status:THeapStatus);
{$endif HASGETFPCHEAPSTATUS}
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
{$ifdef HASGETFPCHEAPSTATUS}
function  SysGetHeapStatus:THeapStatus;
function  SysGetFPCHeapStatus:TFPCHeapStatus;
{$else}
procedure SysGetHeapStatus(var status:THeapStatus);
{$endif HASGETFPCHEAPSTATUS}

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
{$ifdef HASGETFPCHEAPSTATUS}
function GetHeapStatus:THeapStatus;
function GetFPCHeapStatus:TFPCHeapStatus;
{$else}
procedure GetHeapStatus(var status:THeapStatus);
{$endif HASGETFPCHEAPSTATUS}

{$ifndef ValueGetmem}
{ Needed to solve overloading problem with call from assembler (PFV) }
Procedure AsmGetmem(var p:pointer;size:ptrint);
{$endif ValueGetmem}
{$ifndef ValueFreemem}
Procedure AsmFreemem(var p:pointer);
{$endif ValueFreemem}

{ Bootstrapping }
{$ifndef HASGETHEAPSTATUS}
Function  Memavail:ptrint;
Function  Maxavail:ptrint;
Function  Heapsize:ptrint;
{$endif HASGETHEAPSTATUS}

{
  $Log: heaph.inc,v $
  Revision 1.15  2005/03/04 16:49:34  peter
    * fix getheapstatus bootstrapping

  Revision 1.14  2005/02/28 15:38:38  marco
   * getFPCheapstatus  (no, FPC HEAP, not FP CHEAP!)

  Revision 1.13  2005/02/14 17:13:22  peter
    * truncate log

}
