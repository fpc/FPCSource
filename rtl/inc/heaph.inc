{
    $Id$
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
  THeapStatus = record
    MaxHeapSize,
    MaxHeapUsed,
    CurrHeapSize,
    CurrHeapUsed,
    CurrHeapFree  : ptrint;
  end;
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    NeedLock    : boolean;
    Getmem      : Function(Size:ptrint):Pointer;
    Freemem     : Function(p:pointer):ptrint;
    FreememSize : Function(p:pointer;Size:ptrint):ptrint;
    AllocMem    : Function(Size:ptrint):Pointer;
    ReAllocMem  : Function(var p:pointer;Size:ptrint):Pointer;
    MemSize     : function(p:pointer):ptrint;
    GetHeapStatus : procedure(var status:THeapStatus);
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
procedure SysGetHeapStatus(var status:THeapStatus);

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
procedure GetHeapStatus(var status:THeapStatus);

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
  $Log$
  Revision 1.12  2004-11-22 19:34:58  peter
    * GetHeapStatus added, removed MaxAvail,MemAvail,HeapSize

  Revision 1.11  2004/06/29 20:50:32  peter
    * readded support for ReturnIfGrowHeapFails

  Revision 1.10  2004/06/20 09:24:40  peter
  fixed go32v2 compile

  Revision 1.9  2004/06/17 16:16:13  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.8  2004/03/15 21:48:26  peter
    * cmem moved to rtl
    * longint replaced with ptrint in heapmanagers

  Revision 1.7  2003/10/02 14:03:24  marco
   * *memORY overloads

  Revision 1.6  2002/10/30 20:39:13  peter
    * MemoryManager record has a field NeedLock if the wrapper functions
      need to provide locking for multithreaded programs

  Revision 1.5  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.4  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

}
