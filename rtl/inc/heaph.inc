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
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    Getmem      : Function(Size:Longint):Pointer;
    Freemem     : Function(p:pointer):Longint;
    FreememSize : Function(p:pointer;Size:Longint):Longint;
    AllocMem    : Function(Size:longint):Pointer;
    ReAllocMem  : Function(var p:pointer;Size:longint):Pointer;
    MemSize     : function(p:pointer):Longint;
    MemAvail    : Function:Longint;
    MaxAvail    : Function:Longint;
    HeapSize    : Function:Longint;
  end;
procedure GetMemoryManager(var MemMgr: TMemoryManager);
procedure SetMemoryManager(const MemMgr: TMemoryManager);
function  IsMemoryManagerSet: Boolean;

{ Variables }
const
  growheapsize1 : longint=256*1024;  { < 256k will grow with 256k }
  growheapsize2 : longint=1024*1024; { > 256k will grow with 1m }
  ReturnNilIfGrowHeapFails : boolean = false;
var
  heaporg,heapptr,heapend,heaperror,freelist : pointer;

{ Default MemoryManager functions }
Function  SysGetmem(Size:Longint):Pointer;
Function  SysFreemem(p:pointer):Longint;
Function  SysFreememSize(p:pointer;Size:Longint):Longint;
Function  SysMemSize(p:pointer):Longint;
Function  SysAllocMem(size:longint):Pointer;
function  SysTryResizeMem(var p:pointer;size : longint):boolean;
Function  SysReAllocMem(var p:pointer;size:longint):Pointer;
Function  Sysmemavail:Longint;
Function  Sysmaxavail:Longint;
Function  Sysheapsize:longint;

{ Tp7 functions }
Procedure Getmem(Var p:pointer;Size:Longint);
Procedure Freemem(p:pointer;Size:Longint);
Function  memavail:Longint;
Function  maxavail:Longint;

{ FPC additions }
Function  MemSize(p:pointer):Longint;
Function  heapsize:longint;

{ Delphi functions }
function GetMem(size:longint):pointer;
function Freemem(p:pointer):longint;
function AllocMem(Size:Longint):pointer;
function ReAllocMem(var p:pointer;Size:Longint):pointer;

{ Do nothing functions, are only here for tp7 compat }
Procedure mark(var p : pointer);
Procedure release(var p : pointer);

{$ifndef ValueGetmem}
{ Needed to solve overloading problem with call from assembler (PFV) }
Procedure AsmGetmem(var p:pointer;size:Longint);
{$endif ValueGetmem}
{$ifndef ValueFreemem}
Procedure AsmFreemem(var p:pointer);
{$endif ValueFreemem}

{
  $Log$
  Revision 1.4  2002-09-07 15:07:45  peter
    * old logs removed and tabs fixed

}
