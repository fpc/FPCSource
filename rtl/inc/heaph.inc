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
    Freemem     : Function(var p:pointer):Longint;
    FreememSize : Function(var p:pointer;Size:Longint):Longint;
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
var
  heaporg,heapptr,heapend,heaperror,freelist : pointer;

{ Default MemoryManager functions }
Function  SysGetmem(Size:Longint):Pointer;
Function  SysFreemem(var p:pointer):Longint;
Function  SysFreememSize(var p:pointer;Size:Longint):Longint;
Function  SysMemSize(p:pointer):Longint;
Function  SysAllocMem(size:longint):Pointer;
{ the next one is for internal use by heap managers only, don't call directly }
{ from programs! (JM)                                                         }       
Function  InternSysReAllocMem(var p:pointer;size : longint; var doMove: boolean):pointer;
Function  SysReAllocMem(var p:pointer;size:longint):Pointer;
Function  Sysmemavail:Longint;
Function  Sysmaxavail:Longint;
Function  Sysheapsize:longint;

{ Tp7 functions }
Procedure Getmem(Var p:pointer;Size:Longint);
Procedure Freemem(Var p:pointer;Size:Longint);
Function  memavail:Longint;
Function  maxavail:Longint;

{ FPC additions }
Function  MemSize(p:pointer):Longint;
Function  heapsize:longint;

{ Delphi functions }
function GetMem(size:longint):pointer;
function Freemem(var p:pointer):longint;
function AllocMem(Size:Longint):pointer;
function ReAllocMem(var p:pointer;Size:Longint):pointer;

{ Needed to solve overloading problem with call from assembler (PFV) }
Procedure AsmGetmem(var p:pointer;size:Longint);
Procedure AsmFreemem(var p:pointer);

{ Do nothing functions, are only here for tp7 compat }
Procedure mark(var p : pointer);
Procedure release(var p : pointer);

{
  $Log$
  Revision 1.15  2000-01-20 12:35:35  jonas
    * fixed problem with reallocmem and heaptrc

  Revision 1.14  2000/01/07 16:41:34  daniel
    * copyright 2000

  Revision 1.13  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.12  1999/11/01 13:56:50  peter
    * freemem,reallocmem now get var argument

  Revision 1.11  1999/10/30 17:39:05  peter
    * memorymanager expanded with allocmem/reallocmem

  Revision 1.10  1999/09/17 17:14:12  peter
    + new heap manager supporting delphi freemem(pointer)

  Revision 1.9  1999/05/31 20:36:35  peter
    * growing is now 256k or 1mb

  Revision 1.8  1999/02/08 09:31:40  florian
    * fixed small things regarding TEMPHEAP

  Revision 1.7  1998/10/01 14:55:18  peter
    + memorymanager like delphi

  Revision 1.6  1998/09/08 15:03:27  peter
    * moved getmem/freemem/memavail/maxavail to heaph.inc

  Revision 1.5  1998/07/02 14:11:30  michael
  Reinstated the heapsize function.

  Revision 1.3  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.2  1998/04/21 10:23:15  peter
    + heapblocks
}
