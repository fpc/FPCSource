{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

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
    Getmem  : procedure(Var p:pointer;Size:Longint);
    Freemem : procedure(Var p:pointer);
    FreememSize : procedure(Var p:pointer;Size:Longint);
    MemSize : function(p:pointer):Longint;
  end;
procedure GetMemoryManager(var MemMgr: TMemoryManager);
procedure SetMemoryManager(const MemMgr: TMemoryManager);
function  IsMemoryManagerSet: Boolean;

Procedure SysGetmem(Var p:pointer;Size:Longint);
Procedure SysFreemem(Var p:pointer);
Procedure SysFreememSize(Var p:pointer;Size:Longint);
Function  SysMemSize(p:pointer):Longint;

{ Variables }
const
  growheapsize1 : longint=256*1024;  { < 256k will grow with 256k }
  growheapsize2 : longint=1024*1024; { > 256k will grow with 1m }
var
  heaporg,heapptr,heapend,heaperror,freelist : pointer;

{ Needed to some overloading problem with call from assembler (PFV) }
Procedure AsmFreemem(Var p:pointer);

{ Basic (TP7,Delphi) functions }
Procedure getmem(Var p:pointer;Size:Longint);
Procedure freemem(Var p:pointer);
Procedure freemem(Var p:pointer;Size:Longint);
Function  memavail:Longint;
Function  maxavail:Longint;
Procedure mark(var p : pointer);
Procedure release(var p : pointer);

{ Fpc Functions }
Function  heapsize : longint;

{
  $Log$
  Revision 1.10  1999-09-17 17:14:12  peter
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
