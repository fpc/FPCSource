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
    Freemem : procedure(Var p:pointer;Size:Longint);
  end;
procedure GetMemoryManager(var MemMgr: TMemoryManager);
procedure SetMemoryManager(const MemMgr: TMemoryManager);
function  IsMemoryManagerSet: Boolean;

Procedure SysGetmem(Var p:pointer;Size:Longint);
Procedure SysFreemem(Var p:pointer;Size:Longint);

{ Variables }
const
  heapblocks   : boolean=true;
  growheapsize : longint=$100000;
var
  heaporg,heapptr,heapend,heaperror,freelist : pointer;

{ Basic (TP7) functions }
Procedure getmem(Var p:pointer;Size:Longint);
Procedure freemem(Var p:pointer;Size:Longint);
Function  memavail:Longint;
Function  maxavail:Longint;
Procedure mark(var p : pointer);
Procedure release(var p : pointer);

{ Fpc Functions }
Function  heapsize : longint;
Procedure markheap(var oldfreelist,oldheapptr : pointer);
Procedure releaseheap(oldfreelist,oldheapptr : pointer);

{$ifdef TEMPHEAP}
{ Temp Heap Functions }
  const
    allow_special : boolean =true;

  Procedure split_heap;
  Procedure unsplit_heap;
  Procedure switch_to_base_heap;
  Procedure switch_to_temp_heap;
  Procedure switch_heap;  
  Procedure releasetempheap;
  Procedure gettempmem(var p : pointer;size : longint);
{$endif TEMPHEAP}

{
  $Log$
  Revision 1.8  1999-02-08 09:31:40  florian
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
