{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    functions for heap management in the data segment

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}

{ Reuse bigger blocks instead of allocating a new block at freelist/heapptr.
  the tried bigger blocks are always multiple sizes of the current block }
{$define REUSEBIGGER}

{ Allocate small blocks at heapptr instead of walking the freelist }
{$define SMALLATHEAPPTR}

{ Try to find the best matching block in general freelist }
{$define BESTMATCH}

{ DEBUG: Dump info when the heap needs to grow }
{ define DUMPGROW}

{ Default heap settings }
const
  blocksize    = 16;  { at least size of freerecord }
  blockshr     = 4;   { shr value for blocksize=2^blockshr}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
  maxblock     = maxblocksize div blocksize;
  maxreusebigger = 8; { max reuse bigger tries }

  usedmask = 1;            { flag if the block is used or not }
  beforeheapendmask = 2;   { flag if the block is just before a heapptr }
  sizemask = not(blocksize-1);

{****************************************************************************}

{$ifdef DUMPGROW}
  {$define DUMPBLOCKS}
{$endif}

{ Memory manager }
const
  MemoryManager: TMemoryManager = (
    GetMem: SysGetMem;
    FreeMem: SysFreeMem;
    FreeMemSize: SysFreeMemSize;
    AllocMem: SysAllocMem;
    ReAllocMem: SysReAllocMem;
    MemSize: SysMemSize;
    MemAvail: SysMemAvail;
    MaxAvail: SysMaxAvail;
    HeapSize: SysHeapSize;
  );

type
  ppfreerecord = ^pfreerecord;
  pfreerecord  = ^tfreerecord;
  tfreerecord  = record
    size  : longint;
    next,
    prev  : pfreerecord;
  end; { 12 bytes }

  pheaprecord = ^theaprecord;
  theaprecord = record
  { this should overlap with tfreerecord }
    size  : longint;
  end; { 4 bytes }

  tfreelists   = array[0..maxblock] of pfreerecord;
  pfreelists   = ^tfreelists;

var
  internal_memavail  : longint;
  internal_heapsize  : longint;
  freelists          : tfreelists;

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

procedure GetMemoryManager(var MemMgr:TMemoryManager);
begin
  MemMgr:=MemoryManager;
end;


procedure SetMemoryManager(const MemMgr:TMemoryManager);
begin
  MemoryManager:=MemMgr;
end;


function IsMemoryManagerSet:Boolean;
begin
  IsMemoryManagerSet:=(MemoryManager.GetMem<>@SysGetMem) or
                      (MemoryManager.FreeMem<>@SysFreeMem);
end;


procedure GetMem(Var p:pointer;Size:Longint);{$ifndef NEWMM}[public,alias:'FPC_GETMEM'];{$endif}
begin
  p:=MemoryManager.GetMem(Size);
end;


procedure FreeMem(Var p:pointer;Size:Longint);{$ifndef NEWMM}[public,alias:'FPC_FREEMEM'];{$endif}
begin
  MemoryManager.FreeMemSize(p,Size);
  p:=nil;
end;


function MaxAvail:Longint;
begin
  MaxAvail:=MemoryManager.MaxAvail();
end;


function MemAvail:Longint;
begin
  MemAvail:=MemoryManager.MemAvail();
end;


{ FPC Additions }
function HeapSize:Longint;
begin
  HeapSize:=MemoryManager.HeapSize();
end;


function MemSize(p:pointer):Longint;
begin
  MemSize:=MemoryManager.MemSize(p);
end;


{ Delphi style }
function FreeMem(var p:pointer):Longint;
begin
  Freemem:=MemoryManager.FreeMem(p);
end;


function GetMem(size:longint):pointer;
begin
  GetMem:=MemoryManager.GetMem(Size);
end;


function AllocMem(Size:Longint):pointer;
begin
  AllocMem:=MemoryManager.AllocMem(size);
end;


function ReAllocMem(var p:pointer;Size:Longint):pointer;
begin
  ReAllocMem:=MemoryManager.ReAllocMem(p,size);
end;


{ Needed for calls from Assembler }
procedure AsmGetMem(var p:pointer;size:longint);{$ifdef NEWMM}[public,alias:'FPC_GETMEM'];{$endif}
begin
  p:=MemoryManager.GetMem(size);
end;


procedure AsmFreeMem(var p:pointer);{$ifdef NEWMM}[public,alias:'FPC_FREEMEM'];{$endif}
begin
  if p <> nil then
   begin
     MemoryManager.FreeMem(p);
     p:=nil;
   end;
end;


{*****************************************************************************
                         Heapsize,Memavail,MaxAvail
*****************************************************************************}

function SysHeapsize : longint;
begin
  Sysheapsize:=internal_heapsize;
end;


function SysMemavail : longint;
begin
  Sysmemavail:=internal_memavail;
end;


function SysMaxavail : longint;
var
  hp : pfreerecord;
begin
  Sysmaxavail:=heapend-heapptr;
  hp:=freelists[0];
  while assigned(hp) do
   begin
     if hp^.size>Sysmaxavail then
       Sysmaxavail:=hp^.size;
     hp:=hp^.next;
   end;
end;


{$ifdef DUMPBLOCKS}
procedure DumpBlocks;
var
  s,i,j : longint;
  hp  : pfreerecord;
begin
  for i:=1 to maxblock do
   begin
     hp:=freelists[i];
     j:=0;
     while assigned(hp) do
      begin
        inc(j);
        hp:=hp^.next;
      end;
     writeln('Block ',i*blocksize,': ',j);
   end;
{ freelist 0 }
  hp:=freelists[0];
  j:=0;
  s:=0;
  while assigned(hp) do
   begin
     inc(j);
     if hp^.size>s then
      s:=hp^.size;
     hp:=hp^.next;
   end;
  writeln('Main: ',j,' maxsize: ',s);
end;
{$endif}


{*****************************************************************************
                                 SysGetMem
*****************************************************************************}

function SysGetMem(size : longint):pointer;
type
  heaperrorproc=function(size:longint):integer;
var
  proc  : heaperrorproc;
  pcurr : pfreerecord;
  again : boolean;
  s,s1,i,
  sizeleft : longint;
{$ifdef BESTMATCH}
  pbest : pfreerecord;
{$endif}
begin
{ Something to allocate ? }
  if size<=0 then
   begin
     { give an error for < 0 }
     if size<0 then
      HandleError(204);
     { we always need to allocate something, using heapend is not possible,
       because heappend can be changed by growheap (PFV) }
     size:=1;
   end;
{ calc to multiply of 16 after adding the needed 8 bytes heaprecord }
  size:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
  dec(internal_memavail,size);
{ try to find a block in one of the freelists per size }
  s:=size shr blockshr;
  if s<=maxblock then
   begin
     pcurr:=freelists[s];
     { correct size match ? }
     if assigned(pcurr) then
      begin
        { create the block we should return }
        sysgetmem:=pointer(pcurr)+sizeof(theaprecord);
        { fix size }
        pcurr^.size:=pcurr^.size or usedmask;
        { update freelist }
        freelists[s]:=pcurr^.next;
        if assigned(freelists[s]) then
         freelists[s]^.prev:=nil;
        exit;
      end;
{$ifdef SMALLATHEAPPTR}
     if heapend-heapptr>size then
      begin
        sysgetmem:=heapptr;
        if (heapptr+size=heapend) then
         pheaprecord(sysgetmem)^.size:=size or (usedmask or beforeheapendmask)
        else
         pheaprecord(sysgetmem)^.size:=size or usedmask;
        inc(sysgetmem,sizeof(theaprecord));
        inc(heapptr,size);
        exit;
      end;
{$endif}
{$ifdef REUSEBIGGER}
     { try a bigger block }
     s1:=s+s;
     i:=0;
     while (s1<=maxblock) and (i<maxreusebigger) do
      begin
        pcurr:=freelists[s1];
        if assigned(pcurr) then
         begin
           s:=s1;
           break;
         end;
        inc(s1);
        inc(i);
      end;
{$endif}
   end
  else
   pcurr:=nil;
{ not found, then check the main freelist for the first match }
  if not(assigned(pcurr)) then
   begin
     s:=0;
{$ifdef BESTMATCH}
     pbest:=nil;
{$endif}
     pcurr:=freelists[0];
     while assigned(pcurr) do
      begin
{$ifdef BESTMATCH}
        if pcurr^.size=size then
         break
        else
         begin
           if (pcurr^.size>size) then
            begin
              if (not assigned(pbest)) or
                 (pcurr^.size<pbest^.size) then
               pbest:=pcurr;
            end;
         end;
{$else}
        if pcurr^.size>=size then
         break;
{$endif}
        pcurr:=pcurr^.next;
      end;
{$ifdef BESTMATCH}
     if not assigned(pcurr) then
      pcurr:=pbest;
{$endif}
   end;
  { have we found a block, then get it and free up the other left part,
    if no blocks are found then allocated at the heapptr or grow the heap }
  if assigned(pcurr) then
   begin
     { get pointer of the block we should return }
     sysgetmem:=pointer(pcurr);
     { remove the current block from the freelist }
     if assigned(pcurr^.next) then
      pcurr^.next^.prev:=pcurr^.prev;
     if assigned(pcurr^.prev) then
      pcurr^.prev^.next:=pcurr^.next
     else
      freelists[s]:=pcurr^.next;
     { create the left over freelist block, if at least 16 bytes are free }
     sizeleft:=pcurr^.size-size;
     if sizeleft>sizeof(tfreerecord) then
      begin
        pcurr:=pfreerecord(pointer(pcurr)+size);
        { inherit the beforeheapendmask }
        pcurr^.size:=sizeleft or (pheaprecord(sysgetmem)^.size and beforeheapendmask);
        { insert the block in the freelist }
        pcurr^.prev:=nil;
        s1:=sizeleft shr blockshr;
        if s1>maxblock then
         s1:=0;
        pcurr^.next:=freelists[s1];
        if assigned(freelists[s1]) then
         freelists[s1]^.prev:=pcurr;
        freelists[s1]:=pcurr;
        { create the block we need to return }
        pheaprecord(sysgetmem)^.size:=size or usedmask;
      end
     else
      begin
        { create the block we need to return }
        pheaprecord(sysgetmem)^.size:=size or usedmask or (pheaprecord(sysgetmem)^.size and beforeheapendmask);
      end;

     inc(sysgetmem,sizeof(theaprecord));
     exit;
   end;
  { Lastly, the top of the heap is checked, to see if there is }
  { still memory available.                                   }
  repeat
    again:=false;
    if heapend-heapptr>size then
     begin
       sysgetmem:=heapptr;
       if (heapptr+size=heapend) then
        pheaprecord(sysgetmem)^.size:=size or (usedmask or beforeheapendmask)
       else
        pheaprecord(sysgetmem)^.size:=size or usedmask;
       inc(sysgetmem,sizeof(theaprecord));
       inc(heapptr,size);
       exit;
     end;
    { Call the heaperror proc }
    if assigned(heaperror) then
     begin
       proc:=heaperrorproc(heaperror);
       case proc(size) of
        0 : HandleError(203);
        1 : sysgetmem:=nil;
        2 : again:=true;
       end;
     end
    else
     HandleError(203);
  until not again;
end;


{*****************************************************************************
                               SysFreeMem
*****************************************************************************}

Function SysFreeMem(var p : pointer):Longint;
var
  pcurrsize,s : longint;
  pcurr : pfreerecord;
begin
  if p=nil then
   HandleError(204);
{ fix p to point to the heaprecord }
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  pcurrsize:=pcurr^.size and sizemask;
  inc(internal_memavail,pcurrsize);
{ insert the block in it's freelist }
  pcurr^.size:=pcurr^.size and (not usedmask);
  pcurr^.prev:=nil;
  s:=pcurrsize shr blockshr;
  if s>maxblock then
   s:=0;
  pcurr^.next:=freelists[s];
  if assigned(pcurr^.next) then
   pcurr^.next^.prev:=pcurr;
  freelists[s]:=pcurr;
  p:=nil;
  SysFreeMem:=pcurrsize;
end;


{*****************************************************************************
                              SysFreeMemSize
*****************************************************************************}

Function SysFreeMemSize(var p : pointer;size : longint):longint;
var
  pcurrsize,s : longint;
  pcurr : pfreerecord;
begin
  SysFreeMemSize:=0;
  if size<=0 then
   begin
     if size<0 then
      HandleError(204);
     p:=nil;
     exit;
   end;
  if p=nil then
   HandleError(204);
{ fix p to point to the heaprecord }
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  pcurrsize:=pcurr^.size and sizemask;
  inc(internal_memavail,pcurrsize);
{ size check }
  size:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
  if size<>pcurrsize then
   HandleError(204);
{ insert the block in it's freelist }
  pcurr^.size:=pcurr^.size and (not usedmask);
  pcurr^.prev:=nil;
  s:=pcurrsize shr blockshr;
  if s>maxblock then
   s:=0;
  pcurr^.next:=freelists[s];
  if assigned(pcurr^.next) then
   pcurr^.next^.prev:=pcurr;
  freelists[s]:=pcurr;
  p:=nil;
  SysFreeMemSize:=pcurrsize;
end;


{*****************************************************************************
                                 SysMemSize
*****************************************************************************}

function SysMemSize(p:pointer):longint;
begin
  SysMemSize:=(pheaprecord(pointer(p)-sizeof(theaprecord))^.size and sizemask)-sizeof(theaprecord);
end;


{*****************************************************************************
                                 SysAllocMem
*****************************************************************************}

function SysAllocMem(size : longint):pointer;
begin
  sysallocmem:=MemoryManager.GetMem(size);
  if sysallocmem<>nil then
   FillChar(sysallocmem^,size,0);
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysTryResizeMem(var p:pointer;size : longint):boolean;
var
  oldsize,
  currsize,
  foundsize,
  sizeleft,
  s     : longint;
  wasbeforeheapend : boolean;
  hp,
  pnew,
  pcurr : pfreerecord;
begin
{ fix needed size }
  size:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
{ fix p to point to the heaprecord }
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  currsize:=pcurr^.size and sizemask;
  oldsize:=currsize;
  wasbeforeheapend:=(pcurr^.size and beforeheapendmask)<>0;
{ is the allocated block still correct? }
  if currsize=size then
   begin
     SysTryResizeMem:=true;
     exit;
   end;
{ do we need to allocate more memory ? }
  if size>currsize then
   begin
   { the size is bigger than the previous size, we need to allocated more mem.
     We first check if the blocks after the current block are free. If not we
     simply call getmem/freemem to get the new block }
     foundsize:=0;
     hp:=pcurr;
     repeat
       inc(foundsize,hp^.size and sizemask);
       { block used or before a heapptr ? }
       if (hp^.size and beforeheapendmask)<>0 then
        begin
          wasbeforeheapend:=true;
          break;
        end;
       { get next block }
       hp:=pfreerecord(pointer(hp)+(hp^.size and sizemask));
       { when we're at heapptr then we can stop }
       if (hp=heapptr) then
        begin
          inc(foundsize,heapend-heapptr);
          break;
        end;
       if (hp^.size and usedmask)<>0 then
        break;
     until (foundsize>=size);
   { found enough free blocks? }
     if foundsize>=size then
      begin
        { we walk the list again and remove all blocks }
        foundsize:=pcurr^.size and sizemask;
        hp:=pcurr;
        repeat
          { get next block }
          hp:=pfreerecord(pointer(hp)+(hp^.size and sizemask));
          { when we're at heapptr then we can increase it, if there is enough
            room is already checked }
          if (hp=heapptr) then
           begin
             inc(heapptr,size-foundsize);
             foundsize:=size;
             break;
           end;
          s:=hp^.size and sizemask;
          inc(foundsize,s);
          { remove block from freelist }
          s:=s shr blockshr;
          if s>maxblock then
           s:=0;
          if assigned(hp^.next) then
           hp^.next^.prev:=hp^.prev;
          if assigned(hp^.prev) then
           hp^.prev^.next:=hp^.next
          else
           freelists[s]:=hp^.next;
        until (foundsize>=size);
        if wasbeforeheapend then
         pcurr^.size:=foundsize or usedmask or beforeheapendmask
        else
         pcurr^.size:=foundsize or usedmask;
      end
     else
      begin
        { we need to call getmem/move/freemem }
        SysTryResizeMem:=false;
        exit;
      end;
     currsize:=pcurr^.size and sizemask;
   end;
{ is the size smaller then we can adjust the block to that size and insert
  the other part into the freelist }
  if size<currsize then
   begin
     { create the left over freelist block, if at least 16 bytes are free }
     sizeleft:=currsize-size;
     if sizeleft>sizeof(tfreerecord) then
      begin
        pnew:=pfreerecord(pointer(pcurr)+size);
        pnew^.size:=sizeleft or (pcurr^.size and beforeheapendmask);
        { insert the block in the freelist }
        pnew^.prev:=nil;
        s:=sizeleft shr blockshr;
        if s>maxblock then
         s:=0;
        pnew^.next:=freelists[s];
        if assigned(freelists[s]) then
         freelists[s]^.prev:=pnew;
        freelists[s]:=pnew;
        { fix the size of the current block and leave }
        pcurr^.size:=size or usedmask;
      end
     else
      begin
        { fix the size of the current block and leave }
        pcurr^.size:=size or usedmask or (pcurr^.size and beforeheapendmask);
      end;
   end;
  dec(internal_memavail,size-oldsize);
  SysTryResizeMem:=true;
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysReAllocMem(var p:pointer;size : longint):pointer;
var
  p2 : pointer;
begin
  { Free block? }
  if size=0 then
   begin
     if p<>nil then
      MemoryManager.FreeMem(p);
   end
  else
   { Allocate a new block? }
   if p=nil then
    begin
      p:=MemoryManager.GetMem(size);
    end
  else
   { Resize block }
   if not SysTryResizeMem(p,size) then
    begin
      p2:=MemoryManager.GetMem(size);
      if p2<>nil then
       Move(p^,p2^,size);
      MemoryManager.FreeMem(p);
      p:=p2;
    end;
  SysReAllocMem:=p;
end;


{*****************************************************************************
                                Mark/Release
*****************************************************************************}

procedure release(var p : pointer);
begin
end;


procedure mark(var p : pointer);
begin
end;


{*****************************************************************************
                                Grow Heap
*****************************************************************************}

function growheap(size :longint) : integer;
var
  sizeleft,
  NewPos    : longint;
  pcurr     : pfreerecord;
begin
{$ifdef DUMPGROW}
  writeln('grow ',size);
  DumpBlocks;
{$endif}
  { Allocate by 64K size }
  size:=(size+$ffff) and $ffff0000;
  { first try 256K (default) }
  if size<=GrowHeapSize1 then
   begin
     NewPos:=Sbrk(GrowHeapSize1);
     if NewPos>0 then
      size:=GrowHeapSize1;
   end
  else
  { second try 1024K (default) }
   if size<=GrowHeapSize2 then
    begin
      NewPos:=Sbrk(GrowHeapSize2);
      if NewPos>0 then
       size:=GrowHeapSize2;
    end
  { else alloate the needed bytes }
  else
    NewPos:=SBrk(size);
  { try again }
  if NewPos=-1 then
   begin
     NewPos:=Sbrk(size);
     if NewPos=-1 then
      begin
        GrowHeap:=0;
        Exit;
      end;
   end;
{ increase heapend or add to freelist }
  if heapend=pointer(newpos) then
   begin
     heapend:=pointer(newpos+size);
   end
  else
   begin
     { create freelist entry for old heapptr-heapend }
     sizeleft:=heapend-heapptr;
     if sizeleft>sizeof(tfreerecord) then
      begin
        pcurr:=pfreerecord(heapptr);
        pcurr^.size:=sizeleft or beforeheapendmask;
        { insert the block in the freelist }
        pcurr^.next:=freelists[0];
        pcurr^.prev:=nil;
        if assigned(freelists[0]) then
         freelists[0]^.prev:=pcurr;
        freelists[0]:=pcurr;
      end;
     { now set the new heapptr,heapend to the new block }
     heapptr:=pointer(newpos);
     heapend:=pointer(newpos+size);
   end;
{ set the total new heap size }
  inc(internal_memavail,size);
  inc(internal_heapsize,size);
{ try again }
  GrowHeap:=2;
end;


{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeap;
begin
  FillChar(FreeLists,sizeof(TFreeLists),0);
  internal_heapsize:=GetHeapSize;
  internal_memavail:=internal_heapsize;
  HeapOrg:=GetHeapStart;
  HeapPtr:=HeapOrg;
  HeapEnd:=HeapOrg+internal_memavail;
  HeapError:=@GrowHeap;
end;

{
  $Log$
  Revision 1.33  2000-02-02 11:12:29  peter
    * fixed internal_memavail counting for tryresizemem

  Revision 1.32  2000/01/31 23:41:30  peter
    * reallocmem fixed for freemem() call when size=0

  Revision 1.31  2000/01/24 23:56:10  peter
    * fixed reallocmem which didn't work anymore and thus broke a lot
      of objfpc/delphi code

  Revision 1.30  2000/01/20 12:35:35  jonas
    * fixed problem with reallocmem and heaptrc

  Revision 1.29  2000/01/07 16:41:34  daniel
    * copyright 2000

  Revision 1.28  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.27  1999/12/16 19:11:49  peter
    * fixed sysmemsize which did the and sizemask wrong

  Revision 1.26  1999/12/13 21:04:46  peter
    * fixed getmem call with wrong size from reallocmem

  Revision 1.25  1999/12/01 22:57:31  peter
    * cmdline support

  Revision 1.24  1999/11/14 21:34:21  peter
    * fixed reallocmem with a block at the end of an allocated memoryblock,
      had to introduce a flag for such blocks.
    * flags are now stored in the first 4 bits instead of the highest bit,
      this could be done because the sizes of block are always >= 16

  Revision 1.23  1999/11/10 22:29:51  michael
  + Fixed sysreallocmem

  Revision 1.22  1999/11/01 13:56:50  peter
    * freemem,reallocmem now get var argument

  Revision 1.21  1999/10/30 17:39:05  peter
    * memorymanager expanded with allocmem/reallocmem

  Revision 1.20  1999/10/22 22:03:07  sg
  * FreeMem(p) is ignored if p is NIL, instead of throwing an
    runtime error 204. (Delphi ignores NIL FreeMem's, too)

  Revision 1.19  1999/10/01 07:55:54  peter
    * fixed memsize which forgot the sizemask

  Revision 1.18  1999/09/22 21:59:02  peter
    * best match for main freelist
    * removed root field, saves 4 bytes per block
    * fixed crash in dumpblocks

  Revision 1.17  1999/09/20 14:17:37  peter
    * fixed growheap freelist addition when heapend-heapptr<blocksize

  Revision 1.16  1999/09/17 17:14:12  peter
    + new heap manager supporting delphi freemem(pointer)

}
