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
{ define SMALLATHEAPPTR}

{ Try to find the best matching block in general freelist }
{ define BESTMATCH}

{ Concat free blocks when placing big blocks in the mainlist }
{$define CONCATFREE}

{ DEBUG: Dump info when the heap needs to grow }
{ define DUMPGROW}

{ DEBUG: Test the FreeList on correctness }
{$ifdef SYSTEMDEBUG}
{$define TestFreeLists}
{$endif SYSTEMDEBUG}

{$ifdef MT}
var
   cs_systemheap : TRTLCriticalSection;
{$endif MT}

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
    GetMem: @SysGetMem;
    FreeMem: @SysFreeMem;
    FreeMemSize: @SysFreeMemSize;
    AllocMem: @SysAllocMem;
    ReAllocMem: @SysReAllocMem;
    MemSize: @SysMemSize;
    MemAvail: @SysMemAvail;
    MaxAvail: @SysMaxAvail;
    HeapSize: @SysHeapSize;
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
{$ifdef SYSTEMDEBUG}
  tfreecount   = array[0..maxblock] of dword;
{$endif SYSTEMDEBUG}
  pfreelists   = ^tfreelists;

var
  internal_memavail  : longint;
  internal_heapsize  : longint;
  freelists          : tfreelists;
{$ifdef SYSTEMDEBUG}
  freecount : tfreecount;
{$endif SYSTEMDEBUG}
{$ifdef TestFreeLists}
{ this can be turned on by debugger }
const
  test_each : boolean = false;
{$endif TestFreeLists}

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

procedure GetMemoryManager(var MemMgr:TMemoryManager);
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MemMgr:=MemoryManager;
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MemMgr:=MemoryManager;
   end;
end;


procedure SetMemoryManager(const MemMgr:TMemoryManager);
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MemoryManager:=MemMgr;
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MemoryManager:=MemMgr;
   end;
end;


function IsMemoryManagerSet:Boolean;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       IsMemoryManagerSet:=(MemoryManager.GetMem<>@SysGetMem) or
                           (MemoryManager.FreeMem<>@SysFreeMem);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     IsMemoryManagerSet:=(MemoryManager.GetMem<>@SysGetMem) or
                         (MemoryManager.FreeMem<>@SysFreeMem);
   end;
end;


procedure GetMem(Var p:pointer;Size:Longint);
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       p:=MemoryManager.GetMem(Size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     p:=MemoryManager.GetMem(Size);
   end;
end;


procedure FreeMem(p:pointer;Size:Longint);
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MemoryManager.FreeMemSize(p,Size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MemoryManager.FreeMemSize(p,Size);
   end;
end;


function MaxAvail:Longint;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MaxAvail:=MemoryManager.MaxAvail();
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MaxAvail:=MemoryManager.MaxAvail();
   end;
end;


function MemAvail:Longint;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MemAvail:=MemoryManager.MemAvail();
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MemAvail:=MemoryManager.MemAvail();
   end;
end;


{ FPC Additions }
function HeapSize:Longint;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       HeapSize:=MemoryManager.HeapSize();
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     HeapSize:=MemoryManager.HeapSize();
   end;
end;


function MemSize(p:pointer):Longint;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       MemSize:=MemoryManager.MemSize(p);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     MemSize:=MemoryManager.MemSize(p);
   end;
end;


{ Delphi style }
function FreeMem(p:pointer):Longint;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       Freemem:=MemoryManager.FreeMem(p);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     Freemem:=MemoryManager.FreeMem(p);
   end;
end;


function GetMem(size:longint):pointer;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       GetMem:=MemoryManager.GetMem(Size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     GetMem:=MemoryManager.GetMem(Size);
   end;
end;


function AllocMem(Size:Longint):pointer;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       AllocMem:=MemoryManager.AllocMem(size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     AllocMem:=MemoryManager.AllocMem(size);
   end;
end;


function ReAllocMem(var p:pointer;Size:Longint):pointer;
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       ReAllocMem:=MemoryManager.ReAllocMem(p,size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     ReAllocMem:=MemoryManager.ReAllocMem(p,size);
   end;
end;


{$ifdef ValueGetmem}

{ Needed for calls from Assembler }
function fpc_getmem(size:longint):pointer;compilerproc;[public,alias:'FPC_GETMEM'];
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       fpc_GetMem:=MemoryManager.GetMem(size);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     fpc_GetMem:=MemoryManager.GetMem(size);
   end;
end;

{$else ValueGetmem}

{ Needed for calls from Assembler }
procedure AsmGetMem(var p:pointer;size:longint);[public,alias:'FPC_GETMEM'];
begin
  p:=MemoryManager.GetMem(size);
end;

{$endif ValueGetmem}

{$ifdef ValueFreemem}

procedure fpc_freemem(p:pointer);compilerproc;[public,alias:'FPC_FREEMEM'];
begin
{$ifdef MT}
  if IsMultiThread then
   begin
     try
       EnterCriticalSection(cs_systemheap);
       if p <> nil then
         MemoryManager.FreeMem(p);
     finally
       LeaveCriticalSection(cs_systemheap);
     end;
   end
  else
{$endif MT}
   begin
     if p <> nil then
       MemoryManager.FreeMem(p);
   end;
end;

{$else ValueFreemem}

procedure AsmFreeMem(var p:pointer);[public,alias:'FPC_FREEMEM'];
begin
  if p <> nil then
    MemoryManager.FreeMem(p);
end;

{$endif ValueFreemem}


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


{$ifdef TestFreeLists}
procedure TestFreeLists;
var
  i,j : longint;
  hp  : pfreerecord;
begin
  for i:=0 to maxblock do
   begin
     j:=0;
     hp:=freelists[i];
     while assigned(hp) do
      begin
        inc(j);
        if (i>0) and ((hp^.size and sizemask) <> i * blocksize) then
          RunError(204);
        hp:=hp^.next;
      end;
      if j<>freecount[i] then
        RunError(204);
    end;
end;
{$endif TestFreeLists}


{$ifdef CONCATFREE}
{*****************************************************************************
                         Try concat freerecords
*****************************************************************************}

procedure TryConcatFreeRecord(pcurr:pfreerecord);
var
  hp : pfreerecord;
  pcurrsize,s1 : longint;
begin
  pcurrsize:=pcurr^.size and sizemask;
  hp:=pcurr;
  repeat
    { block used or before a heapend ? }
    if (hp^.size and beforeheapendmask)<>0 then
     begin
       { Peter, why can't we add this one if free ?? }
       { It's already added in the previous iteration, we only go to the }
       { next heap record after this check (JM)                          }
       pcurr^.size:=pcurrsize or beforeheapendmask;
       break;
     end;
    { get next block }
    hp:=pfreerecord(pointer(hp)+(hp^.size and sizemask));
    { when we're at heapptr then we can stop and set heapptr to pcurr }
    if (hp=heapptr) then
     begin
       heapptr:=pcurr;
       { remove the block }
       if assigned(pcurr^.next) then
         pcurr^.next^.prev := pcurr^.prev;
       if assigned(pcurr^.prev) then
         pcurr^.prev^.next := pcurr^.next
       else
         freelists[0] := pcurr^.next;
{$ifdef SYSTEMDEBUG}
       dec(freecount[0]);
{$endif SYSTEMDEBUG}
       break;
     end;
    { block is used? then we stop and add the block to the freelist }
    if (hp^.size and usedmask)<>0 then
     begin
       pcurr^.size:=pcurrsize;
       break;
     end;
    { remove block from freelist and increase the size }
    s1:=hp^.size and sizemask;
    inc(pcurrsize,s1);
    s1:=s1 shr blockshr;
    if s1>maxblock then
     s1:=0;
    if assigned(hp^.next) then
     hp^.next^.prev:=hp^.prev;
    if assigned(hp^.prev) then
     hp^.prev^.next:=hp^.next
    else
     freelists[s1]:=hp^.next;
{$ifdef SYSTEMDEBUG}
    dec(freecount[s1]);
{$endif SYSTEMDEBUG}
  until false;
end;
{$endif CONCATFREE}

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
{$ifdef SYSTEMDEBUG}
        dec(freecount[s]);
{$endif SYSTEMDEBUG}
        if assigned(freelists[s]) then
         freelists[s]^.prev:=nil;
{$ifdef TestFreeLists}
        if test_each then
         TestFreeLists;
{$endif TestFreeLists}
        exit;
      end;
{$ifdef SMALLATHEAPPTR}
     if heapend-heapptr>=size then
      begin
        sysgetmem:=heapptr;
        { set end flag if we do not have enough room to add
          another tfreerecord behind }
        if (heapptr+size+sizeof(tfreerecord)>=heapend) then
         pheaprecord(sysgetmem)^.size:=size or (usedmask or beforeheapendmask)
        else
         pheaprecord(sysgetmem)^.size:=size or usedmask;
        inc(sysgetmem,sizeof(theaprecord));
        inc(heapptr,size);
{$ifdef TestFreeLists}
        if test_each then
         TestFreeLists;
{$endif TestFreeLists}
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
            end
         end;
{$else BESTMATCH}
{$ifdef CONCATFREE}
        TryConcatFreeRecord(pcurr);
        if (pcurr <> heapptr) then
          begin
            if pcurr^.size>=size then
              break;
          end
        else
          begin
            pcurr := nil;
            break;
          end;
{$else CONCATFREE}
        if pcurr^.size>=size then
          break;
{$endif CONCATFREE}
{$endif BESTMATCH}
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
{$ifdef SYSTEMDEBUG}
     dec(freecount[s]);
{$endif SYSTEMDEBUG}
     { create the left over freelist block, if at least 16 bytes are free }
     sizeleft:=pcurr^.size-size;
     if sizeleft>=sizeof(tfreerecord) then
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
{$ifdef SYSTEMDEBUG}
        inc(freecount[s1]);
{$endif SYSTEMDEBUG}
        { create the block we need to return }
        pheaprecord(sysgetmem)^.size:=size or usedmask;
      end
     else
      begin
        { create the block we need to return }
        pheaprecord(sysgetmem)^.size:=size or usedmask or (pheaprecord(sysgetmem)^.size and beforeheapendmask);
      end;

     inc(sysgetmem,sizeof(theaprecord));
{$ifdef TestFreeLists}
     if test_each then
      TestFreeLists;
{$endif TestFreeLists}
     exit;
   end;
  { Lastly, the top of the heap is checked, to see if there is }
  { still memory available.                                   }
  repeat
    again:=false;
    if heapend-heapptr>=size then
     begin
       sysgetmem:=heapptr;
       if (heapptr+size+sizeof(tfreerecord)>=heapend) then
        pheaprecord(sysgetmem)^.size:=size or (usedmask or beforeheapendmask)
       else
        pheaprecord(sysgetmem)^.size:=size or usedmask;
       inc(sysgetmem,sizeof(theaprecord));
       inc(heapptr,size);
{$ifdef TestFreeLists}
       if test_each then
        TestFreeLists;
{$endif TestFreeLists}
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
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                               SysFreeMem
*****************************************************************************}

Function SysFreeMem(p : pointer):Longint;
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
{$ifdef SYSTEMDEBUG}
  inc(freecount[s]);
{$endif SYSTEMDEBUG}
  SysFreeMem:=pcurrsize;
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                              SysFreeMemSize
*****************************************************************************}

Function SysFreeMemSize(p : pointer;size : longint):longint;
var
  pcurrsize,s : longint;
  pcurr : pfreerecord;
begin
  SysFreeMemSize:=0;
  if size<=0 then
   begin
     if size<0 then
      HandleError(204);
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
{ set the return values }
  s:=pcurrsize shr blockshr;
  if s>maxblock then
   s:=0;
  pcurr^.next:=freelists[s];
  if assigned(pcurr^.next) then
   pcurr^.next^.prev:=pcurr;
  freelists[s]:=pcurr;
{$ifdef SYSTEMDEBUG}
  inc(freecount[s]);
{$endif SYSTEMDEBUG}
  SysFreeMemSize:=pcurrsize;
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                                 SysMemSize
*****************************************************************************}

function SysMemSize(p:pointer):longint;
begin
{$ifdef MT}
  try
    EnterCriticalSection(cs_systemheap);
{$endif MT}
  SysMemSize:=(pheaprecord(pointer(p)-sizeof(theaprecord))^.size and sizemask)-sizeof(theaprecord);
{$ifdef MT}
  finally
    LeaveCriticalSection(cs_systemheap);
  end;
{$endif MT}
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
{$ifdef TestFreeLists}
     if test_each then
      TestFreeLists;
{$endif TestFreeLists}
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
             if (heapend-heapptr)<sizeof(tfreerecord) then
              wasbeforeheapend:=true;
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
{$ifdef SYSTEMDEBUG}
           dec(freecount[s]);
{$endif SYSTEMDEBUG}
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
{$ifdef TestFreeLists}
        if test_each then
         TestFreeLists;
{$endif TestFreeLists}
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
{$ifdef SYSTEMDEBUG}
        inc(freecount[s]);
{$endif SYSTEMDEBUG}
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
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysReAllocMem(var p:pointer;size : longint):pointer;
var
  oldsize : longint;
  p2 : pointer;
begin
  { Free block? }
  if size=0 then
   begin
     if p<>nil then
      begin
        MemoryManager.FreeMem(p);
        p:=nil;
      end;
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
      oldsize:=MemoryManager.MemSize(p);
      p2:=MemoryManager.GetMem(size);
      if p2<>nil then
       Move(p^,p2^,oldsize);
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
  sizeleft,s1,
  NewPos    : longint;
  pcurr     : pfreerecord;
begin
{$ifdef DUMPGROW}
  writeln('growheap(',size,')  allocating ',(size+$ffff) and $ffff0000);
  DumpBlocks;
{$endif}
  { Allocate by 64K size }
  size:=(size+$ffff) and $ffff0000;
  { first try 256K (default) }
  if size<=GrowHeapSize1 then
   begin
     NewPos:=Sbrk(GrowHeapSize1);
     if NewPos<>-1 then
      size:=GrowHeapSize1;
   end
  else
  { second try 1024K (default) }
   if size<=GrowHeapSize2 then
    begin
      NewPos:=Sbrk(GrowHeapSize2);
      if NewPos<>-1 then
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
        if ReturnNilIfGrowHeapFails then
          GrowHeap:=1
        else
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
     if sizeleft>=sizeof(tfreerecord) then
      begin
        pcurr:=pfreerecord(heapptr);
        pcurr^.size:=sizeleft or beforeheapendmask;
        { insert the block in the freelist }
        s1:=sizeleft shr blockshr;
        if s1>maxblock then
         s1:=0;
        pcurr^.next:=freelists[s1];
        pcurr^.prev:=nil;
        if assigned(freelists[s1]) then
         freelists[s1]^.prev:=pcurr;
        freelists[s1]:=pcurr;
{$ifdef SYSTEMDEBUG}
        inc(freecount[s1]);
{$endif SYSTEMDEBUG}
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
{$ifdef TestFreeLists}
  TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeap;
begin
  FillChar(FreeLists,sizeof(TFreeLists),0);
{$ifdef SYSTEMDEBUG}
  FillChar(FreeCount,sizeof(TFreeCount),0);
{$endif SYSTEMDEBUG}
  internal_heapsize:=GetHeapSize;
  internal_memavail:=internal_heapsize;
  HeapOrg:=GetHeapStart;
  HeapPtr:=HeapOrg;
  HeapEnd:=HeapOrg+internal_memavail;
  HeapError:=@GrowHeap;
{$ifdef MT}
  InitCriticalSection(cs_systemheap);
{$endif MT}
end;

{
  $Log$
  Revision 1.15  2002-09-07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.14  2002/06/17 08:33:04  jonas
    * heap manager now fragments the heap much less

  Revision 1.13  2002/04/21 18:56:59  peter
    * fpc_freemem and fpc_getmem compilerproc

  Revision 1.12  2002/02/10 15:33:45  carl
  * fixed some missing IsMultiThreaded variables

  Revision 1.11  2002/01/02 13:43:09  jonas
    * fix for web bug 1727 from Peter (corrected)

}

