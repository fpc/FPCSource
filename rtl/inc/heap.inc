{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-99 by the Free Pascal development team.

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

{ Dump info when the heap needs to grow }
{ define DUMPGROW}

{ Default heap settings }
const
  blocksize    = 16;  { at least size of freerecord }
  blockshr     = 4;   { shr value for blocksize=2^blockshr}
  maxblocksize = 1024+blocksize; { 1024+8 needed for heaprecord }
  maxblock     = maxblocksize div blocksize;
  maxreusebigger = 8; { max reuse bigger tries }

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
    MemSize: SysMemSize
  );

type
  ppfreerecord = ^pfreerecord;
  pfreerecord  = ^tfreerecord;
  tfreerecord  = record
    size  : longint;
    root  : ppfreerecord;
    next,
    prev  : pfreerecord;
  end; { 16 bytes }

  pheaprecord = ^theaprecord;
  theaprecord = record
  { this should overlap with tfreerecord }
    size  : longint;
    root  : ppfreerecord;
  end; { 8 bytes }

  tfreelists   = array[0..maxblock] of pfreerecord;
  pfreelists   = ^tfreelists;

var
  internal_memavail  : longint;
  internal_heapsize  : longint;
  freelists          : tfreelists;
  checkfreememsize   : boolean;

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


procedure GetMem(Var p:pointer;Size:Longint);[public,alias:'FPC_GETMEM'];
begin
  MemoryManager.GetMem(p,Size);
end;


procedure FreeMem(Var p:pointer);
begin
  MemoryManager.FreeMem(p);
end;


procedure FreeMem(Var p:pointer;Size:Longint);[public,alias:'FPC_FREEMEM'];
begin
  MemoryManager.FreeMemSize(p,Size);
end;


function MemSize(p:pointer):Longint;
begin
  MemSize:=MemoryManager.MemSize(p);
end;


{ Needed for calls from Assembler }
procedure AsmFreeMem(Var p:pointer);
begin
  MemoryManager.FreeMem(p);
end;


{*****************************************************************************
                       Heapsize,Memavail,MaxAvail
*****************************************************************************}

function heapsize : longint;
begin
  heapsize:=internal_heapsize;
end;


function memavail : longint;
begin
  memavail:=internal_memavail;
end;


function maxavail : longint;
var
  hp : pfreerecord;
begin
  maxavail:=heapend-heapptr;
  hp:=freelists[0];
  while assigned(hp) do
   begin
     if hp^.size>maxavail then
       maxavail:=hp^.size;
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
        hp:=hp^.next;
        inc(j);
      end;
     writeln('Block ',i*blocksize,': ',j);
   end;
{ freelist 0 }
  hp:=freelists[0];
  j:=0;
  s:=0;
  while assigned(hp) do
   begin
     hp:=hp^.next;
     inc(j);
     if hp^.size>s then
      s:=hp^.size;
   end;
  writeln('Main: ',j,' maxsize: ',s);
end;
{$endif}


{*****************************************************************************
                                 SysGetMem
*****************************************************************************}

procedure SysGetMem(var p : pointer;size : longint);
type
  heaperrorproc=function(size:longint):integer;
var
  proc  : heaperrorproc;
  pcurr : pfreerecord;
  again : boolean;
  heapfree,
  s,s1,i,
  sizeleft : longint;
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
  pcurr:=nil;
  s:=size shr blockshr;
  if s<=maxblock then
   begin
     { correct size match ? }
     if assigned(freelists[s]) then
      begin
        { create the block we should return }
        p:=pointer(freelists[s])+sizeof(theaprecord);
        { update freelist }
        freelists[s]:=freelists[s]^.next;
        if assigned(freelists[s]) then
         freelists[s]^.prev:=nil;
        exit;
      end;
{$ifdef REUSEBIGGER}
     { try a bigger block }
     s1:=s+s;
     i:=0;
     while (s1<=maxblock) and (i<maxreusebigger) do
      begin
        if assigned(freelists[s1]) then
         begin
           pcurr:=freelists[s1];
           break;
         end;
        inc(s1,s);
        inc(i);
      end;
{$endif}
   end
  else
   s:=0;
  repeat
    { not found, then check the main freelist for the first match }
    heapfree:=heapend-heapptr;
    if not(assigned(pcurr)) and
       ((size>maxblocksize) or (heapfree<size)) then
     begin
       pcurr:=freelists[0];
       while assigned(pcurr) do
        begin
          if pcurr^.size>=size then
           break;
          pcurr:=pcurr^.next;
        end;
     end;
    { have we found a block, then get it and free up the other left part,
      if no blocks are found then allocated at the heapptr or grow the heap }
    if assigned(pcurr) then
     begin
       { get pointer of the block we should return }
       p:=pointer(pcurr);
       { remove the current block from the freelist }
       if assigned(pcurr^.next) then
        pcurr^.next^.prev:=pcurr^.prev;
       if assigned(pcurr^.prev) then
        pcurr^.prev^.next:=pcurr^.next
       else
        pcurr^.root^:=pcurr^.next;
       { create the left over freelist block, if at least 16 bytes are free }
       sizeleft:=pcurr^.size-size;
       s1:=sizeleft shr blockshr;
       if s1>0 then
        begin
          if s1>maxblock then
           s1:=0;
          pcurr:=pfreerecord(pointer(pcurr)+size);
          pcurr^.size:=sizeleft;
          pcurr^.root:=@freelists[s1];
          { insert the block in the freelist }
          pcurr^.next:=freelists[s1];
          pcurr^.prev:=nil;
          if assigned(freelists[s1]) then
           freelists[s1]^.prev:=pcurr;
          freelists[s1]:=pcurr;
        end;
       { create the block we need to return }
       pheaprecord(p)^.size:=size;
       pheaprecord(p)^.root:=@freelists[s];
       inc(p,sizeof(theaprecord));
       exit;
     end;
    { Lastly, the top of the heap is checked, to see if there is }
    { still memory available.                                   }
    again:=false;
    if heapfree<size then
     begin
       if assigned(heaperror) then
        begin
          proc:=heaperrorproc(heaperror);
          case proc(size) of
           0 : HandleError(203);
           1 : p:=nil;
           2 : again:=true;
          end;
        end
       else
        HandleError(203);
     end
    else
     begin
       p:=heapptr;
       pheaprecord(p)^.size:=size;
       pheaprecord(p)^.root:=@freelists[s];
       inc(p,sizeof(theaprecord));
       inc(heapptr,size);
     end;
  until not again;
end;


{*****************************************************************************
                               SysFreeMem
*****************************************************************************}

procedure SysFreeMem(var p : pointer);
begin
  if p=nil then
   HandleError(204);
{ fix p to point to the heaprecord }
  dec(p,sizeof(theaprecord));
  inc(internal_memavail,pheaprecord(p)^.size);
{ insert the block in it's freelist }
  pfreerecord(p)^.prev:=nil;
  pfreerecord(p)^.next:=pfreerecord(p)^.root^;
  if assigned(pfreerecord(p)^.next) then
   pfreerecord(p)^.next^.prev:=pfreerecord(p);
  pfreerecord(p)^.root^:=pfreerecord(p);
  p:=nil;
end;


procedure SysFreeMemSize(var p : pointer;size : longint);
begin
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
  dec(p,sizeof(theaprecord));
  inc(internal_memavail,pheaprecord(p)^.size);
{ size check }
  if checkfreememsize and (size<>-1) then
   begin
     size:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
     if size<>pheaprecord(p)^.size then
      HandleError(204);
   end;
{ insert the block in it's freelist }
  pfreerecord(p)^.prev:=nil;
  pfreerecord(p)^.next:=pfreerecord(p)^.root^;
  if assigned(pfreerecord(p)^.next) then
   pfreerecord(p)^.next^.prev:=pfreerecord(p);
  pfreerecord(p)^.root^:=pfreerecord(p);
  p:=nil;
end;


{*****************************************************************************
                                    MemSize
*****************************************************************************}

function SysMemSize(p:pointer):longint;
begin
  SysMemSize:=pheaprecord(pointer(p)-sizeof(theaprecord))^.size-sizeof(theaprecord);
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
  NewPos,
  wantedsize : longint;
  pcurr      : pfreerecord;
begin
{$ifdef DUMPGROW}
  writeln('grow ',size);
  DumpBlocks;
{$endif}
  wantedsize:=size;
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
     if heapend-heapptr>blocksize then
      begin
        pcurr:=pfreerecord(heapptr);
        pcurr^.size:=heapend-heapptr;
        pcurr^.root:=@freelists[0];
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
  Revision 1.17  1999-09-20 14:17:37  peter
    * fixed growheap freelist addition when heapend-heapptr<blocksize

  Revision 1.16  1999/09/17 17:14:12  peter
    + new heap manager supporting delphi freemem(pointer)

}
