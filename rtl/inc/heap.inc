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

{ Try to find the best matching block in general freelist }
{ define BESTMATCH}

{ DEBUG: Dump info when the heap needs to grow }
{ define DUMPGROW}

{ DEBUG: Test the FreeList on correctness }

{$ifdef SYSTEMDEBUG}
{$define TestFreeLists}
{$endif SYSTEMDEBUG}

const
{$ifdef CPU64}
  blocksize    = 32;  { at least size of freerecord }
  blockshr     = 5;   { shr value for blocksize=2^blockshr}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
{$else}
  blocksize    = 16;  { at least size of freerecord }
  blockshr     = 4;   { shr value for blocksize=2^blockshr}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
{$endif}
  maxblockindex = maxblocksize div blocksize; { highest index in array of lists of memchunks }
  maxreusebigger = 8; { max reuse bigger tries }

  usedflag = 1;        { flag if the block is used or not }
  lastblockflag = 2;   { flag if the block is the last in os chunk }
  firstblockflag = 4;  { flag if the block is the first in os chunk }
  fixedsizeflag = 8;   { flag if the block is of fixed size }
  sizemask = not(blocksize-1);
  fixedsizemask = sizemask and $ffff;

{****************************************************************************}

{$ifdef DUMPGROW}
  {$define DUMPBLOCKS}
{$endif}

{ Forward defines }
procedure SysHeapMutexInit;forward;
procedure SysHeapMutexDone;forward;
procedure SysHeapMutexLock;forward;
procedure SysHeapMutexUnlock;forward;

{ Memory manager }
const
  MemoryManager: TMemoryManager = (
    NeedLock: true;
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

  MemoryMutexManager: TMemoryMutexManager = (
    MutexInit: @SysHeapMutexInit;
    MutexDone: @SysHeapMutexDone;
    MutexLock: @SysHeapMutexLock;
    MutexUnlock: @SysHeapMutexUnlock;
  );

type
  pmemchunk_fixed  = ^tmemchunk_fixed;
  tmemchunk_fixed = record
{$ifdef cpusparc}
    { Sparc needs to alloc aligned on 8 bytes, to allow doubles }
    _dummy : ptrint;
{$endif cpusparc}
    size  : ptrint;
    next_fixed,
    prev_fixed : pmemchunk_fixed;
  end;

  pmemchunk_var  = ^tmemchunk_var;
  tmemchunk_var = record
    prevsize : ptrint;
    size  : ptrint;
    next_var,
    prev_var  : pmemchunk_var;
  end;

  { ``header'', ie. size of structure valid when chunk is in use }
  { should correspond to tmemchunk_var_hdr structure starting with the
    last field. Reason is that the overlap is starting from the end of the
    record. }
  tmemchunk_fixed_hdr = record
{$ifdef cpusparc}
    { Sparc needs to alloc aligned on 8 bytes, to allow doubles }
    _dummy : ptrint;
{$endif cpusparc}
    size : ptrint;
  end;
  tmemchunk_var_hdr = record
    prevsize : ptrint;
    size : ptrint;
  end;

  poschunk = ^toschunk;
  toschunk = record
    size : ptrint;
    next,
    prev : poschunk;
    used : ptrint;
  end;

  tfreelists   = array[1..maxblockindex] of pmemchunk_fixed;
  pfreelists   = ^tfreelists;

var
  internal_memavail  : ptrint;
  internal_heapsize  : ptrint;
  freelists_fixed    : tfreelists;
  freelist_var       : pmemchunk_var;
  freeoslist         : poschunk;
  freeoslistcount    : dword;

{$ifdef TestFreeLists}
{ this can be turned on by debugger }
const
  test_each : boolean = false;
{$endif TestFreeLists}

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

procedure SetMemoryMutexManager(var MutexMgr: TMemoryMutexManager);
begin
  { Release old mutexmanager, the default manager does nothing so
    calling this without initializing is safe }
  MemoryMutexManager.MutexDone;
  { Copy new mutexmanager }
  MemoryMutexManager := MutexMgr;
  { Init new mutexmanager }
  MemoryMutexManager.MutexInit;
end;


procedure GetMemoryManager(var MemMgr:TMemoryManager);
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemMgr := MemoryManager;
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemMgr := MemoryManager;
   end;
end;


procedure SetMemoryManager(const MemMgr:TMemoryManager);
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemoryManager := MemMgr;
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemoryManager := MemMgr;
   end;
end;


function IsMemoryManagerSet:Boolean;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       IsMemoryManagerSet := (MemoryManager.GetMem<>@SysGetMem) or
                           (MemoryManager.FreeMem<>@SysFreeMem);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     IsMemoryManagerSet := (MemoryManager.GetMem<>@SysGetMem) or
                         (MemoryManager.FreeMem<>@SysFreeMem);
   end;
end;


procedure GetMem(Var p:pointer;Size:ptrint);
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       p := MemoryManager.GetMem(Size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     p := MemoryManager.GetMem(Size);
   end;
end;

procedure GetMemory(Var p:pointer;Size:ptrint);
begin
  GetMem(p,size);
end;

procedure FreeMem(p:pointer;Size:ptrint);
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemoryManager.FreeMemSize(p,Size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemoryManager.FreeMemSize(p,Size);
   end;
end;

procedure FreeMemory(p:pointer;Size:ptrint);
begin
  FreeMem(p,size);
end;

function MaxAvail:ptrint;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MaxAvail := MemoryManager.MaxAvail();
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MaxAvail := MemoryManager.MaxAvail();
   end;
end;


function MemAvail:ptrint;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemAvail := MemoryManager.MemAvail();
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemAvail := MemoryManager.MemAvail();
   end;
end;


{ FPC Additions }
function HeapSize:ptrint;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       HeapSize := MemoryManager.HeapSize();
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     HeapSize := MemoryManager.HeapSize();
   end;
end;


function MemSize(p:pointer):ptrint;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemSize := MemoryManager.MemSize(p);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemSize := MemoryManager.MemSize(p);
   end;
end;


{ Delphi style }
function FreeMem(p:pointer):ptrint;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       Freemem := MemoryManager.FreeMem(p);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     Freemem := MemoryManager.FreeMem(p);
   end;
end;

function FreeMemory(p:pointer):ptrint;

begin
 FreeMemory := FreeMem(p);
end;

function GetMem(size:ptrint):pointer;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       GetMem := MemoryManager.GetMem(Size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     GetMem := MemoryManager.GetMem(Size);
   end;
end;

function GetMemory(size:ptrint):pointer;

begin
 GetMemory := Getmem(size);
end;

function AllocMem(Size:ptrint):pointer;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       AllocMem := MemoryManager.AllocMem(size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     AllocMem := MemoryManager.AllocMem(size);
   end;
end;


function ReAllocMem(var p:pointer;Size:ptrint):pointer;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       ReAllocMem := MemoryManager.ReAllocMem(p,size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     ReAllocMem := MemoryManager.ReAllocMem(p,size);
   end;
end;


function ReAllocMemory(var p:pointer;Size:ptrint):pointer;

begin
 ReAllocMemory := ReAllocMem(p,size);
end;

{$ifdef ValueGetmem}

{ Needed for calls from Assembler }
function fpc_getmem(size:ptrint):pointer;compilerproc;[public,alias:'FPC_GETMEM'];
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       fpc_GetMem := MemoryManager.GetMem(size);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     fpc_GetMem := MemoryManager.GetMem(size);
   end;
end;

{$else ValueGetmem}

{ Needed for calls from Assembler }
procedure AsmGetMem(var p:pointer;size:ptrint);[public,alias:'FPC_GETMEM'];
begin
  p := MemoryManager.GetMem(size);
end;

{$endif ValueGetmem}

{$ifdef ValueFreemem}

procedure fpc_freemem(p:pointer);compilerproc;[public,alias:'FPC_FREEMEM'];
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       if p <> nil then
         MemoryManager.FreeMem(p);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
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

function SysHeapsize : ptrint;
begin
  Sysheapsize := internal_heapsize;
end;


function SysMemavail : ptrint;
begin
  Sysmemavail := internal_memavail;
end;


function SysMaxavail: ptrint;
var
  pmc : pmemchunk_var;
  i: longint;
begin
  pmc := freelist_var;
  sysmaxavail := 0;
  while assigned(pmc) do
    begin
      if pmc^.size>sysmaxavail then
        sysmaxavail := pmc^.size;
      pmc := pmc^.next_var;
    end;
  if sysmaxavail = 0 then
    begin
      for i := maxblockindex downto 1 do
        if assigned(freelists_fixed[i]) then
          begin
            sysmaxavail := i shl blockshr;
            exit;
          end;
    end;
end;


{$ifdef DUMPBLOCKS}   // TODO
procedure DumpBlocks;
var
  s,i,j : ptrint;
  hp  : pfreerecord;
begin
  for i := 1 to maxblock do
   begin
     hp := freelists[i];
     j := 0;
     while assigned(hp) do
      begin
        inc(j);
        hp := hp^.next;
      end;
     writeln('Block ',i*blocksize,': ',j);
   end;
{ freelist 0 }
  hp := freelists[0];
  j := 0;
  s := 0;
  while assigned(hp) do
   begin
     inc(j);
     if hp^.size>s then
      s := hp^.size;
     hp := hp^.next;
   end;
  writeln('Main: ',j,' maxsize: ',s);
end;
{$endif}


{$ifdef TestFreeLists}
procedure TestFreeLists;
var
  i,j : ptrint;
  mc  : pmemchunk_fixed;
begin
  for i := 1 to maxblockindex do
   begin
    j := 0;
    mc := freelists_fixed[i];
    while assigned(mc) do
      begin
        inc(j);
      if ((mc^.size and fixedsizemask) <> i * blocksize) then
          RunError(204);
      mc := mc^.next_fixed;
      end;
    end;
end;
{$endif TestFreeLists}

{*****************************************************************************
                                List adding/removal
*****************************************************************************}

procedure append_to_list_fixed(blockindex: ptrint; pmc: pmemchunk_fixed);
begin
  pmc^.prev_fixed := nil;
  pmc^.next_fixed := freelists_fixed[blockindex];
  if freelists_fixed[blockindex]<>nil then
    freelists_fixed[blockindex]^.prev_fixed := pmc;
  freelists_fixed[blockindex] := pmc;
end;

procedure append_to_list_var(pmc: pmemchunk_var);
begin
  pmc^.prev_var := nil;
  pmc^.next_var := freelist_var;
  if freelist_var<>nil then
    freelist_var^.prev_var := pmc;
  freelist_var := pmc;
end;

procedure remove_from_list_fixed(blockindex: ptrint; pmc: pmemchunk_fixed);
begin
  if assigned(pmc^.next_fixed) then
    pmc^.next_fixed^.prev_fixed := pmc^.prev_fixed;
  if assigned(pmc^.prev_fixed) then
    pmc^.prev_fixed^.next_fixed := pmc^.next_fixed
  else
    freelists_fixed[blockindex] := pmc^.next_fixed;
end;

procedure remove_from_list_var(pmc: pmemchunk_var);
begin
  if assigned(pmc^.next_var) then
    pmc^.next_var^.prev_var := pmc^.prev_var;
  if assigned(pmc^.prev_var) then
    pmc^.prev_var^.next_var := pmc^.next_var
  else
    freelist_var := pmc^.next_var;
end;

procedure append_to_oslist(poc: poschunk);
begin
  { decide whether to free block or add to list }
{$ifdef HAS_SYSOSFREE}
  if freeoslistcount >= 3 then
    begin
      dec(internal_heapsize, poc^.size);
      dec(internal_memavail, poc^.size);
      SysOSFree(poc, poc^.size);
    end
  else
    begin
{$endif}
      poc^.prev := nil;
      poc^.next := freeoslist;
      if freeoslist <> nil then
        freeoslist^.prev := poc;
      freeoslist := poc;
      inc(freeoslistcount);
{$ifdef HAS_SYSOSFREE}
   end;
{$endif}
end;

procedure remove_from_oslist(poc: poschunk);
begin
  if assigned(poc^.next) then
    poc^.next^.prev := poc^.prev;
  if assigned(poc^.prev) then
    poc^.prev^.next := poc^.next
  else
    freeoslist := poc^.next;
  dec(freeoslistcount);
end;

procedure append_to_oslist_var(pmc: pmemchunk_var);
var
  poc: poschunk;
begin
  // block eligable for freeing
  poc := pointer(pmc)-sizeof(toschunk);
  remove_from_list_var(pmc);
  append_to_oslist(poc);
end;

procedure append_to_oslist_fixed(blockindex, chunksize: ptrint; poc: poschunk);
var
  pmc: pmemchunk_fixed;
  i, count: ptrint;
begin
  count := (poc^.size - sizeof(toschunk)) div chunksize;
  pmc := pmemchunk_fixed(pointer(poc)+sizeof(toschunk));
  for i := 0 to count - 1 do
    begin
      remove_from_list_fixed(blockindex, pmc);
      pmc := pointer(pmc)+chunksize;
    end;
  append_to_oslist(poc);
end;

{*****************************************************************************
                         Split block
*****************************************************************************}

procedure split_block(pcurr: pmemchunk_var; size: ptrint);
var
  pcurr_tmp : pmemchunk_var;
  sizeleft: ptrint;
begin
  sizeleft := (pcurr^.size and sizemask)-size;
  if sizeleft>=blocksize then
    begin
      pcurr_tmp := pmemchunk_var(pointer(pcurr)+size);
      { update prevsize of block to the right }
      if (pcurr^.size and lastblockflag) = 0 then
        pmemchunk_var(pointer(pcurr)+(pcurr^.size and sizemask))^.prevsize := sizeleft;
      { inherit the lastblockflag }
      pcurr_tmp^.size := sizeleft or (pcurr^.size and lastblockflag);
      pcurr_tmp^.prevsize := size;
      { the block we return is not the last one anymore (there's now a block after it) }
      { decrease size of block to new size }
      pcurr^.size := size or (pcurr^.size and (not sizemask and not lastblockflag));
      { insert the block in the freelist }
      append_to_list_var(pcurr_tmp);
    end;
end;

{*****************************************************************************
                         Try concat freerecords
*****************************************************************************}

procedure concat_two_blocks(mc_left, mc_right: pmemchunk_var);
var
  mc_tmp : pmemchunk_var;
  size_right : ptrint;
begin
  // left block free, concat with right-block
  size_right := mc_right^.size and sizemask;
  inc(mc_left^.size, size_right);
  // if right-block was last block, copy flag
  if (mc_right^.size and lastblockflag) <> 0 then
    begin
      mc_left^.size := mc_left^.size or lastblockflag;
    end
  else
    begin
      // there is a block to the right of the right-block, adjust it's prevsize
      mc_tmp := pmemchunk_var(pointer(mc_right)+size_right);
      mc_tmp^.prevsize := mc_left^.size and sizemask;
    end;
  // remove right-block from doubly linked list
  remove_from_list_var(mc_right);
end;

procedure try_concat_free_chunk_forward(mc: pmemchunk_var);
var
  mc_tmp : pmemchunk_var;
begin
  { try concat forward }
  if (mc^.size and lastblockflag) = 0 then
   begin
     mc_tmp := pmemchunk_var(pointer(mc)+(mc^.size and sizemask));
     if (mc_tmp^.size and usedflag) = 0 then
       begin
         // next block free: concat
         concat_two_blocks(mc, mc_tmp);
       end;
   end;
end;

function try_concat_free_chunk(mc: pmemchunk_var): pmemchunk_var;
var
  mc_tmp : pmemchunk_var;
begin
  try_concat_free_chunk_forward(mc);

  { try concat backward }
  if (mc^.size and firstblockflag) = 0 then
    begin
      mc_tmp := pmemchunk_var(pointer(mc)-mc^.prevsize);
      if (mc_tmp^.size and usedflag) = 0 then
        begin
          // prior block free: concat
          concat_two_blocks(mc_tmp, mc);
          mc := mc_tmp;
        end;
    end;

  result := mc;
end;

{*****************************************************************************
                                Grow Heap
*****************************************************************************}

function alloc_oschunk(blockindex, size: ptrint): pointer;
var
  pmc       : pmemchunk_fixed;
  pmcv      : pmemchunk_var;
  i, count  : ptrint;
  chunksize : ptrint;
begin
  { increase size by size needed for os block header }
  size := size + sizeof(toschunk);
  { blocks available in freelist? }
  result := freeoslist;
  while result <> nil do
    begin
      if poschunk(result)^.size > size then
        begin
          size := poschunk(result)^.size;
          remove_from_oslist(poschunk(result));
          break;
        end;
      result := poschunk(result)^.next;
    end;
  if result = nil then
    begin
{$ifdef DUMPGROW}
      writeln('growheap(',size,')  allocating ',(size+$ffff) and $ffff0000);
      DumpBlocks;
{$endif}
      { allocate by 64K size }
      size := (size+$ffff) and not $ffff;
      { allocate smaller blocks for fixed-size chunks }
      if blockindex<>0 then
        begin
          result := SysOSAlloc(GrowHeapSizeSmall);
          if result<>nil then
            size := GrowHeapSizeSmall;
        end
    { first try 256K (default) }
    else if size<=GrowHeapSize1 then
      begin
        result := SysOSAlloc(GrowHeapSize1);
        if result<>nil then
          size := GrowHeapSize1;
      end
    { second try 1024K (default) }
    else if size<=GrowHeapSize2 then
      begin
        result := SysOSAlloc(GrowHeapSize2);
        if result<>nil then
          size := GrowHeapSize2;
      end
    { else allocate the needed bytes }
    else
      result := SysOSAlloc(size);
    { try again }
    if result=nil then
    begin
      result := SysOSAlloc(size);
      if (result=nil) then
        begin
          if ReturnNilIfGrowHeapFails then
            exit
          else
            HandleError(203);
        end;
    end;
    { set the total new heap size }
    inc(internal_memavail,size);
    inc(internal_heapsize,size);
  end;
  { initialize os-block }
  poschunk(result)^.used := 0;
  poschunk(result)^.size := size;
  inc(result, sizeof(toschunk));
  if blockindex<>0 then
    begin
      { chop os chunk in fixedsize parts }
      chunksize := blockindex shl blockshr;
      count := (size-sizeof(toschunk)) div chunksize;
      pmc := pmemchunk_fixed(result);
      pmc^.prev_fixed := nil;
      i := 0;
      repeat
        pmc^.size := fixedsizeflag or chunksize or (i shl 16);
        pmc^.next_fixed := pointer(pmc)+chunksize;
        inc(i);
        if i < count then
          begin
            pmc := pmemchunk_fixed(pointer(pmc)+chunksize);
            pmc^.prev_fixed := pointer(pmc)-chunksize;
          end
        else
          begin
            break;
          end;
      until false;
      append_to_list_fixed(blockindex, pmc);
      pmc^.prev_fixed := pointer(pmc)-chunksize;
      freelists_fixed[blockindex] := pmemchunk_fixed(result);
    end
  else
    begin
      pmcv := pmemchunk_var(result);
      append_to_list_var(pmcv);
      pmcv^.size := ((size-sizeof(toschunk)) and sizemask) or (firstblockflag or lastblockflag);
      pmcv^.prevsize := 0;
    end;
{$ifdef TestFreeLists}
  TestFreeLists;
{$endif TestFreeLists}
end;

{*****************************************************************************
                                 SysGetMem
*****************************************************************************}

function SysGetMem_Fixed(size: ptrint): pointer;
var
  pcurr: pmemchunk_fixed;
  poc: poschunk;
  s: ptrint;
begin
  result:=nil;
  { try to find a block in one of the freelists per size }
  s := size shr blockshr;
  pcurr := freelists_fixed[s];
  { no free blocks ? }
  if not assigned(pcurr) then
    begin
      pcurr := alloc_oschunk(s, size);
      if not assigned(pcurr) then
        exit;
    end;
  { get a pointer to the block we should return }
  result := pointer(pcurr)+sizeof(tmemchunk_fixed_hdr);
  { flag as in-use }
  pcurr^.size := pcurr^.size or usedflag;
  { update freelist }
  freelists_fixed[s] := pcurr^.next_fixed;
  if assigned(freelists_fixed[s]) then
    freelists_fixed[s]^.prev_fixed := nil;
  poc := poschunk(pointer(pcurr)-((pcurr^.size shr 16)*(pcurr^.size and fixedsizemask)+sizeof(toschunk)));
  inc(poc^.used);
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;

function SysGetMem_Var(size: ptrint): pointer;
var
  pcurr, pcurr_tmp : pmemchunk_var;
{$ifdef BESTMATCH}
  pbest : pmemchunk_var;
{$endif}
begin
  result:=nil;
{$ifdef BESTMATCH}
  pbest := nil;
{$endif}
  pcurr := freelist_var;
  while assigned(pcurr) do
    begin
{$ifdef BESTMATCH}
      if pcurr^.size=size then
        begin
          break;
        end
      else
        begin
          if (pcurr^.size>size) then
            begin
              if (not assigned(pbest)) or
                 (pcurr^.size<pbest^.size) then
               pbest := pcurr;
            end;
        end;
{$else BESTMATCH}
      if pcurr^.size>=size then
        break;
{$endif BESTMATCH}
      pcurr := pcurr^.next_var;
    end;
{$ifdef BESTMATCH}
  if not assigned(pcurr) then
    pcurr := pbest;
{$endif}

  if not assigned(pcurr) then
   begin
    // all os-chunks full, allocate a new one
    pcurr := alloc_oschunk(0, size);
    if not assigned(pcurr) then
      exit;
   end;

  { get pointer of the block we should return }
  result := pointer(pcurr)+sizeof(tmemchunk_var_hdr);
  { remove the current block from the freelist }
  remove_from_list_var(pcurr);
  { create the left over freelist block, if at least 16 bytes are free }
  split_block(pcurr, size);
  { flag block as used }
  pcurr^.size := pcurr^.size or usedflag;

{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;

function SysGetMem(size : ptrint):pointer;
begin
{ Something to allocate ? }
  if size<=0 then
    begin
      { give an error for < 0 }
      if size<0 then
        HandleError(204);
      { we always need to allocate something, using heapend is not possible,
        because heappend can be changed by growheap (PFV) }
      size := 1;
    end;
{ calc to multiple of 16 after adding the needed bytes for memchunk header }
  if size <= (maxblocksize - sizeof(tmemchunk_fixed_hdr)) then
    begin
      size := (size+sizeof(tmemchunk_fixed_hdr)+(blocksize-1)) and fixedsizemask;
      sysgetmem := sysgetmem_fixed(size);
    end
  else
    begin
      size := (size+sizeof(tmemchunk_var_hdr)+(blocksize-1)) and sizemask;
      sysgetmem := sysgetmem_var(size);
    end;
  dec(internal_memavail,size);
end;


{*****************************************************************************
                               SysFreeMem
*****************************************************************************}

function SysFreeMem_Fixed(pcurr: pmemchunk_fixed; size: ptrint): ptrint;
var
  pcurrsize: ptrint;
  blockindex: ptrint;
  poc: poschunk;
begin
  pcurrsize := pcurr^.size and fixedsizemask;
  if size<>pcurrsize then
   HandleError(204);
  inc(internal_memavail,pcurrsize);
  { insert the block in it's freelist }
  pcurr^.size := pcurr^.size and (not usedflag);
  blockindex := pcurrsize shr blockshr;
  append_to_list_fixed(blockindex, pcurr);
  { decrease used blocks count }
  poc := poschunk(pointer(pcurr)-(pcurr^.size shr 16)*pcurrsize-sizeof(toschunk));
  if poc^.used = 0 then
    HandleError(204);
  dec(poc^.used);
  if poc^.used = 0 then
  begin
    // block eligable for freeing
    append_to_oslist_fixed(blockindex, pcurrsize, poc);
  end;
  SysFreeMem_Fixed := pcurrsize;
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;

function SysFreeMem_Var(pcurr: pmemchunk_var; size: ptrint): ptrint;
var
  pcurrsize: ptrint;
begin
  pcurrsize := pcurr^.size and sizemask;
  if size<>pcurrsize then
    HandleError(204);
  inc(internal_memavail,pcurrsize);
  { insert the block in it's freelist }
  pcurr^.size := pcurr^.size and (not usedflag);
  append_to_list_var(pcurr);
  SysFreeMem_Var := pcurrsize;
  pcurr := try_concat_free_chunk(pcurr);
  if (pcurr^.size and (firstblockflag or lastblockflag)) = (firstblockflag or lastblockflag) then
  begin
    append_to_oslist_var(pcurr);
  end;
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;

function SysFreeMem(p: pointer): ptrint;
var
  pcurrsize: ptrint;
begin
  if p=nil then
    HandleError(204);
  pcurrsize := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr))^.size;
  { check if this is a fixed- or var-sized chunk }
  if (pcurrsize and fixedsizeflag) = 0 then
    begin
      result := sysfreemem_var(pmemchunk_var(p-sizeof(tmemchunk_var_hdr)), pcurrsize and sizemask);
    end
  else
    begin
      result := sysfreemem_fixed(pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr)), pcurrsize and fixedsizemask);
    end;
end;

{*****************************************************************************
                              SysFreeMemSize
*****************************************************************************}

Function SysFreeMemSize(p: pointer; size: ptrint):ptrint;
var
  pcurrsize: ptrint;
begin
  SysFreeMemSize := 0;
  if size<=0 then
    begin
      if size<0 then
        HandleError(204);
      exit;
    end;
  if p=nil then
    HandleError(204);

  pcurrsize := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr))^.size;
  { check if this is a fixed- or var-sized chunk }
  if (pcurrsize and fixedsizeflag) = 0 then
    begin
      size := (size+sizeof(tmemchunk_var_hdr)+(blocksize-1)) and sizemask;
      result := sysfreemem_var(pmemchunk_var(p-sizeof(tmemchunk_var_hdr)), size);
    end
  else
    begin
      size := (size+sizeof(tmemchunk_fixed_hdr)+(blocksize-1)) and fixedsizemask;
      result := sysfreemem_fixed(pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr)), size);
    end;
end;


{*****************************************************************************
                                 SysMemSize
*****************************************************************************}

function SysMemSize(p: pointer): ptrint;
begin
  SysMemSize := pmemchunk_fixed(pointer(p)-sizeof(tmemchunk_fixed_hdr))^.size;
  if (SysMemSize and fixedsizeflag) = 0 then
    begin
      SysMemSize := SysMemSize and sizemask;
      dec(SysMemSize, sizeof(tmemchunk_var_hdr));
    end
  else
    begin
      SysMemSize := SysMemSize and fixedsizemask;
      dec(SysMemSize, sizeof(tmemchunk_fixed_hdr));
    end;
end;


{*****************************************************************************
                                 SysAllocMem
*****************************************************************************}

function SysAllocMem(size: ptrint): pointer;
begin
  sysallocmem := MemoryManager.GetMem(size);
  if sysallocmem<>nil then
    FillChar(sysallocmem^,MemoryManager.MemSize(sysallocmem),0);
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysTryResizeMem(var p: pointer; size: ptrint): boolean;
var
  pcurrsize,
  oldsize,
  currsize,
  sizeleft : ptrint;
  pnew,
  pcurr : pmemchunk_var;
begin
  { fix needed size }
  if size <= (maxblocksize - sizeof(tmemchunk_fixed_hdr)) then
    begin
      size := (size+sizeof(tmemchunk_fixed_hdr)+(blocksize-1)) and fixedsizemask;
    end
  else
    begin
      size := (size+sizeof(tmemchunk_var_hdr)+(blocksize-1)) and sizemask;
    end;

  { fix p to point to the heaprecord }
  pcurrsize := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr))^.size;
  if (pcurrsize and fixedsizeflag) = 0 then
    begin
      currsize := pcurrsize and sizemask;
    end
  else
    begin
      currsize := pcurrsize and fixedsizemask;
    end;
  oldsize := currsize;
  { is the allocated block still correct? }
  if (currsize>=size) and (size>(currsize-16)) then
    begin
      SysTryResizeMem := true;
{$ifdef TestFreeLists}
       if test_each then
         TestFreeLists;
{$endif TestFreeLists}
       exit;
   end;

  { don't do resizes on fixed-size blocks }
//  if (pcurrsize and fixedsizeflag) <> 0 then
//  begin
    SysTryResizeMem := false;
    exit;
//  end;

  { get pointer to block }
  pcurr := pmemchunk_var(pointer(p)-sizeof(tmemchunk_var_hdr));

  { do we need to allocate more memory ? }
  if size>currsize then
   begin
   { the size is bigger than the previous size, we need to allocated more mem.
     We first check if the blocks after the current block are free. If not we
     simply call getmem/freemem to get the new block }
    try_concat_free_chunk_forward(pcurr);
    currsize := (pcurr^.size and sizemask);
    SysTryResizeMem := currsize>=size;
        end;
  if currsize>size then
      begin
    { is the size smaller then we can adjust the block to that size and insert
  the other part into the freelist }
     { create the left over freelist block, if at least 16 bytes are free }
    split_block(pcurr, size);
    SysTryResizeMem := true;
   end;
  dec(internal_memavail,size-oldsize);
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysReAllocMem(var p: pointer; size: ptrint):pointer;
var
  minsize : ptrint;
  p2 : pointer;
begin
  { Free block? }
  if size=0 then
   begin
     if p<>nil then
      begin
        MemoryManager.FreeMem(p);
      p := nil;
      end;
  end else
   { Allocate a new block? }
   if p=nil then
    begin
    p := MemoryManager.AllocMem(size);
  end else
   { Resize block }
   if not SysTryResizeMem(p,size) then
    begin
    minsize := MemoryManager.MemSize(p);
    if size < minsize then
      minsize := size;
    p2 := MemoryManager.AllocMem(size);
      if p2<>nil then
      Move(p^,p2^,minsize);
      MemoryManager.FreeMem(p);
    p := p2;
    end;
  SysReAllocMem := p;
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
                       MemoryMutexManager default hooks
*****************************************************************************}

procedure SysHeapMutexInit;
begin
  { nothing todo }
end;

procedure SysHeapMutexDone;
begin
  { nothing todo }
end;

procedure SysHeapMutexLock;
begin
  { give an runtime error. the program is running multithreaded without
    any heap protection. this will result in unpredictable errors so
    stopping here with an error is more safe (PFV) }
  runerror(244);
end;

procedure SysHeapMutexUnLock;
begin
  { see SysHeapMutexLock for comment }
  runerror(244);
end;


{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeap;
begin
  FillChar(freelists_fixed,sizeof(tfreelists),0);
  freelist_var := nil;
  freeoslist := nil;
  freeoslistcount := 0;
  internal_heapsize := 0;
  internal_memavail := 0;
end;

{
  $Log$
  Revision 1.37  2004-10-25 15:38:59  peter
    * compiler defined HEAP and HEAPSIZE removed

  Revision 1.36  2004/08/10 18:58:36  jonas
    * changed formatting to conform to the rest of the compiler/rtl
    * fixed SysMaxAvail so it also looks at the free fixed size blocks

  Revision 1.35  2004/06/29 20:50:32  peter
    * readded support for ReturnIfGrowHeapFails

  Revision 1.34  2004/06/27 19:47:27  florian
    * fixed heap corruption on sparc

  Revision 1.33  2004/06/27 11:57:18  florian
    * finally (hopefully) fixed sysalloc trouble

  Revision 1.32  2004/06/18 14:40:55  peter
    * moved padding for sparc

  Revision 1.31  2004/06/17 16:16:13  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.30  2004/05/31 12:18:16  peter
    * sparc needs alignment on 8 bytes to allow doubles

  Revision 1.29  2004/04/26 16:20:54  peter
    * 64bit fixes

  Revision 1.28  2004/03/15 21:48:26  peter
    * cmem moved to rtl
    * longint replaced with ptrint in heapmanagers

  Revision 1.27  2004/03/15 20:42:39  peter
    * exit with rte 204 instead of looping infinite when a heap record
      size is overwritten with 0

  Revision 1.26  2004/01/29 22:45:25  jonas
    * improved beforeheapend inheritance (remove flag again when possible,
      sometimes resulting in more opportunities for try_concat_free_chunk)

  Revision 1.25  2003/12/15 21:39:16  daniel
    * Small microoptimization

  Revision 1.24  2003/10/02 14:03:24  marco
   * *memORY overloads

  Revision 1.23  2003/09/28 12:43:48  peter
    * fixed wrong check when allocation of a block > 1mb failed

  Revision 1.22  2003/09/27 11:52:35  peter
    * sbrk returns pointer

  Revision 1.21  2003/05/23 14:53:48  peter
    * check newpos < 0 instead of = -1

  Revision 1.20  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.19  2002/11/01 17:38:04  peter
    * fix setmemorymutexmanager to call mutexdone on the already
      installed manager instead of the passed manager

  Revision 1.18  2002/10/30 20:39:13  peter
    * MemoryManager record has a field NeedLock if the wrapper functions
      need to provide locking for multithreaded programs

  Revision 1.17  2002/10/30 19:54:19  peter
    * remove wrong lock from SysMemSize, MemSize() does the locking
      already.

  Revision 1.16  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.15  2002/09/07 15:07:45  peter
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

