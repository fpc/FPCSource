{
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

{ Do not use standard memory manager }
{ Custom memory manager is Multi Threaded and does not require locking }
{ define HAS_MT_MEMORYMANAGER}

{ Do not use standard memory manager }
{ Custom memory manager requires locking when threading is used }
{ define HAS_MEMORYMANAGER}

{ Try to find the best matching block in general freelist }
{ define BESTMATCH}

{ DEBUG: Dump info when the heap needs to grow }
{ define DUMPGROW}

{ Memory profiling: at moment in time of max heap size usage,
  keep statistics of number of each size allocated 
  (with 16 byte granularity) }
{ define DUMP_MEM_USAGE}

{$ifdef HAS_MT_MEMORYMANAGER}
  {$define HAS_MEMORYMANAGER}
{$endif HAS_MT_MEMORYMANAGER}

{$ifdef DUMP_MEM_USAGE}
  {$define SHOW_MEM_USAGE}
{$endif}

const
{$ifdef CPU64}
  blocksize    = 32;  { at least size of freerecord }
  blockshift   = 5;   { shr value for blocksize=2^blockshift}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
{$else}
  blocksize    = 16;  { at least size of freerecord }
  blockshift   = 4;   { shr value for blocksize=2^blockshift}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
{$endif}
  maxblockindex = maxblocksize div blocksize; { highest index in array of lists of memchunks }
  maxreusebigger = 8; { max reuse bigger tries }

  { common flags }
  fixedsizeflag  = 1;   { flag if the block is of fixed size }
  { memchunk var flags }
  usedflag       = 2;   { flag if the block is used or not }
  lastblockflag  = 4;   { flag if the block is the last in os chunk }
  firstblockflag = 8;   { flag if the block is the first in os chunk }
  { os chunk flags }
  ocrecycleflag  = 1;
  { above flags stored in size field }
  sizemask = not(blocksize-1);
  fixedoffsetshift = 16;
  fixedsizemask = sizemask and ((1 shl fixedoffsetshift) - 1);

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
{$ifdef HAS_MT_MEMORYMANAGER}
    NeedLock: false;
{$else HAS_MT_MEMORYMANAGER}
    NeedLock: true;
{$endif HAS_MT_MEMORYMANAGER}
    GetMem: @SysGetMem;
    FreeMem: @SysFreeMem;
    FreeMemSize: @SysFreeMemSize;
    AllocMem: @SysAllocMem;
    ReAllocMem: @SysReAllocMem;
    MemSize: @SysMemSize;
    GetHeapStatus: @SysGetHeapStatus;
    GetFPCHeapStatus: @SysGetFPCHeapStatus;
  );

  MemoryMutexManager: TMemoryMutexManager = (
    MutexInit: @SysHeapMutexInit;
    MutexDone: @SysHeapMutexDone;
    MutexLock: @SysHeapMutexLock;
    MutexUnlock: @SysHeapMutexUnlock;
  );

{$ifndef HAS_MEMORYMANAGER}

{ 
  We use 'fixed' size chunks for small allocations,
  and os chunks with variable sized blocks for big
  allocations.

  * a block is an area allocated by user
  * a chunk is a block plus our bookkeeping
  * an os chunk is a collection of chunks

  Memory layout:
    fixed:                         < chunk size > [ ... user data ... ]
    variable:  < prev chunk size > < chunk size > [ ... user data ... ]

  When all chunks in an os chunk are free, we keep a few around
  but otherwise it will be freed to the OS.

  Fixed os chunks can be converted to variable os chunks and back
  (if not too big). To prevent repeated conversion overhead in case
  of user freeing/allocing same or a small set of sizes, we only do
  the conversion to the new fixed os chunk size format after we
  reuse the os chunk for another fixed size, or variable. Note that
  while the fixed size os chunk is on the freeoslist, it is also 
  still present in a freelists_fixed, therefore we can easily remove 
  the os chunk from the freeoslist if this size is needed again; we 
  don't need to search freeoslist in alloc_oschunk, since it won't
  be present anymore if alloc_oschunk is reached. Note that removing
  from the freeoslist is not really done, only the recycleflag is
  set, allowing to reset the flag easily. alloc_oschunk will clean up
  the list while passing over it, that was a slow function anyway.
}

type
  poschunk = ^toschunk;
  toschunk = record
    size : ptrint;
    next : poschunk;
    used : ptrint;
    { padding inserted automatically by alloc_oschunk }
  end;

  pmemchunk_fixed = ^tmemchunk_fixed;
  tmemchunk_fixed = record
    { aligning is done automatically in alloc_oschunk }
    size  : ptrint;
    next_fixed,
    prev_fixed : pmemchunk_fixed;
  end;

  pmemchunk_var = ^tmemchunk_var;
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
    { aligning is done automatically in alloc_oschunk }
    size : ptrint;
  end;
  tmemchunk_var_hdr = record
    prevsize : ptrint;
    size : ptrint;
  end;

  tfreelists   = array[1..maxblockindex] of pmemchunk_fixed;
  pfreelists   = ^tfreelists;

const
  fixedfirstoffset = ((sizeof(toschunk) + sizeof(tmemchunk_fixed_hdr) + $f) 
      and not $f) - sizeof(tmemchunk_fixed_hdr);
  varfirstoffset = ((sizeof(toschunk) + sizeof(tmemchunk_var_hdr) + $f) 
      and not $f) - sizeof(tmemchunk_var_hdr);
{$ifdef BESTMATCH}
  matcheffort = high(longint);
{$else}
  matcheffort = 10;
{$endif}

var
  internal_status : TFPCHeapStatus;

  freelists_fixed    : tfreelists;
  freelist_var       : pmemchunk_var;
  freeoslist         : poschunk;
  freeoslistend      : poschunk;
  freeoslistcount    : dword;

{$ifdef DUMP_MEM_USAGE}
const
  sizeusageshift = 4;
  sizeusageindex = 2049;
  sizeusagesize = sizeusageindex shl sizeusageshift;
type
  tsizeusagelist = array[0..sizeusageindex] of longint;
var
  sizeusage, maxsizeusage: tsizeusagelist;
{$endif}

{$endif HAS_MEMORYMANAGER}

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


function GetHeapStatus:THeapStatus;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       result:=MemoryManager.GetHeapStatus();
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     result:=MemoryManager.GetHeapStatus();
   end;
end;


function GetFPCHeapStatus:TFPCHeapStatus;
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       result:=MemoryManager.GetFPCHeapStatus();
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     Result:=MemoryManager.GetFPCHeapStatus();
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
function FreeMem(p:pointer):ptrint;[Public,Alias:'FPC_FREEMEM_X'];
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

{$ifndef HAS_MEMORYMANAGER}
{*****************************************************************************
                               GetHeapStatus
*****************************************************************************}

function SysGetFPCHeapStatus:TFPCHeapStatus;
begin
  internal_status.CurrHeapFree:=internal_status.CurrHeapSize-internal_status.CurrHeapUsed;
  result:=internal_status;
end;

function SysGetHeapStatus :THeapStatus;

begin
  internal_status.CurrHeapFree:=internal_status.CurrHeapSize-internal_status.CurrHeapUsed;
  result.TotalAllocated   :=internal_status.CurrHeapUsed;
  result.TotalFree        :=internal_status.CurrHeapFree;
  result.TotalAddrSpace   :=0;
  result.TotalUncommitted :=0;
  result.TotalCommitted   :=0;
  result.FreeSmall        :=0;
  result.FreeBig          :=0;
  result.Unused           :=0;
  result.Overhead         :=0;
  result.HeapErrorCode    :=0;
end;


{$ifdef DUMPBLOCKS}   // TODO
procedure DumpBlocks;
var
  s,i,j : ptrint;
  hpfixed  : pmemchunk_fixed;
  hpvar  : pmemchunk_var;
begin
  { fixed freelist }
  for i := 1 to maxblockindex do
   begin
     hpfixed := freelists_fixed[i];
     j := 0;
     while assigned(hpfixed) do
      begin
        inc(j);
        hpfixed := hpfixed^.next_fixed;
      end;
     writeln('Block ',i*blocksize,': ',j);
   end;
  { var freelist }
  hpvar := freelist_var;
  j := 0;
  s := 0;
  while assigned(hpvar) do
   begin
     inc(j);
     if hpvar^.size>s then
      s := hpvar^.size;
     hpvar := hpvar^.next_var;
   end;
  writeln('Variable: ',j,' maxsize: ',s);
end;
{$endif}



{*****************************************************************************
                                List adding/removal
*****************************************************************************}

procedure append_to_list_var(pmc: pmemchunk_var); inline;
begin
  pmc^.prev_var := nil;
  pmc^.next_var := freelist_var;
  if freelist_var<>nil then
    freelist_var^.prev_var := pmc;
  freelist_var := pmc;
end;

{$ifdef HEAP_DEBUG}

function find_fixed_mc(chunkindex: ptrint; pmc: pmemchunk_fixed): boolean;
var
  pmc_temp: pmemchunk_fixed;
begin
  pmc_temp := freelists_fixed[chunkindex];
  while pmc_temp <> nil do
  begin
    if pmc_temp = pmc then exit(true);
    pmc_temp := pmc_temp^.next_fixed;
  end;
  result := false;
end;

{$endif}

procedure remove_from_list_fixed(blockindex: ptrint; pmc: pmemchunk_fixed); inline;
begin
  if assigned(pmc^.next_fixed) then
    pmc^.next_fixed^.prev_fixed := pmc^.prev_fixed;
  if assigned(pmc^.prev_fixed) then
    pmc^.prev_fixed^.next_fixed := pmc^.next_fixed
  else
    freelists_fixed[blockindex] := pmc^.next_fixed;
end;

procedure remove_from_list_var(pmc: pmemchunk_var); inline;
begin
  if assigned(pmc^.next_var) then
    pmc^.next_var^.prev_var := pmc^.prev_var;
  if assigned(pmc^.prev_var) then
    pmc^.prev_var^.next_var := pmc^.next_var
  else
    freelist_var := pmc^.next_var;
end;

procedure remove_all_from_list_fixed(chunksize: ptrint; poc: poschunk);
var
  pmc, pmc_end: pmemchunk_fixed;
  chunkindex: ptrint;
begin
  pmc := pmemchunk_fixed(pointer(poc)+fixedfirstoffset);
  pmc_end := pmemchunk_fixed(pointer(poc)+(poc^.size and sizemask)-chunksize);
  chunkindex := chunksize shr blockshift;
  repeat
    remove_from_list_fixed(chunkindex, pmc);
    pmc := pointer(pmc)+chunksize;
  until pmc > pmc_end;
end;

procedure append_to_oslist(poc: poschunk; chunksize: ptrint);
var
  pocsize: ptrint;
begin
  { check if already on list }
  if (poc^.size and ocrecycleflag) <> 0 then
    begin
      inc(freeoslistcount);
      poc^.size := poc^.size and not ocrecycleflag;
      exit;
    end;
  { decide whether to free block or add to list }
{$ifdef HAS_SYSOSFREE}
  pocsize := poc^.size and sizemask;
  if (freeoslistcount >= MaxKeptOSChunks) or
     (pocsize > growheapsize2) then
    begin
      if chunksize <> 0 then
        remove_all_from_list_fixed(chunksize, poc);
      dec(internal_status.currheapsize, pocsize);
      SysOSFree(poc, pocsize);
    end
  else
    begin
{$endif}
      if freeoslistend = nil then
        freeoslistend := poc
      else
        freeoslistend^.next := poc;
      freeoslist := poc;
      inc(freeoslistcount);
{$ifdef HAS_SYSOSFREE}
   end;
{$endif}
end;

procedure clear_oschunk_on_freelist_fixed_flag(poc: poschunk); inline;
  { prevent thinking this os chunk is on the fixed freelists }
begin
  pmemchunk_fixed(pointer(poc) + fixedfirstoffset)^.size := 0;
end;

procedure append_to_oslist_var(pmc: pmemchunk_var);
var
  poc: poschunk;
begin
  // block eligable for freeing
  poc := pointer(pmc)-varfirstoffset;
  remove_from_list_var(pmc);
  clear_oschunk_on_freelist_fixed_flag(poc);
  append_to_oslist(poc, 0);
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
  // mc_right can't be a fixed size block
  if mc_right^.size and fixedsizeflag<>0 then
    HandleError(204);
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


function check_concat_free_chunk_forward(mc: pmemchunk_var;reqsize:ptrint):boolean;
var
  mc_tmp : pmemchunk_var;
  freesize : ptrint;
begin
  check_concat_free_chunk_forward:=false;
  freesize:=0;
  mc_tmp:=mc;
  repeat
     inc(freesize,mc_tmp^.size and sizemask);
     if freesize>=reqsize then
       begin
         check_concat_free_chunk_forward:=true;
         exit;
       end;
     if (mc_tmp^.size and lastblockflag) <> 0 then
       break;
     mc_tmp := pmemchunk_var(pointer(mc_tmp)+(mc_tmp^.size and sizemask));
     if (mc_tmp^.size and usedflag) <> 0 then
       break;
  until false;
end;


{*****************************************************************************
                                Grow Heap
*****************************************************************************}

function alloc_oschunk(chunkindex, size: ptrint): pointer;
var
  pmc,
  pmc_next  : pmemchunk_fixed;
  pmcv      : pmemchunk_var;
  poc       : poschunk;
  prev_poc  : poschunk;
  minsize,
  maxsize,
  i         : ptrint;
  chunksize : ptrint;
  pocsize   : ptrint;
begin
  { increase size by size needed for os block header }
  minsize := size + varfirstoffset;
  { for fixed size chunks we keep offset from os chunk to mem chunk in
    upper bits, so maximum os chunk size is 64K on 32bit for fixed size }
  if chunkindex<>0 then
    maxsize := 1 shl (32-fixedoffsetshift)
  else
    maxsize := high(ptrint);
  { blocks available in freelist? }
  poc := freeoslist;
  prev_poc := nil;
  while poc <> nil do
    begin
      if (poc^.size and ocrecycleflag) <> 0 then
      begin
        { oops! we recycled this chunk; remove it from list }
        poc^.size := poc^.size and not ocrecycleflag;
        poc := poc^.next;
        if prev_poc = nil then
          freeoslist := poc
        else
          prev_poc^.next := poc;
        if poc = nil then
          freeoslistend := nil;
        continue;
      end;
      pocsize := poc^.size and sizemask;
      if (pocsize >= minsize) and
         (pocsize <= maxsize) then
        begin
          size := pocsize;
          if prev_poc = nil then
            freeoslist := poc^.next
          else
            prev_poc^.next := poc^.next;
          if poc^.next = nil then
            freeoslistend := nil;
          dec(freeoslistcount);
          pmc := pmemchunk_fixed(pointer(poc)+fixedfirstoffset);
          if pmc^.size <> 0 then
            remove_all_from_list_fixed(pmc^.size and fixedsizemask, poc);
          break;
        end;
      prev_poc := poc;
      poc := poc^.next;
    end;
  if poc = nil then
    begin
{$ifdef DUMPGROW}
      writeln('growheap(',size,')  allocating ',(size+sizeof(toschunk)+$ffff) and not $ffff);
      DumpBlocks;
{$endif}
      { allocate by 64K size }
      size := (size+varfirstoffset+$ffff) and not $ffff;
      { allocate smaller blocks for fixed-size chunks }
      if chunkindex<>0 then
        begin
          poc := SysOSAlloc(GrowHeapSizeSmall);
          if poc<>nil then
            size := GrowHeapSizeSmall;
        end
    { first try 256K (default) }
      else if size<=GrowHeapSize1 then
        begin
          poc := SysOSAlloc(GrowHeapSize1);
          if poc<>nil then
            size := GrowHeapSize1;
        end
    { second try 1024K (default) }
      else if size<=GrowHeapSize2 then
        begin
          poc := SysOSAlloc(GrowHeapSize2);
          if poc<>nil then
            size := GrowHeapSize2;
        end
    { else allocate the needed bytes }
      else
        poc := SysOSAlloc(size);
    { try again }
      if poc=nil then
      begin
        poc := SysOSAlloc(size);
        if poc=nil then
          begin
            if ReturnNilIfGrowHeapFails then
              begin
                result := nil;
                exit
              end
            else
              HandleError(203);
          end;
      end;
      { prevent thinking this os chunk is on some freelist }
      clear_oschunk_on_freelist_fixed_flag(poc);
      poc^.next := nil;
      { set the total new heap size }
      inc(internal_status.currheapsize,size);
      if internal_status.currheapsize>internal_status.maxheapsize then
        internal_status.maxheapsize:=internal_status.currheapsize;
    end;
  { initialize os-block }
  poc^.used := 0;
  poc^.size := size;
  if chunkindex<>0 then
    begin
      { chop os chunk in fixedsize parts,
        maximum of $ffff elements are allowed, otherwise
        there will be an overflow }
      chunksize := chunkindex shl blockshift;
      if size-chunksize>maxsize then
        HandleError(204);
      { we need to align the user pointers to 8 byte at least for
        mmx/sse and doubles on sparc, align to 16 bytes }
      i := fixedfirstoffset;
      result := pointer(poc) + i;
      pmc := pmemchunk_fixed(result);
      pmc^.prev_fixed := nil;
      repeat
        pmc^.size := fixedsizeflag or chunksize or (i shl fixedoffsetshift);
        inc(i, chunksize);
        if i > size - chunksize then break;
        pmc_next := pmemchunk_fixed(pointer(pmc)+chunksize);
        pmc^.next_fixed := pmc_next;
        pmc_next^.prev_fixed := pmc;
        pmc := pmc_next;
      until false;
      pmc_next := freelists_fixed[chunkindex];
      pmc^.next_fixed := pmc_next;
      if pmc_next<>nil then
        pmc_next^.prev_fixed := pmc;
      freelists_fixed[chunkindex] := pmemchunk_fixed(result);
    end
  else
    begin
      { we need to align the user pointers to 8 byte at least for
        mmx/sse and doubles on sparc, align to 16 bytes }
      result := pointer(poc)+varfirstoffset;
      pmcv := pmemchunk_var(result);
      append_to_list_var(pmcv);
      pmcv^.size := ((size-varfirstoffset) and sizemask) or (firstblockflag or lastblockflag);
      pmcv^.prevsize := 0;
    end;
end;

{*****************************************************************************
                                 SysGetMem
*****************************************************************************}

function SysGetMem_Fixed(chunksize: ptrint): pointer;
var
  pmc, pmc_next: pmemchunk_fixed;
  poc: poschunk;
  chunkindex: ptrint;
begin
  { try to find a block in one of the freelists per size }
  chunkindex := chunksize shr blockshift;
  pmc := freelists_fixed[chunkindex];
  { no free blocks ? }
  if assigned(pmc) then
    begin
      { remove oschunk from free list in case we recycle it }
      poc := poschunk(pointer(pmc) - (pmc^.size shr fixedoffsetshift));
      if poc^.used = 0 then
        begin
          poc^.size := poc^.size or ocrecycleflag;
          dec(freeoslistcount);
        end;
    end
  else
    begin
      pmc := alloc_oschunk(chunkindex, chunksize);
      if not assigned(pmc) then
        exit(nil);
      poc := poschunk(pointer(pmc)-fixedfirstoffset);
    end;
  { get a pointer to the block we should return }
  result := pointer(pmc)+sizeof(tmemchunk_fixed_hdr);
  { update freelist }
  pmc_next := pmc^.next_fixed;
  freelists_fixed[chunkindex] := pmc_next;
  if assigned(pmc_next) then
    pmc_next^.prev_fixed := nil;
  inc(poc^.used);
  { statistics }
  inc(internal_status.currheapused,chunksize);
  if internal_status.currheapused>internal_status.maxheapused then
  begin
    internal_status.maxheapused:=internal_status.currheapused;
{$ifdef DUMP_MEM_USAGE}        
    maxsizeusage := sizeusage;
{$endif}        
  end;
end;

function SysGetMem_Var(size: ptrint): pointer;
var
  pcurr : pmemchunk_var;
  pbest : pmemchunk_var;
  iter : longint;
begin
  result:=nil;
  pbest := nil;
  pcurr := freelist_var;
  iter := high(longint);
  while assigned(pcurr) and (iter>0) do
  begin
    if (pcurr^.size>size) then
    begin
      if not assigned(pbest) or (pcurr^.size<pbest^.size) then
      begin
        pbest := pcurr;
        if pcurr^.size = size then
          break;
      end;
      iter := matcheffort;
    end;
    pcurr := pcurr^.next_var;
    dec(iter);
  end;
  pcurr := pbest;

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
  { statistics }
  inc(internal_status.currheapused,size);
  if internal_status.currheapused>internal_status.maxheapused then
  begin
    internal_status.maxheapused:=internal_status.currheapused;
{$ifdef DUMP_MEM_USAGE}        
    maxsizeusage := sizeusage;
{$endif}        
  end;
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
      size := (size+(sizeof(tmemchunk_fixed_hdr)+(blocksize-1))) and fixedsizemask;
      result := sysgetmem_fixed(size);
    end
  else
    begin
      size := (size+(sizeof(tmemchunk_var_hdr)+(blocksize-1))) and sizemask;
      result := sysgetmem_var(size);
    end;

{$ifdef DUMP_MEM_USAGE}
  size := sysmemsize(result);
  if size > sizeusagesize then
    inc(sizeusage[sizeusageindex])
  else
    inc(sizeusage[size shr sizeusageshift]);
{$endif}
end;


{*****************************************************************************
                               SysFreeMem
*****************************************************************************}

function SysFreeMem_Fixed(pmc: pmemchunk_fixed): ptrint;
var
  chunkindex,
  chunksize: ptrint;
  poc: poschunk;
  pmc_next: pmemchunk_fixed;
begin
  chunksize := pmc^.size and fixedsizemask;
  dec(internal_status.currheapused, chunksize);
  { insert the block in it's freelist }
  chunkindex := chunksize shr blockshift;
  pmc_next := freelists_fixed[chunkindex];
  pmc^.prev_fixed := nil;
  pmc^.next_fixed := pmc_next;
  if assigned(pmc_next) then
    pmc_next^.prev_fixed := pmc;
  freelists_fixed[chunkindex] := pmc;
  { decrease used blocks count }
  poc := poschunk(pointer(pmc)-(pmc^.size shr fixedoffsetshift));
  dec(poc^.used);
  if poc^.used <= 0 then
    begin
      { decrease used blocks count }
      if poc^.used=-1 then
        HandleError(204);
      { osblock can be freed? }
      append_to_oslist(poc, chunksize);
    end;
  result := chunksize;
end;

function SysFreeMem_Var(pmcv: pmemchunk_var): ptrint;
var
  chunksize: ptrint;
begin
  chunksize := pmcv^.size and sizemask;
  dec(internal_status.currheapused,chunksize);
  { insert the block in it's freelist }
  pmcv^.size := pmcv^.size and (not usedflag);
  append_to_list_var(pmcv);
  pmcv := try_concat_free_chunk(pmcv);
  if (pmcv^.size and (firstblockflag or lastblockflag)) = (firstblockflag or lastblockflag) then
    append_to_oslist_var(pmcv);
  result := chunksize;
end;


function SysFreeMem(p: pointer): ptrint;
var
  pmc: pmemchunk_fixed;
{$ifdef DUMP_MEM_USAGE}
  size: sizeint;
{$endif}
begin
  if p=nil then
    begin
      result:=0;
      exit;
    end;
{$ifdef DUMP_MEM_USAGE}
  size := sysmemsize(p);
  if size > sizeusagesize then
    dec(sizeusage[sizeusageindex])
  else
    dec(sizeusage[size shr sizeusageshift]);
{$endif}
  pmc := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr));
  { check if this is a fixed- or var-sized chunk }
  if (pmc^.size and fixedsizeflag) = 0 then
    result := sysfreemem_var(pmemchunk_var(p-sizeof(tmemchunk_var_hdr)))
  else
    result := sysfreemem_fixed(pmc);
end;

{*****************************************************************************
                              SysFreeMemSize
*****************************************************************************}

Function SysFreeMemSize(p: pointer; size: ptrint):ptrint;
begin
  if size<=0 then
  begin
    if size<0 then
      HandleError(204);
    exit(0);
  end;
  { can't free partial blocks, ignore size }
  result := SysFreeMem(p);
end;


{*****************************************************************************
                                 SysMemSize
*****************************************************************************}

function SysMemSize(p: pointer): ptrint;
begin
  result := pmemchunk_fixed(pointer(p)-sizeof(tmemchunk_fixed_hdr))^.size;
  if (result and fixedsizeflag) = 0 then
    begin
      result := result and sizemask;
      dec(result, sizeof(tmemchunk_var_hdr));
    end
  else
    begin
      result := result and fixedsizemask;
      dec(result, sizeof(tmemchunk_fixed_hdr));
    end;
end;


{*****************************************************************************
                                 SysAllocMem
*****************************************************************************}

function SysAllocMem(size: ptrint): pointer;
begin
  result := MemoryManager.GetMem(size);
  if result<>nil then
    FillChar(result^,MemoryManager.MemSize(result),0);
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysTryResizeMem(var p: pointer; size: ptrint): boolean;
var
  chunksize,
  oldsize,
  currsize : ptrint;
  pcurr : pmemchunk_var;
begin
  SysTryResizeMem := false;

  { fix p to point to the heaprecord }
  chunksize := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr))^.size;

  { handle fixed memchuncks separate. Only allow resizes when the
    new size fits in the same block }
  if (chunksize and fixedsizeflag) <> 0 then
    begin
      currsize := chunksize and fixedsizemask;

      { 1. Resizing to smaller sizes will never allocate a new block. We just keep the current block. This
           is needed for the expectations that resizing to a small block will not move the contents of
           a memory block
        2. For resizing to greater size first check if the size fits in the fixed block range to prevent
           "truncating" the size by the fixedsizemask }
      if ((size <= (maxblocksize - sizeof(tmemchunk_fixed_hdr))) and
          ((size+sizeof(tmemchunk_fixed_hdr)+(blocksize-1)) and sizemask <= currsize)) then
        begin
          systryresizemem:=true;
          exit;
        end;

      { we need to allocate a new fixed or var memchunck }
      exit;
    end;

  { var memchunck }
  currsize := chunksize and sizemask;
  size := (size+sizeof(tmemchunk_var_hdr)+(blocksize-1)) and sizemask;

  { is the allocated block still correct? }
  if (currsize>=size) and (size>(currsize-blocksize)) then
    begin
      SysTryResizeMem := true;
      exit;
    end;

  { get pointer to block }
  pcurr := pmemchunk_var(pointer(p)-sizeof(tmemchunk_var_hdr));
  oldsize := currsize;

  { do we need to allocate more memory ? }
  if size>currsize then
   begin
     { the size is bigger than the previous size, we need to allocated more mem.
       We first check if the blocks after the current block are free. If not then we
       simply call getmem/freemem to get the new block }
     if check_concat_free_chunk_forward(pcurr,size) then
       repeat
         concat_two_blocks(pcurr,pmemchunk_var(pointer(pcurr)+currsize));
         currsize := pcurr^.size and sizemask;
       until currsize>=size
     else
       exit;
   end;
  { is the size smaller then we can adjust the block to that size and insert
    the other part into the freelist }
  if currsize>size then
    split_block(pcurr, size);

  inc(internal_status.currheapused,size-oldsize);
  SysTryResizeMem := true;
end;


{*****************************************************************************
                                 SysResizeMem
*****************************************************************************}

function SysReAllocMem(var p: pointer; size: ptrint):pointer;
var
  newsize,
  oldsize,
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
   end
  else
   { Allocate a new block? }
   if p=nil then
    begin
      p := MemoryManager.GetMem(size);
    end
  else
   begin
    { Resize block }
{$ifdef DUMP_MEM_USAGE}
    oldsize:=SysMemSize(p);
{$endif}    
    if not SysTryResizeMem(p,size) then
    begin
      oldsize:=MemoryManager.MemSize(p);
      { Grow with bigger steps to prevent the need for
        multiple getmem/freemem calls for fixed blocks. It might cost a bit
        of extra memory, but in most cases a reallocmem is done multiple times. }
      if oldsize<maxblocksize then
        begin
          newsize:=oldsize*2+blocksize;
          if size>newsize then
            newsize:=size;
        end
      else
        newsize:=size;
      { calc size of data to move }
      minsize:=oldsize;
      if newsize < minsize then
        minsize := newsize;
      p2 := MemoryManager.GetMem(newsize);
      if p2<>nil then
        Move(p^,p2^,minsize);
      MemoryManager.FreeMem(p);
      p := p2;
{$ifdef DUMP_MEM_USAGE}
    end else begin
      size := sysmemsize(p);
      if size <> oldsize then
      begin
        if oldsize > sizeusagesize then
          dec(sizeusage[sizeusageindex])
        else if oldsize >= 0 then
          dec(sizeusage[oldsize shr sizeusageshift]);
        if size > sizeusagesize then
          inc(sizeusage[sizeusageindex])
        else if size >= 0 then
          inc(sizeusage[size shr sizeusageshift]);
      end;
{$endif}
    end;
   end;
  SysReAllocMem := p;
end;

{$endif HAS_MEMORYMANAGER}

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
{$ifndef HAS_MT_MEMORYMANAGER}
  { give an runtime error. the program is running multithreaded without
    any heap protection. this will result in unpredictable errors so
    stopping here with an error is more safe (PFV) }
  runerror(244);
{$endif}
end;

procedure SysHeapMutexUnLock;
begin
{$ifndef HAS_MT_MEMORYMANAGER}
  { see SysHeapMutexLock for comment }
  runerror(244);
{$endif}
end;

{$ifndef HAS_MEMORYMANAGER}

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
  fillchar(internal_status,sizeof(internal_status),0);
{$ifdef DUMP_MEM_USAGE}
  fillchar(sizeusage,sizeof(sizeusage),0);
  fillchar(maxsizeusage,sizeof(sizeusage),0);
{$endif}
end;

procedure FinalizeHeap;
var
  poc : poschunk;
  i : longint;
begin
{$ifdef SHOW_MEM_USAGE}
  writeln('Max heap used/size: ', internal_status.maxheapused, '/', 
    internal_status.maxheapsize);
{$endif}
{$ifdef DUMP_MEM_USAGE}
  for i := 0 to sizeusageindex-1 do
    if maxsizeusage[i] <> 0 then
      writeln('size ', i shl sizeusageshift, ' usage ', maxsizeusage[i]);
  writeln('size >', sizeusagesize, ' usage ', maxsizeusage[sizeusageindex]);
{$endif}
{$ifdef HAS_SYSOSFREE}
  while assigned(freeoslist) do
    begin
      poc:=freeoslist^.next;
      SysOSFree(freeoslist, freeoslist^.size and sizemask);
      dec(freeoslistcount);
      freeoslist:=poc;
    end;
  freeoslistend:=nil;
{$endif HAS_SYSOSFREE}
  { release mutex }
  MemoryMutexManager.MutexDone;
end;

{$endif HAS_MEMORYMANAGER}
