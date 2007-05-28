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
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @SysGetHeapStatus;
    GetFPCHeapStatus: @SysGetFPCHeapStatus;
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
  while the fixed size os chunk is on the freelists.oslist, it is also 
  still present in a freelists.fixedlists, therefore we can easily remove 
  the os chunk from the freelists.oslist if this size is needed again; we 
  don't need to search freelists.oslist in alloc_oschunk, since it won't
  be present anymore if alloc_oschunk is reached. Note that removing
  from the freelists.oslist is not really done, only the recycleflag is
  set, allowing to reset the flag easily. alloc_oschunk will clean up
  the list while passing over it, that was a slow function anyway.
}

type
  pfreelists = ^tfreelists;

  poschunk = ^toschunk;
  toschunk = record
    size : ptrint;
    next : poschunk;
    used : ptrint;
    freelists : pfreelists;
    { padding inserted automatically by alloc_oschunk }
  end;

  ppmemchunk_fixed = ^pmemchunk_fixed;
  pmemchunk_fixed = ^tmemchunk_fixed;
  tmemchunk_fixed = record
    { aligning is done automatically in alloc_oschunk }
    size  : ptrint;
    next_fixed,
    prev_fixed : pmemchunk_fixed;
  end;

  ppmemchunk_var = ^pmemchunk_var;
  pmemchunk_var = ^tmemchunk_var;
  tmemchunk_var = record
    prevsize : ptrint;
    freelists : pfreelists;
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
    freelists : pfreelists;
    size : ptrint;
  end;

  pfpcheapstatus = ^tfpcheapstatus;

  tfixedfreelists = array[1..maxblockindex] of pmemchunk_fixed;

  tfreelists = record
    oslist : poschunk;
    oscount : dword;
    fixedlists : tfixedfreelists;
    varlist : pmemchunk_var;
    { chunks waiting to be freed from other thread }
    waitfixed : pmemchunk_fixed;
    lockfixed : trtlcriticalsection;
    waitvar : pmemchunk_var;
    lockvar : trtlcriticalsection;
    { heap statistics }
    internal_status : TFPCHeapStatus;
  end;

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
  main_orig_freelists : pfreelists;
  main_relo_freelists : pfreelists;
threadvar
  freelists : tfreelists;

{$ifdef DUMP_MEM_USAGE}
const
  sizeusageshift = 4;
  sizeusageindex = 2049;
  sizeusagesize = sizeusageindex shl sizeusageshift;
type
  tsizeusagelist = array[0..sizeusageindex] of longint;
threadvar
  sizeusage, maxsizeusage: tsizeusagelist;
{$endif}

{$endif HAS_MEMORYMANAGER}

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

procedure GetMemoryManager(var MemMgr:TMemoryManager);
begin
  MemMgr := MemoryManager;
end;


procedure SetMemoryManager(const MemMgr:TMemoryManager);
begin
  MemoryManager := MemMgr;
end;

function IsMemoryManagerSet:Boolean;
begin
  IsMemoryManagerSet := (MemoryManager.GetMem<>@SysGetMem) 
    or (MemoryManager.FreeMem<>@SysFreeMem);
end;

procedure GetMem(Var p:pointer;Size:ptrint);
begin
  p := MemoryManager.GetMem(Size);
end;

procedure GetMemory(Var p:pointer;Size:ptrint);
begin
  GetMem(p,size);
end;

procedure FreeMem(p:pointer;Size:ptrint);
begin
  MemoryManager.FreeMemSize(p,Size);
end;

procedure FreeMemory(p:pointer;Size:ptrint);
begin
  FreeMem(p,size);
end;


function GetHeapStatus:THeapStatus;
begin
  Result:=MemoryManager.GetHeapStatus();
end;


function GetFPCHeapStatus:TFPCHeapStatus;
begin
  Result:=MemoryManager.GetFPCHeapStatus();
end;


function MemSize(p:pointer):ptrint;
begin
  MemSize := MemoryManager.MemSize(p);
end;


{ Delphi style }
function FreeMem(p:pointer):ptrint;[Public,Alias:'FPC_FREEMEM_X'];
begin
  FreeMem := MemoryManager.FreeMem(p);
end;

function FreeMemory(p:pointer):ptrint;
begin
  FreeMemory := FreeMem(p);
end;

function GetMem(size:ptrint):pointer;
begin
  GetMem := MemoryManager.GetMem(Size);
end;

function GetMemory(size:ptrint):pointer;
begin
  GetMemory := GetMem(size);
end;

function AllocMem(Size:ptrint):pointer;
begin
  AllocMem := MemoryManager.AllocMem(size);
end;


function ReAllocMem(var p:pointer;Size:ptrint):pointer;
begin
  ReAllocMem := MemoryManager.ReAllocMem(p,size);
end;

function ReAllocMemory(var p:pointer;Size:ptrint):pointer;
begin
  ReAllocMemory := ReAllocMem(p,size);
end;


{ Needed for calls from Assembler }
function fpc_getmem(size:ptrint):pointer;compilerproc;[public,alias:'FPC_GETMEM'];
begin
  fpc_GetMem := MemoryManager.GetMem(size);
end;

procedure fpc_freemem(p:pointer);compilerproc;[public,alias:'FPC_FREEMEM'];
begin
  MemoryManager.FreeMem(p);
end;

{$ifndef HAS_MEMORYMANAGER}
{*****************************************************************************
                               GetHeapStatus
*****************************************************************************}

function SysGetFPCHeapStatus:TFPCHeapStatus;
var
  status: pfpcheapstatus;
begin
  status := @freelists.internal_status;
  status^.CurrHeapFree := status^.CurrHeapSize - status^.CurrHeapUsed;
  result := status^;
end;

function SysGetHeapStatus :THeapStatus;
var
  status: pfpcheapstatus;
begin
  status := @freelists.internal_status;
  status^.CurrHeapFree := status^.CurrHeapSize - status^.CurrHeapUsed;
  result.TotalAllocated   :=status^.CurrHeapUsed;
  result.TotalFree        :=status^.CurrHeapFree;
  result.TotalAddrSpace   :=status^.CurrHeapSize;
  result.TotalUncommitted :=0;
  result.TotalCommitted   :=0;
  result.FreeSmall        :=0;
  result.FreeBig          :=0;
  result.Unused           :=0;
  result.Overhead         :=0;
  result.HeapErrorCode    :=0;
end;


{$ifdef DUMPBLOCKS}   // TODO
procedure DumpBlocks(loc_freelists: pfreelists);
var
  s,i,j : ptrint;
  hpfixed  : pmemchunk_fixed;
  hpvar  : pmemchunk_var;
begin
  { fixed freelist }
  for i := 1 to maxblockindex do
   begin
     hpfixed := loc_freelists^.fixedlists[i];
     j := 0;
     while assigned(hpfixed) do
      begin
        inc(j);
        hpfixed := hpfixed^.next_fixed;
      end;
     writeln('Block ',i*blocksize,': ',j);
   end;
  { var freelist }
  hpvar := loc_freelists^.varlist;
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
var
  varlist: ppmemchunk_var;
begin
  varlist := @pmc^.freelists^.varlist;
  pmc^.prev_var := nil;
  pmc^.next_var := varlist^;
  if varlist^<>nil then
    varlist^^.prev_var := pmc;
  varlist^ := pmc;
end;

{$ifdef HEAP_DEBUG}

function find_fixed_mc(loc_freelists: pfreelists; chunkindex: ptrint; 
  pmc: pmemchunk_fixed): boolean;
var
  pmc_temp: pmemchunk_fixed;
begin
  pmc_temp := loc_freelists^.fixedlists[chunkindex];
  while pmc_temp <> nil do
  begin
    if pmc_temp = pmc then exit(true);
    pmc_temp := pmc_temp^.next_fixed;
  end;
  result := false;
end;

{$endif}

procedure remove_from_list_fixed(pmc: pmemchunk_fixed; fixedlist: ppmemchunk_fixed); inline;
begin
  if assigned(pmc^.next_fixed) then
    pmc^.next_fixed^.prev_fixed := pmc^.prev_fixed;
  if assigned(pmc^.prev_fixed) then
    pmc^.prev_fixed^.next_fixed := pmc^.next_fixed
  else
    fixedlist^ := pmc^.next_fixed;
end;

procedure remove_from_list_var(pmc: pmemchunk_var); inline;
begin
  if assigned(pmc^.next_var) then
    pmc^.next_var^.prev_var := pmc^.prev_var;
  if assigned(pmc^.prev_var) then
    pmc^.prev_var^.next_var := pmc^.next_var
  else
    freelists.varlist := pmc^.next_var;
end;

procedure remove_all_from_list_fixed(chunksize: ptrint; poc: poschunk);
var
  pmc, pmc_end: pmemchunk_fixed;
  fixedlist: ppmemchunk_fixed;
begin
  pmc := pmemchunk_fixed(pointer(poc)+fixedfirstoffset);
  pmc_end := pmemchunk_fixed(pointer(poc)+(poc^.size and sizemask)-chunksize);
  fixedlist := @poc^.freelists^.fixedlists[chunksize shr blockshift];
  repeat
    remove_from_list_fixed(pmc, fixedlist);
    pmc := pointer(pmc)+chunksize;
  until pmc > pmc_end;
end;

procedure append_to_oslist(poc: poschunk; chunksize: ptrint);
var
  pocsize: ptrint;
  loc_freelists: pfreelists;
begin
  loc_freelists := poc^.freelists;
  { check if already on list }
  if (poc^.size and ocrecycleflag) <> 0 then
    begin
      inc(loc_freelists^.oscount);
      poc^.size := poc^.size and not ocrecycleflag;
      exit;
    end;
  { decide whether to free block or add to list }
{$ifdef HAS_SYSOSFREE}
  pocsize := poc^.size and sizemask;
  if (loc_freelists^.oscount >= MaxKeptOSChunks) or
     (pocsize > growheapsize2) then
    begin
      if chunksize <> 0 then
        remove_all_from_list_fixed(chunksize, poc);
      dec(loc_freelists^.internal_status.currheapsize, pocsize);
      SysOSFree(poc, pocsize);
    end
  else
    begin
{$endif}
      poc^.next := loc_freelists^.oslist;
      loc_freelists^.oslist := poc;
      inc(loc_freelists^.oscount);
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

function split_block(pcurr: pmemchunk_var; size: ptrint): pmemchunk_var;
var
  pcurr_tmp : pmemchunk_var;
  sizeleft: ptrint;
begin
  sizeleft := (pcurr^.size and sizemask)-size;
  if sizeleft>=sizeof(tmemchunk_var) then
    begin
      pcurr_tmp := pmemchunk_var(pointer(pcurr)+size);
      { update prevsize of block to the right }
      if (pcurr^.size and lastblockflag) = 0 then
        pmemchunk_var(pointer(pcurr)+(pcurr^.size and sizemask))^.prevsize := sizeleft;
      { inherit the lastblockflag }
      pcurr_tmp^.size := sizeleft or (pcurr^.size and lastblockflag);
      pcurr_tmp^.prevsize := size;
      pcurr_tmp^.freelists := pcurr^.freelists;
      { the block we return is not the last one anymore (there's now a block after it) }
      { decrease size of block to new size }
      pcurr^.size := size or (pcurr^.size and (not sizemask and not lastblockflag));
      { insert the block in the freelist }
      append_to_list_var(pcurr_tmp);
      result := pcurr_tmp;
    end
  else
    result := nil;
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


{*****************************************************************************
                                Grow Heap
*****************************************************************************}

function alloc_oschunk(loc_freelists: pfreelists; chunkindex, size: ptrint): pointer;
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
  status    : pfpcheapstatus;
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
  poc := loc_freelists^.oslist;
  prev_poc := nil;
  while poc <> nil do
    begin
      if (poc^.size and ocrecycleflag) <> 0 then
      begin
        { oops! we recycled this chunk; remove it from list }
        poc^.size := poc^.size and not ocrecycleflag;
        poc := poc^.next;
        if prev_poc = nil then
          loc_freelists^.oslist := poc
        else
          prev_poc^.next := poc;
        continue;
      end;
      pocsize := poc^.size and sizemask;
      if (pocsize >= minsize) and
         (pocsize <= maxsize) then
        begin
          size := pocsize;
          if prev_poc = nil then
            loc_freelists^.oslist := poc^.next
          else
            prev_poc^.next := poc^.next;
          dec(loc_freelists^.oscount);
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
      DumpBlocks(loc_freelists);
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
      poc^.freelists := loc_freelists;
      { set the total new heap size }
      status := @loc_freelists^.internal_status;
      inc(status^.currheapsize, size);
      if status^.currheapsize > status^.maxheapsize then
        status^.maxheapsize := status^.currheapsize;
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
      pmc_next := loc_freelists^.fixedlists[chunkindex];
      pmc^.next_fixed := pmc_next;
      if pmc_next<>nil then
        pmc_next^.prev_fixed := pmc;
      loc_freelists^.fixedlists[chunkindex] := pmemchunk_fixed(result);
    end
  else
    begin
      { we need to align the user pointers to 8 byte at least for
        mmx/sse and doubles on sparc, align to 16 bytes }
      result := pointer(poc)+varfirstoffset;
      pmcv := pmemchunk_var(result);
      pmcv^.size := ((size-varfirstoffset) and sizemask) or (firstblockflag or lastblockflag);
      pmcv^.prevsize := 0;
      pmcv^.freelists := loc_freelists;
      append_to_list_var(pmcv);
    end;
end;

{*****************************************************************************
                                 SysGetMem
*****************************************************************************}

function finish_waitfixedlist(loc_freelists: pfreelists): boolean; forward;
procedure finish_waitvarlist(loc_freelists: pfreelists); forward;

function SysGetMem_Fixed(chunksize: ptrint): pointer;
var
  pmc, pmc_next: pmemchunk_fixed;
  poc: poschunk;
  chunkindex: ptrint;
  loc_freelists: pfreelists;
begin
  { try to find a block in one of the freelists per size }
  chunkindex := chunksize shr blockshift;
  loc_freelists := @freelists;
  pmc := loc_freelists^.fixedlists[chunkindex];
  { no free blocks ? }
  if assigned(pmc) then
    begin
      { remove oschunk from free list in case we recycle it }
      poc := poschunk(pointer(pmc) - (pmc^.size shr fixedoffsetshift));
      if poc^.used = 0 then
        begin
          poc^.size := poc^.size or ocrecycleflag;
          dec(loc_freelists^.oscount);
        end;
    end
  else if finish_waitfixedlist(loc_freelists) then
      { freed some to-be freed chunks, retry allocation }
    exit(SysGetMem_Fixed(chunksize))
  else
    begin
      pmc := alloc_oschunk(loc_freelists, chunkindex, chunksize);
      if not assigned(pmc) then
        exit(nil);
      poc := poschunk(pointer(pmc)-fixedfirstoffset);
    end;
  { get a pointer to the block we should return }
  result := pointer(pmc)+sizeof(tmemchunk_fixed_hdr);
  { update freelist }
  pmc_next := pmc^.next_fixed;
  loc_freelists^.fixedlists[chunkindex] := pmc_next;
  if assigned(pmc_next) then
    pmc_next^.prev_fixed := nil;
  inc(poc^.used);
  { statistics }
  with loc_freelists^.internal_status do
  begin
    inc(currheapused, chunksize);
    if currheapused > maxheapused then
    begin
      maxheapused := currheapused;
{$ifdef DUMP_MEM_USAGE}        
      maxsizeusage := sizeusage;
{$endif}        
    end;
  end;
end;

function SysGetMem_Var(size: ptrint): pointer;
var
  pcurr : pmemchunk_var;
  pbest : pmemchunk_var;
  loc_freelists : pfreelists;
  iter : longint;
begin
  result:=nil;
  { free pending items }
  loc_freelists := @freelists;
  finish_waitvarlist(loc_freelists);
  pbest := nil;
  pcurr := loc_freelists^.varlist;
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
    pcurr := alloc_oschunk(loc_freelists, 0, size);
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
  size := pcurr^.size and sizemask;
  pcurr^.size := pcurr^.size or usedflag;
  pcurr^.freelists := loc_freelists;
  { statistics }
  with loc_freelists^.internal_status do
  begin
    inc(currheapused, size);
    if currheapused > maxheapused then
    begin
      maxheapused := currheapused;
{$ifdef DUMP_MEM_USAGE}        
      maxsizeusage := sizeusage;
{$endif}        
    end;
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

procedure waitfree_fixed(pmc: pmemchunk_fixed; loc_freelists: pfreelists);
begin
  entercriticalsection(loc_freelists^.lockfixed);
  pmc^.next_fixed := loc_freelists^.waitfixed;
  loc_freelists^.waitfixed := pmc;
  leavecriticalsection(loc_freelists^.lockfixed);
end;

procedure waitfree_var(pmcv: pmemchunk_var; loc_freelists: pfreelists);
begin
  entercriticalsection(loc_freelists^.lockvar);
  pmcv^.next_var := loc_freelists^.waitvar;
  loc_freelists^.waitvar := pmcv;
  leavecriticalsection(loc_freelists^.lockvar);
end;

function SysFreeMem_Fixed(pmc: pmemchunk_fixed): ptrint;
var
  chunkindex,
  chunksize: ptrint;
  poc: poschunk;
  pmc_next: pmemchunk_fixed;
  loc_freelists: pfreelists;
begin
  poc := poschunk(pointer(pmc)-(pmc^.size shr fixedoffsetshift));
  chunksize := pmc^.size and fixedsizemask;
  loc_freelists := @freelists;
  if loc_freelists <> poc^.freelists then
  begin
    if poc^.freelists = main_orig_freelists then
      poc^.freelists := main_relo_freelists;
    if loc_freelists <> poc^.freelists then
    begin
      { deallocated in wrong thread! add to to-be-freed list of correct thread }
      waitfree_fixed(pmc, poc^.freelists);
      exit(chunksize);
    end;
  end;

  dec(loc_freelists^.internal_status.currheapused, chunksize);
  { insert the block in it's freelist }
  chunkindex := chunksize shr blockshift;
  pmc_next := loc_freelists^.fixedlists[chunkindex];
  pmc^.prev_fixed := nil;
  pmc^.next_fixed := pmc_next;
  if assigned(pmc_next) then
    pmc_next^.prev_fixed := pmc;
  loc_freelists^.fixedlists[chunkindex] := pmc;
  { decrease used blocks count }
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
  loc_freelists: pfreelists;
begin
  chunksize := pmcv^.size and sizemask;
  loc_freelists := @freelists;
  if loc_freelists <> pmcv^.freelists then
  begin
    if pmcv^.freelists = main_orig_freelists then
      pmcv^.freelists := main_relo_freelists;
    if loc_freelists <> pmcv^.freelists then
    begin
      { deallocated in wrong thread! add to to-be-freed list of correct thread }
      waitfree_var(pmcv, pmcv^.freelists);
      exit(chunksize);
    end;
  end;

  dec(loc_freelists^.internal_status.currheapused, chunksize);
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

function finish_waitfixedlist(loc_freelists: pfreelists): boolean;
  { free to-be-freed chunks, return whether we freed anything }
var
  pmc: pmemchunk_fixed;
begin
  if loc_freelists^.waitfixed = nil then 
    exit(false);
  entercriticalsection(loc_freelists^.lockfixed);
  while loc_freelists^.waitfixed <> nil do
  begin
    { keep next_fixed, might be destroyed }
    pmc := loc_freelists^.waitfixed;
    loc_freelists^.waitfixed := pmc^.next_fixed;
    SysFreeMem_Fixed(pmc);
  end;
  leavecriticalsection(loc_freelists^.lockfixed);
  result := true;
end;

procedure finish_waitvarlist(loc_freelists: pfreelists);
  { free to-be-freed chunks, return whether we freed anything }
var
  pmcv: pmemchunk_var;
begin
  loc_freelists := @freelists;
  if loc_freelists^.waitvar = nil then 
    exit;
  entercriticalsection(loc_freelists^.lockvar);
  while loc_freelists^.waitvar <> nil do
  begin
    { keep next_var, might be destroyed }
    pmcv := loc_freelists^.waitvar;
    loc_freelists^.waitvar := pmcv^.next_var;
    SysFreeMem_Var(pmcv);
  end;
  leavecriticalsection(loc_freelists^.lockvar);
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
  pnext : pmemchunk_var;
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

  { var memchunk }

  { do not fragment the heap with small shrinked blocks }
  {  also solves problem with var sized chunks smaller than sizeof(tmemchunk_var) }
  if size < maxblocksize div 2 then
    exit(false);

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
     pnext:=pmemchunk_var(pointer(pcurr)+currsize);
     if ((chunksize and lastblockflag) = 0)
        and ((pnext^.size and usedflag) = 0) 
        and ((pnext^.size and sizemask) >= size-currsize) then
       begin
         concat_two_blocks(pcurr,pnext);
         currsize := pcurr^.size and sizemask;
       end
     else
       exit;
   end;
  { is the size smaller then we can adjust the block to that size and insert
    the other part into the freelist }
  if currsize>size then
  begin
    pnext := split_block(pcurr, size);
    currsize := pcurr^.size and sizemask;
    if pnext <> nil then
      try_concat_free_chunk_forward(pnext);
  end;

  with freelists.internal_status do
  begin
    inc(currheapused, currsize-oldsize);
    if currheapused > maxheapused then
      maxheapused := currheapused;
  end;
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

{$ifndef HAS_MEMORYMANAGER}

{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{$if not(defined(gba)) and not(defined(nds))}
{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeapThread;
var
  loc_freelists: pfreelists;
begin
  loc_freelists := @freelists;
  fillchar(loc_freelists^,sizeof(tfreelists),0);
  initcriticalsection(loc_freelists^.lockfixed);
  initcriticalsection(loc_freelists^.lockvar);
{$ifdef DUMP_MEM_USAGE}
  fillchar(sizeusage,sizeof(sizeusage),0);
  fillchar(maxsizeusage,sizeof(sizeusage),0);
{$endif}
end;

procedure InitHeap;
var
  loc_freelists: pfreelists;
begin
  { we cannot initialize the locks here yet, thread support is
    not loaded yet }
  loc_freelists := @freelists;
  fillchar(loc_freelists^,sizeof(tfreelists),0);
  { main freelist will be copied in memory }
  main_orig_freelists := loc_freelists;
end;
{$endif}

procedure RelocateHeap;
var
  loc_freelists: pfreelists;
begin
  { this function should be called in main thread context }
  loc_freelists := @freelists;
  main_relo_freelists := loc_freelists;
  initcriticalsection(loc_freelists^.lockfixed);
  initcriticalsection(loc_freelists^.lockvar);
  if MemoryManager.RelocateHeap <> nil then
    MemoryManager.RelocateHeap();
end;

procedure FinalizeHeap;
var
  poc : poschunk;
  i : longint;
  loc_freelists: pfreelists;
begin
  loc_freelists := @freelists;
  finish_waitfixedlist(loc_freelists);
  finish_waitvarlist(loc_freelists);
  if main_relo_freelists <> nil then
  begin
    donecriticalsection(loc_freelists^.lockfixed);
    donecriticalsection(loc_freelists^.lockvar);
  end;
{$ifdef SHOW_MEM_USAGE}
  writeln('Max heap used/size: ', loc_freelists^.internal_status.maxheapused, '/', 
    loc_freelists^.internal_status.maxheapsize);
  flush(output);
{$endif}
{$ifdef DUMP_MEM_USAGE}
  for i := 0 to sizeusageindex-1 do
    if maxsizeusage[i] <> 0 then
      writeln('size ', i shl sizeusageshift, ' usage ', maxsizeusage[i]);
  writeln('size >', sizeusagesize, ' usage ', maxsizeusage[sizeusageindex]);
  flush(output);
{$endif}
{$ifdef HAS_SYSOSFREE}
  while assigned(loc_freelists^.oslist) do
    begin
      poc:=loc_freelists^.oslist^.next;
      SysOSFree(loc_freelists^.oslist, loc_freelists^.oslist^.size and sizemask);
      dec(loc_freelists^.oscount);
      loc_freelists^.oslist:=poc;
    end;
{$endif HAS_SYSOSFREE}
end;

{$endif HAS_MEMORYMANAGER}
