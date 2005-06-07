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
    GetHeapStatus: @SysGetHeapStatus;
{$ifdef HASGETFPCHEAPSTATUS}
    GetFPCHeapStatus: @SysGetFPCHeapStatus;
{$endif HASGETFPCHEAPSTATUS}
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
{$ifdef HASGETFPCHEAPSTATUS}
  internal_status : TFPCHeapStatus;
{$else HASGETFPCHEAPSTATUS}
  internal_status : THeapStatus;
{$endif HASGETFPCHEAPSTATUS}

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


{$ifdef HASGETFPCHEAPSTATUS}
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
{$else HASGETFPCHEAPSTATUS}
procedure GetHeapStatus(var status:THeapStatus);
begin
  if IsMultiThread and MemoryManager.NeedLock then
   begin
     try
       MemoryMutexManager.MutexLock;
       MemoryManager.GetHeapStatus(status);
     finally
       MemoryMutexManager.MutexUnlock;
     end;
   end
  else
   begin
     MemoryManager.GetHeapStatus(status);
   end;
end;
{$endif HASGETFPCHEAPSTATUS}



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


{ Bootstrapping }
{$ifndef HASGETHEAPSTATUS}
Function  Memavail:ptrint;
begin
  result:=0;
end;
Function  Maxavail:ptrint;
begin
  result:=0;
end;
Function  Heapsize:ptrint;
begin
  result:=0;
end;
{$endif HASGETHEAPSTATUS}

{*****************************************************************************
                               GetHeapStatus
*****************************************************************************}

{$ifdef HASGETFPCHEAPSTATUS}
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
{$else}
procedure SysGetHeapStatus(var status:THeapStatus);
begin
  internal_status.CurrHeapFree:=internal_status.CurrHeapSize-internal_status.CurrHeapUsed;
  status:=internal_status;
end;
{$endif HASGETFPCHEAPSTATUS}



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
  if (freeoslistcount >= MaxKeptOSChunks)
  or (poc^.size > growheapsize2) then
    begin
      dec(internal_status.currheapsize, poc^.size);
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

function alloc_oschunk(blockindex, size: ptrint): pointer;
var
  pmc       : pmemchunk_fixed;
  pmcv      : pmemchunk_var;
  minsize,
  maxsize,
  i, count  : ptrint;
  chunksize : ptrint;
begin
  { increase size by size needed for os block header }
  minsize := size + sizeof(toschunk);
  if blockindex<>0 then
    maxsize := (size * $ffff) + sizeof(toschunk)
  else
    maxsize := high(ptrint);
  { blocks available in freelist? }
  result := freeoslist;
  while result <> nil do
    begin
      if (poschunk(result)^.size >= minsize) and
         (poschunk(result)^.size <= maxsize) then
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
      writeln('growheap(',size,')  allocating ',(size+sizeof(toschunk)+$ffff) and $ffff0000);
      DumpBlocks;
{$endif}
      { allocate by 64K size }
      size := (size+sizeof(toschunk)+$ffff) and not $ffff;
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
    inc(internal_status.currheapsize,size);
    if internal_status.currheapsize>internal_status.maxheapsize then
      internal_status.maxheapsize:=internal_status.currheapsize;
  end;
  { initialize os-block }
  poschunk(result)^.used := 0;
  poschunk(result)^.size := size;
  inc(result, sizeof(toschunk));
  if blockindex<>0 then
    begin
      { chop os chunk in fixedsize parts,
        maximum of $ffff elements are allowed, otherwise
        there will be an overflow }
      chunksize := blockindex shl blockshr;
      count := (size-sizeof(toschunk)) div chunksize;
      if count>$ffff then
        HandleError(204);
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
  { statistics }
  inc(internal_status.currheapused,size);
  if internal_status.currheapused>internal_status.maxheapused then
    internal_status.maxheapused:=internal_status.currheapused;
{$ifdef TestFreeLists}
  if test_each then
    TestFreeLists;
{$endif TestFreeLists}
end;

function SysGetMem_Var(size: ptrint): pointer;
var
  pcurr : pmemchunk_var;
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
  { statistics }
  inc(internal_status.currheapused,size);
  if internal_status.currheapused>internal_status.maxheapused then
    internal_status.maxheapused:=internal_status.currheapused;
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
  dec(internal_status.currheapused,pcurrsize);
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
  dec(internal_status.currheapused,pcurrsize);
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
    exit;
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
  currsize : ptrint;
  pcurr : pmemchunk_var;
begin
  SysTryResizeMem := false;

  { fix p to point to the heaprecord }
  pcurrsize := pmemchunk_fixed(p-sizeof(tmemchunk_fixed_hdr))^.size;
  if (pcurrsize and fixedsizeflag) = 0 then
    begin
      currsize := pcurrsize and sizemask;
      size := (size+sizeof(tmemchunk_var_hdr)+(blocksize-1)) and sizemask;
    end
  else
    begin
      currsize := pcurrsize and fixedsizemask;
      size := (size+sizeof(tmemchunk_fixed_hdr)+(blocksize-1)) and fixedsizemask;
    end;

  { is the allocated block still correct? }
  if (currsize>=size) and (size>(currsize-blocksize)) then
    begin
      SysTryResizeMem := true;
{$ifdef TestFreeLists}
       if test_each then
         TestFreeLists;
{$endif TestFreeLists}
       exit;
   end;

  { don't do resizes on fixed-size blocks }
  if (pcurrsize and fixedsizeflag) <> 0 then
    exit;

  { get pointer to block }
  pcurr := pmemchunk_var(pointer(p)-sizeof(tmemchunk_var_hdr));
  oldsize := currsize;

  { do we need to allocate more memory ? }
  if size>currsize then
   begin
     { the size is bigger than the previous size, we need to allocated more mem.
       We first check if the blocks after the current block are free. If not we
       simply call getmem/freemem to get the new block }
     if check_concat_free_chunk_forward(pcurr,size) then
       begin
         try_concat_free_chunk_forward(pcurr);
         currsize := (pcurr^.size and sizemask);
       end;
   end;

  { not enough space? }
  if size>currsize then
    exit;

  { is the size smaller then we can adjust the block to that size and insert
    the other part into the freelist }
  if currsize>size then
    split_block(pcurr, size);

  inc(internal_status.currheapused,size-oldsize);
  SysTryResizeMem := true;

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
   end
  else
   { Allocate a new block? }
   if p=nil then
    begin
      p := MemoryManager.GetMem(size);
    end
  else
   { Resize block }
   if not SysTryResizeMem(p,size) then
    begin
      minsize := MemoryManager.MemSize(p);
      if size < minsize then
        minsize := size;
      p2 := MemoryManager.GetMem(size);
      if p2<>nil then
        Move(p^,p2^,minsize);
      MemoryManager.FreeMem(p);
      p := p2;
    end;
  SysReAllocMem := p;
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
  fillchar(internal_status,sizeof(internal_status),0);
end;


