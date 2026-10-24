{
  Tiny PS1 heap manager - PCSX-Redux PSYQo Heap Viewer compatible.

  Layout of EVERY block (allocated or free):

      offset 0: Next    (4 bytes; used only while free)
      offset 4: Size    (4 bytes; total size INCLUDING this header)
      offset 8: user payload

  Size must be 8-byte aligned and contain NO allocation/free flags.
  Free blocks are linked in ascending-address order.  Their list ends at
  @(_heapMetadata.MarkerNext), never at nil.

  The growing heap is [_heapStart, _heapEnd).  Above _heapEnd remains
  untouched space available up to _heapLimit.  The PCSX viewer shows the
  committed portion, not the entire maximum heap capacity.
}

{$define HAS_MEMORYMANAGER}

const
  PS1_HEAP_ALIGN = SizeUInt(8);
  PS1_HEAP_HEADER = SizeUInt(8);


type
  PHeapBlock = ^THeapBlock;
  THeapBlock = packed record
    Next : PHeapBlock;      { +0: next FREE block (otherwise unused) }
    Size : SizeUInt;        { +4: TOTAL bytes, 8-aligned, no flags }
  end;

  THeapMetadata = packed record
    Head           : PHeapBlock; { +0: first free block or Marker }
    Bottom         : Pointer;    { +4: heap beginning }
    Top            : Pointer;    { +8: current committed heap end }
    MaximumHeapEnd : Pointer;    { +12: highest historical heap end }
    MarkerNext     : PHeapBlock; { +16: embedded end-of-list sentinel }
  end;

var
  _heapStart   : PtrUInt = 0;
  _heapEnd     : PtrUInt = 0;
  _heapLimit   : PtrUInt = 0;

  _heapMetadata: THeapMetadata;

  { Payload byte counters; allocation headers are excluded. }
  _heapUsed    : SizeUInt = 0;
  _heapMaxUsed : SizeUInt = 0;

{ Call with interrupts disabled, outside an interrupt handler.
  Guard is space below the active stack, not a measured peak margin. }
function SetStackLength(NewLength: SizeUInt): Boolean;
const
  Guard = PtrUInt(4096);
var
  StackTop, NewBottom, ActiveSP: PtrUInt;
begin
  Result := False;

  if (_heapStart = 0) or
     (NewLength < Guard) or
     (NewLength > High(SizeUInt) - 63) then Exit;

  NewLength := (NewLength + 63) and not SizeUInt(63);
  StackTop := PtrUInt(StackBottom) + StackLength;

  if NewLength > StackTop - _heapStart then Exit;
  NewBottom := StackTop - NewLength;

  { Never overlap committed heap blocks. }
  if NewBottom < _heapEnd then Exit;

  { Never cut into the current stack. }
  ActiveSP := CurrentSP;
  if ActiveSP <= NewBottom then Exit;
  if ActiveSP - NewBottom < Guard then Exit;

  _heapLimit := NewBottom;
  StackBottom := Pointer(NewBottom);
  StackLength := NewLength;

  Result := True;
end;


procedure TraceHeapBuffer(const labelText: ShortString; buffer: Pointer; bytes: PtrUInt);
begin
  WriteLn('HEAP BUFFER ', labelText, ' payload=', PrintHexValue(PtrUInt(buffer)),
          ' requested=', bytes);
end;

procedure TraceHeap(const stage: ShortString);
var
  block: PHeapBlock;
  bottom, top, address, previousEnd, totalFree, largestFree, blocks: PtrUInt;
begin
  bottom := PtrUInt(_heapMetadata.bottom);
  top := PtrUInt(_heapMetadata.top);
  WriteLn('HEAP ', stage, ' bottom=', PrintHexValue(bottom), ' top=', PrintHexValue(top));
  block := _heapMetadata.head;
  previousEnd := bottom;
  totalFree := 0;
  largestFree := 0;
  blocks := 0;
  while block <> PHeapBlock(@_heapMetadata.markerNext) do begin
    address := PtrUInt(block);
    if (address < previousEnd) or (address >= top) or
       ((address and 7) <> 0) or (top - address < SizeOf(THeapBlock)) then begin
      WriteLn('HEAP CORRUPT free-list address=', PrintHexValue(address));
      Exit;
    end;
    if (block^.size < 16) or ((block^.size and 7) <> 0) or
       (block^.size > top - address) then begin
      WriteLn('HEAP CORRUPT block=', PrintHexValue(address), ' size=', block^.size);
      Exit;
    end;
    WriteLn('HEAP FREE header=', PrintHexValue(address), ' end=', PrintHexValue(address + block^.size), ' bytes=', block^.size);
    Inc(totalFree, block^.size);
    if block^.size > largestFree then largestFree := block^.size;
    Inc(blocks);
    previousEnd := address + block^.size;
    block := block^.next;
  end;
  WriteLn('HEAP SUMMARY committed=', top - bottom, ' free=', totalFree,
          ' largest=', largestFree, ' blocks=', blocks);
end;


{ --------------------------------------------------------------------------
  Internal helpers
  -------------------------------------------------------------------------- }

function _HeapMarker: PHeapBlock; inline;
begin
  Result := PHeapBlock(@_heapMetadata.MarkerNext);
end;

function _AlignHeapSize(Value: SizeUInt): SizeUInt; inline;
begin
  Result := (Value + PS1_HEAP_ALIGN - 1) and
            not SizeUInt(PS1_HEAP_ALIGN - 1);
end;

function _BlockSize(Block: PHeapBlock): SizeUInt; inline;
begin
  Result := Block^.Size - PS1_HEAP_HEADER;
end;

function _BlockData(Block: PHeapBlock): Pointer; inline;
begin
  Result := Pointer(PtrUInt(Block) + PS1_HEAP_HEADER);
end;

function _DataBlock(P: Pointer): PHeapBlock; inline;
begin
  Result := PHeapBlock(PtrUInt(P) - PS1_HEAP_HEADER);
end;

procedure _HeapAddUsed(Size: SizeUInt); inline;
begin
  _heapUsed := _heapUsed + Size;
  if _heapUsed > _heapMaxUsed then
    _heapMaxUsed := _heapUsed;
end;

procedure _HeapSubUsed(Size: SizeUInt); inline;
begin
  if Size <= _heapUsed then
    _heapUsed := _heapUsed - Size
  else
    _heapUsed := 0;
end;

{ Validate that Addr is an actual block boundary by scanning the heap.
  A corrupted block size makes the function return False. }
function _IsBlockStart(Addr: PtrUInt): Boolean;
var
  Cur, N: PtrUInt;
  Total: SizeUInt;
begin
  Result := False;
  if (Addr < _heapStart) or (Addr >= _heapEnd) then Exit;

  Cur := _heapStart;
  while Cur < _heapEnd do
  begin
    if Cur = Addr then
    begin
      Result := True;
      Exit;
    end;

    Total := PHeapBlock(Cur)^.Size;
    if (Total < PS1_HEAP_HEADER + PS1_HEAP_ALIGN) or
       ((Total and (PS1_HEAP_ALIGN - 1)) <> 0) or
       (Total > _heapEnd - Cur) then Exit;

    N := Cur + Total;
    if N <= Cur then Exit;
    Cur := N;
  end;
end;

{ Find a free block by exact address; optional result Prev is nil when
  the matching block is first in the free list. }
function _FindFree(Block: PHeapBlock; var Prev: PHeapBlock): Boolean;
var
  Cur: PHeapBlock;
begin
  Prev := nil;
  Cur := _heapMetadata.Head;
  while Cur <> _HeapMarker do
  begin
    if Cur = nil then Break;
    if PtrUInt(Cur) >= PtrUInt(Block) then Break;
    Prev := Cur;
    Cur := Cur^.Next;
  end;
  Result := Cur = Block;
end;

{ Unlink a known free block, with its preceding free block (or nil). }
procedure _UnlinkFree(Block, Prev: PHeapBlock); inline;
begin
  if Prev = nil then
    _heapMetadata.Head := Block^.Next
  else
    Prev^.Next := Block^.Next;
end;

{ Insert an existing free block into the sorted free list.  Merge adjacent
  blocks and discard the final free block by moving heap Top downward. }
procedure _InsertFree(Block: PHeapBlock);
var
  Prev, PrevPrev, Cur: PHeapBlock;
begin
  Prev := nil;
  PrevPrev := nil;
  Cur := _heapMetadata.Head;

  while (Cur <> nil) and (Cur <> _HeapMarker) and
        (PtrUInt(Cur) < PtrUInt(Block)) do
  begin
    PrevPrev := Prev;
    Prev := Cur;
    Cur := Cur^.Next;
  end;

  { Normally impossible for a valid non-free block. }
  if Cur = nil then Exit;
  if Cur = Block then Exit;
  if (Prev <> nil) and
     (PtrUInt(Prev) + Prev^.Size > PtrUInt(Block)) then Exit;
  if (Cur <> _HeapMarker) and
     (PtrUInt(Block) + Block^.Size > PtrUInt(Cur)) then Exit;

  Block^.Next := Cur;
  if Prev = nil then
    _heapMetadata.Head := Block
  else
    Prev^.Next := Block;

  { Coalesce forwards. }
  if (Cur <> _HeapMarker) and
     (PtrUInt(Block) + Block^.Size = PtrUInt(Cur)) then
  begin
    Block^.Size := Block^.Size + Cur^.Size;
    Block^.Next := Cur^.Next;
  end;

  { Coalesce backwards. }
  if (Prev <> nil) and
     (PtrUInt(Prev) + Prev^.Size = PtrUInt(Block)) then
  begin
    Prev^.Size := Prev^.Size + Block^.Size;
    Prev^.Next := Block^.Next;
    Block := Prev;
    { Remember the predecessor of the newly merged block. }
    Prev := PrevPrev;
  end;

  { Release a free block at the top of the committed heap.  The free
    list must not contain blocks beyond Top. }
  if PtrUInt(Block) + Block^.Size = _heapEnd then
  begin
    _UnlinkFree(Block, Prev);
    _heapEnd := PtrUInt(Block);
    _heapMetadata.Top := Pointer(_heapEnd);
  end;
end;

{ --------------------------------------------------------------------------
  Initialize the heap after BSS.  Called by the PS1 RTL startup code.
  -------------------------------------------------------------------------- }
procedure _InitHeap(TheBSSend, TheHeapLimit: PtrUInt);
begin
  _heapStart := (TheBSSend + PS1_HEAP_ALIGN - 1) and
                not PtrUInt(PS1_HEAP_ALIGN - 1);
  _heapLimit := TheHeapLimit and not PtrUInt(PS1_HEAP_ALIGN - 1);
  if _heapLimit < _heapStart then _heapLimit := _heapStart;
  _heapEnd := _heapStart;

  _heapUsed := 0;
  _heapMaxUsed := 0;

  _heapMetadata.Bottom := Pointer(_heapStart);
  _heapMetadata.Top := Pointer(_heapEnd);
  _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);
  _heapMetadata.MarkerNext := nil;
  _heapMetadata.Head := _HeapMarker;


  if pcsxPresent then
    PLongWord(PtrUInt($1F8020A0))^ := LongWord(PtrUInt(@_heapMetadata));
    
end;

{ --------------------------------------------------------------------------
  Allocate: first-fit reuse of free blocks, or extend the committed heap.
  -------------------------------------------------------------------------- }
function _Malloc(Size: SizeUInt): Pointer;
var
  Prev, Block, Remain: PHeapBlock;
  Needed, Left, Payload: SizeUInt;
begin
  Result := nil;
  if Size = 0 then Exit;
  if (_heapStart = 0) or (_heapEnd > _heapLimit) then Exit;
  if Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1) then Exit;

  Payload := _AlignHeapSize(Size);
  Needed := Payload + PS1_HEAP_HEADER;

  Prev := nil;
  Block := _heapMetadata.Head;
  while Block <> _HeapMarker do
  begin
    if Block = nil then Exit; { corrupted free list }
    if Block^.Size >= Needed then
    begin
      Left := Block^.Size - Needed;
      if Left >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
      begin
        Remain := PHeapBlock(PtrUInt(Block) + Needed);
        Remain^.Next := Block^.Next;
        Remain^.Size := Left;
        if Prev = nil then
          _heapMetadata.Head := Remain
        else
          Prev^.Next := Remain;
        Block^.Size := Needed;
      end
      else
        _UnlinkFree(Block, Prev);

      Block^.Next := nil;
      _HeapAddUsed(_BlockSize(Block));
      Result := _BlockData(Block);
      Exit;
    end;
    Prev := Block;
    Block := Block^.Next;
  end;

  if Needed > _heapLimit - _heapEnd then Exit;
  Block := PHeapBlock(_heapEnd);
  Block^.Next := nil;
  Block^.Size := Needed;
  _heapEnd := _heapEnd + Needed;
  _heapMetadata.Top := Pointer(_heapEnd);
  if _heapEnd > PtrUInt(_heapMetadata.MaximumHeapEnd) then
    _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);

  _HeapAddUsed(Payload);
  Result := _BlockData(Block);
end;

{ --------------------------------------------------------------------------
  Predict the PAYLOAD address returned by the next ordinary GetMem(Size).

  This is a read-only copy of _Malloc's first-fit decision: the first free
  block large enough for the size, otherwise the current committed heap top.
  It does NOT reserve memory. Another allocation/free/reallocation between
  this call and GetMem can invalidate the prediction.

  Size matters: a small free block may be skipped for larger requests.
  Return nil if the request is invalid or no block can satisfy it.
  -------------------------------------------------------------------------- }
function GetNextFreeAddress(Size: PtrUInt): Pointer;
var
  Block: PHeapBlock;
  Payload, Needed: SizeUInt;
begin
  Result := nil;
  if Size = 0 then Exit;
  if (_heapStart = 0) or (_heapEnd < _heapStart) or
     (_heapEnd > _heapLimit) then Exit;
  if Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1) then Exit;

  Payload := _AlignHeapSize(Size);
  Needed := Payload + PS1_HEAP_HEADER;

  Block := _heapMetadata.Head;
  while Block <> _HeapMarker do
  begin
    if Block = nil then Exit; { corrupted free list }
    if Block^.Size >= Needed then
    begin
      Result := _BlockData(Block);
      Exit;
    end;
    Block := Block^.Next;
  end;

  { _Malloc would extend the heap if no existing free block fits. }
  if Needed <= _heapLimit - _heapEnd then
    Result := Pointer(_heapEnd + PS1_HEAP_HEADER);
end;

{ --------------------------------------------------------------------------
  Allocate at exactly the specified PAYLOAD address (NOT at/after it).

  Return codes:
    0 = success
    1 = requested block/range occupied
    2 = invalid or out-of-heap address
    3 = requested address not 8-byte aligned
    4 = size/range too large or not enough room
    5 = 8-byte gap (cannot represent a PSYQo free block)
    6 = heap not initialized / corrupt block layout

  The caller's FromHere is left unchanged on error.  The checked entry point defined below reports failures via WriteLn.
  The low-level allocator itself never performs I/O.
  -------------------------------------------------------------------------- }
function _MallocAt(DesiredPayload: PtrUInt; Size: SizeUInt;
                   var Status: LongInt): Pointer;
var
  Block, AllocBlock, TailFree, OldNext, Prev, GapFree: PHeapBlock;
  WantedHeader, Cur, BlockEnd, BlockStart: PtrUInt;
  Payload, Needed, Prefix, Suffix, AllocTotal, Gap: SizeUInt;
begin
  Result := nil;
  Status := 6;
  if (_heapStart = 0) or (_heapEnd < _heapStart) or
     (_heapEnd > _heapLimit) then Exit;

  Status := 4;
  if (Size = 0) or
     (Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1)) then Exit;
  Payload := _AlignHeapSize(Size);
  Needed := Payload + PS1_HEAP_HEADER;

  { Preserve the earlier 'nil means heap beginning' convention. }
  if DesiredPayload = 0 then
    DesiredPayload := _heapStart + PS1_HEAP_HEADER;

  Status := 3;
  if (DesiredPayload and (PS1_HEAP_ALIGN - 1)) <> 0 then Exit;

  Status := 2;
  if (DesiredPayload < _heapStart + PS1_HEAP_HEADER) or
     (DesiredPayload > _heapLimit) then Exit;
  WantedHeader := DesiredPayload - PS1_HEAP_HEADER;

  Status := 4;
  if (WantedHeader > _heapLimit) or
     (Needed > _heapLimit - WantedHeader) then Exit;

  { Exactly this address lies in the committed heap. Locate its parent
    block, so we can distinguish occupied from free memory. }
  if WantedHeader < _heapEnd then
  begin
    Cur := _heapStart;
    Block := nil;
    while Cur < _heapEnd do
    begin
      Block := PHeapBlock(Cur);
      Status := 6;
      if (Block^.Size < PS1_HEAP_HEADER + PS1_HEAP_ALIGN) or
         ((Block^.Size and (PS1_HEAP_ALIGN - 1)) <> 0) or
         (Block^.Size > _heapEnd - Cur) then Exit;
      BlockEnd := Cur + Block^.Size;
      if WantedHeader < BlockEnd then Break;
      Cur := BlockEnd;
    end;

    if (Cur >= _heapEnd) then Exit;
    BlockStart := Cur;
    Status := 1;
    if not _FindFree(Block, Prev) then Exit; { occupied }

    Prefix := WantedHeader - BlockStart;
    if Prefix = PS1_HEAP_ALIGN then
    begin
      Status := 5;  { can't split off a free block only 8 bytes long }
      Exit;
    end;

    { Must fit entirely inside this free block. Never shift the address
      or overwrite an allocated block beyond its end. }
    if Needed > BlockEnd - WantedHeader then Exit;

    OldNext := Block^.Next;
    Suffix := BlockEnd - WantedHeader - Needed;
    AllocTotal := Needed;
    if Suffix < PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
    begin
      AllocTotal := AllocTotal + Suffix;
      Suffix := 0;
    end;

    if Prefix = 0 then
    begin
      if Suffix <> 0 then
      begin
        TailFree := PHeapBlock(WantedHeader + AllocTotal);
        TailFree^.Size := Suffix;
        TailFree^.Next := OldNext;
        if Prev = nil then
          _heapMetadata.Head := TailFree
        else
          Prev^.Next := TailFree;
      end
      else
      begin
        if Prev = nil then
          _heapMetadata.Head := OldNext
        else
          Prev^.Next := OldNext;
      end;
    end
    else
    begin
      Block^.Size := Prefix;
      Block^.Next := OldNext;
      if Suffix <> 0 then
      begin
        TailFree := PHeapBlock(WantedHeader + AllocTotal);
        TailFree^.Size := Suffix;
        TailFree^.Next := OldNext;
        Block^.Next := TailFree;
      end;
    end;

    AllocBlock := PHeapBlock(WantedHeader);
    AllocBlock^.Next := nil;
    AllocBlock^.Size := AllocTotal;
    _HeapAddUsed(AllocTotal - PS1_HEAP_HEADER);
    Result := _BlockData(AllocBlock);
    Status := 0;
    Exit;
  end;

  { Desired address is at/above the top of the committed heap.
    Any skipped space must be representable as a whole free block. }
  Gap := WantedHeader - _heapEnd;
  if Gap = PS1_HEAP_ALIGN then
  begin
    Status := 5;
    Exit;
  end;

  AllocBlock := PHeapBlock(WantedHeader);
  AllocBlock^.Next := nil;
  AllocBlock^.Size := Needed;
  _heapEnd := WantedHeader + Needed;
  _heapMetadata.Top := Pointer(_heapEnd);
  if _heapEnd > PtrUInt(_heapMetadata.MaximumHeapEnd) then
    _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);

  if Gap >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
  begin
    GapFree := PHeapBlock(WantedHeader - Gap);
    GapFree^.Size := Gap;
    GapFree^.Next := nil;
    _InsertFree(GapFree);
  end;

  _HeapAddUsed(Payload);
  Result := _BlockData(AllocBlock);
  Status := 0;
end;

{ Extended entry point provides a status, so callers can emit useful
  errors outside of the low-level memory-manager code. }
function HeapGetMemFromEx(var FromHere: Pointer; Size: PtrUInt;
                             var Status: LongInt): Pointer;
var
  NextAddr: PtrUInt;
  Block: PHeapBlock;
begin
  Result := _MallocAt(PtrUInt(FromHere), Size, Status);
  if Result = nil then Exit;

  Block := _DataBlock(Result);
  NextAddr := PtrUInt(Block) + Block^.Size;
  if NextAddr <= High(PtrUInt) - PS1_HEAP_HEADER then
    FromHere := Pointer(NextAddr + PS1_HEAP_HEADER)
  else
    FromHere := nil;
end;

{ Backward-compatible symbol for direct calls made against the old RTL.
  This now also enforces an exact payload address; no forward search. }
function HeapGetMemFrom(var FromHere: Pointer; Size: PtrUInt): Pointer;
var
  Status: LongInt;
begin
  Result := HeapGetMemFromEx(FromHere, Size, Status);
end;

{ --------------------------------------------------------------------------
  Public helpers (implemented inside this same RTL include).
  These names are imported by the SYSHEAP_IMPORT branch above.
  -------------------------------------------------------------------------- }

{ This works for a currently allocated block from THIS heap only. }
function GetAddressAfterBlock(P: Pointer): Pointer;
var
  Block: PHeapBlock;
  Prev: PHeapBlock;
  HeaderAddr: PtrUInt;
begin
  Result := nil;
  if P = nil then Exit;
  if (PtrUInt(P) and (PS1_HEAP_ALIGN - 1)) <> 0 then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;

  HeaderAddr := PtrUInt(P) - PS1_HEAP_HEADER;
  if not _IsBlockStart(HeaderAddr) then Exit;
  Block := PHeapBlock(HeaderAddr);
  if _FindFree(Block, Prev) then Exit;  { reject already freed blocks }

  { The next block's payload is the current payload + total block size. }
  Result := Pointer(PtrUInt(P) + Block^.Size);
end;

{ Avoid SysUtils and heap-allocated strings in error reporting. }
function _HeapAddrHex(Value: PtrUInt): ShortString;
const
  Digits: string[16] = '0123456789ABCDEF';
var
  I: Integer;
begin
  SetLength(Result, SizeOf(PtrUInt) * 2);
  for I := Length(Result) downto 1 do
  begin
    Result[I] := Digits[(Value and 15) + 1];
    Value := Value shr 4;
  end;
end;

{ Exact-address allocation with diagnostics for failed attempts. }
function GetMemFrom(var FromHere: Pointer; Size: PtrUInt): Pointer;
var
  RequestedAddress: PtrUInt;
  Status: LongInt;
begin
  RequestedAddress := PtrUInt(FromHere);
  Result := HeapGetMemFromEx(FromHere, Size, Status);
  if Result <> nil then Exit;

  case Status of
    1: WriteLn('GetMemFrom: address/range occupied at $',
               _HeapAddrHex(RequestedAddress));
    2: WriteLn('GetMemFrom: address outside PS1 heap: $',
               _HeapAddrHex(RequestedAddress));
    3: WriteLn('GetMemFrom: address must be 8-byte aligned: $',
               _HeapAddrHex(RequestedAddress));
    4: WriteLn('GetMemFrom: not enough room or invalid size at $',
               _HeapAddrHex(RequestedAddress));
    5: WriteLn('GetMemFrom: 8-byte gap cannot hold a heap header at $',
               _HeapAddrHex(RequestedAddress));
    6: WriteLn('GetMemFrom: heap not initialized or corrupt');
  else
    WriteLn('GetMemFrom: allocation failed at $',
             _HeapAddrHex(RequestedAddress));
  end;
end;

procedure GetMemFrom(var FromHere: Pointer; var P: Pointer; Size: PtrUInt);
begin
  P:= GetMemFrom(FromHere, Size);
end;

{ --------------------------------------------------------------------------
  Free: insert in the free list, merge adjacent blocks, trim free tail.
  -------------------------------------------------------------------------- }
procedure _Free(P: Pointer);
var
  Block, Prev: PHeapBlock;
  Addr: PtrUInt;
  Size: SizeUInt;
begin
  if P = nil then Exit;
  if (PtrUInt(P) and (PS1_HEAP_ALIGN - 1)) <> 0 then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  Addr := PtrUInt(P) - PS1_HEAP_HEADER;
  if not _IsBlockStart(Addr) then Exit;
  Block := PHeapBlock(Addr);
  if _FindFree(Block, Prev) then Exit; { double free }

  Size := _BlockSize(Block);
  _HeapSubUsed(Size);
  _InsertFree(Block);
end;

{ --------------------------------------------------------------------------
  Copy bytes (avoids libc dependencies).
  -------------------------------------------------------------------------- }
procedure _CopyMem(Src, Dst: Pointer; Count: SizeUInt); inline;
var
  S, D: PByte;
begin
  S := PByte(Src);
  D := PByte(Dst);
  while Count <> 0 do
  begin
    D^ := S^;
    Inc(S);
    Inc(D);
    Dec(Count);
  end;
end;

{ --------------------------------------------------------------------------
  Reallocate, preferring in-place shrink/grow.
  -------------------------------------------------------------------------- }
function _Realloc(P: Pointer; Size: SizeUInt): Pointer;
var
  Block, Neighbor, FreePart, Prev: PHeapBlock;
  OldPayload, NewPayload, OldTotal, Needed, Capacity, Extra: SizeUInt;
  NewPtr: Pointer;
begin
  Result := nil;
  if P = nil then
  begin
    Result := _Malloc(Size);
    Exit;
  end;
  if Size = 0 then
  begin
    _Free(P);
    Exit;
  end;
  if Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1) then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  if not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER) then Exit;

  Block := _DataBlock(P);
  if _FindFree(Block, Prev) then Exit;
  OldTotal := Block^.Size;
  OldPayload := OldTotal - PS1_HEAP_HEADER;
  NewPayload := _AlignHeapSize(Size);
  Needed := NewPayload + PS1_HEAP_HEADER;

  if Needed = OldTotal then
  begin
    Result := P;
    Exit;
  end;

  if Needed < OldTotal then
  begin
    { A 0..8 byte remainder is left inside the allocation. }
    if OldTotal - Needed >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
    begin
      FreePart := PHeapBlock(PtrUInt(Block) + Needed);
      FreePart^.Size := OldTotal - Needed;
      FreePart^.Next := nil;
      Block^.Size := Needed;
      _HeapSubUsed(OldPayload - NewPayload);
      _InsertFree(FreePart);
    end;
    Result := P;
    Exit;
  end;

  { Expand into an immediately following free block if possible. }
  if PtrUInt(Block) + OldTotal < _heapEnd then
  begin
    Neighbor := PHeapBlock(PtrUInt(Block) + OldTotal);
    if _FindFree(Neighbor, Prev) then
    begin
      Capacity := OldTotal + Neighbor^.Size;
      if Capacity >= Needed then
      begin
        _UnlinkFree(Neighbor, Prev);
        Block^.Size := Capacity;
        Block^.Next := nil;
        if Capacity - Needed >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
        begin
          FreePart := PHeapBlock(PtrUInt(Block) + Needed);
          FreePart^.Size := Capacity - Needed;
          FreePart^.Next := nil;
          Block^.Size := Needed;
          _InsertFree(FreePart);
        end;
        _HeapAddUsed(_BlockSize(Block) - OldPayload);
        Result := P;
        Exit;
      end;
    end;
  end;

  { Expand an allocation at the very top without copying. }
  if PtrUInt(Block) + OldTotal = _heapEnd then
  begin
    Extra := Needed - OldTotal;
    if Extra <= _heapLimit - _heapEnd then
    begin
      Block^.Size := Needed;
      _heapEnd := _heapEnd + Extra;
      _heapMetadata.Top := Pointer(_heapEnd);
      if _heapEnd > PtrUInt(_heapMetadata.MaximumHeapEnd) then
        _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);
      _HeapAddUsed(Extra);
      Result := P;
      Exit;
    end;
  end;

  { Fallback: allocate elsewhere, copy, then release the old block. }
  NewPtr := _Malloc(Size);
  if NewPtr = nil then Exit;
  if OldPayload < Size then
    _CopyMem(P, NewPtr, OldPayload)
  else
    _CopyMem(P, NewPtr, Size);
  _Free(P);
  Result := NewPtr;
end;

{ --------------------------------------------------------------------------
  Optional libc-style internal helper.
  -------------------------------------------------------------------------- }
function _Calloc(Num, Size: SizeUInt): Pointer;
var
  Total: SizeUInt;
begin
  Result := nil;
  if (Num <> 0) and (Size > High(SizeUInt) div Num) then Exit;
  Total := Num * Size;
  Result := _Malloc(Total);
  if Result <> nil then FillChar(Result^, Total, 0);
end;

{ --------------------------------------------------------------------------
  Functions expected by FPC's MemoryManager record.
  -------------------------------------------------------------------------- }
function SysGetMem(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
end;

function SysFreeMem(P: Pointer): PtrUInt;
var
  Size: PtrUInt;
begin
  if P = nil then
  begin
    Result := 0;
    Exit;
  end;
  if (PtrUInt(P) < _heapStart + PS1_HEAP_HEADER) or
     (not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER)) then
  begin
    Result := 0;
    Exit;
  end;
  Size := _BlockSize(_DataBlock(P));
  _Free(P);
  Result := Size;
end;

function SysFreeMemSize(P: Pointer; Size: PtrUInt): PtrUInt;
begin
  Result := SysFreeMem(P);
end;

function SysMemSize(P: Pointer): PtrUInt;
begin
  Result := 0;
  if P = nil then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  if not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER) then Exit;
  Result := _BlockSize(_DataBlock(P));
end;

function SysAllocMem(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
  if Result <> nil then FillChar(Result^, SysMemSize(Result), 0);
end;

function SysReAllocMem(var P: Pointer; Size: PtrUInt): Pointer;
var
  NewPtr: Pointer;
begin
  NewPtr := _Realloc(P, Size);
  { On failure, keep the old pointer.  Size=0 releases it. }
  if (NewPtr <> nil) or (Size = 0) then P := NewPtr;
  Result := NewPtr;
end;

function SysGetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function SysGetHeapStatus: THeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ Kept for compatibility with the PS1 RTL OS allocator interface. }
function SysOSAlloc(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
end;

procedure SysOSFree(P: Pointer; Size: PtrUInt);
begin
  _Free(P);
end;

{ Generic heap.inc normally supplies these, disabled by HAS_MEMORYMANAGER. }
procedure InitHeap; public name '_FPC_InitHeap';
begin
end;

procedure RelocateHeap;
begin
end;

procedure FinalizeHeap;
begin
end;