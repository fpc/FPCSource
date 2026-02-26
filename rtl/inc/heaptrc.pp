{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Heap tracer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$checkpointer off}
{$modeswitch duplicatelocals} {$modeswitch defaultparameters} {$modeswitch class} {$modeswitch result} {$modeswitch out}
{$modeswitch advancedrecords} {$scopedenums on} {$typedaddress on}
unit heaptrc;

{$if defined(win32) or defined(wince)}
  {$define windows}
{$endif}

{$if not defined(FPUNONE) and not defined(FPUSOFT)}
  {$define can_use_ln_in_constants} // :(
{$endif}

{$if not defined(CPUINT8) and not defined(CPUINT16)}
  {$define can_use_int32_case} // :(
{$endif}

// Buggy and does not help: https://gitlab.com/freepascal.org/fpc/source/-/issues/39611.
// As a workaround, you can wrap this with {$define disable_warnings := ...all of these $warns...} and sprinkle disable_warnings here and there.
{$warn 4055 off : Conversion between ordinals and pointers is not portable}
{$warn 5024 off : Parameter not used}
{$warn 5036 off : Local variable does not seem to be initialized}

{-$define debug_heaptrc}

interface

  procedure DumpHeap;
  procedure DumpHeap(SkipIfNoLeaks: boolean);
  procedure CheckHeap(step: SizeUint = High(SizeUint));

  procedure HexDump(var f: text; p: pointer; n: SizeUint); // Utility.

type
  TFillExtraInfoProc = procedure(p: pointer);
  TDisplayExtraInfoProc = procedure(var ptext: text; p: pointer);

  { Allows to add info per memory block, see ppheap.pas of the compiler for example source.
    Presently, you can have only up to 3 different combinations, not counting the default (0, nil, nil)...
    increase HeapTracer.MaxExtraInfos in the implementation if you need more :). }
  procedure SetHeapExtraInfo(size: PtrUint; fillProc: TFillExtraInfoProc; displayProc: TDisplayExtraInfoProc);

  { Add a footprint at the end of memory blocks to check for memory overwrites at the end of a block.
    Default is 4. Set to 0 to disable. }
  function GetTailSize: SizeUint;
  procedure SetTailSize(size: SizeUint);
  property tail_size: SizeUint read GetTailSize write SetTailSize;

  { To maintain correct alignment, this is rounded up to the multiple of 2 * sizeof(pointer) which might be unpleasant,
    so the default is 0. }
  function GetHeadSize: SizeUint;
  procedure SetHeadSize(size: SizeUint);
  property head_size: SizeUint read GetHeadSize write SetHeadSize;

  function GetKeepReleased: boolean;
  procedure SetKeepReleased(keep: boolean);
  property KeepReleased: boolean read GetKeepReleased write SetKeepReleased;

  function GetTraceDepth: SizeUint;
  procedure SetTraceDepth(depth: SizeUint);
  property TraceDepth: SizeUint read GetTraceDepth write SetTraceDepth;

  { Redirection of the output to a file }
  procedure SetHeapTraceOutput(const name: string);
  procedure SetHeapTraceOutput(var fout: text);

  procedure CheckPointer(p: pointer);

{$ifdef debug_heaptrc}
type
  DumpItem = (Ht, Traces);
  DumpItems = set of DumpItem;
  procedure Dump(var f: text; what: DumpItems = [DumpItem.Ht, DumpItem.Traces]);
{$endif}

const
  UseHeapTrace: boolean = true;
  { calls halt() on error by default }
  HaltOnError: boolean = true;
  { Halt on exit if any memory was not freed }
  HaltOnNotReleased : boolean = false;
  MaxLeaksToReport: SizeUint = 1024; { If you *really* need miles of reports, increase. }

  { keep released blocks for a while do detect invalid writes into freed memory;
    value is maximum total size in bytes. }
  KeepReleasedBytes: SizeUint = 0;
  AutoCheckHeapStep: SizeUint = 3;

  PrintLeakedBlock: boolean = false;
  PrintFaultyBlock: boolean = false;
  MaxPrintedBlockLength: integer = 160; // (MaxPrintedBlockLength div 4 * 3) (120) divisible by HexDumpPart.RowLength (20) makes dumps prettier.

  GlobalSkipIfNoLeaks: boolean = false;
  IncludeInternalAllocationsInStatistics: boolean = false; // Some tests rely on statistics.

implementation

const
  DefaultTraceDepth = 16;

  // Implementing SystemRealloc might reduce the effect of heaptrc on the allocation pattern, especially when combined with tail_size = 0.
  // If not implemented, internal allocations will use the wrapped memory manager.
  //
  // Implementing Readable lets heaptrc heuristically detect class instances and display their names in leak reports.
{$if defined(win32) or defined(win64)}
const
  HEAP_GENERATE_EXCEPTIONS = $4;
  MEM_COMMIT = $1000;
  PAGE_READONLY = 2;
  PAGE_READWRITE = 4;
  PAGE_WRITECOPY = 8;

type
  MEMORY_BASIC_INFORMATION = record
    BaseAddress, AllocationBase: pointer;
    AllocationProtect: uint32;
    RegionSize: SizeUint;
    State, Protect, &Type: uint32;
  end;

  function GetProcessHeap: PtrUint; winapi; external 'kernel32' name 'GetProcessHeap';
  function HeapAlloc(hHeap: PtrUint; dwFlags: uint32; dwBytes: PtrUint): pointer; winapi; external 'kernel32' name 'HeapAlloc';
  function HeapReAlloc(hHeap: PtrUint; dwFlags: uint32; lpMem: pointer; dwBytes: PtrUint): pointer; winapi; external 'kernel32' name 'HeapReAlloc';
  function HeapFree(hHeap: PtrUint; dwFlags: uint32; lpMem: pointer): LongBool; winapi; external 'kernel32' name 'HeapFree';
  function VirtualQuery(lpAddress: Pointer; out lpBuffer: MEMORY_BASIC_INFORMATION; dwLength: SizeUint): SizeUint; winapi; external 'kernel32' name 'VirtualQuery';

  function SystemRealloc(p: pointer; oldSize, newSize: SizeUint): pointer;
  var
    ph: PtrUint;
  begin
    ph := GetProcessHeap;
    result := nil;
    if newSize = 0 then
      HeapFree(ph, 0, p)
    else if p = nil then
      result := HeapAlloc(ph, HEAP_GENERATE_EXCEPTIONS, newSize)
    else
      result := HeapReAlloc(ph, HEAP_GENERATE_EXCEPTIONS, p, newSize);
  end;

  function Readable(p: pointer; n: SizeUint): boolean;
  var
    mi: MEMORY_BASIC_INFORMATION;
  begin
    result := (VirtualQuery(p, mi, sizeof(mi)) <> 0) and (SizeUint(p - mi.BaseAddress) + n <= mi.RegionSize) and
      (mi.State = MEM_COMMIT) and (mi.Protect and (PAGE_READONLY or PAGE_READWRITE or PAGE_WRITECOPY) <> 0);
  end;
{$endif SystemRealloc}

  function InternalRealloc(p: pointer; oldSize, newSize: SizeUint): pointer; forward;

{$if declared(Readable)}
  function LooksLikeClassInstance(p: pointer; n: SizeUint; out name: shortstring): boolean;
  var
    v: PVmt;
    i: SizeInt;
  begin
    result := false;
    if n < sizeof(pointer) then exit;
    v := PPointer(p)^;
    if not Readable(v, sizeof(TVmt)) or (v^.vInstanceSize <> SizeInt(n)) or (v^.vInstanceSize2 <> -SizeInt(n)) or
      not Readable(v^.vClassName, 1) or not Readable(v^.vClassName, length(v^.vClassName^)) then exit;
    for i := 1 to length(v^.vClassName^) do
      if not (v^.vClassName^[i] in ['a' .. 'z', 'A' .. 'Z', '_']) and ((i = 1) or not (v^.vClassName^[i] in ['0' .. '9'])) then exit;
    result := true;
    name := v^.vClassName^;
  end;
{$endif Readable}

type
  MemoryRegion = record
    function Push(n: SizeUint): pointer;
    procedure Clear;
    function CalcSumSize: SizeUint;
  private type
    pNode = ^Node;
    Node = record
      n, alloc: uint32;
      prev: pNode;
      data: array[0 .. 0] of byte;
    end;
  const
    SoftAllocLimit = 1024 * 1024;
  var
    top: pNode;
    function PushNewNode(n: SizeUint): pointer;
    class operator Initialize(var self: MemoryRegion);
    class operator Finalize(var self: MemoryRegion);
  end;

  function MemoryRegion.Push(n: SizeUint): pointer;
  var
    locTop: pNode;
    start: SizeUint;
  begin
    locTop := self.top;
    if Assigned(locTop) then
    begin
      start := locTop^.n;
      if n <= SizeUint(locTop^.alloc - start) then
      begin
        locTop^.n := start + n;
        exit(pByte(locTop^.data) + start);
      end;
    end;
    result := PushNewNode(n);
  end;

  procedure MemoryRegion.Clear;
  var
    cur, prev: pNode;
  begin
    cur := top;
    top := nil;
    while Assigned(cur) do
    begin
      prev := cur^.prev;
      InternalRealloc(cur, PtrUint(@pNode(nil)^.data) + cur^.alloc, 0);
      cur := prev;
    end;
  end;

  function MemoryRegion.CalcSumSize: SizeUint;
  var
    n: pNode;
  begin
    result := 0;
    n := top;
    while Assigned(n) do
    begin
      inc(result, n^.n);
      n := n^.prev;
    end;
  end;

  function MemoryRegion.PushNewNode(n: SizeUint): pointer;
  var
    alloc: SizeUint;
    newNode: pNode;
  begin
    alloc := SoftAllocLimit;
    if not Assigned(top) or (top^.alloc < SoftAllocLimit) then
    begin
      alloc := {$ifdef debug_heaptrc} 16 {$else} 256 {$endif} + n + CalcSumSize div 4; { const + n + 25%. }
      if alloc > SoftAllocLimit then alloc := SoftAllocLimit;
    end;
    if alloc < n then alloc := n;
    newNode := InternalRealloc(nil, 0, PtrUint(@pNode(nil)^.data) + alloc);
    newNode^.n := n;
    newNode^.alloc := alloc;
    newNode^.prev := top;
    top := newNode;
    result := pByte(newNode^.data);
  end;

  class operator MemoryRegion.Initialize(var self: MemoryRegion);
  begin
    self.top := nil;
  end;

  class operator MemoryRegion.Finalize(var self: MemoryRegion);
  begin
    self.Clear;
  end;

type
  HashList = record
  type
    pNode = ^Node; ppNode = ^pNode;
    Node = record
    private
      next: pNode;
    end;

    HashItemProc = function(node: pNode; var h: HashList): SizeUint;
    DumpNodeProc = procedure(n: pNode; var f: text);

    procedure Init(hashItem: HashItemProc);
    procedure Add(node: pNode; hash: SizeUint);
    procedure Clear;
    procedure RemoveNode(n: pNode; link: ppNode); inline;
    function GetLink(n: pNode; hash: SizeUint): ppNode; inline;
  {$ifdef debug_heaptrc}
    procedure Dump(var f: text; dumpNode: DumpNodeProc);
  {$endif}

  private const
    FakeEmptyTable: array[0 .. 0] of pNode = (nil);
  var
    h: ppNode;
    nItems, nhmask, minItems, maxItems: SizeUint;
    hashItem: HashItemProc; // in Clear, fields before hashItem are zeroed.

    procedure Rehash(forNItems: SizeUint);
    function CollectItems: pNode;
    procedure ReAddItems(items: pNode);
    class operator Initialize(var self: HashList);
    class operator Finalize(var self: HashList);
  end;

  generic HashListTemplated<Control> = record
    class function Find(var h: HashList; key: pointer; hash: SizeUint; out link: HashList.ppNode): HashList.pNode; static; inline;
    class function FindAndBump(var h: HashList; key: pointer; hash: SizeUint): HashList.pNode; static;
  end;

  procedure HashList.Init(hashItem: HashItemProc);
  begin
    if self.nhmask <> 0 then Clear;
    h := ppNode(FakeEmptyTable);
    self.hashItem := hashItem;
  end;

  procedure HashList.Add(node: pNode; hash: SizeUint);
  var
    hp: ppNode;
  begin
    if nItems >= maxItems then Rehash(nItems + 1);
    hp := @h[hash and nhmask];
    node^.next := hp^;
    hp^ := node;
    inc(nItems);
  end;

  procedure HashList.Clear;
  begin
    if nhmask = 0 then exit;
    if Assigned(h) then InternalRealloc(h, (1 + nhmask) * sizeof(pNode), 0);
    FillChar(self, PtrUint(@HashList(nil^).hashItem), 0);
    h := ppNode(FakeEmptyTable);
  end;

  procedure HashList.RemoveNode(n: pNode; link: ppNode);
  begin
    link^ := n^.next;
    dec(nItems);
    if nItems < minItems then Rehash(nItems);
  end;

  function HashList.GetLink(n: pNode; hash: SizeUint): ppNode;
  begin
    result := @h[hash and nhmask];
    while result^ <> n do result := @result^^.next; // Crash if not found.
  end;

{$ifdef debug_heaptrc}
  procedure HashList.Dump(var f: text; dumpNode: DumpNodeProc);
  var
    hn: pNode;
    ih: SizeInt;
  begin
    writeln(f, 'nItems = ', nItems, ', nhmask = ', nhmask, ', maxItems = ', maxItems, ', minItems = ', minItems);
    for ih := 0 to SizeInt(nhmask) do
    begin
      hn := h[ih];
      if not Assigned(hn) then continue;
      write(f, 'h[', ih, ']: ');
      while Assigned(hn) do
      begin
        dumpNode(hn, f);
        if Assigned(hn^.next) then write(f, ', ') else writeln(f);
        hn := hn^.next;
      end;
    end;
  end;
{$endif}

  procedure HashList.Rehash(forNItems: SizeUint);
  var
    newnh: SizeUint;
    items: pNode;
  begin
    if forNItems = 0 then
    begin
      Clear;
      exit;
    end;
    newnh := SizeUint(1) shl
      {$if sizeof(SizeUint) <= sizeof(uint32)} BsrDWord
      {$elseif sizeof(SizeUint) <= sizeof(uint64)} BsrQWord
      {$else} {$error >64 bits} {$endif}
        (({$ifdef debug_heaptrc} 2 {$else} 10 {$endif} + forNItems) or 1 {“or 1” just triggers node_not_zero optimization});
    // newnh gives 100%~200% load factor for forNItems.
    if newnh < forNItems + forNItems div 2 then newnh := newnh * 2; // newnh gives 75%~150% load factor for forNItems.
    maxItems := newnh * 2; // max 200% load factor.
    minItems := newnh div 4;
    if minItems < {$ifdef debug_heaptrc} 1 {$else} 8 {$endif} then minItems := 0;
    if nhmask + 1 = newnh then exit;
    items := CollectItems;
    if nhmask <> 0 then h := InternalRealloc(h, (1 + nhmask) * sizeof(pNode), 0);
    h := InternalRealloc(nil, 0, newnh * sizeof(pNode));
    FillChar(h^, newnh * sizeof(pNode), 0);
    nhmask := newnh - 1;
    ReAddItems(items);
  end;

  function HashList.CollectItems: pNode;
  var
    hp: ppNode;
    cur, nx: pNode;
    hpLeft: SizeUint;
  begin
    result := nil;
    hp := h; hpLeft := nhmask + 1;
    repeat
      cur := hp^;
      while Assigned(cur) do
      begin
        nx := cur^.next;
        cur^.next := result;
        result := cur;
        cur := nx;
      end;
      inc(hp); dec(hpLeft);
    until hpLeft = 0;
  end;

  procedure HashList.ReAddItems(items: pNode);
  var
    desthp: ppNode;
    next: pNode;
  begin
    while Assigned(items) do
    begin
      next := items^.next;
      desthp := @h[hashItem(items, self) and nhmask];
      items^.next := desthp^;
      desthp^ := items;
      items := next;
    end;
  end;

  class operator HashList.Initialize(var self: HashList);
  begin
    FillChar(self, sizeof(self), 0);
  end;

  class operator HashList.Finalize(var self: HashList);
  begin
    self.Clear;
  end;

  class function HashListTemplated.Find(var h: HashList; key: pointer; hash: SizeUint; out link: HashList.ppNode): HashList.pNode;
  var
    lnk: HashList.ppNode;
  begin
    lnk := @h.h[hash and h.nhmask];
    while Assigned(lnk^) and not Control.KeyEqualsToItem(key, lnk^, h) do
      lnk := @lnk^^.next;
    result := lnk^;
    link := lnk;
  end;

  // Moves the found node to the beginning. Useful when there is a good chance for repeated searches.
  class function HashListTemplated.FindAndBump(var h: HashList; key: pointer; hash: SizeUint): HashList.pNode;
  var
    hp: HashList.ppNode;
    prev: HashList.pNode;
  begin
    hp := h.h + hash and h.nhmask;
    result := hp^;
    prev := nil;
    while Assigned(result) and not Control.KeyEqualsToItem(key, result, h) do
    begin
      prev := result;
      result := result^.next;
    end;
    if Assigned(result) and Assigned(prev) then
    begin
      prev^.next := result^.next;
      result^.next := hp^;
      hp^ := result;
    end;
  end;

type
  // 1 byte: <0 ~ 159>, range 0 ~ 159.
  // 2 bytes <160 ~ 223> xxxxxxxx, value = 160 + (b0 - 160) shl 8 or b1, range 160 ~ 16 543.
  // 3 bytes: <224 ~ 239> xxxxxxxx xxxxxxxx, value = 16 544 + (b0 - 224) shl 16 or b1 shl 8 or b2, range 16 544 ~ 1 065 119.
  // 4 bytes: <240 ~ 250> xxxxxxxx xxxxxxxx xxxxxxxx, value = 1 065 120 + (b0 - 240) shl 24 or b1 shl 16 or b2 shl 8 or b3, range 1 065 120 ~ 185 614 495.
  // 5 bytes: 251 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx, xx..xx — native-endian value, range 185 614 496 ~ 2^32 − 1.
  // 6 bytes: 252 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx, xx..xx — native-endian value, range 2^32 ~ 2^40 − 1.
  // 7 bytes: 253 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx, xx..xx — native-endian value, range 2^40 ~ 2^48 − 1.
  // 8 bytes: 254 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx, xx..xx — native-endian value, range 2^48 ~ 2^56 − 1.
  // 9 bytes: 255 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx, xx..xx — native-endian value, range 2^56 ~ 2^64 − 1.

  VarInt = record
  const
    MaxBytes = 1 + sizeof(PtrUint);
    FirstByte2 = 160;
    FirstByte3 = 224;
    FirstByte4 = 240;
    FirstByte5 = 251;
    Ofs2 = FirstByte2;
    Ofs3 = Ofs2 + (FirstByte3 - FirstByte2) shl 8;
    Ofs4 = Ofs3 + (FirstByte4 - FirstByte3) shl 16;
    Min5 = Ofs4 + (FirstByte5 - FirstByte4) shl 24;

    class function Read(p: pointer; out v: PtrUint): PtrUint; static; inline;
    class function Write(p: pointer; v: PtrUint): PtrUint; static; inline;
    class function EnZig(sv: PtrInt): PtrUint; static; inline; // Zig-zag encoding for signed values.
    class function DeZig(uv: PtrUint): PtrInt; static; inline;
  end;

  class function VarInt.Read(p: pointer; out v: PtrUint): PtrUint;
  var
    pb: pByte absolute p;
  begin
    result := pb[0];
    if uint32(result) <= FirstByte2 - 1 then
    begin
      v := result;
      result := 1;
    end
    else if uint32(result) <= FirstByte3 - 1 then
    begin
      v := Ofs2 + ((result - FirstByte2) shl 8 or pb[1]);
      result := 2;
    end
    else if uint32(result) <= FirstByte4 - 1 then
    begin
      v := Ofs3 + ((result - FirstByte3) shl 16 or pb[1] shl 8 or pb[2]);
      result := 3;
    end
    else if uint32(result) <= FirstByte5 - 1 then
    begin
      v := Ofs4 + ((result - FirstByte4) shl 24 or pb[1] shl 16 or pb[2] shl 8 or pb[3]);
      result := 4;
    end else
    begin
      dec(result, FirstByte5 - 5);
    {$if sizeof(v) <= sizeof(uint32)}
      v := unaligned(pUint32(pb {$ifdef endian_big} + result - 4 {$else} + 1 {$endif})^);
    {$elseif sizeof(v) <= sizeof(uint64)}
      v := unaligned(pUint32(pb + 1)^);
      if result > 5 then
      {$ifdef endian_big}
        v := v shl (result * 8 - 40) or unaligned(pUint32(pb + result - 4)^);
      {$else}
        v := v or PtrUint(unaligned(pUint32(pb + result - 4)^)) shl (result * 8 - 40);
      {$endif}
    {$else} {$error >64 bits} {$endif}
    end;
  end;

  class function VarInt.Write(p: pointer; v: PtrUint): PtrUint;
  var
    pb: pByte absolute p;
  begin
{$ifdef can_use_int32_case}
    case v of
      0 .. Ofs2 - 1:
{$else}
    if v < Ofs2 then
{$endif}
        begin
          pb[0] := v;
          result := 1;
        end
{$ifdef can_use_int32_case};
      Ofs2 .. Ofs3 - 1:
{$else}
    else if v < Ofs3 then
{$endif}
        begin
          dec(v, Ofs2);
          pb[0] := FirstByte2 + v shr 8;
          pb[1] := byte(v);
          result := 2;
        end
{$ifdef can_use_int32_case};
      Ofs3 .. Ofs4 - 1:
{$else}
    else if v < Ofs4 then
{$endif}
        begin
          dec(v, Ofs3);
          pb[0] := FirstByte3 + v shr 16;
          pb[1] := byte(v shr 8);
          pb[2] := byte(v);
          result := 3;
        end
{$ifdef can_use_int32_case};
      Ofs4 .. Min5 - 1:
{$else}
    else if v < Min5 then
{$endif}
        begin
          dec(v, Ofs4);
          pb[0] := FirstByte4 + v shr 24;
          pb[1] := byte(v shr 16);
          pb[2] := byte(v shr 8);
          pb[3] := byte(v);
          result := 4;
        end {$ifdef can_use_int32_case} ; {$endif} { actually this ; is optional even with ‘case’. }
      else
        begin
        {$if sizeof(v) <= sizeof(uint32)}
          pb[0] := 251;
          unaligned(pUint32(pb + 1)^) := v;
          result := 5;
        {$elseif sizeof(v) <= sizeof(uint64)}
          result := 2 + {$if sizeof(v) <= sizeof(uint32)} BsrDWord {$else} BsrQWord {$endif}
            (v or 1 {“or 1” not required logically, triggers node_not_zero optimization}) div 8;
          pb[0] := 246 + result;
          unaligned(pUint32(pb + 1)^) := uint32(v {$ifdef endian_big} shr (8 * result - 40) {$endif});
          unaligned(pUint32(pb + result - 4)^) := uint32(v {$ifdef endian_little} shr (8 * result - 40) {$endif});
        {$else} {$error >64 bits} {$endif}
        end;
{$ifdef can_use_int32_case}
    end; { closes case .. of }
{$endif}
  end;

  class function VarInt.EnZig(sv: PtrInt): PtrUint;
  begin
    result := PtrUint(sv shl 1 xor {$if sizeof(sv) <= sizeof(uint32)} SarLongint(sv, 31) {$elseif sizeof(sv) <= sizeof(uint64)} SarInt64(sv, 63) {$else} {$error >64 bits} {$endif});
  end;

  class function VarInt.DeZig(uv: PtrUint): PtrInt;
  begin
    result := PtrInt(uv shr 1) xor -PtrInt(uv and 1);
  end;

type
  // Stack trace format:
  // 1 byte (SizeType): size in bytes minus 5 (HeaderSize).
  // 4 bytes (HashType): hash (native unaligned uint32).
  // N VarInts until the size is exhausted: zigzag-encoded deltas from the previous pointer, as if the pointer before first was nil.
  StackTrace = record
  const
    MaxItems = 64;
    SizeOffset = 0;
    HashOffset = 1;
    HeaderSize = 5;
    MaxBytes = HeaderSize + {$if MaxItems * (1 + sizeof(pointer)) <= High(byte)} MaxItems * (1 + sizeof(pointer)) {$else} High(byte) {$endif};
  type
    SizeType = byte;   pSizeType = ^SizeType;
    HashType = uint32; pHashType = ^HashType;
    class function Pack(trace: pCodePointer; nTrace: SizeUint; packedTrace: pointer): SizeUint; static;
    class function Unpack(packedTrace: pointer; trace: pCodePointer): SizeUint; static;
  end;

  class function StackTrace.Pack(trace: pCodePointer; nTrace: SizeUint; packedTrace: pointer): SizeUint;
  var
    start: pointer;
    n: SizeUint;
    prev, delta: PtrInt;
    hash: PtrUint;
  begin
    result := HeaderSize;
    prev := 0;
    start := packedTrace;
    inc(packedTrace, HeaderSize);
    hash := 0; // Hashing algorithm: start with 0, for all pointers: “add pointer, xorshift by 16, multiply by $21f0aaad”, xorshift by 15, cast to HashType.
    while (nTrace > 0) and (result <= MaxBytes - VarInt.MaxBytes) do
    begin
    {$push} {$q-,r-}
      hash := hash + PtrUint(trace^);
      hash := (hash xor hash shr 16) * $21f0aaad;
      delta := PtrInt(PtrUint(trace^)) - prev;
      inc(prev, delta);
    {$pop}
      n := VarInt.Write(packedTrace, VarInt.EnZig(delta));
      inc(packedTrace, n); inc(result, n);
      inc(trace); dec(nTrace);
    end;
    unaligned(pSizeType(start + SizeOffset)^) := result - HeaderSize;
    unaligned(pHashType(start + HashOffset)^) := HashType(hash xor (hash shr 15));
  end;

  class function StackTrace.Unpack(packedTrace: pointer; trace: pCodePointer): SizeUint;
  var
    remaining, n: SizeUint;
    ptrv, delta: PtrInt;
  begin
    result := 0;
    ptrv := 0;
    remaining := unaligned(pSizeType(packedTrace + SizeOffset)^);
    inc(packedTrace, HeaderSize);
    while remaining > 0 do
    begin
      n := VarInt.Read(packedTrace, PtrUint(delta));
      inc(packedTrace, n); dec(remaining, n);
    {$push} {$q-,r-} inc(ptrv, VarInt.DeZig(PtrUint(delta))); {$pop}
      trace[result] := CodePointer(PtrUint(ptrv));
      inc(result);
    end;
  end;

type
  StackTracesRegistry = record
  type
    pNode = ^Node;
    Node = record
    case uint32 of
      0: (hn: HashList.Node; dataOfs, refcount: uint32);
      1: (nextFree: pNode);
    end;

    HashListControl = record
      class function KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean; static; inline;
      class function HashItem(hn: HashList.pNode; var h: HashList): SizeUint; static;
    end;

    // Recover node pointer from trace pointer.
    HashListTracePointerControl = record
      class function KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean; static; inline;
    end;

    StoreDataSize = uint32;

  var
    data: pointer;
    dataSize, dataAlloc, garbage: StoreDataSize;
    freeNodes: pNode;
    h: HashList; // in initialization, fields before h are zeroed.
    function Add(packedTrace: pointer; var nodesRgn: MemoryRegion): pNode;
    procedure Release(var stn: pNode);
    procedure CollectGarbage;
  {$ifdef debug_heaptrc}
    class function CountLinkedListItems(n: pointer; nextOfs: PtrUint): SizeUint; static;
    procedure DumpTrace(var f: text; var n: Node; packedTrace: pointer; trace: pCodePointer; nTrace: SizeUint);
    class procedure DumpHNode(hn: HashList.pNode; var f: text); static;
    procedure Dump(var f: text);
  var
    dumping: boolean;
  {$endif}
    class operator Initialize(var self: StackTracesRegistry);
    class operator Finalize(var self: StackTracesRegistry);
  end;

  class function StackTracesRegistry.HashListControl.KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean;
  var
    nodeTrace: pointer;
  begin
    nodeTrace := StackTracesRegistry((pointer(@h) - PtrUint(@StackTracesRegistry(nil^).h))^).data + pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.dataOfs;
    result :=
      (unaligned(StackTrace.pHashType(key + StackTrace.HashOffset)^) = unaligned(StackTrace.pHashType(nodeTrace + StackTrace.HashOffset)^)) and
      (unaligned(StackTrace.pSizeType(key + StackTrace.SizeOffset)^) = unaligned(StackTrace.pSizeType(nodeTrace + StackTrace.SizeOffset)^)) and
      (CompareByte((key + StackTrace.HeaderSize)^, (nodeTrace + StackTrace.HeaderSize)^, unaligned(StackTrace.pSizeType(key + StackTrace.SizeOffset)^)) = 0);
  end;

  class function StackTracesRegistry.HashListControl.HashItem(hn: HashList.pNode; var h: HashList): SizeUint;
  begin
    result := unaligned(StackTrace.pHashType(StackTracesRegistry((pointer(@h) - PtrUint(@StackTracesRegistry(nil^).h))^).data + pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.dataOfs + StackTrace.HashOffset)^);
  end;

  class function StackTracesRegistry.HashListTracePointerControl.KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean;
  begin
    result := key = StackTracesRegistry((pointer(@h) - PtrUint(@StackTracesRegistry(nil^).h))^).data + pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.dataOfs;
  end;

  function StackTracesRegistry.Add(packedTrace: pointer; var nodesRgn: MemoryRegion): pNode;
  var
    existHn: HashList.pNode;
    packedSize, newAlloc: SizeUint;
  begin
  {$ifdef debug_heaptrc} if garbage > 32 + dataSize div 4 then CollectGarbage; {$endif}
    existHn := specialize HashListTemplated<HashListControl>.FindAndBump(h, packedTrace, unaligned(StackTrace.pHashType(packedTrace + StackTrace.HashOffset)^));
    if Assigned(existHn) then
    begin
      result := pointer(existHn) - PtrUint(@Node(nil^).hn);
      if result^.refcount = 0 then dec(garbage, StoreDataSize(unaligned(StackTrace.pSizeType(packedTrace + StackTrace.SizeOffset)^) + StackTrace.HeaderSize));
      inc(result^.refcount); // Overflowing the uint32 refcount may be slightly more realistic than 4 Gb of traces but for now we ignore it completely...
      exit;
    end;

    packedSize := unaligned(StackTrace.pSizeType(packedTrace + StackTrace.SizeOffset)^) + StackTrace.HeaderSize;
    if StoreDataSize(dataAlloc - dataSize) < packedSize then
    begin
      if garbage > 4096 + dataSize div 4 then CollectGarbage;
      if StoreDataSize(dataAlloc - dataSize) < packedSize then
      begin
        if garbage > 1024 + dataSize div 16 then CollectGarbage; // Reallocating anyway so collect more aggressively :).
        newAlloc := {$ifdef debug_heaptrc} 16 {$else} StackTrace.MaxBytes {$endif} + packedSize + dataSize + dataSize div 4;
        if newAlloc < dataSize then exit(nil); // Overflow; 4 Gb of traces must be impossible in practice (which is also the reason for 32-bit offsets) but who knows...
        data := InternalRealloc(data, dataAlloc, newAlloc);
        dataAlloc := newAlloc;
      end;
    end;
    Move(packedTrace^, (data + dataSize)^, packedSize);

    result := freeNodes;
    if Assigned(result) then
      freeNodes := result^.nextFree
    else
      result := nodesRgn.Push(sizeof(Node));
    result^.dataOfs := dataSize;
    result^.refcount := 1;
    h.Add(@result^.hn, unaligned(StackTrace.pHashType(packedTrace + StackTrace.HashOffset)^));
    inc(dataSize, StoreDataSize(packedSize));
  end;

  procedure StackTracesRegistry.Release(var stn: pNode);
  var
    n: pNode;
  begin
    n := stn;
    if not Assigned(n) then exit;
    stn := nil;
    dec(n^.refcount);
    if n^.refcount = 0 then inc(garbage, StoreDataSize(unaligned(StackTrace.pSizeType(data + n^.dataOfs + StackTrace.SizeOffset)^) + StackTrace.HeaderSize));
  end;

  procedure StackTracesRegistry.CollectGarbage;
  var
    dp, destDp, n, moveN: SizeUint;
    stn: pNode;
    stnLink: HashList.ppNode;
    moveSrc, moveDst: pointer;
  begin
  {$ifdef debug_heaptrc} if dumping then exit; {$endif} // Dumping iterates data...
    // Iterate all stack traces stored in data, search for corresponding nodes, check their refcounts.
    // Remove all nodes and traces with refcount = 0, pack other traces tightly.
    dp := 0; destDp := 0;
    moveSrc := nil; moveDst := nil;
    while StoreDataSize(dp) < dataSize do
    begin
      stn := pointer(specialize HashListTemplated<HashListTracePointerControl>.Find(h, data + dp, unaligned(StackTrace.pHashType(data + dp + StackTrace.HashOffset)^), stnLink)) - PtrUint(@Node(nil^).hn);
      n := unaligned(StackTrace.pSizeType(data + dp + StackTrace.SizeOffset)^) + StackTrace.HeaderSize;
      if stn^.refcount > 0 then
      begin
        stn^.dataOfs := destDp;
        if moveSrc = moveDst then
        begin
          moveSrc := data + dp;
          moveDst := data + destDp;
          moveN := 0;
        end;
        inc(moveN, n);
        inc(destDp, n);
      end else
      begin
        h.RemoveNode(@stn^.hn, stnLink);
        stn^.nextFree := freeNodes;
        freeNodes := stn;
        if moveSrc <> moveDst then
        begin
          Move(moveSrc^, moveDst^, moveN);
          moveSrc := nil; moveDst := nil;
        end;
      end;
      inc(dp, n);
    end;
    if moveSrc <> moveDst then Move(moveSrc^, moveDst^, moveN);
    dataSize := destDp;
    garbage := 0;
  end;

{$ifdef debug_heaptrc}
  class function StackTracesRegistry.CountLinkedListItems(n: pointer; nextOfs: PtrUint): SizeUint;
  begin
    result := 0;
    while Assigned(n) do
    begin
      inc(result);
      n := pPointer(n + nextOfs)^;
    end;
  end;

  procedure StackTracesRegistry.DumpTrace(var f: text; var n: Node; packedTrace: pointer; trace: pCodePointer; nTrace: SizeUint);
  var
    iTrace: SizeInt;
  begin
    writeln(f, unaligned(StackTrace.pSizeType(packedTrace + StackTrace.SizeOffset)^) + StackTrace.HeaderSize, ' b, ', nTrace, ' items, dataOfs = ', n.dataOfs, ', refcount = ', n.refcount);
    for iTrace := 0 to SizeInt(nTrace) - 1 do
    {$if false and defined(string_test)}
      // for testing with fictive stack traces that are composed of string pointers :).
      write(f, pAnsiChar(' ') + ord(iTrace = 0), string(trace[iTrace]), pAnsiChar(LineEnding) + length(LineEnding) * ord(iTrace < SizeInt(nTrace) - 1));
    {$else}
      writeln(f, BacktraceStrFunc(trace[iTrace]));
    {$endif}
  end;

  class procedure StackTracesRegistry.DumpHNode(hn: HashList.pNode; var f: text);
  begin
    write(f, pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.dataOfs);
  end;

  procedure StackTracesRegistry.Dump(var f: text);
  var
    stn: pNode;
    dp: SizeUint;
    dummyLink: HashList.ppNode;
    trace: array[0 .. StackTrace.MaxItems - 1] of CodePointer;
    nTrace, savedDataSize: SizeUint;
  begin
    writeln(f, 'traces.dataSize = ', dataSize, ', dataAlloc = ', dataAlloc, ', garbage = ', garbage, ', free nodes: ', CountLinkedListItems(freeNodes, PtrUint(@Node(nil^).nextFree)));
    if h.nItems > 0 then
    begin
      write(f, LineEnding, 'traces.h (dataOfs values): ');
      h.Dump(f, @StackTracesRegistry.DumpHNode);
    end;
    if dataSize > 0 then writeln(f, LineEnding, 'traces.data:');
    dp := 0;
    savedDataSize := dataSize; // While dumping, BacktraceStrFunc can allocate... So CollectGarbage is disabled while dumping and dataSize is cached. Refcounts of lineinfo-related chunks can still be reported incorrectly.
    dumping := true;
    while dp < savedDataSize do
    begin
      stn := pointer(specialize HashListTemplated<HashListTracePointerControl>.Find(h, data + dp, unaligned(StackTrace.pHashType(data + dp + StackTrace.HashOffset)^), dummyLink)) - PtrUint(@Node(nil^).hn);
      nTrace := StackTrace.Unpack(data + stn^.dataOfs, trace);
      DumpTrace(f, stn^, data + stn^.dataOfs, trace, nTrace);
      inc(dp, SizeUint(unaligned(StackTrace.pSizeType(data + stn^.dataOfs + StackTrace.SizeOffset)^) + StackTrace.HeaderSize));
    end;
    dumping := false;
  end;
{$endif}

  class operator StackTracesRegistry.Initialize(var self: StackTracesRegistry);
  begin
    FillChar(self, PtrUint(@StackTracesRegistry(nil^).h), 0);
    self.h.Init(@HashListControl.HashItem);
  end;

  class operator StackTracesRegistry.Finalize(var self: StackTracesRegistry);
  begin
    if Assigned(self.data) then self.data := InternalRealloc(self.data, self.dataAlloc, 0);
  end;

type
  CircularHashListIterator = record
    cur: HashList.pNode;
    hCur, hStart, hEnd: HashList.ppNode;
    hMask: SizeUint;
    function MoveNext: HashList.pNode;
    procedure FixupPreRemoveCur;
    procedure FixupPostRehash(var h: HashList);
  end;

  function CircularHashListIterator.MoveNext: HashList.pNode;
  begin
    result := cur;
    if not Assigned(result) then exit;
    result := result^.next;
    if not Assigned(result) then
      repeat
        inc(hCur);
        if hCur = hEnd then hCur := hStart;
        result := hCur^;
      until Assigned(result);
    cur := result;
  end;

  procedure CircularHashListIterator.FixupPreRemoveCur;
  var
    oldCur: HashList.pNode;
  begin
    oldCur := cur;
    if oldCur = MoveNext then
    begin
      cur := nil;
      hMask := 0;
    end;
  end;

  procedure CircularHashListIterator.FixupPostRehash(var h: HashList);
  begin
    // Just give up and restart. Could search for the new cur position but order is shuffled after rehash anyway so why bother.
    // This also for now replaces the (non-existent) Start.
    // This also handles the case of adding the first item into h (so careful with hMask: it must be 0 if h is empty to force FixupPostRehash after addition).
    hStart := h.h;
    hEnd := h.h + h.nhmask + 1;
    hCur := hStart;
    while not Assigned(hCur^) and (hCur <> hEnd) do
      inc(hCur);
    cur := nil;
    hMask := 0;
    if hCur <> hEnd then
    begin
      cur := hCur^;
      hMask := h.nhmask;
    end;
  end;

type
  HeapTracer = record
    class function HashPointer(p: pointer): SizeUint; static; inline;
    class function HeadTailSizeToIndex(sz: SizeUint): SizeUint; static;

  const
    MaxExtraInfos = 3; // Not counting the default (nil, nil, 0). Optimal value is 2^N - 1.
    Eps = 1e-6;
    // Code assumes HeadTailSizes[0 .. 1] = (0, 1).
    HeadTailSizes: array[0 .. 15] of uint8 = (0, 1, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 32, 40, 48, 56);

    // Code asumes that HasFullUserRequestBit immediately precedes ExtraInfoIndex, i.e., they can be combined into one index.
    HasFullUserRequestShift = 0;
    HasFullUserRequestBit = 1 shl HasFullUserRequestShift;
    ExtraInfoIndexShift = HasFullUserRequestShift + 1;
    ExtraInfoIndexBits = {$ifdef can_use_ln_in_constants} 1 + trunc(ln(MaxExtraInfos) / ln(2) + Eps) {$else} 2 {$endif}; {$if ExtraInfoIndexBits <> 2} {$error fix the hardcoded constant} {$endif}
    ExtraInfoIndexMask = 1 shl ExtraInfoIndexBits - 1;
    FreelistIndexMask = ExtraInfoIndexMask shl 1 or 1; // Mask for merged ExtraInfoIndex:HasFullUserRequestBit.
    TailSizeIndexShift = ExtraInfoIndexShift + ExtraInfoIndexBits;
    TailSizeIndexBits = {$ifdef can_use_ln_in_constants} 1 + trunc(ln(High(HeadTailSizes)) / ln(2) + Eps) {$else} 4 {$endif}; {$if TailSizeIndexBits <> 4} {$error fix the hardcoded constant} {$endif}
    TailSizeIndexMask = 1 shl TailSizeIndexBits - 1;
    HeadSizeIndexShift = TailSizeIndexShift + TailSizeIndexBits;
    HeadSizeIndexBits = {$ifdef can_use_ln_in_constants} 1 + trunc(ln(High(HeadTailSizes)) / ln(2) + Eps) {$else} 4 {$endif}; {$if HeadSizeIndexBits <> 4} {$error fix the hardcoded constant} {$endif}
    HeadSizeIndexMask = 1 shl HeadSizeIndexBits - 1;
    ReallocatingShift = HeadSizeIndexShift + HeadSizeIndexBits; ReallocatingBit = 1 shl ReallocatingShift;
    ReportedShift = ReallocatingShift + 1;                      ReportedBit = 1 shl ReportedShift;
    UserSizeRequestShift = ReportedShift + 1;

    TailSizeUnit = sizeof(uint32);
    HeadSizeUnit = 2 * sizeof(pointer);

    HeadFillerByte = $AA;
    DataFillerByte = $FF;
    TailFillerByte = $CC;
    FreedFillerByte = $F0;
    FillerByteToUint32 = $01010101;
    FillerByteToPtrUint = PtrUint((FillerByteToUint32 shl 32 or FillerByteToUint32) and High(PtrUint)); {$if sizeof(PtrUint) > 8} {$error need more} {$endif}
    DontVerifySize = PtrUint(-99);

  type
    // default size is 4 pointers, which is the floor for the overhead per allocation (not counting traces and hashtables).
    pNode = ^Node;
    Node = record
      function GetUserSizeRequest: SizeUint; inline;
      function Contains(p: pointer): boolean; inline;
    var
      userPtr: pointer;

      // info[0] = HasFullUserRequestBit.
      //           If 1, node includes fullUserRequest.
      // info[1:2] = extra info index.
      //             If 0, node has no user info (one set with SetHeapExtraInfo).
      //             If >0, node has extra info described by extraInfos[info[1:2] - 1].
      //             info[0:2] (combined) is used as index into freeNodes.
      // info[3:6] = tail size index in HeadTailSizes, in uint32s.
      // info[7:10] = head size index in HeadTailSizes, in units of 2 * sizeof(pointer).
      // info[11] = ReallocatingBit.
      //            If 1, node is being reallocated, temporarily blocking its incremental checks.
      // info[12] = ReportedBit.
      //            If 1, node was reported as corrupted, blocking further reports.
      // info[13:bitsizeof(info) - 1] = user request (info shr UserSizeRequestShift).
      //                                If HasFullUserRequestBit is 1, fullUserRequest is used instead, and info[13:bitsizeof(info) - 1] is garbage.
      info: SizeUint;

      trace: StackTracesRegistry.pNode;

    case uint32 of
      0:
      (
        hn: HashList.Node; // Used if node is in h.
        case uint32 of
          0: (extraIfNoFullUserRequest: record end);
          1: (fullUserRequest: SizeUint; extraIfHasFullUserRequest: record end);
      );
      1: (next: pNode); // Used if node is in toFree or freeNodes.
    end;

    HashListControl = record
      class function KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean; static; inline;
      class function HashItem(hn: HashList.pNode; var h: HashList): SizeUint; static;
    end;

    procedure Lock; inline;
    procedure Unlock; inline;

  type
    CheckFlag = (InsideLock, InTraceXxx, InDoGetFreeMem, InFreeToFreeItems, InSimplifiedFreeToFreeItems, InCheckHeap, InPublicCheckHeap);
    CheckFlags = set of CheckFlag;

    // To call prevMgr.FreeMem outside Lock and, when KeepReleased is enabled, save on unlocking and locking again by performing up to length(p) prevMgr.FreeMems at once.
    PrevMgrToFreeBatch = record
      n: SizeUint;
      ht: ^HeapTracer;
      p: array[0 .. 63] of pointer;
      procedure Flush(var ht: HeapTracer); inline;
      procedure Add(p: pointer; cf: CheckFlags); inline; // If cf.InsideLock is set, can UNLOCK AND LOCK AGAIN.
    end;

    ExtraInfoRec = record
      size: SizeUint;
      fill: TFillExtraInfoProc;
      display: TDisplayExtraInfoProc;
    end;

  const
    SumSizeAdjustmentPerItem = PtrUint(@Node(nil^).extraIfNoFullUserRequest); // Added to toFree.sumSize for each item to avoid zero sumSize when toFree is not empty.
    CheckFlagsThatMeanFramesUninterestingInBacktraces = [Low(CheckFlag) .. High(CheckFlag)] - [CheckFlag.InsideLock];

    // Offset from extraIfNoFullUserRequest to extraIfHasFullUserRequest... in practice, sizeof(SizeUint), but WHO KNOWS.
    OffsetFromExtraIfNoFullUserRequestToExtraIfHasFullUserRequest = PtrUint(@Node(nil^).extraIfHasFullUserRequest) - PtrUint(@Node(nil^).extraIfNoFullUserRequest);

    // Value that, if multiplied by 0 or HasFullUserRequestBit, gives 0 or OffsetFromExtraIfNoFullUserRequestToExtraIfHasFullUserRequest, respectively.
    HasFullUserRequestBitToOffsetBetweenExtras = OffsetFromExtraIfNoFullUserRequestToExtraIfHasFullUserRequest div HasFullUserRequestBit;
  {$if OffsetFromExtraIfNoFullUserRequestToExtraIfHasFullUserRequest mod HasFullUserRequestBit <> 0} {$error adjust formula (and probably occurences)} {$endif}

    function CheckHeadAndTail(n: pNode): boolean;
    function CheckFreedMemory(n: pNode; sz: SizeUint): boolean;
    class function IndexNot(p: pointer; n: SizeUint; b: byte): SizeInt; static; // Slow function (or, rather, its speed is unimportant) for rechecking what exactly failed.
    procedure FreeToFreeItems(untilSumSize: SizeUint; justFreed: pNode; var freeBatch: PrevMgrToFreeBatch; cf: CheckFlags);
    procedure FreeToFreeItems(untilSumSize: SizeUint; cf: CheckFlags);
    function AllocateNode(info: SizeUint): pNode;
    procedure FreeNode(n: pNode; var freeBatch: PrevMgrToFreeBatch; cf: CheckFlags);
    class function CfSkipFrames(cf: CheckFlags): SizeUint; static; inline;

    function DoGetMem(size: PtrUint; zeroed: boolean): pointer;
    function DoFreeMem(p: pointer; size: PtrUint): PtrUint;

    class function TraceGetMem(size: PtrUint): pointer; static;
    class function TraceFreeMem(p: pointer): PtrUint; static;
    class function TraceFreeMemSize(p: pointer; size: PtrUint): PtrUint; static;
    class function TraceAllocMem(size: PtrUint): pointer; static;
    class function TraceReallocMem(var p: pointer; size: PtrUint): pointer; static;
    class function TraceMemSize(p: pointer): PtrUint; static;
    class function TraceGetHeapStatus: THeapStatus; static;
    class function TraceGetFPCHeapStatus: TFPCHeapStatus; static;

    procedure CheckHeap(max: SizeUint; cf: CheckFlags);

  {$ifdef debug_heaptrc}
    class procedure DumpHNode(hn: HashList.pNode; var f: text); static;
    procedure Dump(var f: text);
  {$endif}

    class procedure HexDump(var f: text; p: pointer; n: SizeUint); static;
    class procedure HexDumpPart(var f: text; p: pointer; n: SizeUint); static;
    procedure DumpTrace(var f: text; n: pNode);
    procedure DumpStackAndMaybeThrowRunError(cf: CheckFlags);
    procedure ReportBadPointer(p: pointer; cf: CheckFlags);
    procedure ReportCorrupted(n: pNode; cf: CheckFlags);
    procedure ReportWrongSize(n: pNode; size, expectSize: SizeUint; cf: CheckFlags);

    procedure Install;
    procedure Uninstall;
    procedure Report(var f: text; skipIfNoLeaks: boolean);
    procedure SetOutput(const name: string);
    procedure CloseOutfp;

  var
    h: HashList;
    toFree: record
      first, last: pNode;
      sumSize: SizeUint;
    end;
    headTailInfo, // Also has BasicBit set if extraInfo is not set.
    traceDepth: uint32;

    hCheck: CircularHashListIterator; // Circular iterator over h for incremental CheckHeap work.

    traces: StackTracesRegistry;
    nodesRgn: MemoryRegion;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    lck: TRTLCriticalSection;
  {$endif FPC_HAS_FEATURE_THREADING}

    // Free lists for each ExtraInfoIndex:HasFullUserRequestBit.
    // freeNodes[0] is used for nodes with no extra info (index = 0) and no fullUserRequest stored,
    // freeNodes[1] is used for nodes with no extra info (index = 0) and fullUserRequest stored,
    // freeNodes[2] is used for nodes with extraInfos[0] (index = 1) and no fullUserRequest stored,
    // freeNodes[3] is used for nodes with extraInfos[0] (index = 1) and fullUserRequest stored, and so on.
    freeNodes: array[0 .. FreelistIndexMask] of pNode;
    extraInfos: array[0 .. MaxExtraInfos - 1] of ExtraInfoRec;

    mgrInstalled, needCloseOutfp, errorInHeap: boolean;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    lockAlive: boolean; // required because of cthreads
  {$endif FPC_HAS_FEATURE_THREADING}
    insideReport, insideCheckOrDumpHeap, nExtraInfos: uint32;
    prevMgr: TMemoryManager;
    getMemCount, getMemSize, freeMemCount, freeMemSize: int64;

  {$if declared(SystemRealloc)}
    usedByInternalAllocations,
  {$endif SystemRealloc}
    usedAtStartup: SizeUint;
    outfp, mainThreadStderr: pText;
    ownOutfp: text;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    oldInitProc, oldExitProc: pointer;
  {$endif FPC_HAS_FEATURE_THREADING}
  end;

var
  ht: HeapTracer;

{$push} {$q-,r-}
  class function HeapTracer.HashPointer(p: pointer): SizeUint;
  begin
    result := (PtrUint(p) xor PtrUint(p) shr 16) * $21f0aaad;
    result := (result xor result shr 15) * $735a2d97; // 1 multiply-xorshift round is not enough (I’ve tried).
    result := result xor result shr 15;
  end;
{$pop}

  class function HeapTracer.HeadTailSizeToIndex(sz: SizeUint): SizeUint;
  begin
    SizeInt(result) := -1;
    repeat
      inc(SizeInt(result));
    until (sz <= HeapTracer.HeadTailSizes[result]) or (result = High(HeapTracer.HeadTailSizes));
  end;

  function HeapTracer.Node.GetUserSizeRequest: SizeUint;
  var
    infoFetch: SizeUint;
  begin
    infoFetch := self.info;
    result := infoFetch shr UserSizeRequestShift;
    if infoFetch and HasFullUserRequestBit <> 0 then
      result := fullUserRequest;
  end;

{$push} {$q-,r-}
  function HeapTracer.Node.Contains(p: pointer): boolean;
  begin
    result := PtrUint(PtrUint(p) - PtrUint(userPtr)) < GetUserSizeRequest;
  end;
{$pop}

  class function HeapTracer.HashListControl.KeyEqualsToItem(key: pointer; hn: HashList.pNode; var h: HashList): boolean;
  begin
    result := key = pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.userPtr;
  end;

  class function HeapTracer.HashListControl.HashItem(hn: HashList.pNode; var h: HashList): SizeUint;
  begin
    result := HeapTracer.HashPointer(pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.userPtr);
  end;

  procedure HeapTracer.Lock;
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if lockAlive then EnterCriticalSection(lck);
  {$endif FPC_HAS_FEATURE_THREADING}
  end;

  procedure HeapTracer.Unlock;
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if lockAlive then LeaveCriticalSection(lck);
  {$endif FPC_HAS_FEATURE_THREADING}
  end;

  procedure HeapTracer.PrevMgrToFreeBatch.Flush(var ht: HeapTracer);
  begin
    while n > 0 do
    begin
      dec(n);
      ht.prevMgr.FreeMem(p[n]);
    end;
  end;

  procedure HeapTracer.PrevMgrToFreeBatch.Add(p: pointer; cf: CheckFlags);
  begin
    if n >= length(self.p) then
    begin
      if CheckFlag.InsideLock in cf then ht^.Unlock;
      Flush(ht^);
      if CheckFlag.InsideLock in cf then ht^.Lock;
    end;
    self.p[n] := p;
    inc(n);
  end;

  function HeapTracer.CheckHeadAndTail(n: pNode): boolean;
  var
    info, size, remainingUnits: SizeUint;
    pattern: PtrUint;
    p: pointer;
  begin
    info := n^.info;
    size := info shr UserSizeRequestShift;
    if info and HasFullUserRequestBit <> 0 then
      size := n^.fullUserRequest;
    info := info and (TailSizeIndexMask shl TailSizeIndexShift or HeadSizeIndexMask shl HeadSizeIndexShift);

    // Fast test for default 4-byte tails and 0-byte heads.
    if info = 1 shl TailSizeIndexShift then
      exit(unaligned(pUint32(n^.userPtr + size)^ = TailFillerByte * FillerByteToUint32));

    result := false;
    if info and (TailSizeIndexMask shl TailSizeIndexShift) <> 0 then
    begin
      p := n^.userPtr + size;
      remainingUnits := HeadTailSizes[info shr TailSizeIndexShift and TailSizeIndexMask];
      repeat {$if TailSizeUnit <> sizeof(uint32)} {$error TailSizeUnit is assumed to be uint32} {$endif}
        if unaligned(pUint32(p)^) <> TailFillerByte * FillerByteToUint32 then exit;
        inc(p, sizeof(uint32));
        dec(remainingUnits);
      until remainingUnits = 0;
    end;
    if info and (HeadSizeIndexMask shl HeadSizeIndexShift) <> 0 then
    begin
      p := n^.userPtr;
      remainingUnits := HeadTailSizes[info shr HeadSizeIndexShift and HeadSizeIndexMask];
      pattern := HeadFillerByte * FillerByteToPtrUint;
      repeat {$if HeadSizeUnit <> 2 * sizeof(pointer)} {$error HeadSizeUnit is assumed to be two pointers} {$endif}
        dec(p, 2 * sizeof(pointer));
        if (pPtrUint(p)[0] xor pattern) or (pPtrUint(p)[1] xor pattern) <> 0 then exit;
        dec(remainingUnits);
      until remainingUnits = 0;
    end;
    result := true;
  end;

  function HeapTracer.CheckFreedMemory(n: pNode; sz: SizeUint): boolean; // n^.userPtr is aligned to PtrUint; n^.userPtr + sz is not.
  var
    p: pointer;
    pattern: PtrUint;
  begin
    p := n^.userPtr;
    pattern := FreedFillerByte * FillerByteToPtrUint;
    if sz > 2 * sizeof(PtrUint) then
    begin
      result := false;
      repeat
        if (pPtrUint(p)[0] xor pattern) or (pPtrUint(p)[1] xor pattern) <> 0 then exit;
        inc(p, 2 * sizeof(PtrUInt));
        dec(sz, 2 * sizeof(PtrUint));
      until sz <= 2 * sizeof(PtrUint);
      inc(p, sz);
      result := (unaligned(pPtrUint(p)[-2]) xor pattern) or (unaligned(pPtrUint(p)[-1]) xor pattern) = 0;
    end else if sz >= sizeof(PtrUint) then
      result := (pPtrUint(p)[0] xor pattern) or (unaligned(pPtrUint(p + sz)[-1]) xor pattern) = 0
    else
    begin
      while (sz > 0) and (pByte(p)[sz - 1] = FreedFillerByte) do dec(sz);
      result := sz = 0;
    end;
  end;

  class function HeapTracer.IndexNot(p: pointer; n: SizeUint; b: byte): SizeInt;
  begin
    for result := 0 to SizeInt(n) - 1 do
      if pByte(p)[result] <> b then exit;
    result := -1;
  end;

  procedure HeapTracer.FreeToFreeItems(untilSumSize: SizeUint; justFreed: pNode; var freeBatch: PrevMgrToFreeBatch; cf: CheckFlags);
  var
    head, next: pNode;
    size: SizeUint;
  begin
    Include(cf, CheckFlag.InFreeToFreeItems);
    while toFree.sumSize > untilSumSize do
    begin
      head := toFree.first;
      size := head^.GetUserSizeRequest;
      dec(SizeInt(toFree.sumSize), SizeInt(size + SumSizeAdjustmentPerItem));
      next := head^.next;
      toFree.first := next;
      if not Assigned(next) then toFree.last := nil;
      if (head <> justFreed) and (not CheckHeadAndTail(head) or not CheckFreedMemory(head, size)) then
        ReportCorrupted(head, cf);
      FreeNode(head, freeBatch, cf);
    end;
  end;

  procedure HeapTracer.FreeToFreeItems(untilSumSize: SizeUint; cf: CheckFlags);
  var
    freeBatch: PrevMgrToFreeBatch;
  begin
    freeBatch.n := 0;
    freeBatch.ht := @self;
    FreeToFreeItems(untilSumSize, nil, freeBatch, cf + [CheckFlag.InSimplifiedFreeToFreeItems]);
    freeBatch.Flush(self);
  end;

  function HeapTracer.AllocateNode(info: SizeUint): pNode;
  var
    freeListPtr: ^pNode;
    nodeSize: SizeUint;
  begin
    freeListPtr := @freeNodes[info shr HasFullUserRequestShift and FreelistIndexMask];
    result := freeListPtr^;
    if Assigned(result) then
      freeListPtr^ := result^.next
    else
    begin
      nodeSize := PtrUint(@Node(nil^).extraIfNoFullUserRequest) + info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras;
      if info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
        inc(nodeSize, extraInfos[info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].size);
      result := nodesRgn.Push(nodeSize);
    end;
  end;

  procedure HeapTracer.FreeNode(n: pNode; var freeBatch: PrevMgrToFreeBatch; cf: CheckFlags);
  var
    info: SizeUint;
    freeListPtr: ^pNode;
  begin
    traces.Release(n^.trace);
    info := n^.info;
    freeListPtr := @freeNodes[info shr HasFullUserRequestShift and FreelistIndexMask];
    n^.next := freeListPtr^;
    freeListPtr^ := n;
    freeBatch.Add(n^.userPtr - HeadTailSizes[info shr HeadSizeIndexShift and HeadSizeIndexMask] * HeadSizeUnit, cf);
  end;

type
  HeapTracerCheckFlags = HeapTracer.CheckFlags;

  class function HeapTracer.CfSkipFrames(cf: CheckFlags): SizeUint;
{$if sizeof(HeapTracerCheckFlags) = sizeof(uint32)}
  begin
    result := PopCnt(uint32(cf * CheckFlagsThatMeanFramesUninterestingInBacktraces));
  end;
{$else}
  var
    f: CheckFlag;
  begin
    result := 0;
    for f in cf * CheckFlagsThatMeanFramesUninterestingInBacktraces do
      inc(result);
  end;
{$endif}

  function HeapTracer.DoGetMem(size: PtrUint; zeroed: boolean): pointer;
  var
    fullSize, headSize, nTrace, info: SizeUint;
    n: pNode;
    fill: TFillExtraInfoProc;
    packedTrace: array[0 .. StackTrace.MaxBytes - 1] of byte;
    trace: array[0 .. StackTrace.MaxItems - 1] of CodePointer;
  begin
    // First do things that don’t require locking: capture the trace, allocate result, fill.
    nTrace := traceDepth;
    if nTrace > 0 then
    begin
      nTrace := CaptureBacktrace(2, nTrace, trace);
      if nTrace > 0 then StackTrace.Pack(trace, nTrace, pByte(packedTrace));
    end;

  {$push} {$q-,r-}
    info := headTailInfo + size shl UserSizeRequestShift;
  {$pop}
    if size > High(Node.info) shr UserSizeRequestShift then inc(info, HasFullUserRequestBit);
    headSize := HeadTailSizes[info shr HeadSizeIndexShift and HeadSizeIndexMask] * HeadSizeUnit;
    fullSize := headSize + size + SizeUint(HeadTailSizes[info shr TailSizeIndexShift and TailSizeIndexMask] * TailSizeUnit);
    if fullSize < size then RunError(204);

    if zeroed then
      result := prevMgr.AllocMem(fullSize)
    else
      result := prevMgr.GetMem(fullSize);
    if headSize <> 0 then
    begin
      FillChar(result^, headSize, HeadFillerByte);
      inc(result, headSize);
    end;
    if not zeroed then
      FillChar(result^, size, DataFillerByte);
    // tailor to the default tail size
    if info and (TailSizeIndexMask shl TailSizeIndexShift) = 1 shl TailSizeIndexShift then
      unaligned(pUint32(result + size)^) := FillerByteToUint32 * TailFillerByte
    else if info and (TailSizeIndexMask shl TailSizeIndexShift) <> 0 then
      FillChar((result + size)^, fullSize - headSize - size, TailFillerByte);

    Lock;
    // At this point, heaptrc can/will deadlock (and/or get corrupted) if an internal allocation fails and throws an error.
    // But internal allocations are rare so let’s ignore this.
    CheckHeap(AutoCheckHeapStep, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);

    n := AllocateNode(info);
    if info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
    begin
      fill := extraInfos[info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].fill;
      if Assigned(fill) then fill(pointer(@n^.extraIfNoFullUserRequest) + info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
    end;

    n^.userPtr := result;
    n^.info := info;
    n^.trace := nil;
    if info and HasFullUserRequestBit <> 0 then
      n^.fullUserRequest := size;
    if nTrace > 0 then n^.trace := traces.Add(pByte(packedTrace), nodesRgn);
    h.Add(@n^.hn, HashPointer(result));
    if h.nhmask <> hCheck.hMask then hCheck.FixupPostRehash(h);

    inc(getMemCount);
    inc(getMemSize, size);
    Unlock;
  end;

  function HeapTracer.DoFreeMem(p: pointer; size: PtrUint): PtrUint;
  const
    NoTrace = SizeUint(-1);
    UnlockToFillThreshold = 512;
  var
    hn: HashList.pNode;
    hnLink: HashList.ppNode;
    n, toFreePrev: pNode;
    nTrace: SizeUint;
    freeBatch: PrevMgrToFreeBatch;
    packedTrace: array[0 .. StackTrace.MaxBytes - 1] of byte;
    trace: array[0 .. StackTrace.MaxItems - 1] of CodePointer;
  begin
    if not Assigned(p) then exit(0);
    nTrace := NoTrace; // Skip collecting trace if definitely not required.
    if KeepReleasedBytes > 0 then // Careful: “size” is usually DontVerifySize.
    begin
      nTrace := traceDepth;
      if nTrace > 0 then
      begin
        nTrace := CaptureBacktrace(2, nTrace, trace);
        if nTrace > 0 then StackTrace.Pack(trace, nTrace, pByte(packedTrace));
      end;
    end;
    freeBatch.n := 0;

    Lock;
    hn := specialize HashListTemplated<HashListControl>.Find(h, p, HashPointer(p), hnLink);
    if not Assigned(hn) then
    begin
      ReportBadPointer(p, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);
      Unlock;
      exit(0);
    end;
    inc(freeMemCount);
    if hn = hCheck.cur then hCheck.FixupPreRemoveCur;
    n := pointer(hn) - PtrUint(@Node(nil^).hn);
    h.RemoveNode(hn, hnLink); // Even on size mismatch etc., remove from h so that double free still throws an error.
    if hCheck.hmask <> h.nhmask then hCheck.FixupPostRehash(h);
    result := n^.GetUserSizeRequest;
    inc(freeMemSize, result);
    if size <> DontVerifySize then
      if size <> result then
      begin
        dec(freeMemSize, result);
        inc(freeMemSize, size);
        ReportWrongSize(n, size, result, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);
      end;
    if not CheckHeadAndTail(n) then ReportCorrupted(n, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);

    if result >= UnlockToFillThreshold then Unlock;
    FillChar(p^, result, FreedFillerByte);
    if result >= UnlockToFillThreshold then Lock;

    if nTrace = NoTrace then
      FreeNode(n, freeBatch, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem])
    else
    begin
      freeBatch.ht := @self; // VERY DANGEROUS HACK: only freeBatch.Add called by FreeToFreeItems uses freeBatch.ht (and, in fact, only when freeBatch.p overflows :D), so it is set here, saving 1 instruction.
      traces.Release(n^.trace);
      if nTrace > 0 then n^.trace := traces.Add(pByte(packedTrace), nodesRgn);
      n^.next := nil;
      toFreePrev := toFree.last;
      if Assigned(toFreePrev) then
        toFreePrev^.next := n
      else
        toFree.first := n;
      toFree.last := n;
      inc(toFree.sumSize, result + SumSizeAdjustmentPerItem);
      if toFree.sumSize > KeepReleasedBytes then
        FreeToFreeItems(KeepReleasedBytes, n, freeBatch, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);
    end;
    CheckHeap(AutoCheckHeapStep, [CheckFlag.InsideLock, CheckFlag.InTraceXxx, CheckFlag.InDoGetFreeMem]);
    Unlock;
    freeBatch.Flush(self);
  end;

  class function HeapTracer.TraceGetMem(size: PtrUint): pointer;
  begin
    result := ht.DoGetMem(size, false);
  end;

  class function HeapTracer.TraceFreeMem(p: pointer): PtrUint;
  begin
    result := ht.DoFreeMem(p, DontVerifySize);
  end;

  class function HeapTracer.TraceFreeMemSize(p: pointer; size: PtrUint): PtrUint;
  begin
    result := ht.DoFreeMem(p, size);
  end;

  class function HeapTracer.TraceAllocMem(size: PtrUint): pointer;
  begin
    result := ht.DoGetMem(size, true);
  end;

  class function HeapTracer.TraceReallocMem(var p: pointer; size: PtrUint): pointer;
  var
    this: ^HeapTracer;
    nTrace, headSize, fullSize, oldSize, oldHash, info: SizeUint;
    hn: HashList.pNode;
    dummyLink: HashList.ppNode;
    freeListPtr: ^pNode;
    fill: TFillExtraInfoProc;
    prevMgrPtr: pointer;
    n, newn: pNode;
    packedTrace: array[0 .. StackTrace.MaxBytes - 1] of byte;
    trace: array[0 .. StackTrace.MaxItems - 1] of CodePointer;
  begin
    result := p;
    if (size = 0) or not Assigned(result) then
      if size = 0 then
      begin
        p := nil;
        ht.DoFreeMem(result, DontVerifySize);
        exit(nil);
      end else
      begin
        result := ht.DoGetMem(size, false);
        p := result;
        exit;
      end;

    this := @ht;
    nTrace := this^.traceDepth;
    if nTrace > 0 then
    begin
      nTrace := CaptureBacktrace(1, nTrace, trace);
      if nTrace > 0 then StackTrace.Pack(trace, nTrace, pByte(packedTrace));
    end;

    oldHash := HashPointer(p);
    // Validate pointer first, then reallocate, then do the bookkeeping, so on reallocation failure things remain unchanged (and the lock is not taken).
    // Unfortunately this requires blocking twice and having ReallocatingBit.
    this^.Lock;
    hn := specialize HashListTemplated<HashListControl>.Find(this^.h, p, oldHash, dummyLink);
    if not Assigned(hn) then
    begin
      this^.ReportBadPointer(p, [CheckFlag.InsideLock, CheckFlag.InTraceXxx]);
      this^.Unlock;
      exit(p);
    end;
    n := pointer(hn) - PtrUint(@Node(nil^).hn);
    if not this^.CheckHeadAndTail(n) then this^.ReportCorrupted(n, [CheckFlag.InsideLock, CheckFlag.InTraceXxx]);
    info := n^.info or ReallocatingBit;
    n^.info := info;
    this^.Unlock;

    oldSize := info shr UserSizeRequestShift;
    if info and HasFullUserRequestBit <> 0 then
      oldSize := n^.fullUserRequest;
    headSize := HeadTailSizes[info shr HeadSizeIndexShift and HeadSizeIndexMask] * HeadSizeUnit;

    if size < oldSize then
      FillChar((result + size)^, oldSize - size, FreedFillerByte); // ...Ok, on reallocation failure things remain *ALMOST* unchanged, not counting this.

    fullSize := headSize + size + SizeUint(HeadTailSizes[info shr TailSizeIndexShift and TailSizeIndexMask] * TailSizeUnit);
    if fullSize < size then RunError(204);
    prevMgrPtr := p - headSize;
    result := this^.prevMgr.ReallocMem(prevMgrPtr, fullSize);
    p := prevMgrPtr;
    if Assigned(p) then inc(p, headSize);
    if not Assigned(result) then exit;
    inc(result, headSize);

    if size > oldSize then
      FillChar((result + oldSize)^, size - oldSize, DataFillerByte);

    if info and (TailSizeIndexMask shl TailSizeIndexShift) = 1 shl TailSizeIndexShift then
      unaligned(pUint32(result + size)^) := FillerByteToUint32 * TailFillerByte
    else if info and (TailSizeIndexMask shl TailSizeIndexShift) <> 0 then
      FillChar((result + size)^, fullSize - headSize - size, TailFillerByte);
    dec(info, ReallocatingBit);

    this^.Lock;
    if info and HasFullUserRequestBit = 0 then
      if size <= High(Node.info) shr UserSizeRequestShift then
        n^.info := info and SizeUint(1 shl UserSizeRequestShift - 1) + size shl UserSizeRequestShift
      else
      begin
        // Might start requiring fullUserRequest. Tricky and very rare case...
        // ...outright impossible in 64 bits where it can only be tested by artificially lowering the threshold for requiring fullUserRequest...
        inc(info, HasFullUserRequestBit);
        newn := this^.AllocateNode(info);
        newn^.userPtr := n^.userPtr;
        newn^.info := info;
        newn^.trace := n^.trace;
        newn^.fullUserRequest := size;
        // Replace n with newn in hashtable... Ugh...
        newn^.hn := n^.hn; // Ugh...
        this^.h.GetLink(@n^.hn, oldHash)^ := @newn^.hn; // Ugh...
        if @n^.hn = this^.hCheck.cur then this^.hCheck.cur := @newn^.hn; // Ugh...
        // Return n to the corresponding freelist...
        freeListPtr := @this^.freeNodes[info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) shr HasFullUserRequestShift]; { Formula removes HasFullUserRequestBit added above. }
        n^.next := freeListPtr^;
        freeListPtr^ := n;
        n := newn;
      end
    else
    begin
      n^.info := info;
      n^.fullUserRequest := size;
    end;

    if n^.userPtr <> result then
    begin
      if @n^.hn = this^.hCheck.cur then this^.hCheck.FixupPreRemoveCur;
      this^.h.RemoveNode(@n^.hn, this^.h.GetLink(@n^.hn, oldHash)); // dummyLink can become invalid after unlocking, so recover the correct link.

      n^.userPtr := result;
      this^.h.Add(@n^.hn, HashPointer(result));
      if this^.h.nhmask <> this^.hCheck.hmask then this^.hCheck.FixupPostRehash(this^.h);

      // Different getMemSize + freeMemSize logic depending on whether the memory was moved or not because why not.
      inc(this^.freeMemSize, oldSize);
      inc(this^.getMemSize, size);
    end else
      if size < oldSize then
        inc(this^.freeMemSize, SizeUint(oldSize - size))
      else
        inc(this^.getMemSize, SizeUint(size - oldSize));
    this^.traces.Release(n^.trace);
    if nTrace > 0 then n^.trace := this^.traces.Add(pByte(packedTrace), this^.nodesRgn);
    if info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
    begin
      fill := this^.extraInfos[info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].fill;
      if Assigned(fill) then fill(pointer(@n^.extraIfNoFullUserRequest) + info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
    end;
    this^.CheckHeap(AutoCheckHeapStep, [CheckFlag.InsideLock, CheckFlag.InTraceXxx]);
    this^.Unlock;
  end;

  class function HeapTracer.TraceMemSize(p: pointer): PtrUint;
  var
    this: ^HeapTracer;
    hn: HashList.pNode;
    info: SizeUint;
  begin
    if not Assigned(p) then exit(0);
    this := @ht;
    this^.Lock;
    hn := specialize HashListTemplated<HashListControl>.FindAndBump(this^.h, p, HashPointer(p));
    if not Assigned(hn) then
    begin
      this^.ReportBadPointer(p, [CheckFlag.InsideLock, CheckFlag.InTraceXxx]);
      this^.Unlock;
      exit(0);
    end;
    info := pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.info;
    result := info shr UserSizeRequestShift;
    if info and HasFullUserRequestBit <> 0 then
      result := pNode(pointer(hn) - PtrUint(@Node(nil^).hn))^.fullUserRequest;
    this^.CheckHeap(AutoCheckHeapStep, [CheckFlag.InsideLock, CheckFlag.InTraceXxx]);
    this^.Unlock;
    if not this^.CheckHeadAndTail(pointer(hn) - PtrUint(@Node(nil^).hn)) then this^.ReportCorrupted(pointer(hn) - PtrUint(@Node(nil^).hn), [CheckFlag.InTraceXxx]);
  end;

  class function HeapTracer.TraceGetHeapStatus: THeapStatus;
  begin
    result := ht.prevMgr.GetHeapStatus();
  {$if declared(SystemRealloc)} // Otherwise allocated in prevMgr so it should already count them.
    if IncludeInternalAllocationsInStatistics then
    begin
      inc(result.TotalAllocated, ht.usedByInternalAllocations);
      inc(result.TotalAddrSpace, ht.usedByInternalAllocations);
    end;
  {$else SystemRealloc}
    if not IncludeInternalAllocationsInStatistics then // For tests.
      result.TotalAllocated := ht.getMemSize - ht.freeMemSize;
  {$endif SystemRealloc}
  end;

  class function HeapTracer.TraceGetFPCHeapStatus: TFPCHeapStatus;
  begin
    result := ht.prevMgr.GetFPCHeapStatus();
  {$if declared(SystemRealloc)}
    if IncludeInternalAllocationsInStatistics then
    begin
      inc(result.CurrHeapSize, ht.usedByInternalAllocations);
      inc(result.CurrHeapUsed, ht.usedByInternalAllocations);
    end;
  {$else SystemRealloc}
    if not IncludeInternalAllocationsInStatistics then // For tests.
      result.CurrHeapUsed := ht.getMemSize - ht.freeMemSize;
  {$endif SystemRealloc}
  end;

  procedure HeapTracer.CheckHeap(max: SizeUint; cf: CheckFlags);
  var
    n: pNode;
  begin
    if max > h.nItems then max := h.nItems;
    if (max = 0) or (insideReport > 0) or (insideCheckOrDumpHeap > 0) then exit;
    inc(insideCheckOrDumpHeap);
    repeat
      n := pointer(hCheck.cur) - PtrUint(@Node(nil^).hn);
      if (n^.info and ReallocatingBit = 0 {Ignore nodes in the middle of reallocation.}) and not CheckHeadAndTail(n) then
        ReportCorrupted(n, cf + [CheckFlag.InCheckHeap]);
      hCheck.MoveNext;
      dec(max);
    until (max = 0) or not Assigned(hCheck.cur);
    dec(insideCheckOrDumpHeap);
  end;

{$ifdef debug_heaptrc}
  class procedure HeapTracer.DumpHNode(hn: HashList.pNode; var f: text);
  var
    n: pNode;
  begin
    n := pointer(hn) - PtrUint(@Node(nil^).hn);
    write(f, HexStr(PtrUint(n^.userPtr), 1 + {$if sizeof(pointer) <= 4} BsrDWord {$else} BsrQWord {$endif} (PtrUint(n^.userPtr) or 1) div 4), '/', n^.GetUserSizeRequest);
    if Assigned(n^.trace) then write(f, '/t', n^.trace^.dataOfs);
    if n^.info and HasFullUserRequestBit <> 0 then write(f, '/Sz');
    if n^.info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
      write(f, '/Ex', n^.info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1);
  end;

  procedure HeapTracer.Dump(var f: text);
  var
    tf: pNode;
    i, sumSize: SizeInt;
    something: boolean;
  begin
  {$if declared(SystemRealloc)}
    writeln(f, 'usedByInternalAllocations = ', usedByInternalAllocations, LineEnding);
  {$endif SystemRealloc}
    write(f, 'ht.h: ');
    ht.h.Dump(f, @HeapTracer.DumpHNode);
    tf := toFree.first;
    if Assigned(tf) then
    begin
      write(f, LineEnding, 'ht.toFree: ');
      sumSize := 0;
      repeat
        if tf <> toFree.first then write(f, ', ');
        DumpHNode(@tf^.hn, f); // Hack, hn does not exist, but DumpHNode simply applies offset and dumps containing Node.
        inc(SizeUint(sumSize), tf^.GetUserSizeRequest);
        tf := tf^.next;
      until not Assigned(tf);
      writeln(f, ' (', StackTracesRegistry.CountLinkedListItems(toFree.first, PtrUint(@Node(nil^).next)), ' total, ', sumSize, ' b)');
    end;
    something := false;
    for i := 0 to High(freeNodes) do
      if Assigned(freeNodes[i]) then
      begin
        if not something then write(f, LineEnding, 'Free nodes: ') else write(f, ', ');
        something := true;
        write(f, StackTracesRegistry.CountLinkedListItems(freeNodes[i], PtrUint(@Node(nil^).next)), ' (');
        if i and (HasFullUserRequestBit shr HasFullUserRequestShift) = 0 then write(f, '4 pointers') else write(f, '5 pointers');
        if i > HasFullUserRequestBit shr HasFullUserRequestShift then write(f, ' + extra info #', i shr 1 - 1);
        write(f, ')');
      end;
    if something then writeln(f);
    if Assigned(hCheck.cur) then
    begin
      write(f, 'hCheck = ');
      DumpHNode(hCheck.cur, f);
      writeln(f);
    end;
  end;
{$endif}

  class procedure HeapTracer.HexDump(var f: text; p: pointer; n: SizeUint);
  var
    max, nPart: SizeUint;
  begin
    max := MaxPrintedBlockLength;
    if n <= max then
      HexDumpPart(f, p, n)
    else
    begin
      nPart := max div 4 * 3;
      HexDumpPart(f, p, nPart);
      writeln(f, '... (+', n - max, ' bytes) ...');
      nPart := max - nPart;
      HexDumpPart(f, p + n - nPart, nPart);
    end;
  end;

  class procedure HeapTracer.HexDumpPart(var f: text; p: pointer; n: SizeUint);
  const
    RowLength = 80 div 4;
  var
    iRow, nBytesInRow, iByteInRow: SizeUint;
  begin
    iRow := 0;
    while n > 0 do
    begin
      nBytesInRow := RowLength;
      if nBytesInRow > n then nBytesInRow := n;
      for iByteInRow := 0 to nBytesInRow - 1 do
      begin
        write(f, HexStr(pByte(p)[iByteInRow], 2));
        if iByteInRow + 1 < nBytesInRow then write(f, ' ');
      end;
      if iRow > 0 then // pad the last line if it is not the only line
        for iByteInRow := nBytesInRow to RowLength - 1 do
          write(f, '   ');
      write(f, '|');
      for iByteInRow := 0 to nBytesInRow - 1 do
        if (pAnsiChar(p)[iByteInRow] >= ' ') and (pAnsiChar(p)[iByteInRow] <= #126) then
          write(f, pAnsiChar(p)[iByteInRow])
        else
          write(f, '.');
      writeln(f);
      inc(p, nBytesInRow);
      dec(n, nBytesInRow);
      inc(iRow);
    end;
  end;

  procedure HeapTracer.DumpTrace(var f: text; n: pNode);
  var
    trace: array[0 .. StackTrace.MaxItems - 1] of CodePointer;
    iTrace: SizeInt;
  begin
    if Assigned(n^.trace) then
      for iTrace := 0 to SizeInt(StackTrace.Unpack(traces.data + n^.trace^.dataOfs, trace)) - 1 do
        writeln(f, BacktraceStrFunc(trace[iTrace]));
    writeln(f);
  end;

  procedure HeapTracer.DumpStackAndMaybeThrowRunError(cf: CheckFlags);
  begin
    dump_stack(outfp^, 2 + CfSkipFrames(cf));
    writeln(outfp^);
    if not HaltOnError then exit;
    if CheckFlag.InsideLock in cf then Unlock;
    RunError(204);
  end;

  procedure HeapTracer.ReportBadPointer(p: pointer; cf: CheckFlags);
  var
    tfn: pNode;
    display: TDisplayExtraInfoProc;
  begin
    errorInHeap := true;
    if insideReport > 0 then exit;
    inc(insideReport);
    tfn := toFree.first;
    while Assigned(tfn) and not tfn^.Contains(p) do
      tfn := tfn^.next;
    if Assigned(tfn) then
    begin
      write(outfp^, 'pointer $', HexStr(p), ' (');
      if tfn^.userPtr <> p then
        write(outfp^, 'base ', HexStr(tfn^.userPtr), ', ');
      write(outfp^, 'size ', tfn^.GetUserSizeRequest);
      if tfn^.userPtr <> p then
        write(outfp^, ', offset +', p - tfn^.userPtr);
      write(outfp^, ')');
      display := nil;
      if tfn^.info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
      begin
        display := extraInfos[tfn^.info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].display;
        if Assigned(display) then
        begin
          writeln(outfp^);
          display(outfp^, pointer(@tfn^.extraIfNoFullUserRequest) + tfn^.info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
        end;
      end;
      if not Assigned(display) then write(outfp^, ' ');
      write(outfp^, 'was freed');
      if Assigned(tfn^.trace) then
      begin
        writeln(outfp^, ' at');
        DumpTrace(outfp^, tfn);
      end else
        write(outfp^, ', ');
      writeln(outfp^, 'and touched again at');
    end else
      writeln(outfp^, 'pointer $', HexStr(p), ' does not point to valid memory block');
    DumpStackAndMaybeThrowRunError(cf);
    dec(insideReport);
  end;

  procedure HeapTracer.ReportCorrupted(n: pNode; cf: CheckFlags);
  var
    info, size, headSize, tailSize, wrongOfs: SizeUint;
    display: TDisplayExtraInfoProc;
  begin
    errorInHeap := true;
    if insideReport > 0 then exit;
    info := n^.info;
    if info and ReportedBit <> 0 then exit;
    size := info shr UserSizeRequestShift;
    if info and HasFullUserRequestBit <> 0 then size := n^.fullUserRequest;
    n^.info := info + ReportedBit;

    headSize := HeadTailSizes[info shr HeadSizeIndexShift and HeadSizeIndexMask] * HeadSizeUnit;
    inc(insideReport);
    write(outfp^, 'Marked memory at $', HexStr(n^.userPtr));
    if CheckFlag.InFreeToFreeItems in cf then
      writeln(outfp^, ' changed after free')
    else
      writeln(outfp^, ' invalid');
    writeln(outfp^);
    if IndexNot(n^.userPtr - headSize, headSize, HeadFillerByte) >= 0 then
    begin
      writeln(outfp^, 'Head must be all ', HexStr(HeadFillerByte, 2), 's:');
      HexDump(outfp^, n^.userPtr - headSize, headSize);
      writeln(outfp^);
    end;
    if CheckFlag.InFreeToFreeItems in cf then
    begin
      SizeInt(wrongOfs) := IndexNot(n^.userPtr, size, FreedFillerByte);
      if SizeInt(wrongOfs) >= 0 then
      begin
        writeln(outfp^, 'Block content from offset ', wrongOfs, ' must be all ', HexStr(FreedFillerByte, 2), 's:');
        HexDump(outfp^, n^.userPtr + wrongOfs, size - wrongOfs);
        writeln(outfp^);
      end;
    end else if printfaultyblock then
    begin
      writeln(outfp^, 'Block content:');
      HexDump(outfp^, n^.userPtr, size);
      writeln(outfp^);
    end;
    tailSize := HeadTailSizes[info shr TailSizeIndexShift and TailSizeIndexMask] * TailSizeUnit;
    if IndexNot(n^.userPtr + size, tailSize, TailFillerByte) >= 0 then
    begin
      writeln(outfp^, 'Tail must be all ', HexStr(TailFillerByte, 2), 's:');
      HexDump(outfp^, n^.userPtr + size, tailSize);
      writeln(outfp^);
    end;
    if n^.info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
    begin
      display := extraInfos[n^.info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].display;
      if Assigned(display) then
      begin
        writeln(outfp^);
        display(outfp^, pointer(@n^.extraIfNoFullUserRequest) + n^.info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
      end;
    end;
    if Assigned(n^.trace) then
    begin
      if CheckFlag.InFreeToFreeItems in cf then
        writeln(outfp^, 'Freed at:')
      else
        writeln(outfp^, 'Allocated at:');
      DumpTrace(outfp^, n);
    end;
    if ([CheckFlag.InCheckHeap, CheckFlag.InPublicCheckHeap] * cf = [CheckFlag.InCheckHeap]) or (CheckFlag.InFreeToFreeItems in cf) then
      writeln(outfp^, 'Incidentally spotted by:')
    else
      writeln(outfp^, 'Detected by:');
    DumpStackAndMaybeThrowRunError(cf);
    dec(insideReport);
  end;

  procedure HeapTracer.ReportWrongSize(n: pNode; size, expectSize: SizeUint; cf: CheckFlags);
  var
    display: TDisplayExtraInfoProc;
  begin
    errorInHeap := true;
    inc(insideReport);
    writeln(outfp^, 'Wrong size of $', HexStr(n^.userPtr), ': ', expectSize, ' allocated, ', size, ' freed');
    if n^.info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
    begin
      display := extraInfos[n^.info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].display;
      if Assigned(display) then
      begin
        writeln(outfp^);
        display(outfp^, pointer(@n^.extraIfNoFullUserRequest) + n^.info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
      end;
    end;
    if Assigned(n^.trace) then
    begin
      writeln(outfp^, 'Allocated at:');
      DumpTrace(outfp^, n);
    end;
    writeln(outfp^, 'Occured at:');
    DumpStackAndMaybeThrowRunError(cf);
    dec(insideReport);
  end;

  procedure HeapTracer.Install;
  var
    newMgr: TMemoryManager;
  begin
    h.Init(@HashListControl.HashItem);

    GetMemoryManager(prevMgr);
    newMgr := prevMgr;
    newMgr.GetMem := @HeapTracer.TraceGetMem;
    newMgr.FreeMem := @HeapTracer.TraceFreeMem;
    newMgr.FreeMemSize := @HeapTracer.TraceFreeMemSize;
    newMgr.AllocMem := @HeapTracer.TraceAllocMem;
    newMgr.ReallocMem := @HeapTracer.TraceReAllocMem;
    newMgr.MemSize := @HeapTracer.TraceMemSize;
    newMgr.GetHeapStatus := @HeapTracer.TraceGetHeapStatus;
    newMgr.GetFPCHeapStatus := @HeapTracer.TraceGetFPCHeapStatus;
    SetMemoryManager(newMgr);
    mgrInstalled := true;
    usedAtStartup := prevMgr.GetFPCHeapStatus.CurrHeapUsed;
  end;

  procedure HeapTracer.Uninstall;
  begin
    if not mgrInstalled then exit;
    FreeToFreeItems(0, []); // Finalization does this before reporting, but reporting can allocate with BacktraceStrFuncs...

    Finalize(h);
    Finalize(traces);
    Finalize(nodesRgn);

    SetMemoryManager(prevMgr);
    mgrInstalled := false;
  end;

{$if defined(LINUX) or defined(BSD)}
{$if defined(linux)}
    { if libc is not linked explicitly, FPC might chose the wrong startup code, as
      libdl depends on libc on linux, this does not hurt }
    {$linklib c}
{$endif}

const
{$ifdef BSD}   // dlopen is in libc on FreeBSD.
  LibDL = 'c';
{$else}
  {$ifdef HAIKU}
    LibDL = 'root';
  {$else}
    LibDL = 'dl';
  {$endif}
{$endif}

type
  Pdl_info = ^dl_info;
  dl_info = record
    dli_fname      : Pansichar;
    dli_fbase      : pointer;
    dli_sname      : Pansichar;
    dli_saddr      : pointer;
  end;

// *BSD isn't flagged for "weak"  support in 3.2.2
{$if defined(BSD) and (FPC_FULLVERSION<30300)}
  function _dladdr(Lib:pointer; info: Pdl_info): Longint; cdecl; external LibDL name 'dladdr';
{$else}
  function _dladdr(Lib:pointer; info: Pdl_info): Longint; cdecl; weakexternal LibDL name 'dladdr';
{$endif}
{$elseif defined(MSWINDOWS)}
  function _GetModuleFileNameA(hModule:HModule;lpFilename:PAnsiChar;nSize:cardinal):cardinal;stdcall; external 'kernel32' name 'GetModuleFileNameA';
{$endif}

function GetModuleName:shortstring;
{$if defined(LINUX) or defined(BSD)}
var
  res:integer;
  dli:dl_info;
begin
  if assigned(@_dladdr) then
    begin
      res:=_dladdr(@ParamStr,@dli); { get any non-eliminated address in SO space }
      if res<=0 then
        exit;
      if Assigned(dli.dli_fname) then
        GetModuleName:=PAnsiChar(dli.dli_fname);
    end
  else
    GetModuleName:=ParamStr(0);
end;
{$elseif defined(MSWINDOWS)}
var
  buf:array[0..260-1] of ansichar;
begin
  setstring(GetModuleName,PAnsiChar(buf),_GetModuleFileNameA(hInstance,PAnsiChar(buf),length(buf)))
end;
{$else generic GetModuleName}
begin
  GetModuleName:=ParamStr(0);
end;
{$endif GetModuleName}

  procedure HeapTracer.Report(var f: text; skipIfNoLeaks: boolean);
  var
    rem, ih, sz, savedMinItems, savedMaxItems: SizeUint;
    hn: HashList.pNode;
    n: pNode;
    display: TDisplayExtraInfoProc;
    status: TFPCHeapStatus;
    emptyCluster: boolean;
  {$if declared(LooksLikeClassInstance)}
    name: shortstring;
  {$endif LooksLikeClassInstance}
  begin
    if skipIfNoLeaks and (h.nItems = 0) and (getMemCount = freeMemCount) and (getMemSize = freeMemSize) then
      exit;

    inc(insideCheckOrDumpHeap);
    status := prevMgr.GetFPCHeapStatus();
    write(f,
      'Heap dump by heaptrc unit of "' + GetModuleName() + '"', LineEnding,
      getMemCount, ' memory blocks allocated : ', getMemSize, LineEnding,
      freeMemCount, ' memory blocks freed : ', freeMemSize, LineEnding,
      int64(getMemCount) - int64(freeMemCount), ' unfreed memory blocks : ', int64(getMemSize) - int64(freeMemSize), LineEnding,
      'True heap size : ', status.CurrHeapSize);
    if usedAtStartup > 0 then
      write(f, ' (', usedAtStartup, ' used in System startup)');
    writeln(f, LineEnding,
      'True free heap : ', status.CurrHeapFree);
    writeln(f);

    // Reporting from h avoids having prev/next pointers in every Node, at the cost of order and complexity.
    // Disable h rehashes as BacktraceStrFunc might allocate. (Still unsafe because in the worst case it might DEallocate the current node...)
    savedMinItems := h.minItems;
    savedMaxItems := h.maxItems;
    h.minItems := 0;
    h.maxItems := High(SizeUint);
    rem := h.nItems;
    if rem > MaxLeaksToReport then rem := MaxLeaksToReport;
    ih := 0;
    emptyCluster := false;
    while (ih <= h.nhmask) and (rem > 0) do
    begin
      hn := h.h[ih];
      while Assigned(hn) and (rem > 0) do
      begin
        n := pointer(hn) - PtrUint(@Node(nil^).hn);
        sz := n^.GetUserSizeRequest;
        if Assigned(n^.trace) and emptyCluster then // Cluster multiple “N/A”s without extra newlines.
          writeln(f);
        emptyCluster := not Assigned(n^.trace) and not printleakedblock;
        write(f, 'Call trace for block $', HexStr(n^.userPtr), ' size ', sz);
      {$if declared(LooksLikeClassInstance)}
        if LooksLikeClassInstance(n^.userPtr, n^.GetUserSizeRequest, name) then
          write(f, ' (', name, ')');
      {$endif LooksLikeClassInstance}
        if Assigned(n^.trace) then writeln(f) else writeln(f, ': N/A.');
        if n^.info and (ExtraInfoIndexMask shl ExtraInfoIndexShift) <> 0 then
        begin
          display := extraInfos[n^.info shr ExtraInfoIndexShift and ExtraInfoIndexMask - 1].display;
          if Assigned(display) then
          begin
            display(outfp^, pointer(@n^.extraIfNoFullUserRequest) + n^.info and HasFullUserRequestBit * HasFullUserRequestBitToOffsetBetweenExtras);
            emptyCluster := false;
          end;
        end;
        if printleakedblock then HexDump(f, n^.userPtr, sz);
        if Assigned(n^.trace) then
          DumpTrace(f, n)
        else if not emptyCluster then
          writeln(f);
        dec(rem);
        hn := hn^.next;
      end;
      inc(ih);
    end;
    if emptyCluster then writeln(f);
    // Recover rehashes.
    h.minItems := savedMinItems;
    h.maxItems := savedMaxItems;
    dec(insideCheckOrDumpHeap);
  end;

  procedure HeapTracer.SetOutput(const name: string);
  var
    i: SizeInt;
  begin
    CloseOutfp;
    Assign(ownOutfp, name);
  {$push} {$I-}
    Append(ownOutfp);
    if IOResult <> 0 then
    begin
      Rewrite(ownOutfp);
      if IOResult <> 0 then
      begin
        Writeln(outfp^, '[heaptrc] Unable to open "', name, '", writing output to stderr instead.');
        exit;
      end;
    end;
  {$pop}
    outfp := @ownOutfp;
    needCloseOutfp := true;
    for i := 0 to ParamCount do
      write(outfp^, ParamStr(i), pAnsiChar(' '#0 + LineEnding) + 2 * ord(i = ParamCount));
  end;

  procedure HeapTracer.CloseOutfp;
  begin
    if needCloseOutfp then
    begin
      needCloseOutfp := false;
      Close(outfp^);
    end;
    outfp := mainThreadStderr;
  end;

  function InternalRealloc(p: pointer; oldSize, newSize: SizeUint): pointer;
  begin
  {$if declared(SystemRealloc)}
    result := SystemRealloc(p, oldSize, newSize);
    dec(ht.usedByInternalAllocations, oldSize);
    inc(ht.usedByInternalAllocations, newSize);
  {$else SystemRealloc}
    result := ht.prevMgr.ReallocMem(p, newSize);
  {$endif SystemRealloc}
  end;

  procedure DumpHeap;
  begin
    DumpHeap(GlobalSkipIfNoLeaks);
  end;

  procedure DumpHeap(SkipIfNoLeaks: Boolean);
  begin
    if not ht.mgrInstalled then exit;
    ht.Lock;
    ht.Report(ht.outfp^, SkipIfNoLeaks);
    ht.Unlock;
  end;

  procedure CheckHeap(step: SizeUint = High(SizeUint));
  begin
    if not ht.mgrInstalled then exit;
    ht.Lock;
    ht.CheckHeap(step, [ht.CheckFlag.InsideLock, ht.CheckFlag.InPublicCheckHeap]);
    ht.Unlock;
  end;

  procedure HexDump(var f: text; p: pointer; n: SizeUint);
  begin
    HeapTracer.HexDumpPart(f, p, n);
  end;

  procedure SetHeapExtraInfo(size: PtrUint; fillProc: TFillExtraInfoProc; displayProc: TDisplayExtraInfoProc);
  type
    NodeAlignmentCheck = record
      b: byte;
      n: HeapTracer.Node;
    end;
  const
    NodeAlignment = PtrUint(@NodeAlignmentCheck(nil^).n); // In practice, sizeof(pointer).
  var
    i: SizeInt;
    index: SizeUint;
    xi: ^HeapTracer.ExtraInfoRec;
    this: ^HeapTracer;
  begin
    this := @ht;
    index := 0;
    if Assigned(fillProc) or Assigned(displayProc) then
    begin
      size := SizeInt(size + (NodeAlignment - 1)) and -NodeAlignment;
      for i := 0 to SizeInt(this^.nExtraInfos) - 1 do
      begin
        xi := @this^.extraInfos[i];
        if (xi^.fill = fillProc) and (xi^.display = displayProc) and (xi^.size = size) then
        begin
          index := 1 + i;
          break;
        end;
      end;
      if index = 0 then
      begin
        if this^.nExtraInfos >= length(this^.extraInfos) then RunError(203);
        index := 1 + this^.nExtraInfos;
        this^.nExtraInfos := index;
        xi := @this^.extraInfos[index - 1];
        xi^.size := size;
        xi^.fill := fillProc;
        xi^.display := displayProc;
      end;
    end;
    this^.headTailInfo := this^.headTailInfo and uint32(not (ht.ExtraInfoIndexMask shl ht.ExtraInfoIndexShift)) or index shl ht.ExtraInfoIndexShift;
  end;

  function GetTailSize: SizeUint;
  begin
    result := ht.HeadTailSizes[ht.headTailInfo shr ht.TailSizeIndexShift and ht.TailSizeIndexMask] * ht.TailSizeUnit;
  end;

  procedure SetTailSize(size: SizeUint);
  begin
    ht.headTailInfo := ht.headTailInfo
      and uint32(not (ht.TailSizeIndexMask shl ht.TailSizeIndexShift))
      or uint32(uint32(ht.HeadTailSizeToIndex(SizeUint(size + (ht.TailSizeUnit - 1)) div ht.TailSizeUnit)) shl ht.TailSizeIndexShift);
  end;

  function GetHeadSize: SizeUint;
  begin
    result := ht.HeadTailSizes[ht.headTailInfo shr ht.HeadSizeIndexShift and ht.HeadSizeIndexMask] * ht.HeadSizeUnit;
  end;

  procedure SetHeadSize(size: SizeUint);
  begin
    ht.headTailInfo := ht.headTailInfo
      and uint32(not (ht.HeadSizeIndexMask shl ht.HeadSizeIndexShift))
      or uint32(uint32(ht.HeadTailSizeToIndex(SizeUint(size + (ht.HeadSizeUnit - 1)) div ht.HeadSizeUnit)) shl ht.HeadSizeIndexShift);
  end;

  function GetKeepReleased: boolean;
  begin
    result := KeepReleasedBytes <> 0;
  end;

  procedure SetKeepReleased(keep: boolean);
  begin
    if keep then KeepReleasedBytes := High(SizeUint) else KeepReleasedBytes := 0;
  end;

  function GetTraceDepth: SizeUint;
  begin
    result := ht.traceDepth;
  end;

  procedure SetTraceDepth(depth: SizeUint);
  begin
    if depth > StackTrace.MaxItems then depth := StackTrace.MaxItems;
    ht.traceDepth := depth;
  end;

  procedure SetHeapTraceOutput(const name: string);
  begin
    ht.SetOutput(name);
  end;

  procedure SetHeapTraceOutput(var fout: text);
  begin
    ht.CloseOutfp;
    ht.outfp := @fout;
    ht.needCloseOutfp := true;
  end;

{$ifndef Unix}
  {$S-}
{$endif}

{$ifdef go32v2}
var
   __stklen : longword;external name '__stklen';
   __stkbottom : longword;external name '__stkbottom';
   ebss : longword; external name 'end';
{$endif go32v2}

{$ifdef linux}
var
   etext: ptruint; external name '_etext';
   eend : ptruint; external name '_end';
{$endif}

{$ifdef freebsd}
var
   text_start: ptruint; external name '__executable_start';
   etext: ptruint; external name '_etext';
   eend : ptruint; external name '_end';
{$endif}

{$ifdef os2}
(* Currently still EMX based - possibly to be changed in the future. *)
var
   etext: ptruint; external name '_etext';
   eend : ptruint; external name '_end';
{$endif}

{$ifdef windows}
var
   sdata : ptruint; external name '__data_start__';
   ebss : ptruint; external name '__bss_end__';
   TLSKey : PDWord; external name '_FPC_TlsKey';
   TLSSize : DWord; external name '_FPC_TlsSize';

function TlsGetValue(dwTlsIndex : DWord) : pointer;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TlsGetValue';
{$endif}

{$ifdef BEOS}
const
  B_ERROR = -1;

type
  area_id   = Longint;

function area_for(addr : Pointer) : area_id;
            cdecl; external 'root' name 'area_for';
{$endif BEOS}

procedure CheckPointer(p: pointer); {$ifndef debug_heaptrc} [public, alias : 'FPC_CHECKPOINTER']; {$endif}
var
  hp : HashList.ppNode;
  hleft : SizeUint;
  hn: HashList.pNode;
{$ifdef go32v2}
  get_ebp,stack_top : longword;
  bss_end : longword;
{$endif go32v2}
{$ifdef windows}
  datap : pointer;
{$endif windows}
begin
  if p=nil then
    runerror(204);

{$ifdef go32v2}
  if ptruint(p)<$1000 then
    runerror(216);
  asm
     movl %ebp,get_ebp
     leal ebss,%eax
     movl %eax,bss_end
  end;
  stack_top:=__stkbottom+__stklen;
  { allow all between start of code and end of bss }
  if ptruint(p)<=bss_end then
    exit;
  { stack can be above heap !! }

  if (ptruint(p)>=get_ebp) and (ptruint(p)<=stack_top) then
    exit;
{$endif go32v2}

  { I don't know where the stack is in other OS !! }
{$ifdef windows}
  { inside stack ? }
  if (ptruint(p)>ptruint(get_frame)) and
     (p<StackTop) then
    exit;
  { inside data, rdata ... bss }
  if (ptruint(p)>=ptruint(@sdata)) and (ptruint(p)<ptruint(@ebss)) then
    exit;
  { is program multi-threaded and p inside Threadvar range? }
  if TlsKey^<>dword(-1) then
    begin
      datap:=TlsGetValue(tlskey^);
      if ((ptruint(p)>=ptruint(datap)) and
          (ptruint(p)<ptruint(datap)+TlsSize)) then
        exit;
    end;
{$endif windows}

{$IFDEF OS2}
  { inside stack ? }
  if (PtrUInt (P) > PtrUInt (Get_Frame)) and
     (PtrUInt (P) < PtrUInt (StackTop)) then
    exit;
  { inside data or bss ? }
  if (PtrUInt (P) >= PtrUInt (@etext)) and (PtrUInt (P) < PtrUInt (@eend)) then
    exit;
{$ENDIF OS2}

{$ifdef linux}
  { inside stack ? }
  if (ptruint(p)>ptruint(get_frame)) and
     (ptruint(p)<ptruint(StackTop)) then
    exit;
  { inside data or bss ? }
  if (ptruint(p)>=ptruint(@etext)) and (ptruint(p)<ptruint(@eend)) then
    exit;
{$endif linux}

{$ifdef freebsd}
  { inside stack ? }
  if (ptruint(p)>ptruint(get_frame)) and
     (ptruint(p)<ptruint(StackTop)) then
    exit;
  { inside data or bss ? }
  if (ptruint(p)>=ptruint(@text_start)) and (ptruint(p)<ptruint(@eend)) then
    exit;
{$endif linux}
{$ifdef morphos}
  { inside stack ? }
  if (ptruint(p)<ptruint(StackTop)) and (ptruint(p)>ptruint(StackBottom)) then
    exit;
  { inside data or bss ? }
  {$WARNING data and bss checking missing }
{$endif morphos}

  {$ifdef darwin}
  {$warning No checkpointer support yet for Darwin}
  exit;
  {$endif}

{$ifdef BEOS}
  // if we find the address in a known area in our current process,
  // then it is a valid one
  if area_for(p) <> B_ERROR then
    exit;
{$endif BEOS}

  ht.Lock;
  // searching h for exactly p shouldn’t be worth it...
  hp:=ht.h.h;
  hleft:=1+ht.h.nhmask;
  repeat
    hn:=hp^;
    while Assigned(hn) and not HeapTracer.pNode(pointer(hn)-PtrUint(@HeapTracer.Node(nil^).hn))^.Contains(p) do
      hn:=hn^.next;
    if Assigned(hn) then break;
    inc(hp); dec(hleft);
  until hleft=0;
  ht.Unlock;
  if hleft<>0 then exit;
  ht.ReportBadPointer(p,[]);
  RunError(204);
end;

{$ifdef debug_heaptrc}
  procedure Dump(var f: text; what: DumpItems = [DumpItem.Ht, DumpItem.Traces]);
  var
    it: DumpItem;
    needLE: boolean;
  begin
    needLE := false;
    for it in what do
    begin
      if needLE then writeln(f);
      case it of
        DumpItem.Ht: ht.Dump(f);
        DumpItem.Traces: ht.traces.Dump(f);
      end;
      needLE := true;
    end;
  end;
{$endif}

type
  OptionsParser = record
    class function ScanIgnoringCase(s, patLower: pAnsiChar): pAnsiChar; static;
    class function ScanEq(s: pAnsiChar): pAnsiChar; static;
    class function ScanSizeUint(s: pAnsiChar; out v: SizeUint): pAnsiChar; static;
    class procedure ParseEnv(s: pAnsiChar); static;
  end;

  class function OptionsParser.ScanIgnoringCase(s, patLower: pAnsiChar): pAnsiChar;
  begin
    // “or ord(need_lowercase) shl 5” conditionally lowercases s2^ for comparison.
    result := s;
    while patLower^ <> #0 do
    begin
      if ord(s^) or ord(patLower^ in ['a' .. 'z']) shl 5 <> ord(patLower^) then exit;
      inc(s); inc(patLower);
    end;
    result := s;
  end;

  class function OptionsParser.ScanEq(s: pAnsiChar): pAnsiChar;
  begin
    result := s;
    while s^ = ' ' do inc(s);
    if s^ <> '=' then exit;
    inc(s);
    while s^ = ' ' do inc(s);
    result := s;
  end;

  class function OptionsParser.ScanSizeUint(s: pAnsiChar; out v: SizeUint): pAnsiChar;
  begin
    result := s;
    if not (s^ in ['0' .. '9']) then exit;
    v := 0;
    repeat
      if v > High(v) div 10 then exit;
      v := v * 10 + SizeUint(ord(s^) - ord('0'));
      inc(s);
    until not (s^ in ['0' .. '9']);
    result := s;
  end;

  class procedure OptionsParser.ParseEnv(s: pAnsiChar);
  type
    OptionEnum = (KeepReleased, Disabled, NoHalt, HaltOnNotReleased, SkipIfNoLeaks, TailSize, HeadSize, TraceDepth, Log);
  const
    OptionNames: array[OptionEnum] of pAnsiChar = (
      'keepreleased', 'disabled', 'nohalt', 'haltonnotreleased', 'skipifnoleaks', 'tail_size', 'head_size', 'tracedepth', 'log');
  var
    s2: pAnsiChar;
    v: SizeUint;
    opt: OptionEnum;
    outp: shortstring;
  begin
    repeat
      while not (s^ in [#0, 'A' .. 'Z', 'a' .. 'z']) do inc(s);
      if s^ = #0 then break;

      for opt in OptionEnum do
      begin
        s2 := ScanIgnoringCase(s, OptionNames[opt]);
        if s2 <> s then break;
      end;
      if s2 = s then
      begin
        inc(s); // unknown option, skip 1 character
        continue;
      end;
      s := s2;

      case opt of
        OptionEnum.KeepReleased, OptionEnum.TailSize, OptionEnum.HeadSize, OptionEnum.TraceDepth:
          begin
            s2 := ScanEq(s);
            if s2 <> s then
            begin
              s := ScanSizeUint(s2, v);
              if s <> s2 then
                case opt of
                  OptionEnum.KeepReleased: KeepReleasedBytes := v;
                  OptionEnum.TailSize: tail_size := v;
                  OptionEnum.HeadSize: head_size := v;
                  else {OptionEnum.TraceDepth} TraceDepth := v;
                end;
            end else
              if opt = OptionEnum.KeepReleased then // “keepreleased” without =.
                KeepReleasedBytes := High(SizeUint);
          end;
        OptionEnum.Disabled: UseHeapTrace := false;
        OptionEnum.NoHalt: HaltOnError := false;
        OptionEnum.HaltOnNotReleased: HaltOnNotReleased := true;
        OptionEnum.SkipIfNoLeaks: GlobalSkipIfNoLeaks := true;
        OptionEnum.Log:
          begin
            s2 := ScanEq(s);
            if s2 <> s then
            begin
              s := s2;
              while not (s^ in [' ', #0]) do inc(s);
              SetString(outp, s2, s - s2);
              SetHeapTraceOutput(outp);
            end;
          end;
      end;
    until false;
  end;

{$if defined(win32) or defined(win64)}
  function GetEnvironmentStringsA: pAnsiChar; stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
  function FreeEnvironmentStringsA(p: pAnsiChar): LongBool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';

  procedure LoadEnvironment;
  var
    envp, p, p2: pAnsiChar;
  begin
    envp := GetEnvironmentStringsA;
    p := envp;
    while p^ <> #0 do
    begin
      p2 := OptionsParser.ScanIgnoringCase(p, 'heaptrc=');
      if p2 <> p then
      begin
        OptionsParser.ParseEnv(p2);
        break;
      end;
      inc(p, IndexByte(p^, -1, 0) + 1);
    end;
    FreeEnvironmentStringsA(envp);
  end;

{ WinCE: does not have environment strings. Add some way to specify heaptrc options?
  msdos: don’t want to bother with far pointers and their forwarding to ScanIgnoringCase & ParseEnv... }
{$elseif not defined(wince) and not defined(sinclairql) and not defined(msdos) and not defined(msxdos)}
  procedure LoadEnvironment;
  var
    ep: ppAnsiChar;
    p: pAnsiChar;
  begin
    ep := envp;
    if not Assigned(ep) then exit;
    while ep^ <> nil do
    begin
      p := OptionsParser.ScanIgnoringCase(ep^, 'heaptrc=');
      if p <> ep^ then
      begin
        OptionsParser.ParseEnv(p);
        break;
      end;
      inc(ep);
    end;
  end;
{$endif LoadEnvironment}

{$ifdef FPC_HAS_FEATURE_THREADING}
  procedure InitializeAfterUnits;
  begin
    if Assigned(ht.oldInitProc) then TProcedure(ht.oldInitProc)();
    if ht.mgrInstalled then
    begin
      InitCriticalSection(ht.lck);
      ht.lockAlive := true;
    end;
  end;

  procedure FinalizeBeforeUnits;
  begin
    ExitProc := ht.oldExitProc;
    if ht.lockAlive then
    begin
      ht.lockAlive := false;
      DoneCriticalSection(ht.lck);
    end;
  end;
{$endif FPC_HAS_FEATURE_THREADING}

initialization
  ht.headTailInfo := 1 shl ht.TailSizeIndexShift; // 4-byte tails, no heads by default.
  ht.traceDepth := DefaultTraceDepth;
  ht.mainThreadStderr := @stderr;
  ht.outfp := ht.mainThreadStderr;

{$ifdef FPC_HAS_FEATURE_THREADING}
  ht.oldInitProc := InitProc;
  InitProc := @InitializeAfterUnits;
  ht.oldExitProc := ExitProc;
  ExitProc := @FinalizeBeforeUnits;
{$endif FPC_HAS_FEATURE_THREADING}
{$if declared(LoadEnvironment)}
  LoadEnvironment;
{$endif LoadEnvironment}
  if UseHeapTrace then ht.Install;

finalization
  if ht.mgrInstalled then
  begin
    ht.FreeToFreeItems(0, []);
    if UseHeapTrace then
    begin
      IOResult;
      if (ExitCode <> 0) and (ErrorAddr <> nil) then
        writeln(ht.outfp^,
          'No heap dump by heaptrc unit', LineEnding,
          'Exitcode = ', ExitCode)
      else
      begin
        CheckHeap; // Note ht.ReportCorrupted makes a slight difference between CheckHeap and ht.CheckHeap when forming a message.
        ht.Report(ht.outfp^, GlobalSkipIfNoLeaks);
      end;
    end;
    if (ExitCode = 0) and
      (
        ht.errorInHeap or
        HaltOnNotReleased and ((ht.getMemCount <> ht.freeMemCount) or (ht.getMemSize <> ht.freeMemSize) or (ht.h.nItems <> 0))
      )
    then
      ExitCode := 203;
  end;
  ht.CloseOutfp;
  ht.Uninstall;
end.
