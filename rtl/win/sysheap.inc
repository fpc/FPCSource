{
    Basic heap handling for windows platforms

    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

const
  { Exact empirical point at which HEAP_ZERO_MEMORY becomes free. Tested on Windows XP 32-bit and Windows 10 64-bit,
    and is, surprisingly, documented:

    https://learn.microsoft.com/en-us/windows/win32/api/heapapi/nf-heapapi-heapcreate
    > The largest memory block that can be allocated from the heap is slightly less than 512 KB for a 32-bit process
    > and slightly less than 1,024 KB for a 64-bit process.
    > Requests to allocate memory blocks larger than the limit ... [use] VirtualAlloc.

    VirtualAlloc is however explicitly used instead of HeapAlloc(HEAP_ZERO_MEMORY) starting from this threshold, to:

    — make use of VirtualAlloc2 if possible,

    — avoid the HeapAlloc overhead. See:
      https://illmatics.com/Windows 8 Heap Internals.pdf
      https://preshing.com/20110717/the-windows-heap-is-slow-when-launched-from-the-debugger/ }
  SysOSAllocZeroesIfAtLeast = 128 * 1024 * sizeof(pointer) - 4096 - 7; { not like - 4096 - 7 are important: SysOSAlloc gets a multiple of 64 Kb. }

{$ifndef wince}
var
  { nil at start, CodePointer(1) if not found (to not attempt every time). }
  VirtualAlloc2: function(Process: THandle; BaseAddress: pointer; Size: SizeUint; AllocationType, PageProtection: uint32;
    ExtendedParameters: pointer; ParameterCount: uint32): pointer; winapi;

const
  MEM_64K_PAGES = $20400000;

procedure DetectVirtualAlloc2;
var
  kb: THandle;
begin
  kb := GetModuleHandle('KernelBase');
  if kb <> 0 then
    CodePointer(VirtualAlloc2) := WinGetProcAddress(kb, 'VirtualAlloc2');
  if not Assigned(VirtualAlloc2) then
    CodePointer(VirtualAlloc2) := CodePointer(1); { Failed. }
end;
{$endif}

function SysOSAlloc(size: ptruint): pointer;
begin
  if size >= SysOSAllocZeroesIfAtLeast then
  begin
  {$ifndef wince}
    result := nil;
    if not Assigned(VirtualAlloc2) then DetectVirtualAlloc2;
    if CodePointer(VirtualAlloc2) <> CodePointer(1) then
      result := VirtualAlloc2(GetCurrentProcess, nil, (size + 65535) and SizeUint(-65536), MEM_RESERVE or MEM_COMMIT or MEM_64K_PAGES, PAGE_READWRITE, nil, 0);
    if not Assigned(result) then
  {$endif}
      result := VirtualAlloc(nil, size, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
  end else
    result := HeapAlloc(GetProcessHeap, 0, size);
end;

procedure FreeSegmented(p: pointer; oldSize, newSize: SizeUint);
var
  mi: MEMORY_BASIC_INFORMATION;
begin
  repeat
    if VirtualQuery(p + oldSize - 1, mi, sizeof(mi)) <> sizeof(mi) then
      RunError(204);
    if mi.AllocationBase < p + newSize then
      break;
    if not VirtualFree(mi.AllocationBase, 0, MEM_RELEASE) then
      RunError(204);
    oldSize := mi.AllocationBase - p;
  until oldSize <= newSize; { “<” shouldn’t happen... }
end;

{$define HAS_SYSOSFREE}
procedure SysOSFree(p: pointer; size: ptruint);
begin
  if size < SysOSAllocZeroesIfAtLeast then
    HeapFree(GetProcessHeap, 0, p)
  else
    { SysOSRealloc can make the allocation segmented.
      Ideally, this case requires explicit support from the memory manager to avoid FreeSegmented in favor of a single VirtualFree. }
    FreeSegmented(p, size, 0);
end;

{$define FPC_SYSTEM_HAS_SYSOSREALLOC}
function SysOSRealloc(p: pointer; oldsize, newsize: ptruint): pointer;
var
  mi: MEMORY_BASIC_INFORMATION;
  allocatedEnd: pointer;
  allocatedSize, moreToAllocate: SizeUint;
begin
  if (oldsize < SysOSAllocZeroesIfAtLeast) and (newsize < SysOSAllocZeroesIfAtLeast) then
    exit(HeapReAlloc(GetProcessHeap, 0, p, newsize));
  result := nil;
  if (oldsize < SysOSAllocZeroesIfAtLeast) or (newsize < SysOSAllocZeroesIfAtLeast) then
    exit; { We distinguish between VirtualAlloc and HeapAlloc allocations using only their sizes. }
  if newsize > oldsize then
  begin
    { Grow by trying to allocate adjacently. }
    if VirtualQuery(p + oldsize - 1, mi, sizeof(mi)) <> sizeof(mi) then
      RunError(204);
    allocatedEnd := mi.BaseAddress + mi.RegionSize;
    allocatedSize := allocatedEnd - p;
    if allocatedSize >= newsize then
      { Already accomodates the new size. }
      exit(p);
    moreToAllocate := newsize - allocatedSize;
    if PtrUint(allocatedEnd) and 65535 = 0 then { Allocation granularity is 64K even for 4K pages, so this check can be shared between 64K and 4K branches. }
    begin
    {$ifndef wince}
      if CodePointer(VirtualAlloc2) > CodePointer(1) then
      begin
        if Assigned(VirtualAlloc2(GetCurrentProcess, allocatedEnd, (moreToAllocate + 65535) and SizeUint(-65536), MEM_RESERVE or MEM_COMMIT or MEM_64K_PAGES, PAGE_READWRITE, nil, 0)) then
          exit(p);
      end else
    {$endif}
        if Assigned(VirtualAlloc(allocatedEnd, moreToAllocate, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE)) then
          exit(p);
    end;
  end else
  begin
    if newsize < oldsize then { Support shrinking the allocation (FastMM does not bother). }
      FreeSegmented(p, oldsize, newsize);
    result := p;
  end;
end;
