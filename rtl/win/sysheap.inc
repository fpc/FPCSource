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

    If the value is too small, sometimes the following sad things will happen:
      In huge AllocMem: HeapAlloc will zero manually, and RTL will redundantly zero manually on top of that.
      In other cases: SysOSAlloc will zero manually without actual need.

    If the value is too large, sometimes the following sad thing will happen with huge AllocMem:
      HeapAlloc will internally use VirtualAlloc, then RTL will zero manually on top of that,
      immediately faulting all pages into memory and requesting their physical zeroing.

    Alternatively VirtualAlloc could be explicitly used past the arbitrary threshold. }
  SysOSAllocZeroesIfAtLeast = 128 * 1024 * sizeof(pointer) - 4096 - 7; { not like - 4096 - 7 are important: SysOSAlloc gets a multiple of 64 Kb. }

function SysOSAlloc(size: ptruint): pointer;
begin
  SysOSAlloc := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY * ord(size >= SysOSAllocZeroesIfAtLeast), size);
end;

{$define HAS_SYSOSFREE}
procedure SysOSFree(p: pointer; size: ptruint);
begin
  HeapFree(GetProcessHeap, 0, p);
end;

{$define FPC_SYSTEM_HAS_SYSOSREALLOC}
function SysOSRealloc(p: pointer;oldsize,newsize: ptruint): pointer;
begin
  result:=HeapReAlloc(GetProcessHeap, 0, p, newsize);
end;
