{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by the Free Pascal development team.

    Tiny heap manager for the i8086 near heap, embedded targets, etc.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef cpui8086}
  {$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    {$define FPC_TINYHEAP_HUGE}
  {$endif}
{$endif cpui8086}

    var
      { these vars are TP7-compatible }
      HeapOrg: Pointer = nil;  { start of heap }
      HeapEnd: Pointer = nil;  { end of heap }
      FreeList: Pointer = nil; { pointer to the first free block }
      HeapPtr: Pointer = nil;  { pointer to the last free block }

    function MemAvail: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif};
    function MaxAvail: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif};

