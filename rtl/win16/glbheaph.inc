{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by the Free Pascal development team

    This file contains the interface section of the heap
    management implementation for 16-bit Windows that uses
    the Windows global heap.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    var
      { BP7 compatible vars }
      HeapList: Word=0;
      HeapLimit: Word=1024;
      HeapBlock: Word=8192;
      HeapAllocFlags: Word=2;  { 2=GMEM_MOVEABLE }

    function MemAvail: LongInt;
    function MaxAvail: LongInt;
