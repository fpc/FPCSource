{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team.

    Heap manager for the FPC embedded target

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
Unit heapmgr;

interface

implementation

  var
    Memorymanager: TMemoryManager;external name 'FPC_SYSTEM_MEMORYMANAGER';

  Procedure HandleError (Errno : longint);external name 'FPC_HANDLEERROR';

  {*****************************************************************************
        OS Memory allocation / deallocation
   ****************************************************************************}
  function SysOSAlloc(size: ptruint): pointer;
  begin
    result:=nil; // pointer($02000000);
  end;


  procedure SysOSFree(p: pointer; size: ptruint);
  begin
  end;

  {$define FPC_IN_HEAPMGR}
  {$i heap.inc}

  const
    MyMemoryManager: TMemoryManager = (
      NeedLock: false;  // Obsolete
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


initialization
  SetMemoryManager(MyMemoryManager);
  InitHeap;
finalization
  FinalizeHeap;
end.

