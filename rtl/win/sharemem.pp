{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team.

    Shared memory manager

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sharemem;

  interface

  implementation

    const
      fpcmemdll = 'fpcmemdll.dll';

    function  SysGetmem(Size:ptrint):Pointer;external fpcmemdll;
    function  SysFreemem(p:pointer):ptrint;external fpcmemdll;
    function  SysFreememSize(p:pointer;Size:ptrint):ptrint;external fpcmemdll;
    function  SysAllocMem(size:ptrint):Pointer;external fpcmemdll;
    function  SysReAllocMem(var p:pointer;size:ptrint):Pointer;external fpcmemdll;
    function  SysMemSize(p:pointer):ptrint;external fpcmemdll;
    function  SysGetHeapStatus:THeapStatus;external fpcmemdll;
    function  SysGetFPCHeapStatus:TFPCHeapStatus;external fpcmemdll;

    var
      MemoryManager: TMemoryManager = (
      NeedLock: true;
      GetMem: nil;
      FreeMem: nil;
      FreeMemSize: nil;
      AllocMem: nil;
      ReAllocMem: nil;
      MemSize: nil;
      GetHeapStatus: nil;
      GetFPCHeapStatus: nil;
    );

begin
  with MemoryManager do
    begin
      GetMem:=@SysGetmem;
      FreeMem:=@SysFreeMem;
      FreeMemSize:=@SysFreeMemSize;
      AllocMem:=@SysAllocMem;
      ReAllocMem:=@SysReAllocMem;
      MemSize:=@SysMemSize;
      GetHeapStatus:=@SysGetHeapStatus;
      GetFPCHeapStatus:=@SysGetFPCHeapStatus;
    end;
  SetMemoryManager(MemoryManager);
end.
