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
{$mode objfpc}
library fpcmemdll;

    Function  SysGetmem(Size:ptrint):Pointer;
      begin
        Result:=System.SysGetmem(Size);
      end;


    Function  SysFreemem(p:pointer):ptrint;
      begin
        Result:=System.SysFreemem(p);
      end;


    Function  SysFreememSize(p:pointer;Size:ptrint):ptrint;
      begin
        Result:=System.SysFreememSize(p,Size);
      end;


    Function  SysAllocMem(size:ptrint):Pointer;
      begin
        Result:=System.SysAllocMem(Size);
      end;


    Function  SysReAllocMem(var p:pointer;size:ptrint):Pointer;
      begin
        Result:=System.SysReallocMem(p,Size);
      end;


    Function  SysMemSize(p:pointer):ptrint;
      begin
        Result:=System.SysMemSize(p);
      end;


    function  SysGetHeapStatus:THeapStatus;
      begin
        Result:=System.SysGetHeapStatus;
      end;


    function  SysGetFPCHeapStatus:TFPCHeapStatus;
      begin
        Result:=System.SysGetFPCHeapStatus;
      end;


    exports
      SysGetmem,
      SysFreemem,
      SysFreememSize,
      SysAllocMem,
      SysReAllocMem,
      SysMemSize,
      SysGetHeapStatus,
      SysGetFPCHeapStatus;

begin
end.
