{
    Basic UEFI definitions

    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Olivier Coursière

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  SysHeap: THandle = 0;

procedure PrepareSysHeap;
begin
end;

// default memory manager

{$T-}
function SysOSAlloc(size: ptruint): pointer;
var
  s : unicodestring;
  addresse : UINT64;
  status : EFI_STATUS;
  allocatedPointer : pointer;
begin
  if SysHeap = 0 then
    PrepareSysHeap;
  SysOSAlloc := Pointer(0);
  
  status := SysTable.BootServices^.AllocatePool(EfiLoaderData, size,
  	allocatedPointer);
  	
  case status of
  	EFI_SUCCESS : sysTable.ConOut^.OutputString(sysTable.ConOut, 'Success in SysOSAlloc...' + #13#10);
  	EFI_OUT_OF_RESOURCES : sysTable.ConOut^.OutputString(sysTable.ConOut, 'OutOfResources' + #13#10);
  	EFI_INVALID_PARAMETER : sysTable.ConOut^.OutputString(sysTable.ConOut, 'InvalidParameter' + #13#10);
  	EFI_NOT_FOUND : sysTable.ConOut^.OutputString(sysTable.ConOut, 'NotFound' + #13#10);  	
  	else
  	  sysTable.ConOut^.OutputString(sysTable.ConOut, 'Another error' + #13#10);
  end;
  SysOSAlloc := allocatedPointer;
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin
  sysTable.ConOut^.OutputString(sysTable.ConOut, '... in sysOSFree...' + #13#10);
  // if heap isn't set, then nothing was allocated
//  if SysHeap &lt;&gt; 0 then
//    RtlFreeHeap(SysHeap, 0, p);
  //WriteLn('SysOSFree');
end;
