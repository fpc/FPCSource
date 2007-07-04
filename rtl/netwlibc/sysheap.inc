{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                              Heap Management
*****************************************************************************}

{$ifdef autoHeapRelease}

const HeapInitialMaxBlocks = 32;
type THeapSbrkBlockList = array [1.. HeapInitialMaxBlocks] of pointer;
var  HeapSbrkBlockList : ^THeapSbrkBlockList = nil;
     HeapSbrkLastUsed  : dword = 0;
     HeapSbrkAllocated : dword = 0;
     HeapSbrkReleased : boolean = false;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
{ for netware all allocated blocks are saved to free them at }
{ exit (to avoid message "Module did not release xx resources") }
Function SysOSAlloc(size : ptruint):pointer;
var P2 : POINTER;
    i  : longint;
    Slept : longint;
begin
  if HeapSbrkReleased then
  begin
    _ConsolePrintf ('Error: SysOSFree called after all heap memory was released'#13#10);
    exit(nil);
  end;
  SysOSAlloc := _Alloc (size,HeapAllocResourceTag);
  if SysOSAlloc <> nil then begin
    if HeapSbrkBlockList = nil then
    begin
      Pointer (HeapSbrkBlockList) := _Alloc (sizeof (HeapSbrkBlockList^),HeapListAllocResourceTag);
      if HeapSbrkBlockList = nil then
      begin
        _free (SysOSAlloc);
        SysOSAlloc := nil;
        exit;
      end;
      fillchar (HeapSbrkBlockList^,sizeof(HeapSbrkBlockList^),0);
      HeapSbrkAllocated := HeapInitialMaxBlocks;
    end;
    if (HeapSbrkLastUsed > 0) then
      for i := 1 to HeapSbrkLastUsed do
        if (HeapSbrkBlockList^[i] = nil) then
        begin  // reuse free slot
          HeapSbrkBlockList^[i] := SysOSAlloc;
          exit;
        end;
    if (HeapSbrkLastUsed = HeapSbrkAllocated) then
    begin  { grow }
      slept := 0;
      p2 := _ReallocSleepOK (HeapSbrkBlockList, (HeapSbrkAllocated + HeapInitialMaxBlocks) * sizeof(pointer),HeapListAllocResourceTag,Slept);
      if p2 = nil then  // should we better terminate with error ?
      begin
        _free (SysOSAlloc);
        SysOSAlloc := nil;
        exit;
      end;
      HeapSbrkBlockList := p2;
      inc (HeapSbrkAllocated, HeapInitialMaxBlocks);
    end;
    inc (HeapSbrkLastUsed);
    HeapSbrkBlockList^[HeapSbrkLastUsed] := SysOSAlloc;
  end;
end;


procedure FreeSbrkMem;
var i : longint;
begin
  if HeapSbrkBlockList <> nil then
  begin
    for i := 1 to HeapSbrkLastUsed do
      if (HeapSbrkBlockList^[i] <> nil) then
        _free (HeapSbrkBlockList^[i]);
    _free (HeapSbrkBlockList);
    HeapSbrkAllocated := 0;
    HeapSbrkLastUsed := 0;
    HeapSbrkBlockList := nil;
  end;
  HeapSbrkReleased := true;
  {ReturnResourceTag(HeapAllocResourceTag,1);
  ReturnResourceTag(HeapListAllocResourceTag,1);  not in netware.imp, seems to be not needed}
end;

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
var i : longint;
begin
  if HeapSbrkReleased then
  begin
    _ConsolePrintf ('Error: SysOSFree called after all heap memory was released'#13#10);
  end else
  if (HeapSbrkLastUsed > 0) then
    for i := 1 to HeapSbrkLastUsed do
      if (HeapSbrkBlockList^[i] = p) then
      begin
        _free (p);
        HeapSbrkBlockList^[i] := nil;
        exit;
      end;
  HandleError (204);  // invalid pointer operation
end;

{$else autoHeapRelease}

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  _free (p);
end;

function SysOSAlloc(size: ptruint): pointer;
begin
  SysOSAlloc := _Alloc(size,HeapAllocResourceTag);
end;

{$endif autoHeapRelease}




