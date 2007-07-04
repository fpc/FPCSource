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

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
{ for netware all allocated blocks are saved to free them at }
{ exit (to avoid message "Module did not release xx resources") }
Function Sbrk(size : longint):pointer;
var P2 : POINTER;
    i  : longint;
begin
  Sbrk := _malloc (size);
  if Sbrk <> nil then begin
    if HeapSbrkBlockList = nil then
    begin
      Pointer (HeapSbrkBlockList) := _malloc (sizeof (HeapSbrkBlockList^));
      if HeapSbrkBlockList = nil then
      begin
        _free (Sbrk);
        Sbrk := nil;
        exit;
      end;
      fillchar (HeapSbrkBlockList^,sizeof(HeapSbrkBlockList^),0);
      HeapSbrkAllocated := HeapInitialMaxBlocks;
    end;
    if (HeapSbrkLastUsed > 0) then
      for i := 1 to HeapSbrkLastUsed do
        if (HeapSbrkBlockList^[i] = nil) then
        begin  // reuse free slot
          HeapSbrkBlockList^[i] := Sbrk;
          exit;
        end;
    if (HeapSbrkLastUsed = HeapSbrkAllocated) then
    begin  { grow }
      p2 := _realloc (HeapSbrkBlockList, (HeapSbrkAllocated + HeapInitialMaxBlocks) * sizeof(pointer));
      if p2 = nil then  // should we better terminate with error ?
      begin
        _free (Sbrk);
         Sbrk := nil;
         exit;
      end;
      HeapSbrkBlockList := p2;
      inc (HeapSbrkAllocated, HeapInitialMaxBlocks);
    end;
    inc (HeapSbrkLastUsed);
    HeapSbrkBlockList^[HeapSbrkLastUsed] := Sbrk;
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
end;

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

function SysOSAlloc(size: ptruint): pointer;
begin
  result := sbrk(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
var i : longint;
begin
//fpmunmap(p, size);
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

procedure SysOSFree(p: pointer; size: ptruint);
begin
  _free (p);
end;

function SysOSAlloc(size: ptruint): pointer;
begin
  SysOSAlloc := _malloc (size);
end;

{$endif autoHeapRelease}




