{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level memory functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  PBlock = ^TBlock;
  TBlock = record
    Prev, Next : PBlock;
    Ptr        : Pointer;
    Size       : SizeUInt;
  end;

var
  _heapEnd  : PtrUInt;
  _heapLimit: PtrUInt = $80200000;  // TODO: add a way to change this

procedure _InitHeap(theBSSend:PtrUInt);
begin
  _heapEnd:= theBSSend;
end;

procedure _Free(Ptr: Pointer); forward;

function sbrk(incr: PtrInt): Pointer;
var
  currentEnd, newEnd: PtrUInt;
begin
  currentEnd := _heapEnd;
  newEnd     := Align(currentEnd + PtrUInt(incr), 8);

  if newEnd >= _heapLimit then
    Exit(nil);

  _heapEnd := newEnd;
  Result := Pointer(currentEnd);
end;


procedure _updateHeapUsage(incr: PtrInt); inline;
begin
  // implement statistics if needed
end;


// Internal state
var
  _mallocStart: Pointer = nil;
  _mallocHead : PBlock  = nil;
  _mallocTail : PBlock  = nil;

function _findBlock(head: PBlock; size: SizeUInt): PBlock;
var
  prev: PBlock;
  nextBot: PtrUInt;
begin
  prev := head;
  while prev <> nil do
  begin
    if prev^.Next <> nil then
    begin
      nextBot := PtrUInt(prev^.Next);
      nextBot := nextBot - (PtrUInt(prev^.Ptr) + prev^.Size);
      if nextBot >= size then
      begin
        Result := prev;
        Exit;
      end;
    end;
    prev := prev^.Next;
  end;
  Result := prev;
end;

function _Malloc(Size: SizeUInt): Pointer;
var
  _size : SizeUInt;
  newBlk, prev: PBlock;
  ptr   : Pointer;
begin
  if Size = 0 then Exit(nil);

  _size := align(Size + SizeOf(TBlock), 8);

  if _mallocHead = nil then
  begin
    if _mallocStart = nil then
      _mallocStart := sbrk(0);

    newBlk := PBlock(sbrk(_size));
    if newBlk = nil then Exit(nil);

    ptr := Pointer(PtrUInt(newBlk) + SizeOf(TBlock));
    newBlk^.Ptr  := ptr;
    newBlk^.Size := _size - SizeOf(TBlock);
    newBlk^.Prev := nil;
    newBlk^.Next := nil;

    _mallocHead := newBlk;
    _mallocTail := newBlk;

    _updateHeapUsage(Size);
    Exit(ptr);
  end;

  if (PtrUInt(_mallocStart) + _size) < PtrUInt(_mallocHead) then
  begin
    newBlk := PBlock(_mallocStart);
    ptr := Pointer(PtrUInt(newBlk) + SizeOf(TBlock));
    newBlk^.Ptr  := ptr;
    newBlk^.Size := _size - SizeOf(TBlock);
    newBlk^.Prev := nil;
    newBlk^.Next := _mallocHead;
    _mallocHead^.Prev := newBlk;
    _mallocHead       := newBlk;

    _updateHeapUsage(Size);
    Exit(ptr);
  end;

  prev := _findBlock(_mallocHead, _size);
  if prev <> nil then
  begin
    newBlk := PBlock(PtrUInt(prev^.Ptr) + prev^.Size);
    ptr := Pointer(PtrUInt(newBlk) + SizeOf(TBlock));
    newBlk^.Ptr  := ptr;
    newBlk^.Size := _size - SizeOf(TBlock);
    newBlk^.Prev := prev;
    newBlk^.Next := prev^.Next;
    if newBlk^.Next <> nil then
      newBlk^.Next^.Prev := newBlk;
    prev^.Next := newBlk;

    _updateHeapUsage(Size);
    Exit(ptr);
  end;

  newBlk := PBlock(sbrk(_size));
  if newBlk = nil then Exit(nil);

  ptr := Pointer(PtrUInt(newBlk) + SizeOf(TBlock));
  newBlk^.Ptr  := ptr;
  newBlk^.Size := _size - SizeOf(TBlock);
  newBlk^.Prev := _mallocTail;
  newBlk^.Next := nil;
  _mallocTail^.Next := newBlk;
  _mallocTail := newBlk;

  _updateHeapUsage(Size);
  Result := ptr;
end;

function _Calloc(Num, Size: SizeUInt): Pointer;
begin
  Result := _Malloc(Num * Size);
  if Result <> nil then
    FillChar(Result^, Num * Size, 0);
end;

function _Realloc(Ptr: Pointer; Size: SizeUInt): Pointer;
var
  _size : SizeUInt;
  prev  : PBlock;
  newPtr: Pointer;
begin
  if Size = 0 then
  begin
    _Free(Ptr);
    Exit(nil);
  end;
  if Ptr = nil then
    Exit(_Malloc(Size));

  _size := align(Size + SizeOf(TBlock), 8);
  prev  := PBlock(PtrUInt(Ptr) - SizeOf(TBlock));

  if prev^.Size >= _size then
  begin
    _updateHeapUsage(Size - prev^.Size);
    prev^.Size := _size;
    if prev^.Next = nil then
      sbrk(PtrInt(PtrUInt(Ptr) - PtrUInt(sbrk(0)) + _size));
    Exit(Ptr);
  end;

  if prev^.Next = nil then
  begin
    newPtr := sbrk(_size - prev^.Size);
    if newPtr = nil then Exit(nil);
    _updateHeapUsage(Size - prev^.Size);
    prev^.Size := _size;
    Exit(Ptr);
  end;

  if (PtrUInt(prev^.Next^.Ptr) - PtrUInt(Ptr)) > _size then
  begin
    _updateHeapUsage(Size - prev^.Size);
    prev^.Size := _size;
    Exit(Ptr);
  end;

  newPtr := _Malloc(Size);
  if newPtr = nil then Exit(nil);
  Move(Ptr^, newPtr^, prev^.Size);
  _Free(Ptr);
  Result := newPtr;
end;


procedure _Free(Ptr: Pointer);
var
  size: SizeUInt;
  cur : PBlock;
  top : Pointer;
begin
  if (Ptr = nil) or (_mallocHead = nil) then Exit;

  if Ptr = _mallocHead^.Ptr then
  begin
    size := _mallocHead^.Size;
    Inc(size, PtrUInt(_mallocHead^.Ptr) - PtrUInt(_mallocHead));
    _mallocHead := _mallocHead^.Next;

    if _mallocHead <> nil then
      _mallocHead^.Prev := nil
    else
    begin
      _mallocTail := nil;
      sbrk(-PtrInt(size));
    end;

    _updateHeapUsage(-PtrInt(size));
    Exit;
  end;

  cur := _mallocHead;
  while (cur <> nil) and (Ptr <> cur^.Ptr) do
    cur := cur^.Next;
  if cur = nil then Exit;

  if cur^.Next <> nil then
    cur^.Next^.Prev := cur^.Prev
  else
  begin
    top := sbrk(0);
    size := PtrUInt(top) - PtrUInt(cur^.Prev^.Ptr) - cur^.Prev^.Size;
    _mallocTail := cur^.Prev;
    sbrk(-PtrInt(size));
  end;

  _updateHeapUsage(-PtrInt(cur^.Size));
  cur^.Prev^.Next := cur^.Next;
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

function SysOSAlloc (size: ptruint): pointer;
begin
  SysOSAlloc:=_malloc(size);
end;

procedure SysOSFree(p: pointer; size: ptruint);
begin
  _free(p);
end;
