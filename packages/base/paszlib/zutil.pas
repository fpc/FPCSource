unit zutil;

{
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}


const
  {$IFDEF MAXSEG_64K}
  MaxMemBlock = $FFFF;
  {$ELSE}
  MaxMemBlock = MaxInt;
  {$ENDIF}

type
  zByteArray = array[0..(MaxMemBlock div SizeOf(byte))-1] of byte;
  pzByteArray = ^zByteArray;
type
  zIntfArray = array[0..(MaxMemBlock div SizeOf(byte))-1] of integer;
  pzIntfArray = ^zIntfArray;
type
  zuIntArray = array[0..(MaxMemBlock div SizeOf(cardinal))-1] of cardinal;
  PuIntArray = ^zuIntArray;

type
  zuchfArray = zByteArray;
  puchfArray = ^zuchfArray;
type
  zushfArray = array[0..(MaxMemBlock div SizeOf(word))-1] of word;
  pushfArray = ^zushfArray;

procedure zcfree(opaque : pointer; ptr : pointer);
function zcalloc (opaque : pointer; items : cardinal; size : cardinal) : pointer;

implementation

type
  LH = record
    L, H : word;
  end;


procedure zcfree(opaque : pointer; ptr : pointer);

begin
  FreeMem(ptr);
end;

function zcalloc (opaque : pointer; items : cardinal; size : cardinal) : pointer;
var
  p : pointer;
  memsize : cardinal;
begin
  memsize := items * size;
  getmem(p, memsize);
  zcalloc := p;
end;

end.
