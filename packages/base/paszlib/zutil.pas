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

function zmemcmp(s1p, s2p : Pbyte; len : cardinal) : integer;
procedure zcfree(opaque : pointer; ptr : pointer);
function zcalloc (opaque : pointer; items : cardinal; size : cardinal) : pointer;

implementation

type
  LH = record
    L, H : word;
  end;


function zmemcmp(s1p, s2p : Pbyte; len : cardinal) : integer;
var
  j : cardinal;
  source,
  dest : Pbyte;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

procedure zcfree(opaque : pointer; ptr : pointer);

var
  memsize : cardinal;

begin
  dec(Pcardinal(ptr));
  memsize := Pcardinal(ptr)^;
  FreeMem(ptr, memsize+SizeOf(cardinal));
end;

function zcalloc (opaque : pointer; items : cardinal; size : cardinal) : pointer;
var
  p : pointer;
  memsize : cardinal;
begin
  memsize := items * size;
  getmem(p, memsize+sizeOf(cardinal));
  Pcardinal(p)^:= memsize;
  inc(Pcardinal(p));
  zcalloc := p;
end;

end.
