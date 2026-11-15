{ %OPT=-O4 }
{ tests forwarding of loaded/stored values to later loads of the same
  reference by the x86 peephole optimizer }
program tloadfwd1;

{$mode objfpc}

type
  TRec = record
    a, b: longint;
    w: word;
    c: byte;
  end;
  PRec = ^TRec;

  TOverlap = packed record
    case byte of
      0: (w: word);
      1: (lo, hi: byte);
  end;
  POverlap = ^TOverlap;

var
  r1, r2: TRec;
  g1, g2: longint;

{ the second load of p^.a must not be replaced, q may point to it }
function AliasStore(p: PRec; q: plongint): longint; noinline;
var
  x: longint;
begin
  x := p^.a;
  q^ := x + 5;
  Result := x + p^.a;
end;

{ the second load can use the register of the first load }
function Reload(p: PRec): longint; noinline;
begin
  if p^.a > 10 then
    Result := p^.a * 3 + p^.b
  else
    Result := p^.a + p^.b;
end;

{ a store to a different field in between is allowed }
function StoreOther(p: PRec; v: longint): longint; noinline;
var
  x: longint;
begin
  x := p^.a;
  p^.b := v;
  Result := x + p^.a + p^.b;
end;

{ a store to an overlapping location in between must stop the search }
function Overlap(p: POverlap; v: byte): longint; noinline;
var
  x: word;
begin
  x := p^.w;
  p^.hi := v;
  Result := x + p^.w;
end;

{ zero/sign extensions of a stored byte }
function ExtStore(p: PRec; v: byte): longint; noinline;
begin
  p^.c := v;
  Result := longint(p^.c) + shortint(p^.c);
end;

{ a store to another global in between is allowed }
function StoreOtherGlobal(v: longint): longint; noinline;
var
  x: longint;
begin
  x := g1;
  g2 := v;
  Result := x + g1;
end;

{ a store through a pointer in between must stop the search, p may point to g1 }
function StoreGlobalAlias(p: plongint; v: longint): longint; noinline;
var
  x: longint;
begin
  x := g1;
  p^ := v;
  Result := x + g1;
end;

begin
  r1.a := 1;
  r1.b := 2;
  if AliasStore(@r1, @r1.a) <> 7 then
    halt(1);
  if r1.a <> 6 then
    halt(2);
  r1.a := 3;
  if AliasStore(@r1, @r2.a) <> 6 then
    halt(3);
  if r2.a <> 8 then
    halt(4);

  r1.a := 20;
  r1.b := 4;
  if Reload(@r1) <> 64 then
    halt(5);
  r1.a := 5;
  if Reload(@r1) <> 9 then
    halt(6);

  r1.a := 7;
  if StoreOther(@r1, 100) <> 114 then
    halt(7);

  { use the record fields of r2 as storage for the overlap test }
  POverlap(@r2.w)^.w := $1234;
  if Overlap(POverlap(@r2.w), $56) <> $1234 + $5634 then
    halt(8);

  if ExtStore(@r1, 200) <> 200 - 56 then
    halt(9);

  g1 := 10;
  if StoreOtherGlobal(3) <> 20 then
    halt(10);
  if g2 <> 3 then
    halt(11);
  if StoreGlobalAlias(@g1, 5) <> 15 then
    halt(12);

  writeln('ok');
end.
