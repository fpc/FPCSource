program fannkuch;
{ The Computer Language Shootout
  http://shootout.alioth.debian.org/

  contributed by Florian Klaempfl
  modified by Micha Nelissen
  modified by Vincent Snijders
  modified by Steve Fisher

  Compile with
  fpc -O3 fannkuch.pp
}

{$INLINE ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

type
    TIntegerArray = Array[0..99] of longint;

var
   permu, permu_copy, count: TIntegerArray;
   r, n, answer : longint;

procedure swap(var a, b: longint); inline;
var  tmp: longint;
begin  tmp := a;  a := b;  b := tmp   end;

procedure reverse( k: longint); inline;
var
  pi, pj : pLongint;
begin
  pi := @permu_copy[1];
  pj := @permu_copy[k-1];
  while pi<pj do
  begin
    swap(pi^, pj^);
    inc(pi);
    dec(pj);
  end;
end;

function NextPermutation: boolean;
var
  r0: longint;
  tmp: LongInt;
  i : longint;
begin
  r0 := r; // use local variable
  NextPermutation := true;
  repeat
    if r0 = n then
    begin
      NextPermutation := false;
      break;
    end;
    tmp := permu[0];
    for i := 1 to r0 do
      permu[i-1] := permu[i];
    permu[r0] := tmp;

    dec(count[r0]);
    if count[r0] > 0 then
      break;
    inc(r0);
  until false;
  r := r0;
end;

function countflips: integer; inline;
var
  last: LongInt;
  tmp: LongInt;
begin
  countflips := 0;
  last := permu_copy[0];
  repeat
    // Reverse part of the array.
    reverse(last);
    tmp := permu_copy[ last ];
    permu_copy[ last ] := last;
    last := tmp;
    inc(countflips);
  until last = 0;
end;

function fannkuch: longint;
var
  print30, m, i, flips: longint;
begin
  print30 := 0;
  fannkuch := 0;
  m := n - 1;

  // Initial permutation.
  for i := 0 to m do   permu[i] := i;

  r := n;
  repeat
    if print30 < 30 then
    begin
      for i := 0 to m do
        write(permu[i] + 1);
      writeln;
      inc(print30);
    end;
    while r <> 1 do
    begin
      count[r-1] := r;
      dec(r);
    end;
    if (permu[0]<>0) and (permu[m]<>m) then
    begin
      move(permu[0], permu_copy[0], sizeof(longint)*n);
      flips := countflips;
      if flips > fannkuch then
        fannkuch := flips;
    end;
  until not NextPermutation;
end;

begin
  n := 7;
  if paramCount() = 1 then
    Val(ParamStr(1), n);
  answer := fannkuch;
  writeln('Pfannkuchen(', n, ') = ', answer);
end.
