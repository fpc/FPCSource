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

procedure roll_down( var a : array of longint ); inline;
var  tmp : longint;
begin
  tmp := a[ 0 ];
  move( a[1], a[0], high(a)*sizeof(longint) );
  a[ high(a) ] := tmp;
end;


procedure reverse( var a: array of longint ); inline;
var
  pi, pj : pLongint;
begin
  pi := @a[0];
  pj := @a[high(a)];
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
begin
  r0 := r; // use local variable
  NextPermutation := true;
  repeat
    if r0 = n then
    begin
      NextPermutation := false;
      break;
    end;
    roll_down( permu[ 0 .. r0 ] );
    dec(count[r0]);
    if count[r0] > 0 then
      break;
    inc(r0);
  until false;
  r := r0;
end;

function fannkuch: longint;
var
  print30, m, i, last, tmp, flips: longint;
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
      for i := 0 to m do   write(permu[i] + 1);
      writeln;   inc(print30);
    end;
    while r <> 1 do
    begin
      count[r-1] := r;
      dec(r);
    end;
    if (permu[0]<>0) and (permu[m]<>m) then
    begin
      move(permu[0], permu_copy[0], sizeof(longint)*n);
      flips := 0;

      last := permu_copy[0];
      repeat
        // Reverse part of the array.
        reverse( permu_copy[ 1 .. last-1 ] );
        tmp := permu_copy[ last ];
        permu_copy[ last ] := last;
        last := tmp;
        inc(flips);
      until last = 0;

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
