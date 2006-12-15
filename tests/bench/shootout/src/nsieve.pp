{ The Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Joost van der Sluis
}

program nsieve;

{$mode fpc}{$I-}

var
  n : longint;

procedure primes(n : longint); inline;
var
  flags: PBoolean;
  size,i,j,count : longint;
begin
  size := 10000 shl n;
  flags := getmem(size+1);
  fillchar(flags^,memsize(flags),true);
  count := 0;
  for i := 2 to size do
    if flags[i] then
    begin
      inc(count);
      j := i + i;
      while j <= size do begin
        flags[j] := false;
        inc(j, i);
      end;
    end;
  writeln('Primes up to', size:9, count:9);
  freemem(flags);
end;

begin
  val(ParamStr(1), n);
  primes(n);
  primes(n-1);
  primes(n-2);
end.
