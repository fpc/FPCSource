{ The Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Joost van der Sluis
}

program nsieve;

{$mode objfpc}

var n : integer;

procedure primes(n : integer); inline;
var flags : array of boolean;
    size,i,j,count : integer;
begin
  size := 10000 shl n;
  SetLength(flags, size+1);
  for i := 2 to size do flags[i] := true;
//fillchar(flags[0],length(flags),ord(true));
  count := 0;
  for i := 2 to size do
    if flags[i] then
    begin
      count := count + 1;
      j := i + i;
      while j <= size do begin
//      flags[j] := false;
        if flags[j] then flags[j] := false;
        j := j + i;
      end;
    end;
  writeln('Primes up to', size:9, count:9);
end;

begin
  val(ParamStr(1), n);
  primes(n);
  primes(n-1);
  primes(n-2);
end.