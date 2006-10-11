{ The Great Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Ales Katona
}

program sumcol;

{$mode objfpc}

var num, tot: longint;

begin
  while not Eof(input) do begin
    ReadLn(input, num);
    tot := tot + num;
  end;
  WriteLn(tot);
end.