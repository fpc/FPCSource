{ The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org

  contributed by Ales Katona
  modified by Daniel Mantione
  modified by Steve Fisher
}

{$iochecks off}

var
  num, tot: longint;
  textbuf: array[0..8191] of char;

begin
  settextbuf(input, textbuf);
  repeat
    readLn( num );
    tot += num
  until eof;
  writeLn(tot)
end.

