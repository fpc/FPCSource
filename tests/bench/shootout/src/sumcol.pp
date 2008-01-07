{ The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org

  contributed by Ales Katona
  modified by Daniel Mantione
  modified by Steve Fisher
  modified by Vincent Snijders
}

{$iochecks off}

var
  num, tot: longint;
  s: string[128];
  textbuf: array[0..8191] of char;
  infile: ^text;

begin
  infile := @input;
  settextbuf(infile^, textbuf);
  tot := 0;
  repeat
    readLn(infile^, s);
    val(s, num);
    tot := tot + num
  until eof(infile^);
  writeLn(tot)
end.


