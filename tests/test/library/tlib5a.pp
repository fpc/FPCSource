{ %target=win32 }
{ %result=226 }
{ %neededafter }

{ a program whose threadvars cannot be had when it starts: the image already
  holds the 1100 MB of threadvars in its BSS, and the main thread's block
  needs another 1100 MB. The two together are more than the 2 GB of address
  space of a program that is not large-address-aware, wherever the image and
  the block are placed; no more is taken than that needs. tlib5b.pp runs it
  again and checks where the message is written }
program tlib5a;

threadvar
  tv: array[0..1100*1024*1024-1] of byte;

begin
{$ifndef FPC_USE_TLS_DIRECTORY}
  { without a TLS directory the main thread uses the threadvars in the image
    until it starts a thread, so there is no failure to provoke (tlib5b skips
    too): end as the test expects }
  writeln('skipped: the compiler makes no TLS directory');
  halt(226);
{$endif FPC_USE_TLS_DIRECTORY}
  tv[0]:=1;
  writeln('the threadvars were allocated');
end.
