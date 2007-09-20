{ %cpu=powerpc,powerpc64,sparc,arm,x86_64 }

var
  l: longint;
  s: single;
  d: double;

begin
{$if not defined(cpux86_64) or defined(win64)} // or: using sse unit for math
  l := maxlongint;
{$MINFPCONSTPREC default}
  s:= l / 1.0;
  d:= l / 1.0;
  if (s <> d) then
    halt(1);
{$MINFPCONSTPREC 32}
  s:= l / 1.0;
  d:= l / 1.0;
  if (s <> d) then
    halt(2);
{$MINFPCONSTPREC 64}
  s:= l / 1.0;
  d:= l / 1.0;
  if (s = d) then
    halt(3);
{$endif}
end.
