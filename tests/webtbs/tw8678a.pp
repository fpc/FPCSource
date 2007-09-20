{ %cpu=powerpc,powerpc64,sparc,arm,x86_64 }
{ %opt=-CF64 }

var
  l: longint;
  s: single;
  d: double;

begin
{$if not defined(cpux86_64) or defined(win64)} // or: using sse unit for math
  l := maxlongint;
  s:= l / 1.0;
  d:= l / 1.0;
  if (s = d) then
    halt(1);
{$endif}
end.
