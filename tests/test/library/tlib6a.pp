{ %target=win32,win64 }
{ %needlibrary }
{ %norun }
{ %neededafter }

{ a library with its own run time library and threadvars, loaded by the
  program of tlib6b.pp when no thread local storage index is left for it }
library tlib6a;

threadvar
  tv: byte;

begin
  tv:=1;
end.
