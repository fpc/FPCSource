{ %norun }
{ %needlibrary }
{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }

library tw9089a;

{$mode objfpc}{$H+}

var
  myvar: longint; cvar;

exports
  myvar;

initialization
  Writeln('INIT');
  myvar:=-1;

finalization
  Writeln('FINI');
  if (myvar<>1) then
    halt(1);
end.
