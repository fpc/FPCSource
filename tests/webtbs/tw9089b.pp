{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }
{ %norun }
{ %needlibrary }

library tw9089b;

{$mode objfpc}{$H+}

const
{$ifdef windows}
  libname='tw9089a.dll';
{$else}
  libname='tw9089a';
  {$linklib tw9089a}
{$endif}

var
  myvar: longint; cvar; external {$ifdef windows}libname{$endif windows};

function Test: Integer; cdecl; export;
begin
  Result := 0;

  Writeln('Test');
end;

exports
  Test;

var
  t: text;

initialization
  Writeln('INIT2');
  if (myvar<>-1) then
    halt(3);

finalization
  Writeln('FINI2');
  myvar:=1;
  { so tw9089d can check whether the finalization has run at all }
  assign(t,'tw9089b.txt');
  rewrite(t);
  close(t);
end.
