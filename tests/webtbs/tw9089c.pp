{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }
{ %needlibrary }
{ %delfiles=tw9089a tw9089b }

program ptest;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif fpc}

const
{$if defined(windows) or defined(mswindows)}
  libname='tw9089b.dll';
{$else}
  libname='tw9089b';
  {$linklib tw9089b}
{$ifend}

function Test: Integer; cdecl; external libname;

begin
  Writeln(Test);
end.
