{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android,haiku }
{ %needlibrary }
{ %delfiles=tw16949a }

program ptest;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif fpc}

{$if (FPC_FULLVERSION<=30301) and defined(linux)}
  uses
    initc;
{$endif (FPC_FULLVERSION<=30301) and defined(linux)}

const
{$if defined(windows) or defined(mswindows)}
  libname='tw16949a.dll';
{$else}
  libname='tw16949a';
  {$linklib tw16949a}
{$ifend}

function foo: LongInt; cdecl; external libname;

begin
  if foo<>12345 then
    halt(1);
end.
