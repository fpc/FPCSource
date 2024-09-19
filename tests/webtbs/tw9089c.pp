{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android,haiku }
{ %needlibrary }
{ %delfiles=tw9089a tw9089b }

program ptest;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif fpc}

{$if (FPC_FULLVERSION<=30301) and defined(linux)}
  uses
    initc;
{$endif (FPC_FULLVERSION<=30301) and defined(linux)}

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
