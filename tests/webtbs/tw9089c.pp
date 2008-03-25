{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos}
{ %needlibrary }

program ptest;

{$mode objfpc}{$H+}

const
{$ifdef windows}
  libname='tw9089b.dll';
{$else}
  libname='tw9089a';
  {$linklib tw9089b}
{$endif}
  
function Test: Integer; cdecl; external libname;

begin
  Writeln(Test);
end.
