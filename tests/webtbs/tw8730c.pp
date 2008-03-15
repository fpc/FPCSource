{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos}
{ %NEEDLIBRARY }

{$mode delphi}
program MainApp;

uses
  sysutils;

const
{$ifdef windows}
  libname='tw8730b.dll';
{$else}
  libname='tw8730b';
  {$linklib tw8730b}
{$endif}

function Lib2Func: pchar; CDecl; external libname name 'Lib2Func';

begin
  WriteLn( Lib2Func );
  if not(fileexists('tw8730a.txt')) or
     not(fileexists('tw8730b.txt')) then
    halt(1);
end.
