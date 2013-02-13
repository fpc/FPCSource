{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }
{ %needlibrary }
{ %delfiles=tw6586a }

program project1;

{$mode objfpc}{$H+}

uses cmem;

const
{$ifdef windows}
  libname='tw6586a.dll';
{$else}
  libname='tw6586a';
  {$linklib tw6586a}
{$endif}

procedure ExportTest1(input: longint); stdcall; external libname;

begin
  writeln('Watch for Seg fault on closing');
end.
