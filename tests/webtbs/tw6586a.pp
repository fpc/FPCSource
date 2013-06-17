{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }
{ %needlibrary }
{ %norun }

library tw6586a;
{$H+}{$MODE OBJFPC}

uses cmem;

procedure ExportTest1(input: longint); stdcall;
begin
 input:= 5;
end;

exports
  ExportTest1;

begin
end.

