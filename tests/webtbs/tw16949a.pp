{ %norun }
{ %needlibrary }
{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }

library tw16949a;

{$mode objfpc}{$H+}

function foo: LongInt; cdecl;
var
 x: LongInt = 12345;
begin
 Result := x;
end;

exports
 foo;

begin
end.
