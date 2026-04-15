{ %opt=-Cn -Tlinux }
{$mode objfpc}
program tw41703;

function myext(a: longint): longint; cdecl; varargs; external name 'myext';

function check(x: longint): longint;
begin
  result := x;
end;

function wrap(a: longint): longint; inline;
begin
  result := check(a);
end;

var
  r: longint;
begin
  r := wrap(myext(1, 2));
end.
