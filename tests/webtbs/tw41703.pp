{ %target=linux }
{ %norun }
unit tw41703;

{$mode objfpc}

interface

implementation

function myext(a: longint): longint; cdecl; varargs; external name 'myext';

function check(x: longint): longint;
begin
  result := x;
end;

function wrap(a: longint): longint; inline;
begin
  result := check(a);
end;

procedure repro;
var
  r: longint;
begin
  r := wrap(myext(1, 2));
end;

end.
