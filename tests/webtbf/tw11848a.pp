{ %OPT=-Sew -Oodfa }
{ %FAIL }
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

function Test(a: integer): integer;
var
  i : integer;
begin
  if a>0 then
    i:=1
  else
    exit(i);
end;

begin
end.

