{ %OPT=-Sew -vw -Oodfa }
{ %NORUN }
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

function Test(a: integer): integer;
var
  HashItem: Pointer;
begin
  HashItem:=nil;
  while HashItem<>nil do begin
    if (HashItem<>nil) then
      exit(-1);
    HashItem:=HashItem;
  end;
  Result:=0;
end;

begin
end.

