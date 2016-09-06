{ %opt=-gh }

{$mode objfpc}
{$h+}
{$inline on}

uses
  variants;
var
  vv: variant;

function func: variant; inline;
begin
  result:=vv;
end;

function varconstinl(const v: variant): boolean; inline;
begin
  result:=v='abc';
end;

function test: boolean; inline;
begin
  result:=varconstinl(func);
end;

procedure dotest;
begin
  HaltOnNotReleased:=true;
  vv:='abc';
  if not test then
    halt(1);
end;

begin
  dotest;
end.
