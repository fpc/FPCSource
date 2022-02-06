program tanonfunc29;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test accessing parent classes across unit boundaries and multiple nestings }

uses uanonfunc20;

type
  tintfunc = reference to function: longint;

  tsub = class(tbase)
    y: longint;
    procedure bar;
  end;

function callfunc(afunc: tintfunc): longint;
begin
  result := afunc()
end;

procedure tsub.bar;
var
  z: longint;
begin
  y := 456;
  z := callfunc(
    function: longint
    begin
      result := x + y + callfunc(
        function: longint
        begin
          result := x + 87
        end);
    end);
  if z <> 789 then
    halt(1);
end;

var
  c: tsub;
begin
  c := tsub.create;
  c.bar;
  c.free;
end.

