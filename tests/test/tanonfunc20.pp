program tanonfunc20;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test accessing parent classes across unit boundaries }

uses uanonfunc20;

type
  tintfunc = reference to function: longint;

  tsub = class(tbase)
    y: longint;
    procedure bar;
  end;

procedure tsub.bar;
var
  f: tintfunc;
begin
  f := function: longint
  begin
    result := x + y;
  end;
  y := 456;
  writeln(x, ' ', y, ' ', f());
  if f() <> 579 then
    halt(1);
end;

var
  c: tsub;
begin
  c := tsub.create;
  c.bar;
  c.free;
end.

