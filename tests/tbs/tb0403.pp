{$mode objfpc}

type
  tclass = class
    procedure t; virtual;
  end;

procedure tclass.t;
begin
end;

var
  p: pointer;
begin
  p := @tclass.t;
end.
