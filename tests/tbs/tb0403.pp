{$mode objfpc}

type
  tclass = class
    procedure t; virtual;
  end;

procedure tclass.t;
begin
end;

var
  p: codepointer;
begin
  p := @tclass.t;
end.
