{%NORUN}
program tw24486;

{$mode delphi}

type
  tproc1 = procedure(a: integer);

var
  proc1: tproc1;

type
  tclass1 = class
    class procedure p1(a: integer); static;
  end;

{ tclass1 }

class procedure tclass1.p1(a: integer);
begin
end;

begin
  proc1 := tclass1.p1;
end.
