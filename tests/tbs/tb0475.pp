{$mode delphi}
type
  to1 = class
     fp : longint;
     property p : longint read fp write fp;
  end;

procedure p(const v);
  begin
  end;

var
  a : pchar;
  o1 : to1;

begin
  o1:=to1.create;
  p(a[0]);
  p(o1.p);
  o1.free;
end.
