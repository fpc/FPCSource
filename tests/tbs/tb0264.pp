{ Old file: tbs0307.pp }
{ "with object_type" doesn't work correctly!           OK 0.99.13 (?) }

type
  tobj = object
    l: longint;
    constructor init;
    procedure setV(v: longint);
    destructor done;
  end;

constructor tobj.init;
begin
  l := 0;
end;

procedure tobj.setV(v: longint);
begin
  l := v;
end;

destructor tobj.done;
begin
end;

var t: tobj;

begin
  t.init;
  with t do
    setV(5);
  writeln(t.l, ' (should be 5!)');
  if t.L<>5 then
    Halt(1);
  t.done;
end.
