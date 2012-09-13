{$mode objfpc}

type
  tc = class
    procedure test; virtual;
  end;

  trec = record
    c: tc;
  end;

procedure tc.test;
begin
end;

procedure doit(r: trec);
begin
  r.c.test;
end;

var
  r: trec;
  c: tc;
begin
  c:=tc.create;
  r.c:=c;
  doit(r);
  c.free;
end.
