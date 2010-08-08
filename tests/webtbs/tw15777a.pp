{ %opt=-vw -Sew }

{ should not cause warnings about potential problems with coerced univ
  parameters, since no procvars are involved }

{$mode macpas}

type
  tr = record
    l : longint;
  end;

procedure test(l: univ longint);
begin
  writeln(l);
end;

var
  r: tr;
  s: single;
begin
  r.l:=12345;
  test(r);
  s:=1234;
  test(s);
end.

