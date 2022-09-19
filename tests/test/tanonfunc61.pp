program tanonfunc61;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test calling into overloaded routines and creating anonymous methods in them. 

  Same as tanonfunc23 but with mode delphi
}

type
  tproc = reference to procedure;
  tcharproc = reference to procedure(c: char);
  tintproc = reference to procedure(i: longint);

procedure baz(p: tproc);
begin
  p();
end;

procedure bar(p: tcharproc); overload;
begin
  baz(procedure
    begin
      p('a');
    end);
end;

procedure bar(p: tintproc); overload;
begin
  baz(procedure
    begin
      p(123);
    end);
end;

procedure foo;
var
  acc: integer;
begin
  acc := 0;
  bar(procedure(c: char)
    begin
      if c = 'a' then inc(acc);
    end);
  bar(procedure(i: longint)
    begin
      if i = 123 then inc(acc);
    end);
  if acc <> 2 then halt(1);
end;

begin
  foo;
end.

