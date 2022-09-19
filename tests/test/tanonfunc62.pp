program tanonfunc62;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test calling into overloaded routines and creating anonymous methods (with no
  params) in them 

  similar to tanonfunc23 and tanonfunc61 but mode Delphi and no params in tcharproc
}

type
  tproc = reference to procedure;
  tcharproc = reference to procedure;
  tintproc = reference to procedure(i: longint);

procedure baz(p: tproc);
begin
  p();
end;

procedure bar(p: tcharproc); overload;
begin
  baz(procedure
    begin
      p();
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
  bar(procedure{(c: char)}
    begin
      {if c = 'a' then }inc(acc);
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

