{ %NORUN }

program tanonfunc70;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test calling into overloaded routines and creating anonymous methods in them. 

  Similar to tanonfunc61 but with additonal calls to func ref before and after
  anonfunc use of func ref.  With func ref inside an anon method, the func
  references outside the anon method will also error out.
}

type
  tproc = reference to procedure;
  tcharproc = reference to procedure(c: char);

procedure baz(p: tproc);
begin
  p();
end;

procedure bar(p: tcharproc); overload;
begin
  p('b');
  baz(procedure
    begin
      p('a');
    end);
  p('c');
end;

begin
end.

