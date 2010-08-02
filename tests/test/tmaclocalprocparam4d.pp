{ %fail }

{$mode tp}
{$modeswitch nestedprocvars}

type
  tprocedure = procedure;

procedure test(procedure pp);

  procedure nested;
    begin
    end;

begin
  { this should be a plain pointer }
  test(@nested);
end;

begin
end.
