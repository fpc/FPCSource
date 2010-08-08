{ %fail }

{$modeswitch nestedprocvars}

type
  tprocedure = procedure;

procedure test(procedure nestedproc);
begin
end;

var
  pp: tprocedure;
begin
  { passing global procvars to nested procedures is not allowed to
    ensure that they can also be implemented using compile-time
    generated trampolines if necesarry }
  test(pp);
end.
