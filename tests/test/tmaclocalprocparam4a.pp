{ %fail }

{$modeswitch nestedprocvars}

type
  tprocedure = procedure;
  tnestedprocedure = procedure is nested;

var
  pp: tprocedure;
  pn: tnestedprocedure;
begin
  { passing global procvars to nested procedures is not allowed to
    ensure that they can also be implemented using compile-time
    generated trampolines if necesarry }
  pn:=pp;
end.
