{ %fail }

{$modeswitch nestedprocvars}

type
  tprocedure = procedure;
  tnestedprocedure = procedure is nested;

var
  pp: tprocedure;
  pn: tnestedprocedure;
begin
  pp:=pn;
end.
