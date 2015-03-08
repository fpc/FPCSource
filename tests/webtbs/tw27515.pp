{$modeswitch nestedprocvars}
procedure foo(procedure bar;x : longint); begin end;
procedure foo(procedure bar(x: longint)); begin end;
procedure foo(procedure baz(x: tobject)); begin end;

begin
end.
