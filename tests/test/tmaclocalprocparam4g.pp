{ %fail }

{$modeswitch nestedprocvars}

{ should fail because such inline definitions are always nested }
procedure test(procedure pp is nested);
begin
end;

begin
end.
