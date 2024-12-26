{ %FAIL }

program tw41075c;

{$modeswitch ANONYMOUSFUNCTIONS}

//{$modeswitch CLASSICPROCVARS} // with this its a regular error: expected procedure, got pointer
{$modeswitch POINTERTOPROCVAR} // with this (and CLASSICPROCVARS) the code works fine

procedure test(p: TProcedure);
begin
  p;
end;

begin
  test(@(procedure // Error: Internal error 2021052602
  begin
    writeln('Hello');
  end));
end.

