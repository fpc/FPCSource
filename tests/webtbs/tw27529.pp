{ %OPT=-Sm -dMY_VAR:=123 }
program test;

{$IF DEFINED(MY_VAR)}
  {$INFO MY_VAR defined}
  {$IF MY_VAR=123}
    {$INFO MY_VAR = 123}
  {$ELSE}
    {$INFO MY_VAR <> 123}
  {$ENDIF}
{$ENDIF}

begin
  writeln(MY_VAR);
  if MY_VAR<>123 then
    halt(1);
end.
