{ %FAIL }

{ an anonymous function by itself isn't a valid expression or statement }

program tanonfunc54;

{$mode objfpc}
{$modeswitch anonymousfunctions}

begin
  procedure begin end;
end.

