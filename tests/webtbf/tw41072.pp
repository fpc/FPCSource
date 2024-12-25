{ %FAIL }

program tw41072;
{$mode objfpc}
{$modeswitch anonymousfunctions}

var
  P: TProcedure;

begin
  P:=@procedure begin end;
end.

