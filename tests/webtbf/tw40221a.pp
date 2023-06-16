{ %FAIL }

{$mode objfpc} {$modeswitch anonymousfunctions}
procedure Main;
var
	c: int32;
begin
	c := 12;
	TProcedure(procedure begin writeln(c); end);
end;

begin
	Main;
end.

