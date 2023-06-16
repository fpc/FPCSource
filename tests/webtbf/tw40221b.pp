{ %FAIL }

{$mode objfpc} {$modeswitch anonymousfunctions}
procedure Main;
type
  TProcMethod = procedure of object;
var
	c: int32;
begin
	c := 12;
	TProcMethod(procedure begin writeln(c); end);
end;

begin
	Main;
end.

