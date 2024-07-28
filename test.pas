{$MODE OBJFPC}
{$H+}
uses libstd;
var
	s : string;
begin

	printf('GO'#10);;

	s:= 'Hallo';
	s:= s + '!' ;

	if s <> 'Hallo' then begin
		printf('cool'#10);
	end;

	s:= s + #10 + #0;

	printf(pchar(@s[1]));

end.