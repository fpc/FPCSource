{ %opt=-vh -Sewh }
{$modeswitch anonymousfunctions}
var
	p: procedure(const s: string);
begin
	p := procedure(const s: string) begin writeln(s); end;
	p('test');
end.
