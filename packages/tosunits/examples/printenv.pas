Program  printenv;

uses sysutils, gemdos;

Var count, i: integer;
    s: AnsiString;

begin
	writeln('Arguments:');
	for i := 0 to paramcount do
	begin
	  s := ParamStr(i);
	  writeln(i,': ',s);
	end;
    writeln('');
	
	writeln('Environment:');
	count := GetEnvironmentVariableCount;
	for i := 1 to count do
	begin
	  s := GetEnvironmentString(i);
	  writeln(s);
	end;

    gemdos_pterm(0);
end.
