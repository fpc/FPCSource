program dumppars;

uses	dos;

var	i:longint;

begin
	writeln('De parameters zijn:');
	for i:=1 to paramcount do
		writeln(paramstr(i));
	writeln('Path is:');
	writeln(getenv('path'));
end.