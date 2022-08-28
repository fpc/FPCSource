{ %fail }
{$mode objfpc} {$longstrings on}
procedure ThrowException; noreturn;
begin
	raise TObject.Create;
end;

procedure DoSomethingWithString;
begin
	writeln(Copy('hey', 1, 2));
	ThrowException;
end;

begin
	try
		DoSomethingWithString;
	except
		writeln('catch');
	end;
end.
