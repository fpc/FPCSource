{ %opt=-vh -Seh }
program unused;

uses
	types;

begin
	{$IF Epsilon > '1' )}
		Writeln('Epsilon > 1');
	{$ELSE}
		Writeln('Epsilon <= 1');
	{$ENDIF}
end.
