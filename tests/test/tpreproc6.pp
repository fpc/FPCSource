{ %opt=-vh -Seh }
program unused;

uses
	types;

begin
	{$IF Declared(Epsilon)}
		Writeln('Epsilon declared');
	{$ELSE}
		Writeln('Epsilon not declared');
	{$ENDIF}
end.
