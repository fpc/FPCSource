{ %opt=-vh -Seh }
program unused;

uses
	types;

begin
	{$IF FromBeginning > 1 )}
		Writeln('FromBeginning > 1');
	{$ELSE}
		Writeln('FromBeginning <= 1');
	{$ENDIF}
end.
