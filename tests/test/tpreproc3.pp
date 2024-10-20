{ %opt=-vh -Seh }
program unused;

uses
	types;

begin
	{$IF CurveKappa > 1 )}
		Writeln('CurveKappa > 1');
	{$ELSE}
		Writeln('CurveKappa <= 1');
	{$ENDIF}
end.
