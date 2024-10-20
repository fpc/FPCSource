{ %opt=-vh -Seh }
program unused;

uses
	ctypes;

begin
	{$IF SIZEOF(cint) >= 8)}
		Writeln('cint is 8 bytes or larger')
	{$ELSEIF SIZEOF(cint) >= 4)}
		Writeln('cint is 4 bytes or larger')
	{$ELSEIF SIZEOF(cint) >= 2)}
		Writeln('cint is 2 bytes or larger')
	{$ELSE}
		Writeln('cint is a puny 1 byte')
	{$ENDIF}
end.
