{ %opt=-vh -Seh }
program unused;

uses
	ctypes;

begin
	{$IF HIGH(cint) >255 )}
		Writeln('cint is larger than one byte')
	{$ELSE}
		Writeln('cint is one byte')
	{$ENDIF}
end.
