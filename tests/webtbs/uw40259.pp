{$mode objfpc} {$longstrings on} {$modeswitch advancedrecords}
{.$define enable_UseFromA}
unit uw40259;

interface

type
	Console = record
		Initialized: boolean;
		procedure Dump(const where: string);
		class operator Initialize(var self: Console);
		class operator Finalize(var self: Console);
	end;

{$ifdef enable_UseFromA}
	procedure UseFromA;
{$endif}

var
	Con: Console;

implementation

	procedure Console.Dump(const where: string);
	begin
		if not Initialized then begin
                	write('/UNINITIALIZED/ ');
                        Halt(1);
                end;
                writeln(where);
	end;

	class operator Console.Initialize(var self: Console);
	begin
		writeln('Console.Initialize');
		self.Initialized := true;
	end;

	class operator Console.Finalize(var self: Console);
	begin
		self.Initialized := false;
		writeln('Console.Finalize');
	end;

var
        Con2: Console;

{$ifdef enable_UseFromA}
	procedure UseFromA;
	begin
		Con.Dump('UseFromA');
	end;
{$endif}

end.
