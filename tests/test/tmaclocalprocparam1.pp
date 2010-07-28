program tmaclocalprocparam;
{$MODE MACPAS}

	var
		failed: Boolean;


	procedure Outside (procedure P);
	begin
		P;
	end;

	procedure Global;

		var
			nonlocalvar: integer;

		procedure Local;
		begin
			nonlocalvar := 42;
		end;

	begin
		nonlocalvar := 24;
		Outside(Local);
		failed := (nonlocalvar <> 42);
	end;



begin
	Global;

	if failed then
		writeln('Failed')
	else
		writeln('Succeded');

   {$IFC UNDEFINED THINK_Pascal}
	if failed then
		Halt(1);
   {$ENDC}
end.
