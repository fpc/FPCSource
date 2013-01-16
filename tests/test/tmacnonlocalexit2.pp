{ %fail }
program tmacnonlocalexit2;
{$MODE MACPAS}

	var
		failed: Boolean;

	procedure Global;

                procedure AnotherLocal;
                  begin
                  end;


		procedure Local;
		begin
			Exit(AnotherLocal);
			failed := true;
		end;

	begin
		Local;
		failed := true;
	end;


begin
	failed := false;

	Global;

	if failed then
		writeln('Failed')
	else
		writeln('Succeded');

  {$IFC NOT UNDEFINED FPC}
	if failed then
		Halt(1);
  {$ENDC}
end.
