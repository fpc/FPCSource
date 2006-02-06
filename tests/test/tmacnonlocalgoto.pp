program tmacnonlocalgoto;
{$MODE MACPAS}

	label
		1;

	var
		failed: Boolean;

	procedure Global;

		procedure Local;
		begin
			goto 1;
			failed := true;
		end;

	begin
		Local;
		failed := true;
	end;


begin
	failed := false;

	Global;
1:
	if failed then
		writeln('Failed')
	else
		writeln('Succeded');

  {$IFC NOT UNDEFINED FPC}
	if failed then
		Halt(1);
  {$ENDC}
end.
