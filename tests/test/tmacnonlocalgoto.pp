program tmacnonlocalgoto;
{$MODE MACPAS}

	label
		1;

	var
		failed: Boolean;

	procedure Global(l: longint);
        label 2;

		procedure Local(v: longint);
		begin
                        if (v = 1) then
				Global(v+1)
                        else if (v = 3) then
				goto 2
			else
				goto 1;
			failed := true;
		end;

	begin
		Local(l+1);
	2:
                if (l <> 2) then
                  failed := true;
		Local(5);
		failed := true;
        
	end;


begin
	failed := false;

	Global(0);
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

