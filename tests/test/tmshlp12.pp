{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp12;

type
	TMyObject = class
		procedure DoThis;
	end;
	THelper1 = class helper for TMyObject
		procedure DoThis;
	end;
	THelper2 = class helper for TMyObject
		procedure DoThis;
	end;

var
	Res: integer;

procedure TMyObject.DoThis;
begin
	Res := 1;
end;

procedure THelper1.DoThis;
begin
	Res := 2;
end;

procedure THelper2.DoThis;
begin
	Res := 3;
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create;
	obj.DoThis;
	writeln(Res);
	if Res <> 3 then
		Halt(1);
end.
