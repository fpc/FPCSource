{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp1;

type
	TMyObject = class
		procedure DoThis_1;
	end;
	THelper1 = class helper for TMyObject
		procedure DoThis_2;
	end;
	THelper2 = class helper for TMyObject
		procedure DoThis_3;
	end;

procedure TMyObject.DoThis_1;
begin
end;

procedure THelper1.DoThis_2;
begin
end;

procedure THelper2.DoThis_3;
begin
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create;
	obj.DoThis_1;
	obj.DoThis_2;
	obj.DoThis_3;
end.
