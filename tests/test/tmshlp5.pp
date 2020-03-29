{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp5;

type
	TMyObject = class
		constructor Create1;
	end;
	THelper1 = class helper for TMyObject
		constructor Create2;
	end;
	THelper2 = class helper for TMyObject
		constructor Create3;
	end;

constructor TMyObject.Create1;
begin
end;

constructor THelper1.Create2;
begin
end;

constructor THelper2.Create3;
begin
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create1;
	obj := TMyObject.Create2;
	obj := TMyObject.Create3;
end.
