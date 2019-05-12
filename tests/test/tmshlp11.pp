{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp11;

type
	TMyObject = class
		class function Create1: TMyObject;
	end;
	THelper1 = class helper for TMyObject
		class function Create2: TMyObject;
	end;
	THelper2 = class helper for TMyObject
		class function Create3: TMyObject;
	end;

class function TMyObject.Create1: TMyObject;
begin
	result := TMyObject.Create;
end;

class function THelper1.Create2: TMyObject;
begin
	result := TMyObject.Create;
end;

class function THelper2.Create3: TMyObject;
begin
	result := TMyObject.Create;
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create1;
	obj := TMyObject.Create2;
	obj := TMyObject.Create3;
end.
