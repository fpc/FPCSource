{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp13;

type
	THelper1 = class helper for TObject
		class var field1: integer;
	end;
	THelper2 = class helper for TObject
		class var field2: integer;
	end;

begin
	TObject.field1 := 1;
	TObject.field2 := 2;
end.
