{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp14;

type
	THelper1 = class helper for TObject
		type TInteger = integer;
	end;
	THelper2 = class helper for TObject
		type TString = string;
	end;

var
	obj: TObject;
begin
	writeln(sizeof(TObject.TInteger));
	writeln(sizeof(TObject.TString));
end.
