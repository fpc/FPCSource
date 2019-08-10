{ %NORUN }

{$mode objfpc}
{$modeswitch typehelpers}
{$modeswitch multihelpers}

program tmshlp8;
uses
	umshlp1;

type
	TClassHelper = class helper for TObject
		procedure DoThis;
	end;
	TStringHelper = type helper for String
		function Length: integer;
	end;

procedure TClassHelper.DoThis;
begin
	DoThisExt;
end;

function TStringHelper.Length: integer;
begin
	result := LengthExt;
end;

var
	obj: TObject;
	str: string;
begin
	obj := TObject.Create;
	obj.DoThis;
	writeln(str.Length + str.LengthTimesTwo);
end.
