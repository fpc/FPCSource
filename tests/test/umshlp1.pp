{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

unit umshlp1;
interface

type
	TExtClassHelper = class helper for TObject
		procedure DoThisExt;
	end;
	TExtStringHelper = type helper for String
		function LengthExt: integer;
	end;
	TExtStringHelperMore = type helper for String
		function LengthTimesTwo: integer;
	end;

implementation
	
procedure TExtClassHelper.DoThisExt;
begin	
end;

function TExtStringHelper.LengthExt: integer;
begin
	result := System.Length(self);
end;

function TExtStringHelperMore.LengthTimesTwo: integer;
begin
	result := System.Length(self) * 2;
end;

end.
