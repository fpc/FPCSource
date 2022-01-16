{$mode objfpc}
{$modeswitch typehelpers}
{$modeswitch multihelpers}

program tmshlp3;

type
	TStringHelper1 = type helper for String
		function Length: integer;
	end;

function TStringHelper1.Length: integer;
begin
	result := System.Length(self);
end;

type
	TStringHelper2 = type helper for string
		function LengthSquared: integer;
	end;

function TStringHelper2.LengthSquared: integer;
begin
	result := self.Length * self.Length;
end;

var
	s: string = 'abcd';
begin
	if (s.Length <> 4) or (s.LengthSquared <> 16 ) then
		Halt(1);
end.