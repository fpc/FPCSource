{ %NORUN }
{$mode delphi}
{
  test delphi mode
}
program tgenconst6;

type
	TList<T; const U: integer> = class
		list: array[0..U-1] of T;
		function capacity: integer;
	end;

function TList<T; U>.capacity: integer;
begin
	result := U;	
end;	

var
	nums:TList<integer,16>;
	strs:TList<string,16>;
begin
	nums := TList<integer,16>.Create;
	strs := TList<string,16>.Create;
end.
