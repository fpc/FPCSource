{ %NORUN }
{$mode objfpc}
{$modeswitch advancedrecords}
{ 
  testing range checking for arrays and for-loops
}

program tgenconst17;

type
	generic TStaticList<T; const Length: SizeUInt> = record
	  Values: array[0..Length - 1] of T;
	  procedure Display;
	end;

procedure TStaticList.Display;
var 
	I, n: SizeUInt;
begin
  for I := 0 to Length - 1 do
  	WriteLn(Values[I]);
end;

var
	list: specialize TStaticList<Integer, 20>;
begin
end.
