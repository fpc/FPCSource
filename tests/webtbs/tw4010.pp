{ Source provided for Free Pascal Bug Report 4010 }
{ Submitted by "Adrian" on  2005-05-23 }
{ e-mail: adrian@veith-system.de }

{$mode objfpc}
program Bug;

type
  TDynamicByteArray = array of byte;

function MaxByte(const ar: array of byte): Integer;
var
  i: Integer;
begin
  Result:= Low(Byte);
  for i:= Low(ar) to High(ar) do
    if ar[i] > Result then Result:= ar[i];
end;

function GenByteArray(const aStr: string): TDynamicByteArray;
begin
	SetLength(result, Length(aStr));
	if Length(aStr) > 0 then
		Move(aStr[1], result[0], Length(aStr));
end;

var
  ar1: TDynamicByteArray;
begin
  ar1:= GenByteArray('foo');
  MaxByte(ar1);
  MaxByte(GenByteArray('foo')); // compiler stops here
end.
