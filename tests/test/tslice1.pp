program Test;

{$Mode ObjFPC}{$H+}

function sumafter1(arr: Array of Integer): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i in arr[1..High(arr)] do
    Result:=Result+i;
end;

begin
  if sumafter1([1,2,3,4]) <> 2+3+4 then
    Halt(1);
  WriteLn('ok');
end.
