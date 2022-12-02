{ %FAIL }
{ %OPT=-Sew }

{ Note: we are speculating for "Unreachable code" warnings here }

program tb0700;

var
  arr: array of LongInt;
begin
  arr := Nil;
  if TypeInfo(arr[0]) <> TypeInfo(LongInt) then
    Writeln('False');
end.
