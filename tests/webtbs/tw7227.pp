
program openarray;
{$ifdef fpc}{$mode delphi}{$endif}
type
 PDouble = ^Double;

function CheckValues(values : array of PDouble) : boolean;
var i : integer;
begin
 Result := True;
 for i := Low(values) to High(values) do
  if values[i]^ = 0 then
    Result := False;
end;

var values : array of PDouble;
    i : integer;
begin
 SetLength(values, 5);
 for i := 0 to High(values) do
 begin
  New(values[i]);
  values[i]^ := i+1;
 end;

 for i := 0 to High(values) do
   writeln(values[i]^);

 if CheckValues(values) then
   WriteLn('OK')
 else
   writeln('not OK');
end.
