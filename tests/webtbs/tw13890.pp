program test_bits;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

uses Classes;

var
  bits: TBits;
  i, j: Integer;
  count: Integer;
  
procedure AllocateSomething;
begin
  Inc(count);
end;

begin
  bits := TBits.Create;
  count := 0;
  for i := 0 to 9 do
  begin
    j := bits.OpenBit;
    if j = bits.Size then
    begin
      AllocateSomething;
      bits[j] := True;
    end;
  end;
  bits.Free;
  writeln(count);
  if count <> 10 then
    Halt(1);
end.
