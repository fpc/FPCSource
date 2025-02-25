{ %OPT=-O3 -OoNOCONSTPROP }

{ Test adding a typecast Boolean to a 64-bit integer }
program tw41148;

{$mode objfpc}
procedure v64(out code: int32);
var
  v: uint64;
  r : record
    b: boolean;
  end;
begin
  r.b := true;
  v := uint64(High(int64)) + uint64(r.b);
  WriteLn('     Calculated: ' , BinStr(v, 64));
  if v <> uint64($8000000000000000) then
    begin
      WriteLn('FAIL - expected: 1000000000000000000000000000000000000000000000000000000000000000');      
      code := 1;
    end
  else
    code := 0;
end;

var
  code: int32;

begin
  v64(code);
  if code <> 0 then
    Halt(code);
 WriteLn('ok');
end.
