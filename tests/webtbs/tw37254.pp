{ OPT=-O- -O1 }
{$mode objfpc}
function Get8bShr1_CORRECT(n: SizeUint): SizeUint;
begin
  result := uint8(n) shr 1;
end;


function Get8bShr1_BUGGY_x86_32(n: SizeUint): SizeUint;
begin
  result := n;
  result := uint8(result) shr 1;
end;


begin
  writeln('Correct: ', HexStr(Get8bShr1_CORRECT($AAAAAA), bitsizeof(SizeUint) div 4));
  writeln('Wrong: ', HexStr(Get8bShr1_BUGGY_x86_32($AAAAAA), bitsizeof(SizeUint) div 4));
  if Get8bShr1_BUGGY_x86_32($AAAAAA)<>$55 then
    halt(1);
  writeln('ok');
end.
