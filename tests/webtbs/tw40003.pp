{ %OPT=-O2 }

program tw40003;

{ Code triggers faulty optimisation in OptPass2Movx }

{$mode objfpc} {$typedaddress on}
var
    value, thirdByte: uint32;
begin
    (@value)^ := $11223344 + random(0);
    thirdByte := byte(value shr 16);
    writeln('byte($11223344 shr 16) = $', HexStr(thirdByte, 1 + BsrDWord(thirdByte) div 4), ' (must be $22)');
    if thirdbyte <> 34 then
      begin
        WriteLn('FAIL');
        Halt(1);
      end;
    WriteLn('ok');
end.
