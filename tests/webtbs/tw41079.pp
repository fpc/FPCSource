{ %OPT=-O2 }
program tw41079;
var
  A, B, C: QWord;
  Fail: Boolean;
begin
  A := 140737488355327;
  WriteLn('A                : ',BinStr(A, 64));
  B := (A shr 47) shl 48;
  WriteLn('(A shr 47) shl 48: ',BinStr(B, 64));
  Fail := B <> 0;
  C := A shr 47;
  WriteLn('C := A shr 47    : ',BinStr(C, 64));
  Fail := Fail or (C <> 0);
  C := C shl 48;
  WriteLn('C := C shl 48    : ',BinStr(C, 64));
  Fail := Fail or (C <> 0);
  if Fail then
    begin
      WriteLn('FAILED');
      Halt(1);
    end;
  WriteLn('ok');
end.
