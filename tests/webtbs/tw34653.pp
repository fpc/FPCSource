{ %OPT=-O2 }
var d: LongInt;
begin
    WriteLn;

    d := 828;
    if ((d mod 2) = 0) xor ((d < 0) and ((d mod 3) = 0)) then
      WriteLn('YES')
    else
      begin
        WriteLn('NO');
        halt(1);
      end;

    if ((d mod 2) = 0) xor ((d < 0) and ((d mod 3) = 0)) then
        WriteLn('YES')
    else
      begin
        WriteLn('NO');
        halt(1);
      end;
end.
