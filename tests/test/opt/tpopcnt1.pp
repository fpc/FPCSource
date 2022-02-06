{ %CPU=i386,x86_64 }
{ %OPT=-O2 -CpCOREAVX }

var
  X: LongWord;
begin
  for X := 0 to 9 do
    if PopCnt(X) = 0 then
      begin
        if X <> 0 then
          begin
            WriteLn('FAIL: PopCnt(', X, ') = 0 returned True');
            Halt(1);
          end;
      end
    else
      begin
        if X = 0 then
          begin
            WriteLn('FAIL: PopCnt(0) = 0 returned False');
            Halt(1);
          end;
      end;

  WriteLn('ok');
end.
