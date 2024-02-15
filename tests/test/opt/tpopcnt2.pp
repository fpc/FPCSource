{ %CPU=i386,x86_64 }
{ %OPT=-O2 -CpCOREAVX }
uses
  cpu;

var
  X, Y: LongWord;
begin
  if not popcntsupport then
    begin
      writeln('This CPU doesn''t support POPCNT instruction');
      halt(0);
    end;
  for X := 0 to 9 do
    begin
      Y := PopCnt(X);
      { Condition arranged this way so the input of PopCnt is checked first.
        If both X and Y are zero or both X and Y are not zero, then all is well. }
      if (X = 0) xor (Y = 0) then
        begin
          WriteLn('FAIL: Condition implies PopCnt(', X, ') = ', Y, ' and not ', PopCnt(X));
          Halt(1);
        end;
    end;

  WriteLn('ok');
end.
