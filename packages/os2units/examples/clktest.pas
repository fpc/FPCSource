uses doscalls, clkdll;

Var
  ID: Array[0..255] of AnsiChar;
  NextDate: TDateTime;
begin
  WriteLn('RC=',ClkQuerySTData(@ID, NextDate));
  WriteLn(PAnsiChar(@ID));
  With NextDate do
    WriteLn(Hours,':',Minutes,':',Seconds);
end.
