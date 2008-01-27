uses doscalls, clkdll;

Var
  ID: Array[0..255] of Char;
  NextDate: TDateTime;
begin
  WriteLn('RC=',ClkQuerySTData(@ID, NextDate));
  WriteLn(PChar(@ID));
  With NextDate do
    WriteLn(Hours,':',Minutes,':',Seconds);
end.
