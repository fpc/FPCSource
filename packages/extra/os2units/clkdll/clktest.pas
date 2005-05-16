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

{
$Log: clktest.pas,v $
Revision 1.2  2005/02/14 17:13:21  peter
  * truncate log

}
