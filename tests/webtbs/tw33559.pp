{$mode delphi}
program Project1;
{$APPTYPE CONSOLE}
uses SysUtils, StrUtils;
const
  FullStr = '123'#0'45';
  TestStr = '123'#0'!';
begin
  Writeln('StartsWith=', FullStr.StartsWith(TestStr));
  if FullStr.StartsWith(TestStr) then
    halt(1);
  Writeln('Pos=', Pos(TestStr, FullStr) > 0);
  if Pos(TestStr, FullStr) > 0 then 
    halt(1);
  Writeln('StartsText=', StartsText(TestStr, FullStr));
  if StartsText(TestStr, FullStr) then
    halt(1);
end.
