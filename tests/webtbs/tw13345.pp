{ %opt=-gh }

program iftest;
{$mode DELPHI}
uses uw13345c;
//uses uw13345b, uw13345c;
begin
 HaltOnNotReleased:=true;
 Writeln('START');
 GTEST.Test;
 Writeln('END');
end.

