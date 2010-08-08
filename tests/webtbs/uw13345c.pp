unit uw13345c;
{$mode DELPHI}
interface
 type ITestIF=interface
       procedure Test;
      end;
 var GTEST:ITestIF;
implementation
uses uw13345b;
initialization
  writeln('initc start');
end.
