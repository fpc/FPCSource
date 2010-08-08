{$ifdef fpc}
{$mode objfpc}
{$endif}
program tw12385;

uses
  SysUtils;

var
  s: string; 
  cr: Extended; 

Procedure TestIt(CR : Extended; Fmt,Expected : String);
  
begin
  S:=FormatFloat(Fmt,cr);
  If S<>Expected then
    begin
    Writeln('"',S,'"<>"',Expected,'" (latter is correct)');
    Halt(1);
    end;
end;

begin
  DecimalSeparator:='.';
  ThousandSeparator:=',';
  TestIt(1234.567,'00000000.00','00001234.57');
  TestIt(-1234.567,'00000000.00','-00001234.57');
  TestIt(-1234.567,'000.00','-1234.57');
  TestIt(-1,'000.000','-001.000');
//  TestIt(-80,'#,##0.00','-80.00');
  TestIt(-140,'#,##0.00','-140.00');
  TestIt(140,'#,##0.00','140.00');
  TestIt(80,'#,##0.00','80.00');
  TestIt(-2.45,'#,##0.00','-2.45');
  TestIt(-1400,'#,##0.00','-1,400.00');
  TestIt(-1400,'##,##0.00','-1,400.00');
end.
