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
  TestIt(-10,'###,###,##0.00','-10.00');
end.
