Program Example91;

{ This program demonstrates the TextToFloat function }
{$mode objfpc}
{$h+ }

Uses SysUtils;

Const
  NrValues = 5;
  TestStr : Array[1..NrValues] of pchar =
           ('1,1','-0,2','1,2E-4','0','1E4');

Procedure Testit;

Var
  I : Integer;
  E : Extended;

begin
  Writeln('Using DecimalSeparator : ',DecimalSeparator);
  For I:=1 to NrValues do
    begin
    Writeln('Converting : ',TestStr[i]);
    If TextToFloat(TestStr[i],E) then
      Writeln('Converted value : ',E)
    else
      Writeln('Unable to convert value.');
    end;
end;

Begin
  DecimalSeparator:=',';
  Testit;
  DecimalSeparator:='.';
  Testit;
End.