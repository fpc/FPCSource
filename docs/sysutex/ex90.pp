Program Example90;

{ This program demonstrates the StrToFloat function }
{$mode objfpc}
{$h+ }

Uses SysUtils;

Const
  NrValues = 5;
  TestStr : Array[1..NrValues] of string =
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
    Try
      E:=StrToFloat(TestStr[i]);
      Writeln('Converted value : ',E);
    except
      On E : Exception do
        Writeln('Exception when converting : ',E.Message);
    end;
    end;
end;

Begin
  DecimalSeparator:=',';
  Testit;
  DecimalSeparator:='.';
  Testit;
End.