Program Example82;

{$mode objfpc}

{ This program demonstrates the StrToInt function }

Uses sysutils;

Begin
  Writeln (StrToInt('1234'));
  Writeln (StrToInt('-1234'));
  Writeln (StrToInt('0'));
  Try
    Writeln (StrToInt('12345678901234567890'));
  except
    On E : EConvertError do
      Writeln ('Invalid number encountered');
  end;
End.