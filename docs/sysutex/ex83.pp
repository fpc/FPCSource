Program Example82;

{$mode objfpc}

{ This program demonstrates the StrToInt function }

Uses sysutils;

Begin
  Writeln (StrToIntDef('1234',0));
  Writeln (StrToIntDef('-1234',0));
  Writeln (StrToIntDef('0',0));
  Try
    Writeln (StrToIntDef('12345678901234567890',0));
  except
    On E : EConvertError do
      Writeln ('Invalid number encountered');
  end;
End.