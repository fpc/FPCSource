Program Example19;

{ This program demonstrates the Tomorrow function }

Uses SysUtils,DateUtils;

Begin
  Writeln(FormatDateTime('"Today is" dd mmmm yyyy',Today));
  Writeln(FormatDateTime('"Tomorrow will be" dd mmmm yyyy',Tomorrow));
End.