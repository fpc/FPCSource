Program Example18;

{ This program demonstrates the Yesterday function }

Uses SysUtils,DateUtils;

Begin
  Writeln(FormatDateTime('"Today is " dd mmmm yyyy',Today));
  Writeln(FormatDateTime('"Yesterday was " dd mmmm yyyy',Yesterday));
End.