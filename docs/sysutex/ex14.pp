Program Example14;

{ This program demonstrates the FormatDateTime function }

Uses sysutils;

Var ThisMoment : TDateTime;

Begin
  ThisMoment:=Now;
  Writeln ('Now : ',FormatDateTime('hh:nn',ThisMoment));
  Writeln ('Now : ',FormatDateTime('DD MM YYYY',ThisMoment));
  Writeln ('Now : ',FormatDateTime('c',ThisMoment));
End.