Program Example13;

{ This program demonstrates the FileDateToDateTime function }

Uses sysutils;

Var
  ThisAge : Longint;

Begin
 Write ('ex13.pp created on :');
 ThisAge:=FileAge('ex13.pp');
 Writeln (DateTimeToStr(FileDateToDateTime(ThisAge)));
End.