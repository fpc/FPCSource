Program Example10;

{ This program demonstrates the DecodeTime function }

Uses sysutils;

Var HH,MM,SS,MS: Word;

Begin
  DecodeTime(Time,HH,MM,SS,MS);
  Writeln (format('The time is %d:%d:%d.%d',[hh,mm,ss,ms]));
End.