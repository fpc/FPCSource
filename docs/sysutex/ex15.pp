Program Example15;

{ This program demonstrates the IncMonth function }

Uses sysutils;

Var ThisDay : TDateTime;

Begin
  ThisDay:=Date;
  Writeln ('ThisDay : ',DateToStr(ThisDay));
  Writeln ('6 months ago :',DateToStr(IncMonth(ThisDay,-6)));
  Writeln ('6 months from now :' ,DateToStr(IncMonth(ThisDay,6)));
  Writeln ('12 months ago :',DateToStr(IncMonth(ThisDay,-12)));
  Writeln ('12 months from now :' ,DateToStr(IncMonth(ThisDay,12)));
  Writeln ('18 months ago :',DateToStr(IncMonth(ThisDay,-18)));
  Writeln ('18 months from now :' ,DateToStr(IncMonth(ThisDay,18)));
End.