Program Example36;

{ This program demonstrates the FileAge function }

Uses sysutils;

Var S : TDateTime;
    fa : Longint;
Begin
  fa:=FileAge('ex36.pp');
  If Fa<>-1 then
    begin
    S:=FileDateTodateTime(fa);
    Writeln ('I''m from ',DateTimeToStr(S))
    end;
End.