Program Example97;

{ This program demonstrates the TryRecodeDateTime function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;
  D : TDateTime ;

Begin
  If TryRecodeDateTime(Now,2000,2,RecodeLeaveFieldAsIs,0,0,0,0,D) then
    begin
    S:=FormatDateTime(Fmt,D);
    Writeln('This moment in februari 2000 : ',S);
    end
  else
    Writeln('This moment did/does not exist in februari 2000');
End.