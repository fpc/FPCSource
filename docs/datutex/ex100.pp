Program Example100;

{ This program demonstrates the CompareTime function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss.zzz';

Procedure Test(D1,D2 : TDateTime);

Var
  Cmp : Integer;

begin
  Write(FormatDateTime(Fmt,D1),' has ');
  Cmp:=CompareDateTime(D1,D2);
  If Cmp<0 then
    write('earlier time than ')
  else if Cmp>0 then
    Write('later time than ')
  else
    Write('equal time with ');
  Writeln(FormatDateTime(Fmt,D2));
end;

Var
  D,N : TDateTime;

Begin
  D:=Today;
  N:=Now;
  Test(D,D);
  Test(N,N);
  Test(N+1,N);
  Test(N-1,N);
  Test(N+OneSecond,N);
  Test(N-OneSecond,N);
End.