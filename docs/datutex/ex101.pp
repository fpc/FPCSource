Program Example101;

{ This program demonstrates the SameDateTime function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss.zzz';

Procedure Test(D1,D2 : TDateTime);

begin
  Write(FormatDateTime(Fmt,D1),' is the same datetime as ');
  Writeln(FormatDateTime(Fmt,D2),' : ',SameDateTime(D1,D2));
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