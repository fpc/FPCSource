{ The results of the following constants
  differ on 1.0 and 1.1 compiler
  as constants are evaluated as 32bit integers in 1.1
  and as 64bit integers in 1.1
  But in all cases int64(-1) should give -1 and not $ffffffff PM }
{$R-}
const
   u1 : qword = $ffffffff;
   i1 : int64 = $ffffffff;
   u2 : qword = -1;
   i2 : int64 = -1;
var
  l : longint;
begin
  l:=-1;
  Writeln(' qword($ffffffff) = ',u1);
  Writeln(' int64($ffffffff) = ',i1);
  Writeln(' qword(-1) = ',u2);
  Writeln(' int64(-1) = ',i2);
  if i2<>-1 then
    begin
      Writeln('"const i2 : int64 = -1;" code');
      Writeln('generates a wrong int64 constant');
      RunError(1);
    end;

  if u2<>qword(int64(l)) then
    begin
      Writeln('"const u2 : qword = -1;" code');
      Writeln('generates a wrong int64 constant');
      RunError(1);
    end;

  if qword(l)<>u2 then
    begin
      writeln('qword(longint) sign extension generates wrong code');
      halt(1);
    end;
end.
