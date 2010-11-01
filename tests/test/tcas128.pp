{ %cpu=x86_64 }

uses
  cpu,sysutils;

var
  i1,i2,i3,i4 : int128rec;

begin
  writeln('Start');
  i1.lo:=11;
  i1.hi:=12;
  i2.lo:=21;
  i2.hi:=22;
  i3:=i1;
  i4.lo:=0;
  i4.hi:=0;
  i4:=InterlockedCompareExchange128(i1,i2,i3);
  {
  writeln(i4.lo);
  writeln(i4.hi);
  writeln(i1.lo);
  writeln(i1.hi);
  writeln(i2.lo);
  writeln(i2.hi);
  }
  if (i4.lo<>11) or (i4.hi<>12) or (i1.lo<>i2.lo) or (i1.hi<>i2.hi) then
    halt(1);
  writeln('ok');
end.
