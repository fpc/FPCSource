{ %opt=-gh }

{$mode objfpc}

program test;
type
  tr = record
    b: byte;
    str: ansistring;
    b2,b3: byte;
  end;
  pr = ^tr;
var
  A: pr;
begin
  HaltOnNotReleased:=true;
  getmem(a,sizeof(tr)*4);
  Initialize(a^, 4);
  a[0].str:='test';
  a[0].str:=a[0].str+'ab';
  a[1].str:='test';
  a[1].str:=a[1].str+'ab';
  a[2].str:='test';
  a[2].str:=a[2].str+'ab';
  a[3].str:='test';
  a[3].str:=a[3].str+'ab';
  Finalize(A[1], 2);
  if (a[0].str<>'testab') then
    halt(1);
  if (a[1].str<>'') then
    halt(2);
  if (a[2].str<>'') then
    halt(3);
  if (a[3].str<>'testab') then
    halt(4);
  Finalize(a^,4);
  freemem(a);
end.
