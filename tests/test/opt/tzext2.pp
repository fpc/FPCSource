{ %CPU=x86_64 }
{ %OPT=-O1 }
{ a 32-bit store followed by a load of the same location must not be reduced
  to the store if the upper 32 bits of the register are not zero: the load
  clears them (MovMov2Mov 1) }
program tzext2;

function f1(a: qword; c: byte): qword; noinline;
var
  d: dword;
begin
  d:=a shr c;
  f1:=qword(d)+a;
end;

function f2(a: qword): qword; noinline;
var
  d: dword;
begin
  d:=a;
  f2:=qword(d)+a;
end;

function f3(a, b: qword): qword; noinline;
var
  d: dword;
begin
  d:=a+b;
  f3:=qword(d) or (a shl 1);
end;

begin
  if f1($fedcba9876543210,4)<>$fedcba9876543210+$87654321 then
    halt(1);
  if f2($123456789abcdef0)<>$123456789abcdef0+$9abcdef0 then
    halt(2);
  if f3($100000001,$100000002)<>(qword(3) or ($100000001 shl 1)) then
    halt(3);
  writeln('ok');
end.
