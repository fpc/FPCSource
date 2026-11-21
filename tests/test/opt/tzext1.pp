{ %CPU=x86_64 }
{ %OPT=-O3 }
program tzext1;

procedure dummy; noinline;
begin
end;

function f1(a: qword): qword; noinline;
var
  d: dword;
begin
  d:=hi(a);
  f1:=qword(d)+a;
end;

function f2(a: dword; b: qword): qword; noinline;
var
  d: dword;
begin
  d:=a+1;
  dummy;
  f2:=qword(d)*b;
end;

function f3(a: qword; c: byte): qword; noinline;
var
  d: dword;
begin
  d:=a shr c;
  f3:=qword(d)+a;
end;

function f4(a: dword): qword; noinline;
begin
  if a=0 then
    f4:=255
  else
    f4:=qword(BsrDWord(a));
end;

begin
  if f1($123456789abcdef0)<>$123456789abcdef0+$12345678 then
    halt(1);
  if f2($ffffffff,3)<>0 then
    halt(2);
  if f2($7ffffffe,2)<>$fffffffe then
    halt(3);
  if f3($fedcba9876543210,4)<>$fedcba9876543210+$87654321 then
    halt(4);
  if f4($80000000)<>31 then
    halt(5);
  if f4(0)<>255 then
    halt(6);
  writeln('ok');
end.
