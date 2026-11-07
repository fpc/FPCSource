{ %CPU=i386 }
{ %OPT=-O1 }
program tb0772;

{ A by-value parameter of 16 bytes or more is copied onto the stack with a call
  to FPC_MOVE after "lea -16(%esp),%esp; mov %esp,%edx".  In a routine that
  contains an assembler block, -O1 turned that pair into "lea -16(%esp),%edx",
  so the stack pointer was never lowered: the call to FPC_MOVE stored its return
  address inside the destination and FPC_MOVE returned to the parameter's last
  four bytes. }

type
  TRec = record
    a, b, c, d: longint;
  end;

function Sum(r: TRec; k: longint): longint; stdcall;
begin
  Sum := r.a * 10000 + r.b * 1000 + r.c * 100 + r.d * 10 + k;
end;

function CallWithAsm(const r: TRec): longint;
begin
  asm
    nop
  end;
  CallWithAsm := Sum(r, 5);
end;

var
  r: TRec;
begin
  r.a := 1;
  r.b := 2;
  r.c := 3;
  r.d := 4;
  if CallWithAsm(r) <> 12345 then
    begin
      writeln('FAIL: wrong result');
      halt(1);
    end;
  writeln('ok');
end.
