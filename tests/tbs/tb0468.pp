{ %CPU=i386 }
{ %OPT=-Sg }
program tb0468;

{$asmmode intel}

procedure x;

label a;

var b:pointer;

begin
  b:=@a;
a:
end;

procedure jumptabproc; assembler;

label a,b,c,d;

const jumptable:array[0..3] of pointer=(@a,@b,@b,@d);

asm
a:
 nop
b:
 nop
c:
 nop
d:
 nop
end;

begin
end.
