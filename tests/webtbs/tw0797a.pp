{ %CPU=i386 }
program test;
{$INLINE ON}
{$ASMMODE ATT}

var
  j : longint;

  procedure Tst(var j : longint); assembler;inline;
  var
    i : longint;
  asm
    movl j,%ebx
    movl (%ebx),%eax
    movl $5,i
    addl i,%eax
    movl %eax,(%ebx)
  end;

begin
   j:=5;
   Tst(j);
   if (j<>10) then
     begin
       halt(1);
     end;
end.
