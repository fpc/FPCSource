{ This code was first written by Florian
  to test the GDB output for FPU
  he thought first that FPU output was wrong
  but in fact it is a bug in FPC :( }
program bug0309;

var
   a,b : double;

begin
   asm
      fninit;
   end;
   a:=1;
   b:=2;
   asm
      movl $1,%eax
      fldl a
      fldl b
      fadd
      fstpl a
   end;
   { the above generates wrong code in binary writer
     fldl is replaced by flds !!
     if using -alt option to force assembler output
     all works correctly PM }
   writeln('a = ',a,' should be 3');
   a:=1.0;
   a:=a+b;
   writeln('a = ',a,' should be 3');
end.
