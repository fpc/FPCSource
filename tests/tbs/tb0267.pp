{ %CPU=i386 }
{ Old file: tbs0309.pp }
{ problem with ATT assembler written by bin writer     OK 0.99.14 (PFV) }

{ This code was first written by Florian
  to test the GDB output for FPU
  he thought first that FPU output was wrong
  but in fact it is a bug in FPC :( }
program bug0309;

var
   a,b : double;
   _as,bs : single;
   al,bl : extended;
   aw,bw : integer;
   ai,bi : longint;
   ac : comp;
begin
{$ifdef CPU86}
{$asmmode att}
   asm
      fninit;
   end;
   a:=1;
   b:=2;
   asm
      movl  $1,%eax
      fldl  a
      fldl  b
      faddp %st,%st(1)
      fstpl a
   end;
   { the above generates wrong code in binary writer
     fldl is replaced by flds !!
     if using -alt option to force assembler output
     all works correctly PM }
   writeln('a = ',a,' should be 3');
   if a<>3.0 then
     Halt(1);
   a:=1.0;
   a:=a+b;
   writeln('a = ',a,' should be 3');
   _as:=0;
   al:=0;
   asm
     fldl a
     fsts _as
     fstpt al
   end;
   if (_as<>3.0) or (al<>3.0) then
     Halt(1);
   ai:=5;
   bi:=5;
   asm
     fildl ai
     fstpl a
   end;
   if a<>5.0 then
     Halt(1);

   ac:=5;
   asm
     fildl ai
     fstpl a
   end;
   if a<>5.0 then
     Halt(1);
   aw:=-4;
   bw:=45;
   asm
     fildw aw
     fstpl a
   end;
   if a<>-4.0 then
     Halt(1);
   ac:=345;
   asm
     fildq ac
     fstpl a
   end;
   if a<>345.0 then
     Halt(1);

{$endif CPU86}
end.
