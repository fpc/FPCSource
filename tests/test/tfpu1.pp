{ %CPU=I386 }
program test_fp_instructions;


  function test : extended;

  var
    x,y : integer;
    statusword,controlword : word;
    z,t : longint;
    a,b,c : comp;
    begin
       x:=5;
       c:=5;
       t:=5;
       z:=4;
       a:=20;
       { test all FPU instructions using 's' and 'l' suffix
         for word and dword size PM }
{$asmmode att}
       asm
         fildl z
         fiadds x
         fistpq b
         fildl z
         ficoms x
         fistpq b
         fildl z
         ficomps x
         fildl z
         fidivs x
         fistpq b
         fildl z
         fidivrs x
         fistpq b
         fildl z
         fisubs x
         fistpq b
         fildl z
         fisubrs x
         fistpq b
         fildl z
         fimuls x
         fistpq b
       end;
       if a<>b then
         begin
           Writeln('Error in FPU att syntax code generation');
           Halt(1);
         end;
       asm
         fildl z
         fiaddl t
         fistpq b
         fildl z
         ficoml t
         fistpq b
         fildl z
         ficompl t
         fildl z
         fidivl t
         fistpq b
         fildl z
         fidivrl t
         fistpq b
         fildl z
         fisubl t
         fistpq b
         fildl z
         fisubrl t
         fistpq b
         fildl z
         fimull t
         fistpq b
       end;
       if a<>b then
         begin
           Writeln('Error in FPU att syntax code generation');
           Halt(1);
         end;
       { test CW and SW instructions }
       { FSTSW FNSTSW
         FLDCW FSTCW FNSTCW }
       asm
         fstsw statusword
         fstsww statusword
         fnstsw statusword
         fnstsww statusword
         fstcw controlword
         fstcww controlword
         fnstcw controlword
         fnstcww controlword
         fldcw controlword
         fldcww controlword
       end;
{$asmmode intel}
       asm
         fild dword ptr z
         fimul dword ptr t
         fistp qword ptr b
         fild dword ptr z
         fimul word ptr x
         fistp qword ptr b
       end;
       if a<>b then
         begin
           Writeln('Error in FPU code generation');
           Halt(1);
         end;
       { test CW and SW instructions }
       asm
         fstsw word ptr [statusword]
         fnstsw word ptr [statusword]
         fstcw word ptr [controlword]
         fnstcw word ptr[controlword]
         fldcw word ptr [controlword]
       end;
       test:=b;
    end;

var
  z : extended;

begin
  z:=test;
end.
