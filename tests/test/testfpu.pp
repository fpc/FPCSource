program test_fp_instructions;


  function test : extended;

  var
    x,y : integer;
    z,t : longint;
    a,b,c : comp;
    begin
       x:=5;
       c:=5;
       t:=5;
       z:=4;
       a:=20;
{$asmmode att}
       asm
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
         fimull t
         fistpq b
       end;
       if a<>b then
         begin
           Writeln('Error in FPU att syntax code generation');
           Halt(1);
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
       test:=b;
    end;

var
  z : extended;

begin
  z:=test;
end.