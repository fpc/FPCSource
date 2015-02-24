{ Old file: tbs0079.pp }
{  Shows problems with stackframe with assembler keyword OK 0.99.1 (CEC) }
{ This test does not really
  give a good result
  because you need to look into
  the assembler to see if there is an error or not :( PM }

{$ifdef CPUI386}
{$asmmode intel}
{$endif CPUI386}

procedure nothing(x,y: longint);assembler;
{$ifdef CPUI386}
{$define SUPPORTED}
asm
  mov eax,x
  mov ebx,y
end;
{$endif CPUI386}
{$ifdef CPU68K}
{$define SUPPORTED}
asm
  move.l  x,d0
  move.l  y,d1
end;
{$endif CPU68K}
{$ifdef CPUPOWERPC}
{$define SUPPORTED}
asm
  mr r5,x
  mr r6,y
end;
{$endif CPUPOWERPC}
{$ifdef CPUARM}
{$define SUPPORTED}
asm
  mov r2,x
  mov r3,y
end;
{$endif CPUARM}
{$ifdef CPUX86_64}
{$define SUPPORTED}
asm
  movl x,%eax
  movl y,%ecx
end;
{$endif CPUX86_64}
{$ifdef CPUSPARC}
{$define SUPPORTED}
asm
  mov x,%i0
  mov y,%i1
end;
{$endif CPUSPARC}
{$ifdef CPUMIPS}
{$define SUPPORTED}
asm
  move $t1,x
  move $t2,y
end;
{$endif CPUMIPS}
{$ifdef CPUAARCH64}
{$define SUPPORTED}
asm
  mov w2, x
  mov w3, y
end;
{$endif CPUAARCH64}
{$ifndef SUPPORTED}
asm
end;
{$endif ndef SUPPORTED}

{procedure nothing(x,y: longint);
begin
 asm
  mov eax,x
  mov ebx,y
 end;
end; }

Begin
{$ifndef SUPPORTED}
  Writeln('The CPU of this test is not supported by test tbs/tb0072.pp');
  Writeln('Please add assembler code corresponding to this CPU to source');
  halt(1);
{$endif ndef SUPPORTED}
end.
