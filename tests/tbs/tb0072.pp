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
asm
  mov eax,x
  mov ebx,y
end;
{$endif CPUI386}
{$ifdef CPU68K}
asm
  move.l  x,d0
  move.l  y,d1
end;
{$endif CPU68K}
{$ifdef CPUPOWERPC}
asm
  mr r5,x
  mr r6,y
end;
{$endif CPUPOWERPC}
{$ifdef CPUARM}
asm
  mov r2,x
  mov r3,y
end;
{$endif CPUARM}
{$ifdef CPUX86_64}
asm
  movl x,%eax
  movl y,%ecx
end;
{$endif CPUX86_64}
{$ifdef CPUSPARC}
asm
  mov x,%i0
  mov y,%i1
end;
{$endif CPUSPARC}

{procedure nothing(x,y: longint);
begin
 asm
  mov eax,x
  mov ebx,y
 end;
end; }

Begin
end.
