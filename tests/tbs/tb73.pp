{ %OPT= -Rintel }

{ Old file: tbs0079.pp }
{  Shows problems with stackframe with assembler keyword OK 0.99.1 (CEC) }

procedure nothing(x,y: longint);assembler;
asm
  mov eax,x
  mov ebx,y
end;


{procedure nothing(x,y: longint);
begin
 asm
  mov eax,x
  mov ebx,y
 end;
end; }

Begin
end.
