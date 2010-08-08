{ %CPU=i386 }
{ Source provided for Free Pascal Bug Report 1327 }
{ Submitted by "Fernando Oscar Schmitt" on  2001-01-08 }
{ e-mail: pulp@cpovo.net }
{NOT to execute! ONLY for disassemble!}

{$asmmode intel}

procedure NotToExecute;
begin
  asm
    lea eax,[ebx+1*ecx]
    mov ebx,[ecx+eax*2]
    add ecx,[4*eax+ebx]
    sub edx,[esi*8+edi]

    adc eax,[ebx+1*ecx+100000]
    sbb ebx,[ecx+eax*2+200000]
    movzx ecx,byte ptr [4*eax+ebx+400000]
    movsx edx,word ptr [esi*8+edi+800000]

    and [2*ecx],eax
    xor [edx*4],ebx

    or [2*ecx+20],eax
    not dword ptr [edx*4+40]
  end;
end;

const
  has_error : boolean = false;

procedure test;
var
  x,y : array[0..5] of longint;
  i : longint;
  begin
    for i:=0 to 5 do
      begin
        x[i]:=6*i;
        y[i]:=5*i;
      end;
    asm
      lea edi,x
      lea esi,y
      mov ecx,0
    @LLloop:
      mov ebx,[edi+4*ecx]
      sub ebx,[4*ecx+esi]
      mov [edi+4*ecx],ebx
      inc ecx
      cmp ecx,6
      jne  @LLloop
    end ['esi','edi','ecx','ebx'];
    for i:=0 to 5 do
      if x[i]<>i then
        has_error:=true;
  end;

begin
  test;
  if has_error then
    begin
      Writeln('bug 1327 is not fixed');
      Halt(1);
    end;
end.
