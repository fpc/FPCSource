{ %CPU=i386 }
{$R-}
program test_register_pushing;

const
  haserror : boolean = false;
  
procedure dotest;
var
  wpush,lpush: longint;
begin
{$asmmode att}
  asm
    movl   %esp,wpush
    pushw  %es
    subl   %esp,wpush
    popw   %es
  end;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %es" into 2 bytes');
      haserror:=true;
    end;
    
  asm
    movl   %esp,lpush
    pushl  %es
    subl   %esp,lpush
    popl   %es
  end;
  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %es" into 4 bytes');
      haserror:=true;
    end;

  asm
    movl   %esp,wpush
    pushw  %gs
    subl   %esp,wpush
    popw   %gs
  end;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %gs" into 2 bytes');
      haserror:=true;
    end;
  asm
    movl   %esp,lpush
    pushl  %gs
    subl   %esp,lpush
    popl   %gs
  end;
  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %gs" into 4 bytes');
      haserror:=true;
    end;
{$asmmode intel}
  asm
    mov    lpush,esp
    push   es
    sub    lpush,esp
    pop    es
  end;
  Writeln('Intel "push es" uses ',lpush,' bytes');
  if haserror then
    Halt(1);
end;

begin
  dotest;
end.
