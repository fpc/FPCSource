{ %CPU=i386 }
{$R-}
program test_register_pushing;

var
  before, after : longint;
  wpush,lpush : longint;
const
  haserror : boolean = false;

begin
{$ifdef CPUI386}
{$asmmode att}
  asm
    movl   %esp,before
    pushw  %es
    movl   %esp,after
    popw   %es
  end;
  wpush:=before-after;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %es" into 2 bytes');
      haserror:=true;
    end;
  asm
    movl   %esp,before
    pushl  %es
    movl   %esp,after
    popl   %es
  end;
  lpush:=before-after;

  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %es" into 4 bytes');
      haserror:=true;
    end;

  asm
    movl   %esp,before
    pushw  %gs
    movl   %esp,after
    popw   %gs
  end;
  wpush:=before-after;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %gs" into 2 bytes');
      haserror:=true;
    end;
  asm
    movl   %esp,before
    pushl  %gs
    movl   %esp,after
    popl   %gs
  end;
  lpush:=before-after;

  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %gs" into 4 bytes');
      haserror:=true;
    end;
{$asmmode intel}
  asm
    mov    before,esp
    push   es
    mov    after,esp
    pop    es
  end;
  Writeln('Intel "push es" uses ',before-after,' bytes');
{$endif CPUI386}
  if haserror then
    Halt(1);
end.
