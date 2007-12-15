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
{$ifndef FPC_PIC}
    movl   %esp,before
    pushw  %es
    movl   %esp,after
    popw   %es
{$else not FPC_PIC}
    call   .LPIC
.LPIC:
    popl   %ecx
{$ifdef darwin}
    movl   %esp,before-.LPIC(%ecx)
    pushw  %es
    movl   %esp,after-.LPIC(%ecx)
    popw   %es
{$else darwin}
    addl   $_GLOBAL_OFFSET_TABLE_,%ecx
    movl   %esp,before@GOT(%ecx)
    pushw  %es
    movl   %esp,after@GOT(%ecx)
    popw   %es
{$endif darwin}
{$endif not FPC_PIC}
  end;
  wpush:=before-after;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %es" into 2 bytes');
      haserror:=true;
    end;
  asm
{$ifndef FPC_PIC}
    movl   %esp,before
    pushl  %es
    movl   %esp,after
    popl   %es
{$else not FPC_PIC}
    call   .LPIC
.LPIC:
    popl   %ecx
{$ifdef darwin}
    movl   %esp,before-.LPIC(%ecx)
    pushl  %es
    movl   %esp,after-.LPIC(%ecx)
    popl   %es
{$else darwin}
    addl   $_GLOBAL_OFFSET_TABLE_,%ecx
    movl   %esp,before@GOT(%ecx)
    pushl  %es
    movl   %esp,after@GOT(%ecx)
    popl   %es
{$endif darwin}
{$endif not FPC_PIC}
  end;
  lpush:=before-after;

  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %es" into 4 bytes');
      haserror:=true;
    end;

  asm
{$ifndef FPC_PIC}
    movl   %esp,before
    pushw  %gs
    movl   %esp,after
    popw   %gs
{$else not FPC_PIC}
    call   .LPIC
.LPIC:
    popl   %ecx
{$ifdef darwin}
    movl   %esp,before-.LPIC(%ecx)
    pushw  %gs
    movl   %esp,after-.LPIC(%ecx)
    popw   %gs
{$else darwin}
    addl   $_GLOBAL_OFFSET_TABLE_,%ecx
    movl   %esp,before@GOT(%ecx)
    pushw  %gs
    movl   %esp,after@GOT(%ecx)
    popw   %gs
{$endif darwin}
{$endif not FPC_PIC}
  end;
  wpush:=before-after;
  if wpush<>2 then
    begin
      Writeln('Compiler does not push "pushw %gs" into 2 bytes');
      haserror:=true;
    end;
  asm
{$ifndef FPC_PIC}
    movl   %esp,before
    pushl  %gs
    movl   %esp,after
    popl   %gs
{$else not FPC_PIC}
    call   .LPIC
.LPIC:
    popl   %ecx
{$ifdef darwin}
    movl   %esp,before-.LPIC(%ecx)
    pushl  %gs
    movl   %esp,after-.LPIC(%ecx)
    popl   %gs
{$else darwin}
    addl   $_GLOBAL_OFFSET_TABLE_,%ecx
    movl   %esp,before@GOT(%ecx)
    pushl  %gs
    movl   %esp,after@GOT(%ecx)
    popl   %gs
{$endif darwin}
{$endif not FPC_PIC}
  end;
  lpush:=before-after;

  if lpush<>4 then
    begin
      Writeln('Compiler does not push "pushl %gs" into 4 bytes');
      haserror:=true;
    end;
{$asmmode intel}
  asm
{$ifndef FPC_PIC}
    mov    before,esp
    push   es
    mov    after,esp
    pop    es
{$else not FPC_PIC}
    call   @@LPIC
@@LPIC:
    pop    ecx
{$ifdef darwin}
    mov    [before-@@LPIC+ecx],esp
    push   es
    mov    [after-@@LPIC+ecx],esp
    pop    es
{$else darwin}
    add    ecx,@_GLOBAL_OFFSET_TABLE_
    mov    [ecx].OFFSET before,esp
    push   es
    mov    [ecx].OFFSET after,esp
    pop    es
{$endif darwin}
{$endif not FPC_PIC}
  end;
  Writeln('Intel "push es" uses ',before-after,' bytes');
{$endif CPUI386}
  if haserror then
    Halt(1);
end.
