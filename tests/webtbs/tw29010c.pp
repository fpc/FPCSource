{ %cpu=i8086 }
program tw29010c;

var
  Error: Boolean;

  sp_initial: word;
  sp_after_push: word;
  sp_final: word;

  global_proc: procedure;
  global_word: word;
  global_longint: longint;

procedure check_esps(bytes: word);
begin
  if (sp_initial - sp_after_push) <> bytes then
  begin
    Writeln('Wrong push size, expected ', bytes, ', got ', sp_initial - sp_after_push);
    Error := True;
  end;
  if (sp_final - sp_after_push) <> bytes then
  begin
    Writeln('Wrong pop size, expected ', bytes, ', got ', sp_final - sp_after_push);
    Error := True;
  end;
end;

procedure check_word;
begin
  check_esps(2);
end;

procedure check_dword;
begin
  check_esps(4);
end;

procedure check_proc;
begin
  check_esps(SizeOf(global_proc));
end;

procedure testproc;
var
  local_proc: procedure;
  local_word: word;
  local_longint: longint;
begin
  Writeln('testing push/pop global_proc');
  asm
    mov sp_initial, sp
    push global_proc
    mov sp_after_push, sp
    pop global_proc
    mov sp_final, sp
    mov sp, sp_initial
    call check_proc
  end;

  Writeln('testing push/pop word [global_proc]');
  asm
    mov sp_initial, sp
    push word [global_proc]
    mov sp_after_push, sp
    pop word [global_proc]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;
  
  Writeln('testing push/pop word ptr global_proc');
  asm
    mov sp_initial, sp
    push word ptr global_proc
    mov sp_after_push, sp
    pop word ptr global_proc
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_proc]');
  asm
    mov sp_initial, sp
    push word ptr [global_proc]
    mov sp_after_push, sp
    pop word ptr [global_proc]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop global_word');
  asm
    mov sp_initial, sp
    push global_word
    mov sp_after_push, sp
    pop global_word
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop global_longint');
  asm
    mov sp_initial, sp
    push global_longint
    mov sp_after_push, sp
    pop global_longint
    mov sp_final, sp
    mov sp, sp_initial
    call check_dword
  end;

  Writeln('testing push/pop word [global_longint]');
  asm
    mov sp_initial, sp
    push word [global_longint]
    mov sp_after_push, sp
    pop word [global_longint]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr global_longint');
  asm
    mov sp_initial, sp
    push word ptr global_longint
    mov sp_after_push, sp
    pop word ptr global_longint
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_longint]');
  asm
    mov sp_initial, sp
    push word ptr [global_longint]
    mov sp_after_push, sp
    pop word ptr [global_longint]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop local_proc');
  asm
    mov sp_initial, sp
    push local_proc
    mov sp_after_push, sp
    pop local_proc
    mov sp_final, sp
    mov sp, sp_initial
    call check_proc
  end;

  Writeln('testing push/pop word [local_proc]');
  asm
    mov sp_initial, sp
    push word [local_proc]
    mov sp_after_push, sp
    pop word [local_proc]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr local_proc');
  asm
    mov sp_initial, sp
    push word ptr local_proc
    mov sp_after_push, sp
    pop word ptr local_proc
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_proc]');
  asm
    mov sp_initial, sp
    push word ptr [local_proc]
    mov sp_after_push, sp
    pop word ptr [local_proc]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop local_word');
  asm
    mov sp_initial, sp
    push local_word
    mov sp_after_push, sp
    pop local_word
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop local_longint');
  asm
    mov sp_initial, sp
    push local_longint
    mov sp_after_push, sp
    pop local_longint
    mov sp_final, sp
    mov sp, sp_initial
    call check_dword
  end;

  Writeln('testing push/pop word [local_longint]');
  asm
    mov sp_initial, sp
    push word [local_longint]
    mov sp_after_push, sp
    pop word [local_longint]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr local_longint');
  asm
    mov sp_initial, sp
    push word ptr local_longint
    mov sp_after_push, sp
    pop word ptr local_longint
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_longint]');
  asm
    mov sp_initial, sp
    push word ptr [local_longint]
    mov sp_after_push, sp
    pop word ptr [local_longint]
    mov sp_final, sp
    mov sp, sp_initial
    call check_word
  end;
end;

begin
  Error := False;
  testproc;
  if Error then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
