{ %cpu=x86_64 }
program tw29010b;

{$ifdef fpc}
  {$asmmode intel}
{$else fpc}
  {$apptype console}
{$endif}

var
  Error: Boolean;

  rsp_initial: qword;
  rsp_after_push: qword;
  rsp_final: qword;

  global_proc: procedure;
  global_word: word;
  global_int64: int64;

procedure check_rsps(bytes: int64);
begin
  if (rsp_initial - rsp_after_push) <> bytes then
  begin
    Writeln('Wrong push size, expected ', bytes, ', got ', rsp_initial - rsp_after_push);
    Error := True;
  end;
  if (rsp_final - rsp_after_push) <> bytes then
  begin
    Writeln('Wrong pop size, expected ', bytes, ', got ', rsp_final - rsp_after_push);
    Error := True;
  end;
end;

procedure check_word;
begin
  check_rsps(2);
end;

procedure check_qword;
begin
  check_rsps(8);
end;

procedure testproc;
var
  local_proc: procedure;
  local_word: word;
  local_int64: int64;
begin
  Writeln('testing push/pop global_proc');
  asm
    mov rsp_initial, rsp
    push global_proc
    mov rsp_after_push, rsp
    pop global_proc
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_qword
  end;

  Writeln('testing push/pop word [global_proc]');
  asm
    mov rsp_initial, rsp
    push word [global_proc]
    mov rsp_after_push, rsp
    pop word [global_proc]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;
  
  Writeln('testing push/pop word ptr global_proc');
  asm
    mov rsp_initial, rsp
    push word ptr global_proc
    mov rsp_after_push, rsp
    pop word ptr global_proc
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_proc]');
  asm
    mov rsp_initial, rsp
    push word ptr [global_proc]
    mov rsp_after_push, rsp
    pop word ptr [global_proc]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop global_word');
  asm
    mov rsp_initial, rsp
    push global_word
    mov rsp_after_push, rsp
    pop global_word
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop global_int64');
  asm
    mov rsp_initial, rsp
    push global_int64
    mov rsp_after_push, rsp
    pop global_int64
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_qword
  end;

  Writeln('testing push/pop word [global_int64]');
  asm
    mov rsp_initial, rsp
    push word [global_int64]
    mov rsp_after_push, rsp
    pop word [global_int64]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr global_int64');
  asm
    mov rsp_initial, rsp
    push word ptr global_int64
    mov rsp_after_push, rsp
    pop word ptr global_int64
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_int64]');
  asm
    mov rsp_initial, rsp
    push word ptr [global_int64]
    mov rsp_after_push, rsp
    pop word ptr [global_int64]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop local_proc');
  asm
    mov rsp_initial, rsp
    push local_proc
    mov rsp_after_push, rsp
    pop local_proc
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_qword
  end;

  Writeln('testing push/pop word [local_proc]');
  asm
    mov rsp_initial, rsp
    push word [local_proc]
    mov rsp_after_push, rsp
    pop word [local_proc]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr local_proc');
  asm
    mov rsp_initial, rsp
    push word ptr local_proc
    mov rsp_after_push, rsp
    pop word ptr local_proc
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_proc]');
  asm
    mov rsp_initial, rsp
    push word ptr [local_proc]
    mov rsp_after_push, rsp
    pop word ptr [local_proc]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop local_word');
  asm
    mov rsp_initial, rsp
    push local_word
    mov rsp_after_push, rsp
    pop local_word
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop local_int64');
  asm
    mov rsp_initial, rsp
    push local_int64
    mov rsp_after_push, rsp
    pop local_int64
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_qword
  end;

  Writeln('testing push/pop word [local_int64]');
  asm
    mov rsp_initial, rsp
    push word [local_int64]
    mov rsp_after_push, rsp
    pop word [local_int64]
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr local_int64');
  asm
    mov rsp_initial, rsp
    push word ptr local_int64
    mov rsp_after_push, rsp
    pop word ptr local_int64
    mov rsp_final, rsp
    mov rsp, rsp_initial
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_int64]');
  asm
    mov rsp_initial, rsp
    push word ptr [local_int64]
    mov rsp_after_push, rsp
    pop word ptr [local_int64]
    mov rsp_final, rsp
    mov rsp, rsp_initial
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
