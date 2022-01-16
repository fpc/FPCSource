{ %cpu=x86_64 }
program tw29010b;

{$ifdef fpc}
  {$asmmode intel}
{$else fpc}
  {$apptype console}
{$endif}

type
  PRspStruct = ^TRspStruct;
  TRspStruct = record
    rsp_initial: qword;
    rsp_after_push: qword;
    rsp_final: qword;
  end;

var
  Error: Boolean;

  Rsp: PRspStruct;

  global_proc: procedure;
  global_word: word;
  global_int64: int64;

procedure check_rsps(bytes: int64);
begin
  with Rsp^ do
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

  local_rspstruct: PRspStruct;
begin
  local_rspstruct := Rsp;
{$ifndef FPC_PIC}
  Writeln('testing push/pop global_proc');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push global_proc
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop global_proc
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_qword
  end;

  Writeln('testing push/pop word [global_proc]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word [global_proc]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word [global_proc]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr global_proc');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr global_proc
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr global_proc
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_proc]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr [global_proc]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr [global_proc]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop global_word');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push global_word
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop global_word
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop global_int64');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push global_int64
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop global_int64
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_qword
  end;

  Writeln('testing push/pop word [global_int64]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word [global_int64]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word [global_int64]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr global_int64');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr global_int64
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr global_int64
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr [global_int64]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr [global_int64]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr [global_int64]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;
{$endif FPC_PIC}

  Writeln('testing push/pop local_proc');
  asm
    mov rbx, local_rspstruct
    mov [rbx+Trspstruct.rsp_initial], rsp
    push local_proc
    mov [rbx+Trspstruct.rsp_after_push], rsp
    pop local_proc
    mov [rbx+Trspstruct.rsp_final], rsp
    mov rsp, [rbx+Trspstruct.rsp_initial]
    call check_qword
  end;

  Writeln('testing push/pop word [local_proc]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word [local_proc]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word [local_proc]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr local_proc');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr local_proc
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr local_proc
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_proc]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr [local_proc]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr [local_proc]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop local_word');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push local_word
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop local_word
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop local_int64');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push local_int64
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop local_int64
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_qword
  end;

  Writeln('testing push/pop word [local_int64]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word [local_int64]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word [local_int64]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr local_int64');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr local_int64
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr local_int64
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;

  Writeln('testing push/pop word ptr [local_int64]');
  asm
    mov rbx, local_rspstruct
    mov [rbx + TRspStruct.rsp_initial], rsp
    push word ptr [local_int64]
    mov [rbx + TRspStruct.rsp_after_push], rsp
    pop word ptr [local_int64]
    mov [rbx + TRspStruct.rsp_final], rsp
    mov rsp, [rbx + TRspStruct.rsp_initial]
    call check_word
  end;
end;

begin
  Error := False;
  New(Rsp);
  testproc;
  Dispose(Rsp);
  if Error then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
