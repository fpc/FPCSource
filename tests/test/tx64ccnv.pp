{ %cpu=x86_64 }

function proc_msabidefault(para1,para2,para3,para4: qword): boolean; ms_abi_default;
assembler;
asm
  cmpq $1, %rcx
  jne .Lerror
  cmpq $2, %rdx
  jne .Lerror
  cmpq $3, %r8
  jne .Lerror
  cmpq $4, %r9
  jne .Lerror
  movq $1,%rax
  jmp .Lok
.Lerror:
  movq $0,%rax
.Lok:
end;

function proc_msabicdecl(para1,para2,para3,para4: qword): boolean; ms_abi_cdecl;
assembler;
asm
  cmpq $1, %rcx
  jne .Lerror
  cmpq $2, %rdx
  jne .Lerror
  cmpq $3, %r8
  jne .Lerror
  cmpq $4, %r9
  jne .Lerror
  movq $1,%rax
  jmp .Lok
.Lerror:
  movq $0,%rax
.Lok:
end;


function proc_sysvabidefault(para1,para2,para3,para4,para5,para6: qword): boolean; sysv_abi_default;
assembler;
asm
  cmpq $1, %rdi
  jne .Lerror
  cmpq $2, %rsi
  jne .Lerror
  cmpq $3, %rdx
  jne .Lerror
  cmpq $4, %rcx
  jne .Lerror
  cmpq $5, %r8
  jne .Lerror
  cmpq $6, %r9
  jne .Lerror
  movq $1,%rax
  jmp .Lok
.Lerror:
  movq $0,%rax
.Lok:
end;


function proc_sysvabicdecl_extern(para1,para2,para3,para4,para5,para6: qword): boolean; sysv_abi_cdecl; varargs; external name '_FPC_PROCC_SYSVABICDECL';

function proc_sysvabicdecl(para1,para2,para3,para4,para5,para6: qword): boolean; sysv_abi_cdecl; [public, alias: '_FPC_PROCC_SYSVABICDECL'];
assembler;
asm
  cmpb $0, %al
  jne .Lerror
  cmpq $1, %rdi
  jne .Lerror
  cmpq $2, %rsi
  jne .Lerror
  cmpq $3, %rdx
  jne .Lerror
  cmpq $4, %rcx
  jne .Lerror
  cmpq $5, %r8
  jne .Lerror
  cmpq $6, %r9
  jne .Lerror
  movq $1,%rax
  jmp .Lok
.Lerror:
  movq $0,%rax
.Lok:
end;

begin
  if not proc_msabidefault(1,2,3,4) then
    halt(1);
  if not proc_msabicdecl(1,2,3,4) then
    halt(2);
  if not proc_sysvabidefault(1,2,3,4,5,6) then
    halt(3);
  if not proc_sysvabicdecl_extern(1,2,3,4,5,6) then
    halt(4);
end.
