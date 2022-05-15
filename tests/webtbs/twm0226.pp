{ %cpu=x86_64 }

program test;

type
  rec1 = record
    onlyfield: Single;
  end;

  rec2 = record
    onlyfield: Double;
  end;

function fn(a1: rec1; a2: rec2): boolean; ms_abi_cdecl; assembler; nostackframe;
asm
  xorl %eax, %eax
  cmpl $0x3f800000, %ecx
  jne .Lend
  movq $0x4000000000000000, %rcx
  cmpq %rcx,%rdx
  jne .Lend
  movl $1, %eax
.Lend:
end;

var
  myrec1: rec1;
  myrec2: rec2;
begin
  myrec1.onlyfield := 1.0;
  myrec2.onlyfield := 2.0;
  if not fn(myrec1, myrec2) then
    halt(1);
end.
