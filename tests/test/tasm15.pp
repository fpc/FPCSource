{ %CPU=i386 }

const
  expect: array [0..41] of byte = (
    $C3,              // ret
    $C2,$05,$00,      // ret 5
    $C3,              // retn
    $C2,$05,$00,      // retn 5
    $CB,              // retf
    $CA,$05,$00,      // retf 5
    $66,$C3,          // retw
    $66,$C2,$05,$00,  // retw 5
    $66,$C3,          // retnw
    $66,$C2,$05,$00,  // retnw 5
    $66,$CB,          // retfw
    $66,$CA,$05,$00,  // retfw 5
    $C3,              // retd
    $C2,$05,$00,      // retd 5
    $C3,              // retnd
    $C2,$05,$00,      // retnd 5
    $CB,              // retfd
    $CA,$05,$00       // retfd 5
  );

{$asmmode intel}
procedure test; assembler; nostackframe;
asm
  ret
  ret 5
  retn
  retn 5
  retf
  retf 5
  retw
  retw 5
  retnw
  retnw 5
  retfw
  retfw 5
  retd
  retd 5
  retnd
  retnd 5
  retfd
  retfd 5
end;

{$asmmode att}
procedure test2; assembler; nostackframe;
asm
  ret
  ret $5
  ret
  ret $5
  lret
  lret $5
  retw
  retw $5
  retw
  retw $5
  lretw
  lretw $5
  ret
  ret $5
  ret
  ret $5
  lret
  lret $5
end;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

begin
  if CompareByte(Pointer(@test)^, expect, SizeOf(expect)) <> 0 then
    Error;
  if CompareByte(Pointer(@test2)^, expect, SizeOf(expect)) <> 0 then
    Error;
  Writeln('Ok!')
end.
