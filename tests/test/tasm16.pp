{ %CPU=i8086,i386,x86_64 }

program tasm16;

{$ifdef cpui8086}
{$asmcpu 80386}
{$endif cpui8086}

const
{$ifdef cpui8086}
  expect1: array [0..3] of byte = (
    $2E,              // segcs
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
  expect2: array [0..3] of byte = (
    $3E,              // segds
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
  expect3: array [0..3] of byte = (
    $36,              // segss
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
  expect4: array [0..3] of byte = (
    $26,              // seges
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
  expect5: array [0..3] of byte = (
    $64,              // segfs
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
  expect6: array [0..3] of byte = (
    $65,              // seggs
    $67,$66,          // db $67,$66
    $AD               // lodsw
  );
{$else}
  expect1: array [0..4] of byte = (
    $2E,              // segcs
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
  expect2: array [0..4] of byte = (
    $3E,              // segds
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
  expect3: array [0..4] of byte = (
    $36,              // segss
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
  expect4: array [0..4] of byte = (
    $26,              // seges
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
  expect5: array [0..4] of byte = (
    $64,              // segfs
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
  expect6: array [0..4] of byte = (
    $65,              // seggs
    $67,$66,          // db $67,$66
    $66,$AD           // lodsw
  );
{$endif}
  expect7: array [0..0] of byte = (
    $2E               // segcs
  );
  expect8: array [0..0] of byte = (
    $3E               // segds
  );
  expect9: array [0..0] of byte = (
    $36               // segss
  );
  expect10: array [0..0] of byte = (
    $26               // seges
  );
  expect11: array [0..0] of byte = (
    $64               // segfs
  );
  expect12: array [0..0] of byte = (
    $65               // seggs
  );
  expect13: array [0..1] of byte = (
    $2E,$AC           // segcs lodsb
  );
  expect14: array [0..1] of byte = (
    $3E,$AC           // segds lodsb
  );
  expect15: array [0..1] of byte = (
    $36,$AC           // segss lodsb
  );
  expect16: array [0..1] of byte = (
    $26,$AC           // seges lodsb
  );
  expect17: array [0..1] of byte = (
    $64,$AC           // segfs lodsb
  );
  expect18: array [0..1] of byte = (
    $65,$AC           // seggs lodsb
  );
{$ifdef cpui8086}
  expect19: array [0..2] of byte = (
    $F3,              // rep
    $66,              // db $66
    $A5               // movsw
  );
{$else}
  expect19: array [0..3] of byte = (
    $F3,              // rep
    $66,              // db $66
    $66,$A5           // movsw
  );
{$endif}

{$asmmode intel}

procedure test1; assembler; nostackframe;
asm
  segcs; db $67,$66; lodsw
end;
procedure test2; assembler; nostackframe;
asm
  segds; db $67,$66; lodsw
end;
procedure test3; assembler; nostackframe;
asm
  segss; db $67,$66; lodsw
end;
procedure test4; assembler; nostackframe;
asm
  seges; db $67,$66; lodsw
end;
procedure test5; assembler; nostackframe;
asm
  segfs; db $67,$66; lodsw
end;
procedure test6; assembler; nostackframe;
asm
  seggs; db $67,$66; lodsw
end;
procedure test7; assembler; nostackframe;
asm
  segcs
end;
procedure test8; assembler; nostackframe;
asm
  segds
end;
procedure test9; assembler; nostackframe;
asm
  segss
end;
procedure test10; assembler; nostackframe;
asm
  seges
end;
procedure test11; assembler; nostackframe;
asm
  segfs
end;
procedure test12; assembler; nostackframe;
asm
  seggs
end;
procedure test13; assembler; nostackframe;
asm
  segcs lodsb
end;
procedure test14; assembler; nostackframe;
asm
  segds lodsb
end;
procedure test15; assembler; nostackframe;
asm
  segss lodsb
end;
procedure test16; assembler; nostackframe;
asm
  seges lodsb
end;
procedure test17; assembler; nostackframe;
asm
  segfs lodsb
end;
procedure test18; assembler; nostackframe;
asm
  seggs lodsb
end;
procedure test19; assembler; nostackframe;
asm
  rep; db $66; movsw
end;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

{$ifdef cpui8086}
{ This version works in all i8086 memory models }
function CompareCode(cp: CodePointer; dp: Pointer; sz: SizeInt): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to sz - 1 do
    if Mem[Seg(cp^):Ofs(cp^) + I] <> Mem[Seg(dp^):Ofs(dp^) + I] then
    begin
      CompareCode := False;
      exit;
    end;
  CompareCode := True;
end;
{$else cpui8086}
function CompareCode(cp: CodePointer; dp: Pointer; sz: SizeInt): Boolean;
begin
  CompareCode := CompareByte(cp^, dp^, sz) = 0;
end;
{$endif}

begin
  if not CompareCode(CodePointer(@test1), @expect1, SizeOf(expect1)) then
    Error;
  if not CompareCode(CodePointer(@test2), @expect2, SizeOf(expect2)) then
    Error;
  if not CompareCode(CodePointer(@test3), @expect3, SizeOf(expect3)) then
    Error;
  if not CompareCode(CodePointer(@test4), @expect4, SizeOf(expect4)) then
    Error;
  if not CompareCode(CodePointer(@test5), @expect5, SizeOf(expect5)) then
    Error;
  if not CompareCode(CodePointer(@test6), @expect6, SizeOf(expect6)) then
    Error;
  if not CompareCode(CodePointer(@test7), @expect7, SizeOf(expect7)) then
    Error;
  if not CompareCode(CodePointer(@test8), @expect8, SizeOf(expect8)) then
    Error;
  if not CompareCode(CodePointer(@test9), @expect9, SizeOf(expect9)) then
    Error;
  if not CompareCode(CodePointer(@test10), @expect10, SizeOf(expect10)) then
    Error;
  if not CompareCode(CodePointer(@test11), @expect11, SizeOf(expect11)) then
    Error;
  if not CompareCode(CodePointer(@test12), @expect12, SizeOf(expect12)) then
    Error;
  if not CompareCode(CodePointer(@test13), @expect13, SizeOf(expect13)) then
    Error;
  if not CompareCode(CodePointer(@test14), @expect14, SizeOf(expect14)) then
    Error;
  if not CompareCode(CodePointer(@test15), @expect15, SizeOf(expect15)) then
    Error;
  if not CompareCode(CodePointer(@test16), @expect16, SizeOf(expect16)) then
    Error;
  if not CompareCode(CodePointer(@test17), @expect17, SizeOf(expect17)) then
    Error;
  if not CompareCode(CodePointer(@test18), @expect18, SizeOf(expect18)) then
    Error;
  if not CompareCode(CodePointer(@test19), @expect19, SizeOf(expect19)) then
    Error;
  Writeln('Ok!')
end.
