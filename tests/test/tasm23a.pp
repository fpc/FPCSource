{ %CPU=i386 }

program tasm23a;

{$ASMMODE INTEL}
{$S-}

const
  t_size = 19;
procedure t; nostackframe; assembler;
asm
  mov eax, [ebx[5]][edi][54][-17][45][4]      { mov eax, [ebx+edi+5Bh] }
  mov eax, [[ebx+5]+[edi+54]+[-17]+[45]+[4]]  { mov eax, [ebx+edi+5Bh] }
  mov eax, [5[7]]                             { mov eax, [000Ch] }
  mov eax, [5+[7]]                            { mov eax, [000Ch] }
end;
procedure t_verify; nostackframe; assembler;
asm
  mov eax, [ebx+edi+5Bh]  { mov eax, [ebx[5]][edi][54][-17][45][4]     }
  mov eax, [ebx+edi+5Bh]  { mov eax, [[ebx+5]+[edi+54]+[-17]+[45]+[4]] }
  mov eax, [000Ch]        { mov eax, [5[7]]                            }
  mov eax, [000Ch]        { mov eax, [5+[7]]                           }
end;

const
  t2_size = 34;
procedure t2; assembler;
var
  locl: longword;
asm
  mov eax, locl                             { mov еax, [еbp-04] }
  mov eax, cs:locl                          { mov еax, cs:[еbp-04] }
  mov eax, [cs:locl]                        { mov еax, cs:[еbp-04] }
  mov eax, [cs:[locl]]                      { mov еax, cs:[еbp-04] }
  mov eax, cs:locl[5]                       { mov еax, cs:[еbp+01] }
  mov eax, cs:5[locl]                       { mov еax, cs:[еbp+01] }
end;
procedure t2_verify; assembler;
var
  locl: longword;
asm
  mov eax, [ebp-04]     { mov еax, locl        }
  mov eax, cs:[ebp-04]  { mov еax, cs:locl     }
  mov eax, cs:[ebp-04]  { mov еax, [cs:locl]   }
  mov eax, cs:[ebp-04]  { mov еax, [cs:[locl]] }
  mov eax, cs:[ebp+01]  { mov еax, cs:locl[5]  }
  mov eax, cs:[ebp+01]  { mov еax, cs:5[locl]  }
end;

{ This version works in all i8086 memory models }
function CompareCode(cp, cp2: CodePointer; sz: Integer): Boolean;
var
  lastbyte: Byte;
begin
  if CompareByte(cp^, cp2^, sz) <> 0 then
  begin
    CompareCode := False;
    exit;
  end;
  { check also that the last byte is a retn instruction }
  lastbyte:=PByte(cp)[sz-1];
  if lastbyte<>$C3 then
  begin
    CompareCode := False;
    exit;
  end;
  CompareCode := True;
end;

procedure Error(N: Integer);
begin
  Writeln('Error! ', N);
  Halt(1);
end;

begin
  if not CompareCode(CodePointer(@t), CodePointer(@t_verify), t_size) then
    Error(1);
  if not CompareCode(CodePointer(@t2), CodePointer(@t2_verify), t2_size) then
    Error(2);

  Writeln('Ok!');
end.
