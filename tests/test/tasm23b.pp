{ %CPU=x86_64 }

program tasm23b;

{$ASMMODE INTEL}
{$S-}

const
  t_size = 25;
procedure t; assembler;
asm
  mov eax, [rbx[5]][rdi][54][-17][45][4]      { mov eax, [rbx+rdi+5Bh] }
  mov eax, [[rbx+5]+[rdi+54]+[-17]+[45]+[4]]  { mov eax, [rbx+rdi+5Bh] }
  mov ebx, [5[7]]                             { mov ebx, [000Ch] }
  mov ebx, [5+[7]]                            { mov ebx, [000Ch] }
end;
procedure t_verify; assembler;
asm
  mov eax, [rbx+rdi+5Bh]  { mov eax, [rbx[5]][rdi][54][-17][45][4]     }
  mov eax, [rbx+rdi+5Bh]  { mov eax, [[rbx+5]+[rdi+54]+[-17]+[45]+[4]] }
  mov ebx, [000Ch]        { mov ebx, [5[7]]                            }
  mov ebx, [000Ch]        { mov ebx, [5+[7]]                           }
end;

const
  t2_size = 37;
procedure t2; assembler;
var
  locl: longword;
asm
  mov eax, locl                             { mov еax, [rbp-08] }
  mov eax, fs:locl                          { mov еax, fs:[rbp-04] }
  mov eax, [fs:locl]                        { mov еax, fs:[rbp-04] }
  mov eax, [fs:[locl]]                      { mov еax, fs:[rbp-04] }
  mov eax, fs:locl[5]                       { mov еax, fs:[rbp+01] }
  mov eax, fs:5[locl]                       { mov еax, fs:[rbp+01] }
end;
procedure t2_verify; assembler;
var
  locl: longword;
asm
  mov eax, [rbp-04]     { mov еax, locl        }
  mov eax, fs:[rbp-04]  { mov еax, fs:locl     }
  mov eax, fs:[rbp-04]  { mov еax, [fs:locl]   }
  mov eax, fs:[rbp-04]  { mov еax, [fs:[locl]] }
  mov eax, fs:[rbp+01]  { mov еax, fs:locl[5]  }
  mov eax, fs:[rbp+01]  { mov еax, fs:5[locl]  }
end;

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
  { size differs at least between linux and windows, so leave the ret check away
  lastbyte:=PByte(cp)[sz-1];
  if lastbyte<>$C3 then
  begin
    CompareCode := False;
    exit;
  end;
  }
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
