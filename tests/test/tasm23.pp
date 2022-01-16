{ %CPU=i8086 }

program tasm23;

{$S-}

{$IFNDEF FPC}
type
  CodePointer = Pointer;
{$ENDIF FPC}

const
{$IFDEF FPC_MM_HUGE}
  hugeadd = 7;
{$ELSE FPC_MM_HUGE}
  hugeadd = 0;
{$ENDIF FPC_MM_HUGE}

const
  t_size = 13 + hugeadd;
procedure t; assembler;
asm
  mov ax, [bx[5]][di][54][-17][45][4]      { mov ax, [bx+di+5Bh] }
  mov ax, [[bx+5]+[di+54]+[-17]+[45]+[4]]  { mov ax, [bx+di+5Bh] }
  mov ax, [5[7]]                           { mov ax, [000Ch] }
  mov ax, [5+[7]]                          { mov ax, [000Ch] }
end;
procedure t_verify; assembler;
asm
  mov ax, [bx+di+5Bh]  { mov ax, [bx[5]][di][54][-17][45][4]     }
  mov ax, [bx+di+5Bh]  { mov ax, [[bx+5]+[di+54]+[-17]+[45]+[4]] }
  mov ax, [000Ch]      { mov ax, [5[7]]                          }
  mov ax, [000Ch]      { mov ax, [5+[7]]                         }
end;

const
  t2_size = 33 + hugeadd;
procedure t2; assembler;
var
  locl: word;
asm
  mov ax, locl                             { mov ax, [bp-02] }
  mov ax, cs:locl                          { mov ax, cs:[bp-02] }
  mov ax, [cs:locl]                        { mov ax, cs:[bp-02] }
  mov ax, [cs:[locl]]                      { mov ax, cs:[bp-02] }
  mov ax, cs:locl[5]                       { mov ax, cs:[bp+03] }
  mov ax, cs:5[locl]                       { mov ax, cs:[bp+03] }
end;
procedure t2_verify; assembler;
var
  locl: word;
asm
  mov ax, [bp-02]     { mov ax, locl        }
  mov ax, cs:[bp-02]  { mov ax, cs:locl     }
  mov ax, cs:[bp-02]  { mov ax, [cs:locl]   }
  mov ax, cs:[bp-02]  { mov ax, [cs:[locl]] }
  mov ax, cs:[bp+03]  { mov ax, cs:locl[5]  }
  mov ax, cs:[bp+03]  { mov ax, cs:5[locl]  }
end;

{ This version works in all i8086 memory models }
function CompareCode(cp, cp2: CodePointer; sz: Integer): Boolean;
var
  I: Integer;
  lastbyte: Byte;
begin
  for I := 0 to sz - 1 do
    if Mem[Seg(cp^):Ofs(cp^) + I] <> Mem[Seg(cp2^):Ofs(cp2^) + I] then
    begin
      Writeln(I, ' ', Mem[Seg(cp^):Ofs(cp^) + I], ' ', Mem[Seg(cp2^):Ofs(cp2^) + I]);
      CompareCode := False;
      exit;
    end;
  { check also that the last byte is a retn or retf instruction }
  lastbyte:=Mem[Seg(cp^):Ofs(cp^)+sz-1];
  if (lastbyte<>$C3) and (lastbyte<>$CB) then
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
