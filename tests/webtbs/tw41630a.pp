{ %cpu=i386,x86_64 }
{ %OPT=-O2 }
{$mode objfpc}
{$asmmode intel}

{ The same bracket syntax must distinguish a pointer value from a var/out/
  constref scalar. Immediate stores test inferred widths; sentinels catch
  stores wider than the declared type. Explicit ptr widths take precedence. }

procedure ClearByte(var V: Byte); assembler;
asm
  mov [V], 0
end;

procedure ClearWord(var V: Word); assembler;
asm
  mov [V], 0
end;

procedure ClearDWord(out V: LongWord); assembler;
asm
  mov [V], 0
end;

{$ifdef cpux86_64}
procedure ClearQWord(var V: QWord); assembler;
asm
  mov [V], 0
end;
{$endif}

function IsZero(constref V: Word): Boolean; assembler;
asm
  cmp [V], 0
  sete al
end;

procedure SetLowByte(var V: LongWord); assembler;
asm
  mov byte ptr [V], $5a
end;

procedure SetLowWord(var V: LongWord); assembler;
asm
  mov word ptr [V], $abcd
end;

procedure ClearThroughPointer(V: Pointer); assembler;
asm
  mov byte ptr [V], 0
end;

procedure SetSingle(out V: Single); assembler;
asm
  mov [V], $3f800000
end;

procedure CopySingle(var Dest: Single; constref Src: Single); assembler;
asm
  movss xmm0, [Src]
  movss [Dest], xmm0
end;

procedure ClearPointer(var V: Pointer); assembler;
asm
  mov [V], 0
end;

type
  TGuarded = packed record
    Before: LongWord;
    Data: QWord;
    After: LongWord;
  end;
var
  G: TGuarded;
  F: Single;

procedure Reset;
begin
  G.Before := $13579bdf;
  G.Data := High(QWord);
  G.After := $2468ace0;
end;

procedure Check(Expected: QWord);
begin
  if (G.Before <> $13579bdf) or (G.After <> $2468ace0) or (G.Data <> Expected) then
    Halt(1);
end;

begin
  Reset;
  ClearByte(PByte(@G.Data)^);
  Check(QWord($ffffffffffffff00));
  Reset;
  ClearWord(PWord(@G.Data)^);
  Check(QWord($ffffffffffff0000));
  if not IsZero(PWord(@G.Data)^) then Halt(2);
  Reset;
  if IsZero(PWord(@G.Data)^) then Halt(3);
  ClearDWord(PLongWord(@G.Data)^);
  Check(QWord($ffffffff00000000));
{$ifdef cpux86_64}
  Reset;
  ClearQWord(G.Data);
  Check(0);
{$endif}
  Reset;
  SetLowByte(PLongWord(@G.Data)^);
  Check(QWord($ffffffffffffff5a));
  Reset;
  SetLowWord(PLongWord(@G.Data)^);
  Check(QWord($ffffffffffffabcd));
  Reset;
  ClearThroughPointer(@G.Data);
  Check(QWord($ffffffffffffff00));
  Reset;
  SetSingle(PSingle(@G.Data)^);
  Check(QWord($ffffffff3f800000));
  Reset;
  F := 1.25;
  CopySingle(PSingle(@G.Data)^, F);
  Check(QWord($ffffffff3fa00000));
  Reset;
  ClearPointer(PPointer(@G.Data)^);
{$ifdef cpux86_64}
  Check(0);
{$else}
  Check(QWord($ffffffff00000000));
{$endif}
  writeln('ok');
end.
