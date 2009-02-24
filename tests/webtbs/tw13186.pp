{ %cpu=i386 }
TYPE TRSC = 0..64;
FUNCTION COUNTBIT32 { 32 BIT VERSION }
  (VAR A: QWORD): TRSC;
  ASSEMBLER; NOSTACKFRAME;
  ASM
    POPCNT (%EAX),%EDX
    POPCNT 4(%EAX),%EAX
    ADDL %EDX,%EAX
  END;
const
  expected : array[0..8] of byte =
    ($F3,$0F,$B8,$10,
     $F3,$0F,$B8,$40,$04
    );

var
  i : Integer;
begin
  for i:=low(expected) to high(expected) do
    if (pbyte(@countbit32)+i)^<>expected[i] then
      halt(1);
  writeln('ok');
end.
