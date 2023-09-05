procedure SHA1Transform(var ctx: TSHA1Context; const Buf: Pointer); assembler; nostackframe;
{$asmmode intel}
{
  RCX - pointer to ctx
  RDX - Buf
}
asm
.seh_pushreg RSI
  PUSH  RSI
.seh_pushreg RDI
  PUSH  RDI
.seh_pushreg RBX
  PUSH  RBX
.seh_pushreg RBP
  PUSH  RBP
.seh_stackalloc 128
  // Allocaste 128 bytes on the stack.
  LEA   RSP, [RSP-128]
.seh_endprologue

  // EAX = A, R8D = B, R9D = C, R10D = D, R11D = E.
  MOV   EAX, TSHA1Context.State[RCX] // A, B, C, D, E := *ctx.State[0 .. 4].
  MOV   R8D, TSHA1Context.State[RCX + 4 * 1]
  MOV   R9D, TSHA1Context.State[RCX + 4 * 2]
  MOV   R10D,TSHA1Context.State[RCX + 4 * 3]
  MOV   R11D,TSHA1Context.State[RCX + 4 * 4] // RDX is still required for rounds 0..15 to read buf parts.

  // Round 0. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 0]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 0]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP],ESI // Data[I and 15 = 0] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R10D
  AND   ESI, R8D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 1. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 1] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 1]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 1] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 1]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4], ESI // Data[I and 15 = 1] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R9D
  AND   ESI, EAX
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 2. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 2] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 2]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 2] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 2]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], ESI // Data[I and 15 = 2] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R8D
  AND   ESI, R11D
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 3. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 3] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 3]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 3] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 3]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], ESI // Data[I and 15 = 3] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, EAX
  AND   ESI, R10D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 4. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 4] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 4]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 4] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 4]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], ESI // Data[I and 15 = 4] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R11D
  AND   ESI, R9D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 5. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 5] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 5]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 5] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 5]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], ESI // Data[I and 15 = 5] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R10D
  AND   ESI, R8D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 6. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 6] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 6]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 6] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 6]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], ESI // Data[I and 15 = 6] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R9D
  AND   ESI, EAX
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 7. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 7] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 7]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 7] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 7]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], ESI // Data[I and 15 = 7] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R8D
  AND   ESI, R11D
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 8. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 8] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 8]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 8] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 8]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], ESI // Data[I and 15 = 8] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, EAX
  AND   ESI, R10D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 9. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 9] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 9]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 9] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 9]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], ESI // Data[I and 15 = 9] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R11D
  AND   ESI, R9D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 10. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 10] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 10]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 10] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 10]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], ESI // Data[I and 15 = 10] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R10D
  AND   ESI, R8D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 11. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 11] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 11]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 11] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 11]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], ESI // Data[I and 15 = 11] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R9D
  AND   ESI, EAX
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 12. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RDX + 4 * 12] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 12]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RDX + 4 * 12] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 12]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], ESI // Data[I and 15 = 12] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R8D
  AND   ESI, R11D
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 13. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE EBP, [RDX + 4 * 13] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 13]));
{$else CPUX86_HAS_MOVBE}
  MOV   EBP, [RDX + 4 * 13] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 13]));
  BSWAP EBP
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], EBP // Data[I and 15 = 13] := Blkv; Keep in EBX for Round 16.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, EAX
  AND   ESI, R10D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 14. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE EBX, [RDX + 4 * 14] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 14]));
{$else CPUX86_HAS_MOVBE}
  MOV   EBX, [RDX + 4 * 14] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 14]));
  BSWAP EBX
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], EBX // Data[I and 15 = 14] := Blkv; Keep in EBX for Round 17.
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R11D
  AND   ESI, R9D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 15. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE EDX, [RDX + 4 * 15] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 15]));
{$else CPUX86_HAS_MOVBE}
  MOV   EDX, [RDX + 4 * 15] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 15]));
  BSWAP EDX
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], EDX // Data[I and 15 = 15] := Blkv; Keep in EDX for Round 18.
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R10D
  AND   ESI, R8D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 16. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 2]
  XOR   EBP, [RSP]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP], EBP // Data[I and 15 = 0] := Blkv; Keep in EBP for Round 19.
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R9D
  AND   ESI, EAX
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 17. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 3]
  XOR   EBX, [RSP + 4 * 1]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], EBX // Data[I and 15 = 1] := Blkv; Keep in EBX for Round 20.
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R8D
  AND   ESI, R11D
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 18. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EDX already contains [RSP + 4 * 15]
  XOR   EDX, [RSP + 4 * 10]
  XOR   EDX, [RSP + 4 * 4]
  XOR   EDX, [RSP + 4 * 2]
  ROL   EDX, 1
  ADD   R8D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], EDX // Data[I and 15 = 2] := Blkv; Keep in EDX for Round 21.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, EAX
  AND   ESI, R10D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 19. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP]
  XOR   EBP, [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 5]
  XOR   EBP, [RSP + 4 * 3]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBP // Data[I and 15 = 3] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5)
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ESI, R11D
  AND   ESI, R9D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 20. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 6]
  XOR   EBX, [RSP + 4 * 4]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], EBX // Data[I and 15 = 4] := Blkv; Keep in EBX for Round 23.
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 21. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EDX already contains [RSP + 4 * 2]
  XOR   EDX, [RSP + 4 * 13]
  XOR   EDX, [RSP + 4 * 7]
  XOR   EDX, [RSP + 4 * 5]
  ROL   EDX, 1
  ADD   R10D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], EDX // Data[I and 15 = 5] := Blkv; Keep in EDX for Round 24.
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K40;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 22. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 6]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], EBP // Data[I and 15 = 6] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K40;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K40]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 23. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 7]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], EBX // Data[I and 15 = 7] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K40]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 24. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EDX already contains [RSP + 4 * 5]
  XOR   EDX, [RSP]
  XOR   EDX, [RSP + 4 * 10]
  XOR   EDX, [RSP + 4 * 8]
  ROL   EDX, 1
  ADD   EAX, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], EDX // Data[I and 15 = 8] := Blkv; Keep in EDX for Round 27.
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 25. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 6]
  XOR   EBP, [RSP + 4 * 1]
  XOR   EBP, [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 9]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], EBP // Data[I and 15 = 9] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 26. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 10]
  ROL   EBX, 1
  ADD   R10D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], EBX // Data[I and 15 = 10] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K40;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 27. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EDX already contains [RSP + 4 * 8]
  XOR   EDX, [RSP + 4 * 3]
  XOR   EDX, [RSP + 4 * 13]
  XOR   EDX, [RSP + 4 * 11]
  ROL   EDX, 1
  ADD   R9D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], EDX // Data[I and 15 = 11] := Blkv; Keep in EDX for Round 30.
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K40;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K40]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 28. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 4]
  XOR   EBP, [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 12]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], EBP // Data[I and 15 = 12] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K40]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 29. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 5]
  XOR   EBX, [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 13]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], EBX // Data[I and 15 = 13] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 30. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EDX already contains [RSP + 4 * 11]
  XOR   EDX, [RSP + 4 * 6]
  XOR   EDX, [RSP]
  XOR   EDX, [RSP + 4 * 14]
  ROL   EDX, 1
  ADD   R11D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], EDX // Data[I and 15 = 14] := Blkv; Keep in EDX for Round 33.
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 31. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 1]
  XOR   EBP, [RSP + 4 * 15]
  ROL   EBP, 1
  ADD   R10D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], EBP // Data[I and 15 = 15] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K40;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 32. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP], EBX // Data[I and 15 = 0] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K40;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K40]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 33. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EDX already contains [RSP + 4 * 14]
  XOR   EDX, [RSP + 4 * 9]
  XOR   EDX, [RSP + 4 * 3]
  XOR   EDX, [RSP + 4 * 1]
  ROL   EDX, 1
  ADD   R8D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], EDX // Data[I and 15 = 1] := Blkv; Keep in EDX for Round 36.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K40]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 34. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 10]
  XOR   EBP, [RSP + 4 * 4]
  XOR   EBP, [RSP + 4 * 2]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], EBP // Data[I and 15 = 2] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 35. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP]
  XOR   EBX, [RSP + 4 * 11]
  XOR   EBX, [RSP + 4 * 5]
  XOR   EBX, [RSP + 4 * 3]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBX // Data[I and 15 = 3] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 36. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EDX already contains [RSP + 4 * 1]
  XOR   EDX, [RSP + 4 * 12]
  XOR   EDX, [RSP + 4 * 6]
  XOR   EDX, [RSP + 4 * 4]
  ROL   EDX, 1
  ADD   R10D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], EDX // Data[I and 15 = 4] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K40;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 37. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 2]
  XOR   EBP, [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 5]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], EBP // Data[I and 15 = 5] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K40;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K40]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 38. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 3]
  XOR   EBX, [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 6]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], EBX // Data[I and 15 = 6] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K40]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 39. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EDX already contains [RSP + 4 * 4]
  XOR   EDX, [RSP + 4 * 15]
  XOR   EDX, [RSP + 4 * 9]
  XOR   EDX, [RSP + 4 * 7]
  ROL   EDX, 1
  ADD   EAX, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], EDX // Data[I and 15 = 7] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K40;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 40. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 5]
  XOR   EBP, [RSP]
  XOR   EBP, [RSP + 4 * 10]
  XOR   EBP, [RSP + 4 * 8]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], EBP // Data[I and 15 = 8] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R9D
  AND   ESI, R10D
  MOV   EDI, R8D
  AND   EDI, R9D
  OR    ESI, EDI
  LEA   R11D,[R11D + ESI + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 41. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 6]
  XOR   EBX, [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 11]
  XOR   EBX, [RSP + 4 * 9]
  ROL   EBX, 1
  ADD   R10D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], EBX // Data[I and 15 = 9] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R8D
  AND   ESI, R9D
  MOV   EDI, EAX
  AND   EDI, R8D
  OR    ESI, EDI
  LEA   R10D,[R10D + ESI + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 42. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EDX already contains [RSP + 4 * 7]
  XOR   EDX, [RSP + 4 * 2]
  XOR   EDX, [RSP + 4 * 12]
  XOR   EDX, [RSP + 4 * 10]
  ROL   EDX, 1
  ADD   R9D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], EDX // Data[I and 15 = 10] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, EAX
  AND   ESI, R8D
  MOV   EDI, R11D
  AND   EDI, EAX
  OR    ESI, EDI
  LEA   R9D, [R9D + ESI + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 43. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 11]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], EBP // Data[I and 15 = 11] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R11D
  AND   ESI, EAX
  MOV   EDI, R10D
  AND   EDI, R11D
  OR    ESI, EDI
  LEA   R8D, [R8D + ESI + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 44. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 12]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], EBX // Data[I and 15 = 12] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R10D
  AND   ESI, R11D
  MOV   EDI, R9D
  AND   EDI, R10D
  OR    ESI, EDI
  LEA   EAX, [EAX + ESI + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 45. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EDX already contains [RSP + 4 * 10]
  XOR   EDX, [RSP + 4 * 5]
  XOR   EDX, [RSP + 4 * 15]
  XOR   EDX, [RSP + 4 * 13]
  ROL   EDX, 1
  ADD   R11D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], EDX // Data[I and 15 = 13] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R9D
  AND   ESI, R10D
  MOV   EDI, R8D
  AND   EDI, R9D
  OR    ESI, EDI
  LEA   R11D,[R11D + ESI + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 46. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 6]
  XOR   EBP, [RSP]
  XOR   EBP, [RSP + 4 * 14]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], EBP // Data[I and 15 = 14] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R8D
  AND   ESI, R9D
  MOV   EDI, EAX
  AND   EDI, R8D
  OR    ESI, EDI
  LEA   R10D,[R10D + ESI + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 47. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 15]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], EBX // Data[I and 15 = 15] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, EAX
  AND   ESI, R8D
  MOV   EDI, R11D
  AND   EDI, EAX
  OR    ESI, EDI
  LEA   R9D, [R9D + ESI + K60]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 48. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EDX already contains [RSP + 4 * 13]
  XOR   EDX, [RSP + 4 * 8]
  XOR   EDX, [RSP + 4 * 2]
  XOR   EDX, [RSP]
  ROL   EDX, 1
  ADD   R8D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 16], EDX // Data[I and 31 = 16] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R11D
  AND   ESI, EAX
  MOV   EDI, R10D
  AND   EDI, R11D
  OR    ESI, EDI
  LEA   R8D, [R8D + ESI + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 49. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 1]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 17], EBP // Data[I and 31 = 17] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R10D
  AND   ESI, R11D
  MOV   EDI, R9D
  AND   EDI, R10D
  OR    ESI, EDI
  LEA   EAX, [EAX + ESI + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 50. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 2]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 18], EBX // Data[I and 31 = 18] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R9D
  AND   ESI, R10D
  MOV   EDI, R8D
  AND   EDI, R9D
  OR    ESI, EDI
  LEA   R11D,[R11D + ESI + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 51. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EDX already contains [RSP + 4 * 16]
  XOR   EDX, [RSP + 4 * 11]
  XOR   EDX, [RSP + 4 * 5]
  XOR   EDX, [RSP + 4 * 3]
  ROL   EDX, 1
  ADD   R10D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 19], EDX // Data[I and 31 = 19] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R8D
  AND   ESI, R9D
  MOV   EDI, EAX
  AND   EDI, R8D
  OR    ESI, EDI
  LEA   R10D,[R10D + ESI + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 52. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 17]
  XOR   EBP, [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 6]
  XOR   EBP, [RSP + 4 * 4]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 20], EBP // Data[I and 31 = 20] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, EAX
  AND   ESI, R8D
  MOV   EDI, R11D
  AND   EDI, EAX
  OR    ESI, EDI
  LEA   R9D, [R9D + ESI + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 53. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 18]
  XOR   EBX, [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 5]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 21], EBX // Data[I and 31 = 21] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R11D
  AND   ESI, EAX
  MOV   EDI, R10D
  AND   EDI, R11D
  OR    ESI, EDI
  LEA   R8D, [R8D + ESI + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 54. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EDX already contains [RSP + 4 * 19]
  XOR   EDX, [RSP + 4 * 14]
  XOR   EDX, [RSP + 4 * 8]
  XOR   EDX, [RSP + 4 * 6]
  ROL   EDX, 1
  ADD   EAX, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 22], EDX // Data[I and 31 = 22] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R10D
  AND   ESI, R11D
  MOV   EDI, R9D
  AND   EDI, R10D
  OR    ESI, EDI
  LEA   EAX, [EAX + ESI + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 55. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 20]
  XOR   EBP, [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 7]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 23], EBP // Data[I and 31 = 23] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R9D
  AND   ESI, R10D
  MOV   EDI, R8D
  AND   EDI, R9D
  OR    ESI, EDI
  LEA   R11D,[R11D + ESI + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 56. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 21]
  XOR   EBX, [RSP + 4 * 16]
  XOR   EBX, [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 8]
  ROL   EBX, 1
  ADD   R10D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 24], EBX // Data[I and 31 = 24] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R8D
  AND   ESI, R9D
  MOV   EDI, EAX
  AND   EDI, R8D
  OR    ESI, EDI
  LEA   R10D,[R10D + ESI + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 57. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EDX already contains [RSP + 4 * 22]
  XOR   EDX, [RSP + 4 * 17]
  XOR   EDX, [RSP + 4 * 11]
  XOR   EDX, [RSP + 4 * 9]
  ROL   EDX, 1
  ADD   R9D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 25], EDX // Data[I and 31 = 25] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, EAX
  AND   ESI, R8D
  MOV   EDI, R11D
  AND   EDI, EAX
  OR    ESI, EDI
  LEA   R9D, [R9D + ESI + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 58. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 23]
  XOR   EBP, [RSP + 4 * 18]
  XOR   EBP, [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 10]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 26], EBP // Data[I and 31 = 26] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R11D
  AND   ESI, EAX
  MOV   EDI, R10D
  AND   EDI, R11D
  OR    ESI, EDI
  LEA   R8D, [R8D + ESI + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 59. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 24]
  XOR   EBX, [RSP + 4 * 19]
  XOR   EBX, [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 11]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 27], EBX // Data[I and 31 = 27] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ESI, R10D
  AND   ESI, R11D
  MOV   EDI, R9D
  AND   EDI, R10D
  OR    ESI, EDI
  LEA   EAX, [EAX + ESI + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 60. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EDX already contains [RSP + 4 * 25]
  XOR   EDX, [RSP + 4 * 20]
  XOR   EDX, [RSP + 4 * 14]
  XOR   EDX, [RSP + 4 * 12]
  ROL   EDX, 1
  ADD   R11D,EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 28], EDX // Data[I and 31 = 28] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 61. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 26]
  XOR   EBP, [RSP + 4 * 21]
  XOR   EBP, [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 13]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 29], EBP // Data[I and 31 = 29] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K80;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 62. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 27]
  XOR   EBX, [RSP + 4 * 22]
  XOR   EBX, [RSP + 4 * 16]
  XOR   EBX, [RSP + 4 * 14]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 30], EBX // Data[I and 31 = 30] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K80;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K80]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 63. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EDX already contains [RSP + 4 * 28]
  XOR   EDX, [RSP + 4 * 23]
  XOR   EDX, [RSP + 4 * 17]
  XOR   EDX, [RSP + 4 * 15]
  ROL   EDX, 1
  ADD   R8D, EDX // Z := Z + Blkv;
  MOV   [RSP + 4 * 31], EDX // Data[I and 31 = 31] := Blkv;
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 64. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  MOV   ESI, [RSP + 4 * 26]
  XOR   ESI, [RSP + 4 * 16]
  XOR   ESI, [RSP + 4 * 4]
  XOR   ESI, [RSP]
  ROL   ESI, 2
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP], ESI // Data[I and 31 = 0] := Blkv;
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 65. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  MOV   ESI, [RSP + 4 * 27]
  XOR   ESI, [RSP + 4 * 17]
  XOR   ESI, [RSP + 4 * 5]
  XOR   ESI, [RSP + 4 * 1]
  ROL   ESI, 2
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], ESI // Data[I and 31 = 1] := Blkv;
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 66. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  MOV   ESI, [RSP + 4 * 28]
  XOR   ESI, [RSP + 4 * 18]
  XOR   ESI, [RSP + 4 * 6]
  XOR   ESI, [RSP + 4 * 2]
  ROL   ESI, 2
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], ESI // Data[I and 31 = 2] := Blkv;
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K80;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 67. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 29]
  XOR   EBP, [RSP + 4 * 19]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 3]
  ROL   EBP, 2
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBP // Data[I and 31 = 3] := Blkv;
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K80;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K80]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 68. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 30]
  XOR   EBX, [RSP + 4 * 20]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 4]
  ROL   EBX, 2
  ADD   R8D, EBX // Z := Z + Blkv;
  // Data[I and 31 = 4] := Blkv; Don't actually write it to the stack, but preserve it in EBX to merge with RDX below.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K80]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 69. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EDX already contains [RSP + 4 * 31]
  XOR   EDX, [RSP + 4 * 21]
  XOR   EDX, [RSP + 4 * 9]
  XOR   EDX, [RSP + 4 * 5]
  ROL   EDX, 2
  ADD   EAX, EDX // Z := Z + Blkv;
  SHL   RDX, 32 // Data[I and 31 = 5] := Blkv; - merge with Data[I and 31 = 4] above.
  OR    RDX, RBX
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 70. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  MOV   EDI, [RSP]
  XOR   EDI, [RSP + 4 * 22]
  XOR   EDI, [RSP + 4 * 10]
  XOR   EDI, [RSP + 4 * 6]
  ROL   EDI, 2
  ADD   R11D,EDI // Z := Z + Blkv;
  // Data[I and 31 = 6] := Blkv; Don't actually write it to the stack, but preserve it in EDI to merge with RSI below.
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D,ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D,[R11D + ESI + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 71. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  MOV   ESI, [RSP + 4 * 1]
  XOR   ESI, [RSP + 4 * 23]
  XOR   ESI, [RSP + 4 * 11]
  XOR   ESI, [RSP + 4 * 7]
  ROL   ESI, 2
  ADD   R10D,ESI // Z := Z + Blkv;
  SHL   RSI, 32
  OR    RDI, RSI // Data[I and 31 = 7] := Blkv; - merge with Data[I and 31 = 6] above.
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K80;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 72. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  MOV   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP + 4 * 24]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 8]
  ROL   EBX, 2
  ADD   R9D, EBX // Z := Z + Blkv;
  // Data[I and 31 = 8] := Blkv; Don't actually write it to the stack, but preserve it in EBX to merge with RBP below.
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K80;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K80]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 73. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 25]
  XOR   EBP, [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 9]
  ROL   EBP, 2
  ADD   R8D, EBP // Z := Z + Blkv;
  SHL   RBP, 32
  OR    RBX, RBP // Data[I and 31 = 9] := Blkv; - merge with Data[I and 31 = 8] above.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 74. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // RDX already contains [RSP + 4 * 4] and [RSP + 4 * 5]
  XOR   RDX, [RSP + 4 * 26]
  XOR   RDX, [RSP + 4 * 14]
  XOR   RDX, [RSP + 4 * 10]
  MOV   ESI, EDX
  ROL   ESI, 2
  ADD   EAX, ESI // Z := Z + Blkv;
  // Data[I and 31 = 10] := Blkv; - not required.
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 75. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  SHR   RDX, 32
  ROL   EDX, 2
  ADD   R11D, EDX // Z := Z + Blkv;
  // Data[I and 31 = 11] := Blkv; - not required.
  MOV   ESI, EAX // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R11D, ESI
  MOV   ESI, R8D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R9D
  XOR   ESI, R10D
  LEA   R11D, [R11D + ESI + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 76. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // RDI already contains [RSP + 4 * 6] and [RSP + 4 * 7]
  XOR   RDI, [RSP + 4 * 28]
  XOR   RDI, [RSP + 4 * 16]
  XOR   RDI, [RSP + 4 * 12]
  MOV   ESI, EDI
  ROL   ESI, 2
  ADD   R10D,ESI // Z := Z + Blkv;
  // Data[I and 31 = 12] := Blkv; - not required.
  MOV   ESI, R11D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R10D,ESI
  MOV   ESI, EAX // ... + (W xor X xor Y) + K80;
  XOR   ESI, R8D
  XOR   ESI, R9D
  LEA   R10D,[R10D + ESI + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 77. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  SHR   RDI, 32
  ROL   EDI, 2
  ADD   R9D, EDI // Z := Z + Blkv;
  // Data[I and 31 = 13] := Blkv; - not required.
  MOV   ESI, R10D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R9D, ESI
  MOV   ESI, R11D // ... + (W xor X xor Y) + K80;
  XOR   ESI, EAX
  XOR   ESI, R8D
  LEA   R9D, [R9D + ESI + K80]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 78. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // RBX already contains [RSP + 4 * 8] and [RSP + 4 * 9]
  XOR   RBX, [RSP + 4 * 30]
  XOR   RBX, [RSP + 4 * 18]
  XOR   RBX, [RSP + 4 * 14]
  MOV   ESI, EBX
  ROL   ESI, 2
  ADD   R8D, ESI // Z := Z + Blkv;
  // Data[I and 31 = 14] := Blkv; - not required.
  MOV   ESI, R9D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   R8D, ESI
  MOV   ESI, R10D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R11D
  XOR   ESI, EAX
  LEA   R8D, [R8D + ESI + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 79. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  SHR   RBX, 32
  ROL   EBX, 2
  ADD   EAX, EBX // Z := Z + Blkv;
  // Data[I and 31 = 15] := Blkv; - not required.
  MOV   ESI, R8D // Z := Z + RolDWord(V, 5);
  ROL   ESI, 5
  ADD   EAX, ESI
  MOV   ESI, R9D // ... + (W xor X xor Y) + K80;
  XOR   ESI, R10D
  XOR   ESI, R11D
  LEA   EAX, [EAX + ESI + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  ADD   TSHA1Context.State[RCX], EAX // ctx.State[0 .. 4] += A, B, C, D, E
  ADD   TSHA1Context.State[RCX + 4 * 1], R8D
  ADD   TSHA1Context.State[RCX + 4 * 2], R9D
  ADD   TSHA1Context.State[RCX + 4 * 3], R10D
  ADD   TSHA1Context.State[RCX + 4 * 4], R11D
  ADD	QWORD PTR TSHA1Context.Length[RCX], 64 // ctx.Length += 64

  // Restore stack
  LEA   RSP, [RSP+128]
  POP   RBP
  POP   RBX
  POP   RDI
  POP   RSI
end;
