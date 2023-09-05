procedure SHA1Transform(var ctx: TSHA1Context; const Buf: Pointer); assembler; nostackframe;
{$asmmode intel}
{
  RDI - pointer to ctx
  RSI - Buf
}
asm
  PUSH  RBX
  PUSH  RBP
  // Allocaste 128 bytes on the stack.
  LEA   RSP, [RSP-128]

  // EAX = A, R8D = B, R9D = C, R10D = D, R11D = E.
  MOV   EAX, TSHA1Context.State[RDI] // A, B, C, D, E := *ctx.State[0 .. 4].
  MOV   R8D, TSHA1Context.State[RDI + 4 * 1]
  MOV   R9D, TSHA1Context.State[RDI + 4 * 2]
  MOV   R10D,TSHA1Context.State[RDI + 4 * 3]
  MOV   R11D,TSHA1Context.State[RDI + 4 * 4] // RSI is still required for rounds 0..15 to read buf parts.

  // Round 0. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 0]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 0]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ECX // Z := Z + Blkv;
  MOV   [RSP],ECX // Data[I and 15 = 0] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R10D
  AND   ECX, R8D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 1. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 1] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 1]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 1] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 1]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4], ECX // Data[I and 15 = 1] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R9D
  AND   ECX, EAX
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 2. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 2] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 2]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 2] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 2]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], ECX // Data[I and 15 = 2] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R8D
  AND   ECX, R11D
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 3. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 3] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 3]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 3] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 3]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], ECX // Data[I and 15 = 3] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, EAX
  AND   ECX, R10D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 4. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 4] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 4]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 4] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 4]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], ECX // Data[I and 15 = 4] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R11D
  AND   ECX, R9D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 5. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 5] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 5]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 5] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 5]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], ECX // Data[I and 15 = 5] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R10D
  AND   ECX, R8D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 6. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 6] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 6]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 6] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 6]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], ECX // Data[I and 15 = 6] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R9D
  AND   ECX, EAX
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 7. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 7] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 7]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 7] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 7]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], ECX // Data[I and 15 = 7] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R8D
  AND   ECX, R11D
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 8. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 8] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 8]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 8] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 8]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], ECX // Data[I and 15 = 8] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, EAX
  AND   ECX, R10D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 9. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 9] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 9]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 9] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 9]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], ECX // Data[I and 15 = 9] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R11D
  AND   ECX, R9D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 10. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 10] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 10]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 10] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 10]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], ECX // Data[I and 15 = 10] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R10D
  AND   ECX, R8D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 11. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 11] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 11]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 11] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 11]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R10D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], ECX // Data[I and 15 = 11] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R9D
  AND   ECX, EAX
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 12. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ECX, [RSI + 4 * 12] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 12]));
{$else CPUX86_HAS_MOVBE}
  MOV   ECX, [RSI + 4 * 12] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 12]));
  BSWAP ECX
{$endif CPUX86_HAS_MOVBE}
  ADD   R9D, ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], ECX // Data[I and 15 = 12] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R8D
  AND   ECX, R11D
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 13. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE EBP, [RSI + 4 * 13] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 13]));
{$else CPUX86_HAS_MOVBE}
  MOV   EBP, [RSI + 4 * 13] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 13]));
  BSWAP EBP
{$endif CPUX86_HAS_MOVBE}
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], EBP // Data[I and 15 = 13] := Blkv; Keep in EBX for Round 16.
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, EAX
  AND   ECX, R10D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 14. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE EBX, [RSI + 4 * 14] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 14]));
{$else CPUX86_HAS_MOVBE}
  MOV   EBX, [RSI + 4 * 14] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 14]));
  BSWAP EBX
{$endif CPUX86_HAS_MOVBE}
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], EBX // Data[I and 15 = 14] := Blkv; Keep in EBX for Round 17.
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R11D
  AND   ECX, R9D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 15. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
{$ifdef CPUX86_HAS_MOVBE}
  MOVBE ESI, [RSI + 4 * 15] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 15]));
{$else CPUX86_HAS_MOVBE}
  MOV   ESI, [RSI + 4 * 15] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 15]));
  BSWAP ESI
{$endif CPUX86_HAS_MOVBE}
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], ESI // Data[I and 15 = 15] := Blkv; Keep in ESI for Round 18.
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R9D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R10D
  AND   ECX, R8D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K20]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 16. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 2]
  XOR   EBP, [RSP]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP], EBP // Data[I and 15 = 0] := Blkv; Keep in EBP for Round 19.
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, R8D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R9D
  AND   ECX, EAX
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K20]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 17. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 3]
  XOR   EBX, [RSP + 4 * 1]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], EBX // Data[I and 15 = 1] := Blkv; Keep in EBX for Round 20.
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, EAX // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R8D
  AND   ECX, R11D
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K20]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 18. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // ESI already contains [RSP + 4 * 15]
  XOR   ESI, [RSP + 4 * 10]
  XOR   ESI, [RSP + 4 * 4]
  XOR   ESI, [RSP + 4 * 2]
  ROL   ESI, 1
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], ESI // Data[I and 15 = 2] := Blkv; Keep in ESI for Round 21.
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R11D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, EAX
  AND   ECX, R10D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K20]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 19. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP]
  XOR   EBP, [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 5]
  XOR   EBP, [RSP + 4 * 3]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBP // Data[I and 15 = 3] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5)
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R10D // ... + ((W and (X xor Y)) xor Y);
  XOR   ECX, R11D
  AND   ECX, R9D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K20]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 20. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 6]
  XOR   EBX, [RSP + 4 * 4]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], EBX // Data[I and 15 = 4] := Blkv; Keep in EBX for Round 23.
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 21. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // ESI already contains [RSP + 4 * 2]
  XOR   ESI, [RSP + 4 * 13]
  XOR   ESI, [RSP + 4 * 7]
  XOR   ESI, [RSP + 4 * 5]
  ROL   ESI, 1
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], ESI // Data[I and 15 = 5] := Blkv; Keep in ESI for Round 24.
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K40;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 22. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 6]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], EBP // Data[I and 15 = 6] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K40;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K40]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 23. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 7]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], EBX // Data[I and 15 = 7] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K40]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 24. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // ESI already contains [RSP + 4 * 5]
  XOR   ESI, [RSP]
  XOR   ESI, [RSP + 4 * 10]
  XOR   ESI, [RSP + 4 * 8]
  ROL   ESI, 1
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], ESI // Data[I and 15 = 8] := Blkv; Keep in ESI for Round 27.
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 25. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 6]
  XOR   EBP, [RSP + 4 * 1]
  XOR   EBP, [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 9]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], EBP // Data[I and 15 = 9] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 26. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 10]
  ROL   EBX, 1
  ADD   R10D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], EBX // Data[I and 15 = 10] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K40;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 27. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // ESI already contains [RSP + 4 * 8]
  XOR   ESI, [RSP + 4 * 3]
  XOR   ESI, [RSP + 4 * 13]
  XOR   ESI, [RSP + 4 * 11]
  ROL   ESI, 1
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], ESI // Data[I and 15 = 11] := Blkv; Keep in ESI for Round 30.
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K40;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K40]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 28. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 4]
  XOR   EBP, [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 12]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], EBP // Data[I and 15 = 12] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K40]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 29. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 5]
  XOR   EBX, [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 13]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], EBX // Data[I and 15 = 13] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 30. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // ESI already contains [RSP + 4 * 11]
  XOR   ESI, [RSP + 4 * 6]
  XOR   ESI, [RSP]
  XOR   ESI, [RSP + 4 * 14]
  ROL   ESI, 1
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], ESI // Data[I and 15 = 14] := Blkv; Keep in ESI for Round 33.
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 31. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 1]
  XOR   EBP, [RSP + 4 * 15]
  ROL   EBP, 1
  ADD   R10D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], EBP // Data[I and 15 = 15] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K40;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 32. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP], EBX // Data[I and 15 = 0] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K40;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K40]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 33. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // ESI already contains [RSP + 4 * 14]
  XOR   ESI, [RSP + 4 * 9]
  XOR   ESI, [RSP + 4 * 3]
  XOR   ESI, [RSP + 4 * 1]
  ROL   ESI, 1
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], ESI // Data[I and 15 = 1] := Blkv; Keep in ESI for Round 36.
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K40]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 34. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 10]
  XOR   EBP, [RSP + 4 * 4]
  XOR   EBP, [RSP + 4 * 2]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], EBP // Data[I and 15 = 2] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 35. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP]
  XOR   EBX, [RSP + 4 * 11]
  XOR   EBX, [RSP + 4 * 5]
  XOR   EBX, [RSP + 4 * 3]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBX // Data[I and 15 = 3] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K40]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 36. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // ESI already contains [RSP + 4 * 1]
  XOR   ESI, [RSP + 4 * 12]
  XOR   ESI, [RSP + 4 * 6]
  XOR   ESI, [RSP + 4 * 4]
  ROL   ESI, 1
  ADD   R10D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 4], ESI // Data[I and 15 = 4] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K40;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K40]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 37. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 2]
  XOR   EBP, [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 5]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 5], EBP // Data[I and 15 = 5] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K40;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K40]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 38. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 3]
  XOR   EBX, [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 6]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 6], EBX // Data[I and 15 = 6] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K40]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 39. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // ESI already contains [RSP + 4 * 4]
  XOR   ESI, [RSP + 4 * 15]
  XOR   ESI, [RSP + 4 * 9]
  XOR   ESI, [RSP + 4 * 7]
  ROL   ESI, 1
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 7], ESI // Data[I and 15 = 7] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K40;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K40]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 40. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 5]
  XOR   EBP, [RSP]
  XOR   EBP, [RSP + 4 * 10]
  XOR   EBP, [RSP + 4 * 8]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 8], EBP // Data[I and 15 = 8] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R9D
  AND   ECX, R10D
  MOV   EDX, R8D
  AND   EDX, R9D
  OR    ECX, EDX
  LEA   R11D,[R11D + ECX + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 41. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 6]
  XOR   EBX, [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 11]
  XOR   EBX, [RSP + 4 * 9]
  ROL   EBX, 1
  ADD   R10D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 9], EBX // Data[I and 15 = 9] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R8D
  AND   ECX, R9D
  MOV   EDX, EAX
  AND   EDX, R8D
  OR    ECX, EDX
  LEA   R10D,[R10D + ECX + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 42. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // ESI already contains [RSP + 4 * 7]
  XOR   ESI, [RSP + 4 * 2]
  XOR   ESI, [RSP + 4 * 12]
  XOR   ESI, [RSP + 4 * 10]
  ROL   ESI, 1
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 10], ESI // Data[I and 15 = 10] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, EAX
  AND   ECX, R8D
  MOV   EDX, R11D
  AND   EDX, EAX
  OR    ECX, EDX
  LEA   R9D, [R9D + ECX + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 43. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 8]
  XOR   EBP, [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 13]
  XOR   EBP, [RSP + 4 * 11]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 11], EBP // Data[I and 15 = 11] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R11D
  AND   ECX, EAX
  MOV   EDX, R10D
  AND   EDX, R11D
  OR    ECX, EDX
  LEA   R8D, [R8D + ECX + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 44. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 9]
  XOR   EBX, [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 14]
  XOR   EBX, [RSP + 4 * 12]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 12], EBX // Data[I and 15 = 12] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R10D
  AND   ECX, R11D
  MOV   EDX, R9D
  AND   EDX, R10D
  OR    ECX, EDX
  LEA   EAX, [EAX + ECX + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 45. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // ESI already contains [RSP + 4 * 10]
  XOR   ESI, [RSP + 4 * 5]
  XOR   ESI, [RSP + 4 * 15]
  XOR   ESI, [RSP + 4 * 13]
  ROL   ESI, 1
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 13], ESI // Data[I and 15 = 13] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R9D
  AND   ECX, R10D
  MOV   EDX, R8D
  AND   EDX, R9D
  OR    ECX, EDX
  LEA   R11D,[R11D + ECX + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 46. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 11]
  XOR   EBP, [RSP + 4 * 6]
  XOR   EBP, [RSP]
  XOR   EBP, [RSP + 4 * 14]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 14], EBP // Data[I and 15 = 14] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R8D
  AND   ECX, R9D
  MOV   EDX, EAX
  AND   EDX, R8D
  OR    ECX, EDX
  LEA   R10D,[R10D + ECX + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 47. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 1]
  XOR   EBX, [RSP + 4 * 15]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 15], EBX // Data[I and 15 = 15] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, EAX
  AND   ECX, R8D
  MOV   EDX, R11D
  AND   EDX, EAX
  OR    ECX, EDX
  LEA   R9D, [R9D + ECX + K60]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 48. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // ESI already contains [RSP + 4 * 13]
  XOR   ESI, [RSP + 4 * 8]
  XOR   ESI, [RSP + 4 * 2]
  XOR   ESI, [RSP]
  ROL   ESI, 1
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 16], ESI // Data[I and 31 = 16] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R11D
  AND   ECX, EAX
  MOV   EDX, R10D
  AND   EDX, R11D
  OR    ECX, EDX
  LEA   R8D, [R8D + ECX + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 49. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBP already contains [RSP + 4 * 14]
  XOR   EBP, [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 3]
  XOR   EBP, [RSP + 4 * 1]
  ROL   EBP, 1
  ADD   EAX, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 17], EBP // Data[I and 31 = 17] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R10D
  AND   ECX, R11D
  MOV   EDX, R9D
  AND   EDX, R10D
  OR    ECX, EDX
  LEA   EAX, [EAX + ECX + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 50. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBX already contains [RSP + 4 * 15]
  XOR   EBX, [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 4]
  XOR   EBX, [RSP + 4 * 2]
  ROL   EBX, 1
  ADD   R11D,EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 18], EBX // Data[I and 31 = 18] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R9D
  AND   ECX, R10D
  MOV   EDX, R8D
  AND   EDX, R9D
  OR    ECX, EDX
  LEA   R11D,[R11D + ECX + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 51. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // ESI already contains [RSP + 4 * 16]
  XOR   ESI, [RSP + 4 * 11]
  XOR   ESI, [RSP + 4 * 5]
  XOR   ESI, [RSP + 4 * 3]
  ROL   ESI, 1
  ADD   R10D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 19], ESI // Data[I and 31 = 19] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R8D
  AND   ECX, R9D
  MOV   EDX, EAX
  AND   EDX, R8D
  OR    ECX, EDX
  LEA   R10D,[R10D + ECX + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 52. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 17]
  XOR   EBP, [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 6]
  XOR   EBP, [RSP + 4 * 4]
  ROL   EBP, 1
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 20], EBP // Data[I and 31 = 20] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, EAX
  AND   ECX, R8D
  MOV   EDX, R11D
  AND   EDX, EAX
  OR    ECX, EDX
  LEA   R9D, [R9D + ECX + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 53. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 18]
  XOR   EBX, [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 7]
  XOR   EBX, [RSP + 4 * 5]
  ROL   EBX, 1
  ADD   R8D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 21], EBX // Data[I and 31 = 21] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R11D
  AND   ECX, EAX
  MOV   EDX, R10D
  AND   EDX, R11D
  OR    ECX, EDX
  LEA   R8D, [R8D + ECX + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 54. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // ESI already contains [RSP + 4 * 19]
  XOR   ESI, [RSP + 4 * 14]
  XOR   ESI, [RSP + 4 * 8]
  XOR   ESI, [RSP + 4 * 6]
  ROL   ESI, 1
  ADD   EAX, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 22], ESI // Data[I and 31 = 22] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R10D
  AND   ECX, R11D
  MOV   EDX, R9D
  AND   EDX, R10D
  OR    ECX, EDX
  LEA   EAX, [EAX + ECX + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 55. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // EBP already contains [RSP + 4 * 20]
  XOR   EBP, [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 9]
  XOR   EBP, [RSP + 4 * 7]
  ROL   EBP, 1
  ADD   R11D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 23], EBP // Data[I and 31 = 23] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R9D
  AND   ECX, R10D
  MOV   EDX, R8D
  AND   EDX, R9D
  OR    ECX, EDX
  LEA   R11D,[R11D + ECX + K60]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 56. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBX already contains [RSP + 4 * 21]
  XOR   EBX, [RSP + 4 * 16]
  XOR   EBX, [RSP + 4 * 10]
  XOR   EBX, [RSP + 4 * 8]
  ROL   EBX, 1
  ADD   R10D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 24], EBX // Data[I and 31 = 24] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R8D
  AND   ECX, R9D
  MOV   EDX, EAX
  AND   EDX, R8D
  OR    ECX, EDX
  LEA   R10D,[R10D + ECX + K60]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 57. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // ESI already contains [RSP + 4 * 22]
  XOR   ESI, [RSP + 4 * 17]
  XOR   ESI, [RSP + 4 * 11]
  XOR   ESI, [RSP + 4 * 9]
  ROL   ESI, 1
  ADD   R9D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 25], ESI // Data[I and 31 = 25] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, EAX
  AND   ECX, R8D
  MOV   EDX, R11D
  AND   EDX, EAX
  OR    ECX, EDX
  LEA   R9D, [R9D + ECX + K60]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 58. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBP already contains [RSP + 4 * 23]
  XOR   EBP, [RSP + 4 * 18]
  XOR   EBP, [RSP + 4 * 12]
  XOR   EBP, [RSP + 4 * 10]
  ROL   EBP, 1
  ADD   R8D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 26], EBP // Data[I and 31 = 26] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R11D
  AND   ECX, EAX
  MOV   EDX, R10D
  AND   EDX, R11D
  OR    ECX, EDX
  LEA   R8D, [R8D + ECX + K60]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 59. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // EBX already contains [RSP + 4 * 24]
  XOR   EBX, [RSP + 4 * 19]
  XOR   EBX, [RSP + 4 * 13]
  XOR   EBX, [RSP + 4 * 11]
  ROL   EBX, 1
  ADD   EAX, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 27], EBX // Data[I and 31 = 27] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (((W or X) and Y) or (W and X)) + K60;
  OR    ECX, R10D
  AND   ECX, R11D
  MOV   EDX, R9D
  AND   EDX, R10D
  OR    ECX, EDX
  LEA   EAX, [EAX + ECX + K60]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 60. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  // ESI already contains [RSP + 4 * 25]
  XOR   ESI, [RSP + 4 * 20]
  XOR   ESI, [RSP + 4 * 14]
  XOR   ESI, [RSP + 4 * 12]
  ROL   ESI, 1
  ADD   R11D,ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 28], ESI // Data[I and 31 = 28] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 61. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // EBP already contains [RSP + 4 * 26]
  XOR   EBP, [RSP + 4 * 21]
  XOR   EBP, [RSP + 4 * 15]
  XOR   EBP, [RSP + 4 * 13]
  ROL   EBP, 1
  ADD   R10D,EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 29], EBP // Data[I and 31 = 29] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K80;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 62. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBX already contains [RSP + 4 * 27]
  XOR   EBX, [RSP + 4 * 22]
  XOR   EBX, [RSP + 4 * 16]
  XOR   EBX, [RSP + 4 * 14]
  ROL   EBX, 1
  ADD   R9D, EBX // Z := Z + Blkv;
  MOV   [RSP + 4 * 30], EBX // Data[I and 31 = 30] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K80;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K80]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 63. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // ESI already contains [RSP + 4 * 28]
  XOR   ESI, [RSP + 4 * 23]
  XOR   ESI, [RSP + 4 * 17]
  XOR   ESI, [RSP + 4 * 15]
  ROL   ESI, 1
  ADD   R8D, ESI // Z := Z + Blkv;
  MOV   [RSP + 4 * 31], ESI // Data[I and 31 = 31] := Blkv;
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 64. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  MOV   ECX, [RSP + 4 * 26]
  XOR   ECX, [RSP + 4 * 16]
  XOR   ECX, [RSP + 4 * 4]
  XOR   ECX, [RSP]
  ROL   ECX, 2
  ADD   EAX, ECX // Z := Z + Blkv;
  MOV   [RSP], ECX // Data[I and 31 = 0] := Blkv;
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 65. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  MOV   ECX, [RSP + 4 * 27]
  XOR   ECX, [RSP + 4 * 17]
  XOR   ECX, [RSP + 4 * 5]
  XOR   ECX, [RSP + 4 * 1]
  ROL   ECX, 2
  ADD   R11D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 1], ECX // Data[I and 31 = 1] := Blkv;
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 66. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  MOV   ECX, [RSP + 4 * 28]
  XOR   ECX, [RSP + 4 * 18]
  XOR   ECX, [RSP + 4 * 6]
  XOR   ECX, [RSP + 4 * 2]
  ROL   ECX, 2
  ADD   R10D,ECX // Z := Z + Blkv;
  MOV   [RSP + 4 * 2], ECX // Data[I and 31 = 2] := Blkv;
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K80;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 67. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  // EBP already contains [RSP + 4 * 29]
  XOR   EBP, [RSP + 4 * 19]
  XOR   EBP, [RSP + 4 * 7]
  XOR   EBP, [RSP + 4 * 3]
  ROL   EBP, 2
  ADD   R9D, EBP // Z := Z + Blkv;
  MOV   [RSP + 4 * 3], EBP // Data[I and 31 = 3] := Blkv;
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K80;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K80]
  ROR   R11D, 2 // W := RorDWord(W, 2);

  // Round 68. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // EBX already contains [RSP + 4 * 30]
  XOR   EBX, [RSP + 4 * 20]
  XOR   EBX, [RSP + 4 * 8]
  XOR   EBX, [RSP + 4 * 4]
  ROL   EBX, 2
  ADD   R8D, EBX // Z := Z + Blkv;
  // Data[I and 31 = 4] := Blkv; Don't actually write it to the stack, but preserve it in EBX to merge with RSI below.
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K80]
  ROR   R10D, 2 // W := RorDWord(W, 2);

  // Round 69. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // ESI already contains [RSP + 4 * 31]
  XOR   ESI, [RSP + 4 * 21]
  XOR   ESI, [RSP + 4 * 9]
  XOR   ESI, [RSP + 4 * 5]
  ROL   ESI, 2
  ADD   EAX, ESI // Z := Z + Blkv;
  SHL   RSI, 32 // Data[I and 31 = 5] := Blkv; - merge with Data[I and 31 = 4] above.
  OR    RSI, RBX
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 70. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  MOV   EDX, [RSP]
  XOR   EDX, [RSP + 4 * 22]
  XOR   EDX, [RSP + 4 * 10]
  XOR   EDX, [RSP + 4 * 6]
  ROL   EDX, 2
  ADD   R11D,EDX // Z := Z + Blkv;
  // Data[I and 31 = 6] := Blkv; Don't actually write it to the stack, but preserve it in EDX to merge with RCX below.
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D,ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D,[R11D + ECX + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 71. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  MOV   ECX, [RSP + 4 * 1]
  XOR   ECX, [RSP + 4 * 23]
  XOR   ECX, [RSP + 4 * 11]
  XOR   ECX, [RSP + 4 * 7]
  ROL   ECX, 2
  ADD   R10D,ECX // Z := Z + Blkv;
  SHL   RCX, 32
  OR    RDX, RCX // Data[I and 31 = 7] := Blkv; - merge with Data[I and 31 = 6] above.
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K80;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 72. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  MOV   EBX, [RSP + 4 * 2]
  XOR   EBX, [RSP + 4 * 24]
  XOR   EBX, [RSP + 4 * 12]
  XOR   EBX, [RSP + 4 * 8]
  ROL   EBX, 2
  ADD   R9D, EBX // Z := Z + Blkv;
  // Data[I and 31 = 8] := Blkv; Don't actually write it to the stack, but preserve it in EBX to merge with RBP below.
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K80;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K80]
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
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 74. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  // RSI already contains [RSP + 4 * 4] and [RSP + 4 * 5]
  XOR   RSI, [RSP + 4 * 26]
  XOR   RSI, [RSP + 4 * 14]
  XOR   RSI, [RSP + 4 * 10]
  MOV   ECX, ESI
  ROL   ECX, 2
  ADD   EAX, ECX // Z := Z + Blkv;
  // Data[I and 31 = 10] := Blkv; - not required.
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  // Round 75. V = EAX, W = R8D, X = R9D, Y = R10D, Z = R11D
  SHR   RSI, 32
  ROL   ESI, 2
  ADD   R11D, ESI // Z := Z + Blkv;
  // Data[I and 31 = 11] := Blkv; - not required.
  MOV   ECX, EAX // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R11D, ECX
  MOV   ECX, R8D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R9D
  XOR   ECX, R10D
  LEA   R11D, [R11D + ECX + K80]
  ROR   R8D, 2 // W := RorDWord(W, 2);

  // Round 76. V = R11D, W = EAX, X = R8D, Y = R9D, Z = R10D
  // RDX already contains [RSP + 4 * 6] and [RSP + 4 * 7]
  XOR   RDX, [RSP + 4 * 28]
  XOR   RDX, [RSP + 4 * 16]
  XOR   RDX, [RSP + 4 * 12]
  MOV   ECX, EDX
  ROL   ECX, 2
  ADD   R10D,ECX // Z := Z + Blkv;
  // Data[I and 31 = 12] := Blkv; - not required.
  MOV   ECX, R11D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R10D,ECX
  MOV   ECX, EAX // ... + (W xor X xor Y) + K80;
  XOR   ECX, R8D
  XOR   ECX, R9D
  LEA   R10D,[R10D + ECX + K80]
  ROR   EAX, 2 // W := RorDWord(W, 2);

  // Round 77. V = R10D, W = R11D, X = EAX, Y = R8D, Z = R9D
  SHR   RDX, 32
  ROL   EDX, 2
  ADD   R9D, EDX // Z := Z + Blkv;
  // Data[I and 31 = 13] := Blkv; - not required.
  MOV   ECX, R10D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R9D, ECX
  MOV   ECX, R11D // ... + (W xor X xor Y) + K80;
  XOR   ECX, EAX
  XOR   ECX, R8D
  LEA   R9D, [R9D + ECX + K80]
  ROR   R11D,2 // W := RorDWord(W, 2);

  // Round 78. V = R9D, W = R10D, X = R11D, Y = EAX, Z = R8D
  // RBX already contains [RSP + 4 * 8] and [RSP + 4 * 9]
  XOR   RBX, [RSP + 4 * 30]
  XOR   RBX, [RSP + 4 * 18]
  XOR   RBX, [RSP + 4 * 14]
  MOV   ECX, EBX
  ROL   ECX, 2
  ADD   R8D, ECX // Z := Z + Blkv;
  // Data[I and 31 = 14] := Blkv; - not required.
  MOV   ECX, R9D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   R8D, ECX
  MOV   ECX, R10D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R11D
  XOR   ECX, EAX
  LEA   R8D, [R8D + ECX + K80]
  ROR   R10D,2 // W := RorDWord(W, 2);

  // Round 79. V = R8D, W = R9D, X = R10D, Y = R11D, Z = EAX
  SHR   RBX, 32
  ROL   EBX, 2
  ADD   EAX, EBX // Z := Z + Blkv;
  // Data[I and 31 = 15] := Blkv; - not required.
  MOV   ECX, R8D // Z := Z + RolDWord(V, 5);
  ROL   ECX, 5
  ADD   EAX, ECX
  MOV   ECX, R9D // ... + (W xor X xor Y) + K80;
  XOR   ECX, R10D
  XOR   ECX, R11D
  LEA   EAX, [EAX + ECX + K80]
  ROR   R9D, 2 // W := RorDWord(W, 2);

  ADD   TSHA1Context.State[RDI], EAX // ctx.State[0 .. 4] += A, B, C, D, E
  ADD   TSHA1Context.State[RDI + 4 * 1], R8D
  ADD   TSHA1Context.State[RDI + 4 * 2], R9D
  ADD   TSHA1Context.State[RDI + 4 * 3], R10D
  ADD   TSHA1Context.State[RDI + 4 * 4], R11D
  ADD	QWORD PTR TSHA1Context.Length[RDI], 64 // ctx.Length += 64

  // Restore stack
  LEA   RSP, [RSP+128]
  POP   RBP
  POP   RBX
end;
