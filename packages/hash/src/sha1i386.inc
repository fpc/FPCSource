procedure SHA1Transform(var ctx: TSHA1Context; const Buf: Pointer); assembler; nostackframe;
// eax = ctx, edx = buf
{$asmmode intel}
{$warn 7102 off : Use of [EBP + offset] for parameters invalid here} // EBP is used for 'E' rather than for base pointer.
{$warn 7104 off : Use of [EBP-offset] is not recommended for local variable access} // False warning because of treating K60 and K80 as negative offsets.
asm
  push  ebx
  push  esi
  push  edi
  push  ebp
  push  eax // Save ctx.
  sub   esp, 64 // [esp] = Data.

  // ebx = A, ecx = B, esi = C, edi = D, ebp = E.
  mov   ebx, TSHA1Context.State[eax] // A, B, C, D, E := *ctx.State[0 .. 4].
  mov   ecx, TSHA1Context.State[eax + 4 * 1]
  mov   esi, TSHA1Context.State[eax + 4 * 2]
  mov   edi, TSHA1Context.State[eax + 4 * 3]
  mov   ebp, TSHA1Context.State[eax + 4 * 4] // From now on, eax is used for temporaries. Edx is still required for rounds 0..15 to read buf parts.

  // Round 0. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [edx] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 0]));
  bswap eax
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp], eax // Data[I and 15 = 0] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebp, eax
  mov   eax, esi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, edi
  and   eax, ecx
  xor   eax, edi
  lea   ebp, [ebp + eax + K20]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 1. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [edx + 4 * 1] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 1]));
  bswap eax
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4], eax // Data[I and 15 = 1] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   edi, eax
  mov   eax, ecx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, esi
  and   eax, ebx
  xor   eax, esi
  lea   edi, [edi + eax + K20]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 2. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [edx + 4 * 2] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 2]));
  bswap eax
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 2], eax // Data[I and 15 = 2] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   esi, eax
  mov   eax, ebx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ecx
  and   eax, ebp
  xor   eax, ecx
  lea   esi, [esi + eax + K20]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 3. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [edx + 4 * 3] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 3]));
  bswap eax
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 3], eax // Data[I and 15 = 3] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ecx, eax
  mov   eax, ebp // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebx
  and   eax, edi
  xor   eax, ebx
  lea   ecx, [ecx + eax + K20]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 4. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [edx + 4 * 4] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 4]));
  bswap eax
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 4], eax // Data[I and 15 = 4] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebx, eax
  mov   eax, edi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebp
  and   eax, esi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K20]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 5. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [edx + 4 * 5] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 5]));
  bswap eax
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 5], eax // Data[I and 15 = 5] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebp, eax
  mov   eax, esi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, edi
  and   eax, ecx
  xor   eax, edi
  lea   ebp, [ebp + eax + K20]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 6. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [edx + 4 * 6] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 6]));
  bswap eax
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 6], eax // Data[I and 15 = 6] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   edi, eax
  mov   eax, ecx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, esi
  and   eax, ebx
  xor   eax, esi
  lea   edi, [edi + eax + K20]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 7. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [edx + 4 * 7] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 7]));
  bswap eax
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 7], eax // Data[I and 15 = 7] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   esi, eax
  mov   eax, ebx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ecx
  and   eax, ebp
  xor   eax, ecx
  lea   esi, [esi + eax + K20]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 8. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [edx + 4 * 8] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 8]));
  bswap eax
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 8], eax // Data[I and 15 = 8] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ecx, eax
  mov   eax, ebp // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebx
  and   eax, edi
  xor   eax, ebx
  lea   ecx, [ecx + eax + K20]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 9. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [edx + 4 * 9] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 9]));
  bswap eax
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 9], eax // Data[I and 15 = 9] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebx, eax
  mov   eax, edi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebp
  and   eax, esi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K20]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 10. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [edx + 4 * 10] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 10]));
  bswap eax
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 10], eax // Data[I and 15 = 10] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebp, eax
  mov   eax, esi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, edi
  and   eax, ecx
  xor   eax, edi
  lea   ebp, [ebp + eax + K20]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 11. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [edx + 4 * 11] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 11]));
  bswap eax
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 11], eax // Data[I and 15 = 11] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   edi, eax
  mov   eax, ecx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, esi
  and   eax, ebx
  xor   eax, esi
  lea   edi, [edi + eax + K20]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 12. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [edx + 4 * 12] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 12]));
  bswap eax
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 12], eax // Data[I and 15 = 12] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   esi, eax
  mov   eax, ebx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ecx
  and   eax, ebp
  xor   eax, ecx
  lea   esi, [esi + eax + K20]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 13. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [edx + 4 * 13] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 13]));
  bswap eax
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 13], eax // Data[I and 15 = 13] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ecx, eax
  mov   eax, ebp // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebx
  and   eax, edi
  xor   eax, ebx
  lea   ecx, [ecx + eax + K20]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 14. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [edx + 4 * 14] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 14]));
  bswap eax
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 14], eax // Data[I and 15 = 14] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebx, eax
  mov   eax, edi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebp
  and   eax, esi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K20]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 15. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [edx + 4 * 15] // Blkv := BEtoN(Unaligned(PCardinal(Buf)[I = 15]));
  bswap eax
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 15], eax // Data[I and 15 = 15] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebp, eax
  mov   eax, esi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, edi
  and   eax, ecx
  xor   eax, edi
  lea   ebp, [ebp + eax + K20]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // edx is no longer of interest, it will be used as a temporary in Round 40..59.

  // Round 16. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 13] // Blkv := RolDWord(Data[(I + 13) and 15 = 13] xor Data[(I + 8) and 15 = 8] xor Data[(I + 2) and 15 = 2] xor Data[I and 15 = 0], 1);
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp], eax // Data[I and 15 = 0] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   edi, eax
  mov   eax, ecx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, esi
  and   eax, ebx
  xor   eax, esi
  lea   edi, [edi + eax + K20]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 17. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 14] // Blkv := RolDWord(Data[(I + 13) and 15 = 14] xor Data[(I + 8) and 15 = 9] xor Data[(I + 2) and 15 = 3] xor Data[I and 15 = 1], 1);
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 1]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 1], eax // Data[I and 15 = 1] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   esi, eax
  mov   eax, ebx // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ecx
  and   eax, ebp
  xor   eax, ecx
  lea   esi, [esi + eax + K20]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 18. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 15] // Blkv := RolDWord(Data[(I + 13) and 15 = 15] xor Data[(I + 8) and 15 = 10] xor Data[(I + 2) and 15 = 4] xor Data[I and 15 = 2], 1);
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 2]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 2], eax // Data[I and 15 = 2] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ecx, eax
  mov   eax, ebp // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebx
  and   eax, edi
  xor   eax, ebx
  lea   ecx, [ecx + eax + K20]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 19. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp] // Blkv := RolDWord(Data[(I + 13) and 15 = 0] xor Data[(I + 8) and 15 = 11] xor Data[(I + 2) and 15 = 5] xor Data[I and 15 = 3], 1);
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 3]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 3], eax // Data[I and 15 = 3] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5)
  rol   eax, 5
  add   ebx, eax
  mov   eax, edi // ... + ((W and (X xor Y)) xor Y);
  xor   eax, ebp
  and   eax, esi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K20]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 20. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 1] // Blkv := RolDWord(Data[(I + 13) and 15 = 1] xor Data[(I + 8) and 15 = 12] xor Data[(I + 2) and 15 = 6] xor Data[I and 15 = 4], 1);
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp + 4 * 4]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 4], eax // Data[I and 15 = 4] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K40;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K40]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 21. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 2] // Blkv := RolDWord(Data[(I + 13) and 15 = 2] xor Data[(I + 8) and 15 = 13] xor Data[(I + 2) and 15 = 7] xor Data[I and 15 = 5], 1);
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 5]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 5], eax // Data[I and 15 = 5] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K40;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K40]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 22. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 3] // Blkv := RolDWord(Data[(I + 13) and 15 = 3] xor Data[(I + 8) and 15 = 14] xor Data[(I + 2) and 15 = 8] xor Data[I and 15 = 6], 1);
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 6]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 6], eax // Data[I and 15 = 6] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K40;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K40]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 23. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 4] // Blkv := RolDWord(Data[(I + 13) and 15 = 4] xor Data[(I + 8) and 15 = 15] xor Data[(I + 2) and 15 = 9] xor Data[I and 15 = 7], 1);
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 7]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 7], eax // Data[I and 15 = 7] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K40;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K40]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 24. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 5] // Blkv := RolDWord(Data[(I + 13) and 15 = 5] xor Data[(I + 8) and 15 = 0] xor Data[(I + 2) and 15 = 10] xor Data[I and 15 = 8], 1);
  xor   eax, [esp]
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 8]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 8], eax // Data[I and 15 = 8] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K40;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K40]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 25. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 6] // Blkv := RolDWord(Data[(I + 13) and 15 = 6] xor Data[(I + 8) and 15 = 1] xor Data[(I + 2) and 15 = 11] xor Data[I and 15 = 9], 1);
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 9]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 9], eax // Data[I and 15 = 9] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K40;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K40]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 26. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 7] // Blkv := RolDWord(Data[(I + 13) and 15 = 7] xor Data[(I + 8) and 15 = 2] xor Data[(I + 2) and 15 = 12] xor Data[I and 15 = 10], 1);
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 10]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 10], eax // Data[I and 15 = 10] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K40;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K40]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 27. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 8] // Blkv := RolDWord(Data[(I + 13) and 15 = 8] xor Data[(I + 8) and 15 = 3] xor Data[(I + 2) and 15 = 13] xor Data[I and 15 = 11], 1);
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 11]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 11], eax // Data[I and 15 = 11] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K40;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K40]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 28. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 9] // Blkv := RolDWord(Data[(I + 13) and 15 = 9] xor Data[(I + 8) and 15 = 4] xor Data[(I + 2) and 15 = 14] xor Data[I and 15 = 12], 1);
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 12]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 12], eax // Data[I and 15 = 12] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K40;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K40]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 29. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 10] // Blkv := RolDWord(Data[(I + 13) and 15 = 10] xor Data[(I + 8) and 15 = 5] xor Data[(I + 2) and 15 = 15] xor Data[I and 15 = 13], 1);
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 13]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 13], eax // Data[I and 15 = 13] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K40;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K40]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 30. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 11] // Blkv := RolDWord(Data[(I + 13) and 15 = 11] xor Data[(I + 8) and 15 = 6] xor Data[(I + 2) and 15 = 0] xor Data[I and 15 = 14], 1);
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp]
  xor   eax, [esp + 4 * 14]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 14], eax // Data[I and 15 = 14] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K40;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K40]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 31. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 12] // Blkv := RolDWord(Data[(I + 13) and 15 = 12] xor Data[(I + 8) and 15 = 7] xor Data[(I + 2) and 15 = 1] xor Data[I and 15 = 15], 1);
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 15]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 15], eax // Data[I and 15 = 15] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K40;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K40]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 32. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 13] // Blkv := RolDWord(Data[(I + 13) and 15 = 13] xor Data[(I + 8) and 15 = 8] xor Data[(I + 2) and 15 = 2] xor Data[I and 15 = 0], 1);
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp], eax // Data[I and 15 = 0] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K40;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K40]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 33. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 14] // Blkv := RolDWord(Data[(I + 13) and 15 = 14] xor Data[(I + 8) and 15 = 9] xor Data[(I + 2) and 15 = 3] xor Data[I and 15 = 1], 1);
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 1]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 1], eax // Data[I and 15 = 1] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K40;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K40]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 34. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 15] // Blkv := RolDWord(Data[(I + 13) and 15 = 15] xor Data[(I + 8) and 15 = 10] xor Data[(I + 2) and 15 = 4] xor Data[I and 15 = 2], 1);
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 2]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 2], eax // Data[I and 15 = 2] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K40;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K40]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 35. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp] // Blkv := RolDWord(Data[(I + 13) and 15 = 0] xor Data[(I + 8) and 15 = 11] xor Data[(I + 2) and 15 = 5] xor Data[I and 15 = 3], 1);
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 3]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 3], eax // Data[I and 15 = 3] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K40;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K40]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 36. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 1] // Blkv := RolDWord(Data[(I + 13) and 15 = 1] xor Data[(I + 8) and 15 = 12] xor Data[(I + 2) and 15 = 6] xor Data[I and 15 = 4], 1);
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp + 4 * 4]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 4], eax // Data[I and 15 = 4] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K40;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K40]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 37. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 2] // Blkv := RolDWord(Data[(I + 13) and 15 = 2] xor Data[(I + 8) and 15 = 13] xor Data[(I + 2) and 15 = 7] xor Data[I and 15 = 5], 1);
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 5]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 5], eax // Data[I and 15 = 5] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K40;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K40]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 38. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 3] // Blkv := RolDWord(Data[(I + 13) and 15 = 3] xor Data[(I + 8) and 15 = 14] xor Data[(I + 2) and 15 = 8] xor Data[I and 15 = 6], 1);
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 6]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 6], eax // Data[I and 15 = 6] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K40;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K40]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 39. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 4] // Blkv := RolDWord(Data[(I + 13) and 15 = 4] xor Data[(I + 8) and 15 = 15] xor Data[(I + 2) and 15 = 9] xor Data[I and 15 = 7], 1);
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 7]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 7], eax // Data[I and 15 = 7] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K40;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K40]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 40. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 5] // Blkv := RolDWord(Data[(I + 13) and 15 = 5] xor Data[(I + 8) and 15 = 0] xor Data[(I + 2) and 15 = 10] xor Data[I and 15 = 8], 1);
  xor   eax, [esp]
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 8]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 8], eax // Data[I and 15 = 8] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, esi
  and   eax, edi
  mov   edx, ecx
  and   edx, esi
  or    eax, edx
  lea   ebp, [ebp + eax + K60]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 41. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 6] // Blkv := RolDWord(Data[(I + 13) and 15 = 6] xor Data[(I + 8) and 15 = 1] xor Data[(I + 2) and 15 = 11] xor Data[I and 15 = 9], 1);
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 9]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 9], eax // Data[I and 15 = 9] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ecx
  and   eax, esi
  mov   edx, ebx
  and   edx, ecx
  or    eax, edx
  lea   edi, [edi + eax + K60]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 42. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 7] // Blkv := RolDWord(Data[(I + 13) and 15 = 7] xor Data[(I + 8) and 15 = 2] xor Data[(I + 2) and 15 = 12] xor Data[I and 15 = 10], 1);
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 10]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 10], eax // Data[I and 15 = 10] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebx
  and   eax, ecx
  mov   edx, ebp
  and   edx, ebx
  or    eax, edx
  lea   esi, [esi + eax + K60]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 43. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 8] // Blkv := RolDWord(Data[(I + 13) and 15 = 8] xor Data[(I + 8) and 15 = 3] xor Data[(I + 2) and 15 = 13] xor Data[I and 15 = 11], 1);
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 11]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 11], eax // Data[I and 15 = 11] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebp
  and   eax, ebx
  mov   edx, edi
  and   edx, ebp
  or    eax, edx
  lea   ecx, [ecx + eax + K60]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 44. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 9] // Blkv := RolDWord(Data[(I + 13) and 15 = 9] xor Data[(I + 8) and 15 = 4] xor Data[(I + 2) and 15 = 14] xor Data[I and 15 = 12], 1);
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 12]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 12], eax // Data[I and 15 = 12] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, edi
  and   eax, ebp
  mov   edx, esi
  and   edx, edi
  or    eax, edx
  lea   ebx, [ebx + eax + K60]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 45. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 10] // Blkv := RolDWord(Data[(I + 13) and 15 = 10] xor Data[(I + 8) and 15 = 5] xor Data[(I + 2) and 15 = 15] xor Data[I and 15 = 13], 1);
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 13]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 13], eax // Data[I and 15 = 13] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, esi
  and   eax, edi
  mov   edx, ecx
  and   edx, esi
  or    eax, edx
  lea   ebp, [ebp + eax + K60]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 46. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 11] // Blkv := RolDWord(Data[(I + 13) and 15 = 11] xor Data[(I + 8) and 15 = 6] xor Data[(I + 2) and 15 = 0] xor Data[I and 15 = 14], 1);
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp]
  xor   eax, [esp + 4 * 14]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 14], eax // Data[I and 15 = 14] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ecx
  and   eax, esi
  mov   edx, ebx
  and   edx, ecx
  or    eax, edx
  lea   edi, [edi + eax + K60]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 47. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 12] // Blkv := RolDWord(Data[(I + 13) and 15 = 12] xor Data[(I + 8) and 15 = 7] xor Data[(I + 2) and 15 = 1] xor Data[I and 15 = 15], 1);
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 15]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 15], eax // Data[I and 15 = 15] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebx
  and   eax, ecx
  mov   edx, ebp
  and   edx, ebx
  or    eax, edx
  lea   esi, [esi + eax + K60]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 48. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 13] // Blkv := RolDWord(Data[(I + 13) and 15 = 13] xor Data[(I + 8) and 15 = 8] xor Data[(I + 2) and 15 = 2] xor Data[I and 15 = 0], 1);
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp], eax // Data[I and 15 = 0] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebp
  and   eax, ebx
  mov   edx, edi
  and   edx, ebp
  or    eax, edx
  lea   ecx, [ecx + eax + K60]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 49. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 14] // Blkv := RolDWord(Data[(I + 13) and 15 = 14] xor Data[(I + 8) and 15 = 9] xor Data[(I + 2) and 15 = 3] xor Data[I and 15 = 1], 1);
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 1]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 1], eax // Data[I and 15 = 1] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, edi
  and   eax, ebp
  mov   edx, esi
  and   edx, edi
  or    eax, edx
  lea   ebx, [ebx + eax + K60]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 50. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 15] // Blkv := RolDWord(Data[(I + 13) and 15 = 15] xor Data[(I + 8) and 15 = 10] xor Data[(I + 2) and 15 = 4] xor Data[I and 15 = 2], 1);
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 2]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 2], eax // Data[I and 15 = 2] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, esi
  and   eax, edi
  mov   edx, ecx
  and   edx, esi
  or    eax, edx
  lea   ebp, [ebp + eax + K60]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 51. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp] // Blkv := RolDWord(Data[(I + 13) and 15 = 0] xor Data[(I + 8) and 15 = 11] xor Data[(I + 2) and 15 = 5] xor Data[I and 15 = 3], 1);
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 3]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 3], eax // Data[I and 15 = 3] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ecx
  and   eax, esi
  mov   edx, ebx
  and   edx, ecx
  or    eax, edx
  lea   edi, [edi + eax + K60]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 52. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 1] // Blkv := RolDWord(Data[(I + 13) and 15 = 1] xor Data[(I + 8) and 15 = 12] xor Data[(I + 2) and 15 = 6] xor Data[I and 15 = 4], 1);
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp + 4 * 4]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 4], eax // Data[I and 15 = 4] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebx
  and   eax, ecx
  mov   edx, ebp
  and   edx, ebx
  or    eax, edx
  lea   esi, [esi + eax + K60]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 53. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 2] // Blkv := RolDWord(Data[(I + 13) and 15 = 2] xor Data[(I + 8) and 15 = 13] xor Data[(I + 2) and 15 = 7] xor Data[I and 15 = 5], 1);
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 5]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 5], eax // Data[I and 15 = 5] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebp
  and   eax, ebx
  mov   edx, edi
  and   edx, ebp
  or    eax, edx
  lea   ecx, [ecx + eax + K60]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 54. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 3] // Blkv := RolDWord(Data[(I + 13) and 15 = 3] xor Data[(I + 8) and 15 = 14] xor Data[(I + 2) and 15 = 8] xor Data[I and 15 = 6], 1);
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 6]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 6], eax // Data[I and 15 = 6] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, edi
  and   eax, ebp
  mov   edx, esi
  and   edx, edi
  or    eax, edx
  lea   ebx, [ebx + eax + K60]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 55. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 4] // Blkv := RolDWord(Data[(I + 13) and 15 = 4] xor Data[(I + 8) and 15 = 15] xor Data[(I + 2) and 15 = 9] xor Data[I and 15 = 7], 1);
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 7]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 7], eax // Data[I and 15 = 7] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, esi
  and   eax, edi
  mov   edx, ecx
  and   edx, esi
  or    eax, edx
  lea   ebp, [ebp + eax + K60]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 56. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 5] // Blkv := RolDWord(Data[(I + 13) and 15 = 5] xor Data[(I + 8) and 15 = 0] xor Data[(I + 2) and 15 = 10] xor Data[I and 15 = 8], 1);
  xor   eax, [esp]
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 8]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 8], eax // Data[I and 15 = 8] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ecx
  and   eax, esi
  mov   edx, ebx
  and   edx, ecx
  or    eax, edx
  lea   edi, [edi + eax + K60]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 57. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 6] // Blkv := RolDWord(Data[(I + 13) and 15 = 6] xor Data[(I + 8) and 15 = 1] xor Data[(I + 2) and 15 = 11] xor Data[I and 15 = 9], 1);
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 9]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 9], eax // Data[I and 15 = 9] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebx
  and   eax, ecx
  mov   edx, ebp
  and   edx, ebx
  or    eax, edx
  lea   esi, [esi + eax + K60]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 58. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 7] // Blkv := RolDWord(Data[(I + 13) and 15 = 7] xor Data[(I + 8) and 15 = 2] xor Data[(I + 2) and 15 = 12] xor Data[I and 15 = 10], 1);
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 10]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 10], eax // Data[I and 15 = 10] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, ebp
  and   eax, ebx
  mov   edx, edi
  and   edx, ebp
  or    eax, edx
  lea   ecx, [ecx + eax + K60]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 59. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 8] // Blkv := RolDWord(Data[(I + 13) and 15 = 8] xor Data[(I + 8) and 15 = 3] xor Data[(I + 2) and 15 = 13] xor Data[I and 15 = 11], 1);
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 11]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 11], eax // Data[I and 15 = 11] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (((W or X) and Y) or (W and X)) + K60;
  or    eax, edi
  and   eax, ebp
  mov   edx, esi
  and   edx, edi
  or    eax, edx
  lea   ebx, [ebx + eax + K60]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 60. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 9] // Blkv := RolDWord(Data[(I + 13) and 15 = 9] xor Data[(I + 8) and 15 = 4] xor Data[(I + 2) and 15 = 14] xor Data[I and 15 = 12], 1);
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 12]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 12], eax // Data[I and 15 = 12] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K80;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K80]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 61. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 10] // Blkv := RolDWord(Data[(I + 13) and 15 = 10] xor Data[(I + 8) and 15 = 5] xor Data[(I + 2) and 15 = 15] xor Data[I and 15 = 13], 1);
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 13]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 13], eax // Data[I and 15 = 13] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K80;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K80]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 62. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 11] // Blkv := RolDWord(Data[(I + 13) and 15 = 11] xor Data[(I + 8) and 15 = 6] xor Data[(I + 2) and 15 = 0] xor Data[I and 15 = 14], 1);
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp]
  xor   eax, [esp + 4 * 14]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 14], eax // Data[I and 15 = 14] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K80;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K80]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 63. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 12] // Blkv := RolDWord(Data[(I + 13) and 15 = 12] xor Data[(I + 8) and 15 = 7] xor Data[(I + 2) and 15 = 1] xor Data[I and 15 = 15], 1);
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 15]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 15], eax // Data[I and 15 = 15] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K80;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K80]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 64. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 13] // Blkv := RolDWord(Data[(I + 13) and 15 = 13] xor Data[(I + 8) and 15 = 8] xor Data[(I + 2) and 15 = 2] xor Data[I and 15 = 0], 1);
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp], eax // Data[I and 15 = 0] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K80;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K80]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 65. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 14] // Blkv := RolDWord(Data[(I + 13) and 15 = 14] xor Data[(I + 8) and 15 = 9] xor Data[(I + 2) and 15 = 3] xor Data[I and 15 = 1], 1);
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 1]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 1], eax // Data[I and 15 = 1] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K80;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K80]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 66. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 15] // Blkv := RolDWord(Data[(I + 13) and 15 = 15] xor Data[(I + 8) and 15 = 10] xor Data[(I + 2) and 15 = 4] xor Data[I and 15 = 2], 1);
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 2]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 2], eax // Data[I and 15 = 2] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K80;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K80]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 67. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp] // Blkv := RolDWord(Data[(I + 13) and 15 = 0] xor Data[(I + 8) and 15 = 11] xor Data[(I + 2) and 15 = 5] xor Data[I and 15 = 3], 1);
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 3]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 3], eax // Data[I and 15 = 3] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K80;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K80]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 68. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 1] // Blkv := RolDWord(Data[(I + 13) and 15 = 1] xor Data[(I + 8) and 15 = 12] xor Data[(I + 2) and 15 = 6] xor Data[I and 15 = 4], 1);
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp + 4 * 4]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 4], eax // Data[I and 15 = 4] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K80;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K80]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 69. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 2] // Blkv := RolDWord(Data[(I + 13) and 15 = 2] xor Data[(I + 8) and 15 = 13] xor Data[(I + 2) and 15 = 7] xor Data[I and 15 = 5], 1);
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 5]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 5], eax // Data[I and 15 = 5] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K80;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K80]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 70. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 3] // Blkv := RolDWord(Data[(I + 13) and 15 = 3] xor Data[(I + 8) and 15 = 14] xor Data[(I + 2) and 15 = 8] xor Data[I and 15 = 6], 1);
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 8]
  xor   eax, [esp + 4 * 6]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 6], eax // Data[I and 15 = 6] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K80;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K80]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 71. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 4] // Blkv := RolDWord(Data[(I + 13) and 15 = 4] xor Data[(I + 8) and 15 = 15] xor Data[(I + 2) and 15 = 9] xor Data[I and 15 = 7], 1);
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 9]
  xor   eax, [esp + 4 * 7]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 7], eax // Data[I and 15 = 7] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K80;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K80]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 72. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 5] // Blkv := RolDWord(Data[(I + 13) and 15 = 5] xor Data[(I + 8) and 15 = 0] xor Data[(I + 2) and 15 = 10] xor Data[I and 15 = 8], 1);
  xor   eax, [esp]
  xor   eax, [esp + 4 * 10]
  xor   eax, [esp + 4 * 8]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 8], eax // Data[I and 15 = 8] := Blkv;
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K80;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K80]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 73. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 6] // Blkv := RolDWord(Data[(I + 13) and 15 = 6] xor Data[(I + 8) and 15 = 1] xor Data[(I + 2) and 15 = 11] xor Data[I and 15 = 9], 1);
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 11]
  xor   eax, [esp + 4 * 9]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 9], eax // Data[I and 15 = 9] := Blkv;
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K80;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K80]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 74. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 7] // Blkv := RolDWord(Data[(I + 13) and 15 = 7] xor Data[(I + 8) and 15 = 2] xor Data[(I + 2) and 15 = 12] xor Data[I and 15 = 10], 1);
  xor   eax, [esp + 4 * 2]
  xor   eax, [esp + 4 * 12]
  xor   eax, [esp + 4 * 10]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  mov   [esp + 4 * 10], eax // Data[I and 15 = 10] := Blkv;
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K80;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K80]
  ror   esi, 2 // W := RorDWord(W, 2);

  // Round 75. V = ebx, W = ecx, X = esi, Y = edi, Z = ebp
  mov   eax, [esp + 4 * 8] // Blkv := RolDWord(Data[(I + 13) and 15 = 8] xor Data[(I + 8) and 15 = 3] xor Data[(I + 2) and 15 = 13] xor Data[I and 15 = 11], 1);
  xor   eax, [esp + 4 * 3]
  xor   eax, [esp + 4 * 13]
  xor   eax, [esp + 4 * 11]
  rol   eax, 1
  add   ebp, eax // Z := Z + Blkv;
  mov   [esp + 4 * 11], eax // Data[I and 15 = 11] := Blkv;
  mov   eax, ebx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebp, eax
  mov   eax, ecx // ... + (W xor X xor Y) + K80;
  xor   eax, esi
  xor   eax, edi
  lea   ebp, [ebp + eax + K80]
  ror   ecx, 2 // W := RorDWord(W, 2);

  // Round 76. V = ebp, W = ebx, X = ecx, Y = esi, Z = edi
  mov   eax, [esp + 4 * 9] // Blkv := RolDWord(Data[(I + 13) and 15 = 9] xor Data[(I + 8) and 15 = 4] xor Data[(I + 2) and 15 = 14] xor Data[I and 15 = 12], 1);
  xor   eax, [esp + 4 * 4]
  xor   eax, [esp + 4 * 14]
  xor   eax, [esp + 4 * 12]
  rol   eax, 1
  add   edi, eax // Z := Z + Blkv;
  mov   [esp + 4 * 12], eax // Data[I and 15 = 12] := Blkv;
  mov   eax, ebp // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   edi, eax
  mov   eax, ebx // ... + (W xor X xor Y) + K80;
  xor   eax, ecx
  xor   eax, esi
  lea   edi, [edi + eax + K80]
  ror   ebx, 2 // W := RorDWord(W, 2);

  // Round 77. V = edi, W = ebp, X = ebx, Y = ecx, Z = esi
  mov   eax, [esp + 4 * 10] // Blkv := RolDWord(Data[(I + 13) and 15 = 10] xor Data[(I + 8) and 15 = 5] xor Data[(I + 2) and 15 = 15] xor Data[I and 15 = 13], 1);
  xor   eax, [esp + 4 * 5]
  xor   eax, [esp + 4 * 15]
  xor   eax, [esp + 4 * 13]
  rol   eax, 1
  add   esi, eax // Z := Z + Blkv;
  // Data[I and 15 = 13] := Blkv; - not required.
  mov   eax, edi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   esi, eax
  mov   eax, ebp // ... + (W xor X xor Y) + K80;
  xor   eax, ebx
  xor   eax, ecx
  lea   esi, [esi + eax + K80]
  ror   ebp, 2 // W := RorDWord(W, 2);

  // Round 78. V = esi, W = edi, X = ebp, Y = ebx, Z = ecx
  mov   eax, [esp + 4 * 11] // Blkv := RolDWord(Data[(I + 13) and 15 = 11] xor Data[(I + 8) and 15 = 6] xor Data[(I + 2) and 15 = 0] xor Data[I and 15 = 14], 1);
  xor   eax, [esp + 4 * 6]
  xor   eax, [esp]
  xor   eax, [esp + 4 * 14]
  rol   eax, 1
  add   ecx, eax // Z := Z + Blkv;
  // Data[I and 15 = 14] := Blkv; - not required.
  mov   eax, esi // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ecx, eax
  mov   eax, edi // ... + (W xor X xor Y) + K80;
  xor   eax, ebp
  xor   eax, ebx
  lea   ecx, [ecx + eax + K80]
  ror   edi, 2 // W := RorDWord(W, 2);

  // Round 79. V = ecx, W = esi, X = edi, Y = ebp, Z = ebx
  mov   eax, [esp + 4 * 12] // Blkv := RolDWord(Data[(I + 13) and 15 = 12] xor Data[(I + 8) and 15 = 7] xor Data[(I + 2) and 15 = 1] xor Data[I and 15 = 15], 1);
  xor   eax, [esp + 4 * 7]
  xor   eax, [esp + 4 * 1]
  xor   eax, [esp + 4 * 15]
  rol   eax, 1
  add   ebx, eax // Z := Z + Blkv;
  // Data[I and 15 = 15] := Blkv; - not required.
  mov   eax, ecx // Z := Z + RolDWord(V, 5);
  rol   eax, 5
  add   ebx, eax
  mov   eax, esi // ... + (W xor X xor Y) + K80;
  xor   eax, edi
  xor   eax, ebp
  lea   ebx, [ebx + eax + K80]
  ror   esi, 2 // W := RorDWord(W, 2);

  add   esp, 64
  pop   eax // eax = ctx
  add   TSHA1Context.State[eax], ebx // ctx.State[0 .. 4] += A, B, C, D, E
  add   TSHA1Context.State[eax + 4 * 1], ecx
  add   TSHA1Context.State[eax + 4 * 2], esi
  add   TSHA1Context.State[eax + 4 * 3], edi
  add   TSHA1Context.State[eax + 4 * 4], ebp
  add	dword ptr TSHA1Context.Length[eax], 64 // ctx.Length += 64
  adc	dword ptr TSHA1Context.Length[eax + 4], 0
  pop   ebp
  pop   edi
  pop   esi
  pop   ebx
end;

