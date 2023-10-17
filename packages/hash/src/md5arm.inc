// ARM assembler optimized version
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// R0 = Context, R1 = Buffer
asm
  STMFD SP!, {R4, R5, R6, R7, R8, R9, R10, LR}
  ADD   R0, R0, TMDContext.State
  LDM   R0, {R4, R5, R6, R7}
  ADR   R2, .LMD5RoundConstants // It so happens that the calculated offset at this point is a valid shifter immediate (0x9F0 = $9F rol $4 = 159 rol 4)

  // Note: Two LDR's side-by-side can sometimes be macro-fused and save a cycle.

// Round 1
//R4 := R5 + roldword(dword(R4 + ((R5 and R6) or ((not R5) and R7)) + Data[0] + $d76aa478), 7);
  LDR   R3, [R1]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R5
  AND   R9, R5, R6
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #25
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R5) or ((not R4) and R6)) + Data[1] + $e8c7b756), 12);
  LDR   R3, [R1, #4]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R4
  AND   R9, R4, R5
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #20
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R4) or ((not R7) and R5)) + Data[2] + $242070db), 17);
  LDR   R3, [R1, #8]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R7
  AND   R9, R7, R4
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #15
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R7) or ((not R6) and R4)) + Data[3] + $c1bdceee), 22);
  LDR   R3, [R1, #12]
  BIC   R8, R4, R6
  AND   R9, R6, R7
  ORR   R8, R8, R9
  LDR   R10,[R2], #4 // Increment round constant pointer
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #10
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R6) or ((not R5) and R7)) + Data[4] + $f57c0faf), 7);
  LDR   R3, [R1, #16]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R5
  AND   R9, R5, R6
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #25
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R5) or ((not R4) and R6)) + Data[5] + $4787c62a), 12);
  LDR   R3, [R1, #20]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R4
  AND   R9, R4, R5
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #20
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R4) or ((not R7) and R5)) + Data[6] + $a8304613), 17);
  LDR   R3, [R1, #24]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R7
  AND   R9, R7, R4
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #15
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R7) or ((not R6) and R4)) + Data[7] + $fd469501), 22);
  LDR   R3, [R1, #28]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R6
  AND   R9, R6, R7
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #10
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R6) or ((not R5) and R7)) + Data[8] + $698098d8), 7);
  LDR   R3, [R1, #32]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R5
  AND   R9, R5, R6
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #25
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R5) or ((not R4) and R6)) + Data[9] + $8b44f7af), 12);
  LDR   R3, [R1, #36]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R4
  AND   R9, R4, R5
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #20
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R4) or ((not R7) and R5)) + Data[10] + $ffff5bb1), 17);
  LDR   R3, [R1, #40]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R7
  AND   R9, R7, R4
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #15
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R7) or ((not R6) and R4)) + Data[11] + $895cd7be), 22);
  LDR   R3, [R1, #44]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R6
  AND   R9, R6, R7
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #10
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R6) or ((not R5) and R7)) + Data[12] + $6b901122), 7);
  LDR   R3, [R1, #48]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R5
  AND   R9, R5, R6
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #25
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R5) or ((not R4) and R6)) + Data[13] + $fd987193), 12);
  LDR   R3, [R1, #52]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R4
  AND   R9, R4, R5
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #20
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R4) or ((not R7) and R5)) + Data[14] + $a679438e), 17);
  LDR   R3, [R1, #56]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R7
  AND   R9, R7, R4
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #15
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R7) or ((not R6) and R4)) + Data[15] + $49b40821), 22);
  LDR   R3, [R1, #60]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R6
  AND   R9, R6, R7
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #10
  ADD   R5, R5, R6

// Round 2
//R4 := R5 + roldword(dword(R4 + ((R5 and R7) or (R6 and (not R7))) + Data[1]  + $f61e2562),  5);
  LDR   R3, [R1, #4]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R7
  AND   R9, R5, R7
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #27
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R6) or (R5 and (not R6))) + Data[6]  + $c040b340),  9);
  LDR   R3, [R1, #24]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R6
  AND   R9, R4, R6
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #23
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R5) or (R4 and (not R5))) + Data[11] + $265e5a51), 14);
  LDR   R3, [R1, #44]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R5
  AND   R9, R7, R5
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #18
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R4) or (R7 and (not R4))) + Data[0]  + $e9b6c7aa), 20);
  LDR   R3, [R1]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R4
  AND   R9, R6, R4
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #12
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R7) or (R6 and (not R7))) + Data[5]  + $d62f105d),  5);
  LDR   R3, [R1, #20]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R7
  AND   R9, R5, R7
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #27
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R6) or (R5 and (not R6))) + Data[10]  + $02441453),  9);
  LDR   R3, [R1, #40]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R6
  AND   R9, R4, R6
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #23
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R5) or (R4 and (not R5))) + Data[15] + $d8a1e681), 14);
  LDR   R3, [R1, #60]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R5
  AND   R9, R7, R5
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #18
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R4) or (R7 and (not R4))) + Data[4]  + $e7d3fbc8), 20);
  LDR   R3, [R1, #16]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R4
  AND   R9, R6, R4
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #12
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R7) or (R6 and (not R7))) + Data[9]  + $21e1cde6),  5);
  LDR   R3, [R1, #36]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R7
  AND   R9, R5, R7
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #27
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R6) or (R5 and (not R6))) + Data[14] + $c33707d6),  9);
  LDR   R3, [R1, #56]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R6
  AND   R9, R4, R6
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #23
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R5) or (R4 and (not R5))) + Data[3]  + $f4d50d87), 14);
  LDR   R3, [R1, #12]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R5
  AND   R9, R7, R5
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #18
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R4) or (R7 and (not R4))) + Data[8]  + $455a14ed), 20);
  LDR   R3, [R1, #32]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R4
  AND   R9, R6, R4
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #12
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + ((R5 and R7) or (R6 and (not R7))) + Data[13] + $a9e3e905),  5);
  LDR   R3, [R1, #52]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R6, R7
  AND   R9, R5, R7
  ORR   R8, R8, R9
  ADD   R4, R4, R3
  ADD   R8, R8, R10
  ADD   R4, R4, R8
  ROR   R4, R4, #27
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + ((R4 and R6) or (R5 and (not R6))) + Data[2]  + $fcefa3f8),  9);
  LDR   R3, [R1, #8]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R5, R6
  AND   R9, R4, R6
  ORR   R8, R8, R9
  ADD   R7, R7, R3
  ADD   R8, R8, R10
  ADD   R7, R7, R8
  ROR   R7, R7, #23
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + ((R7 and R5) or (R4 and (not R5))) + Data[7]  + $676f02d9), 14);
  LDR   R3, [R1, #28]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R4, R5
  AND   R9, R7, R5
  ORR   R8, R8, R9
  ADD   R6, R6, R3
  ADD   R8, R8, R10
  ADD   R6, R6, R8
  ROR   R6, R6, #18
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + ((R6 and R4) or (R7 and (not R4))) + Data[12] + $8d2a4c8a), 20);
  LDR   R3, [R1, #48]
  LDR   R10,[R2], #4 // Increment round constant pointer
  BIC   R8, R7, R4
  AND   R9, R6, R4
  ORR   R8, R8, R9
  ADD   R5, R5, R3
  ADD   R8, R8, R10
  ADD   R5, R5, R8
  ROR   R5, R5, #12
  ADD   R5, R5, R6

// Round 3
//R4 := R5 + roldword(dword(R4 + (R5 xor R6 xor R7) + Data[5]  + $fffa3942),  4);
  LDR   R3, [R1, #20]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R5, R6
  ADD   R9, R3, R9
  EOR   R8, R8, R7
  ADD   R4, R4, R9
  ADD   R4, R4, R8
  ROR   R4, R4, #28
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R4 xor R5 xor R6) + Data[8]  + $8771f681), 11);
  LDR   R3, [R1, #32]
  LDR   R9 ,[R2], #4 // Increment round constant pointer
  EOR   R8, R4, R5
  ADD   R9, R3, R9
  EOR   R8, R8, R6
  ADD   R7, R7, R9
  ADD   R7, R7, R8
  ROR   R7, R7, #21
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R7 xor R4 xor R5) + Data[11] + $6d9d6122), 16);
  LDR   R3, [R1, #44]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R7, R4
  ADD   R9, R3, R9
  EOR   R8, R8, R5
  ADD   R6, R6, R9
  ADD   R6, R6, R8
  ROR   R6, R6, #16
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R6 xor R7 xor R4) + Data[14] + $fde5380c), 23);
  LDR   R3, [R1, #56]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R6, R7
  ADD   R9, R3, R9
  EOR   R8, R8, R4
  ADD   R5, R5, R9
  ADD   R5, R5, R8
  ROR   R5, R5, #9
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R5 xor R6 xor R7) + Data[1]  + $a4beea44),  4);
  LDR   R3, [R1, #4]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R5, R6
  ADD   R9, R3, R9
  EOR   R8, R8, R7
  ADD   R4, R4, R9
  ADD   R4, R4, R8
  ROR   R4, R4, #28
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R4 xor R5 xor R6) + Data[4]  + $4bdecfa9), 11);
  LDR   R3, [R1, #16]
  LDR   R9 ,[R2], #4 // Increment round constant pointer
  EOR   R8, R4, R5
  ADD   R9, R3, R9
  EOR   R8, R8, R6
  ADD   R7, R7, R9
  ADD   R7, R7, R8
  ROR   R7, R7, #21
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R7 xor R4 xor R5) + Data[7]  + $f6bb4b60), 16);
  LDR   R3, [R1, #28]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R7, R4
  ADD   R9, R3, R9
  EOR   R8, R8, R5
  ADD   R6, R6, R9
  ADD   R6, R6, R8
  ROR   R6, R6, #16
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R6 xor R7 xor R4) + Data[10] + $bebfbc70), 23);
  LDR   R3, [R1, #40]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R6, R7
  ADD   R9, R3, R9
  EOR   R8, R8, R4
  ADD   R5, R5, R9
  ADD   R5, R5, R8
  ROR   R5, R5, #9
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R5 xor R6 xor R7) + Data[13] + $289b7ec6),  4);
  LDR   R3, [R1, #52]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R5, R6
  ADD   R9, R3, R9
  EOR   R8, R8, R7
  ADD   R4, R4, R9
  ADD   R4, R4, R8
  ROR   R4, R4, #28
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R4 xor R5 xor R6) + Data[0]  + $eaa127fa), 11);
  LDR   R3, [R1]
  LDR   R9 ,[R2], #4 // Increment round constant pointer
  EOR   R8, R4, R5
  ADD   R9, R3, R9
  EOR   R8, R8, R6
  ADD   R7, R7, R9
  ADD   R7, R7, R8
  ROR   R7, R7, #21
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R7 xor R4 xor R5) + Data[3]  + $d4ef3085), 16);
  LDR   R3, [R1, #12]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R7, R4
  ADD   R9, R3, R9
  EOR   R8, R8, R5
  ADD   R6, R6, R9
  ADD   R6, R6, R8
  ROR   R6, R6, #16
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R6 xor R7 xor R4) + Data[6]  + $04881d05), 23);
  LDR   R3, [R1, #24]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R6, R7
  ADD   R9, R3, R9
  EOR   R8, R8, R4
  ADD   R5, R5, R9
  ADD   R5, R5, R8
  ROR   R5, R5, #9
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R5 xor R6 xor R7) + Data[9]  + $d9d4d039),  4);
  LDR   R3, [R1, #36]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R5, R6
  ADD   R9, R3, R9
  EOR   R8, R8, R7
  ADD   R4, R4, R9
  ADD   R4, R4, R8
  ROR   R4, R4, #28
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R4 xor R5 xor R6) + Data[12] + $e6db99e5), 11);
  LDR   R3, [R1, #48]
  LDR   R9 ,[R2], #4 // Increment round constant pointer
  EOR   R8, R4, R5
  ADD   R9, R3, R9
  EOR   R8, R8, R6
  ADD   R7, R7, R9
  ADD   R7, R7, R8
  ROR   R7, R7, #21
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R7 xor R4 xor R5) + Data[15] + $1fa27cf8), 16);
  LDR   R3, [R1, #60]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R7, R4
  ADD   R9, R3, R9
  EOR   R8, R8, R5
  ADD   R6, R6, R9
  ADD   R6, R6, R8
  ROR   R6, R6, #16
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R6 xor R7 xor R4) + Data[2]  + $c4ac5665), 23);
  LDR   R3, [R1, #8]
  LDR   R9, [R2], #4 // Increment round constant pointer
  EOR   R8, R6, R7
  ADD   R9, R3, R9
  EOR   R8, R8, R4
  ADD   R5, R5, R9
  ADD   R5, R5, R8
  ROR   R5, R5, #9
  MVN   R10,#0 // All 1s for Round 4
  ADD   R5, R5, R6

// Round 4
//R4 := R5 + roldword(dword(R4 + (R6 xor (R5 or (not R7))) + Data[0]  + $f4292244),  6);
  LDR   R3, [R1]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R7
  ADD   R9, R9, R3
  ORR   R8, R5, R8
  ADD   R4, R4, R9
  EOR   R8, R6, R8
  ADD   R4, R4, R8
  ROR   R4, R4, #26
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R5 xor (R4 or (not R6))) + Data[7]  + $432aff97), 10);
  LDR   R3, [R1, #28]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R6
  ADD   R9, R9, R3
  ORR   R8, R4, R8
  ADD   R7, R7, R9
  EOR   R8, R5, R8
  ADD   R7, R7, R8
  ROR   R7, R7, #22
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R4 xor (R7 or (not R5))) + Data[14] + $ab9423a7), 15);
  LDR   R3, [R1, #56]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R5
  ADD   R9, R9, R3
  ORR   R8, R7, R8
  ADD   R6, R6, R9
  EOR   R8, R4, R8
  ADD   R6, R6, R8
  ROR   R6, R6, #17
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R7 xor (R6 or (not R4))) + Data[5]  + $fc93a039), 21);
  LDR   R3, [R1, #20]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R4
  ADD   R9, R9, R3
  ORR   R8, R6, R8
  ADD   R5, R5, R9
  EOR   R8, R7, R8
  ADD   R5, R5, R8
  ROR   R5, R5, #11
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R6 xor (R5 or (not R7))) + Data[12] + $655b59c3),  6);
  LDR   R3, [R1, #48]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R7
  ADD   R9, R9, R3
  ORR   R8, R5, R8
  ADD   R4, R4, R9
  EOR   R8, R6, R8
  ADD   R4, R4, R8
  ROR   R4, R4, #26
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R5 xor (R4 or (not R6))) + Data[3]  + $8f0ccc92), 10);
  LDR   R3, [R1, #12]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R6
  ADD   R9, R9, R3
  ORR   R8, R4, R8
  ADD   R7, R7, R9
  EOR   R8, R5, R8
  ADD   R7, R7, R8
  ROR   R7, R7, #22
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R4 xor (R7 or (not R5))) + Data[10] + $ffeff47d), 15);
  LDR   R3, [R1, #40]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R5
  ADD   R9, R9, R3
  ORR   R8, R7, R8
  ADD   R6, R6, R9
  EOR   R8, R4, R8
  ADD   R6, R6, R8
  ROR   R6, R6, #17
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R7 xor (R6 or (not R4))) + Data[1]  + $85845dd1), 21);
  LDR   R3, [R1, #4]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R4
  ADD   R9, R9, R3
  ORR   R8, R6, R8
  ADD   R5, R5, R9
  EOR   R8, R7, R8
  ADD   R5, R5, R8
  ROR   R5, R5, #11
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R6 xor (R5 or (not R7))) + Data[8]  + $6fa87e4f),  6);
  LDR   R3, [R1, #32]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R7
  ADD   R9, R9, R3
  ORR   R8, R5, R8
  ADD   R4, R4, R9
  EOR   R8, R6, R8
  ADD   R4, R4, R8
  ROR   R4, R4, #26
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R5 xor (R4 or (not R6))) + Data[15] + $fe2ce6e0), 10);
  LDR   R3, [R1, #60]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R6
  ADD   R9, R9, R3
  ORR   R8, R4, R8
  ADD   R7, R7, R9
  EOR   R8, R5, R8
  ADD   R7, R7, R8
  ROR   R7, R7, #22
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R4 xor (R7 or (not R5))) + Data[6]  + $a3014314), 15);
  LDR   R3, [R1, #24]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R5
  ADD   R9, R9, R3
  ORR   R8, R7, R8
  ADD   R6, R6, R9
  EOR   R8, R4, R8
  ADD   R6, R6, R8
  ROR   R6, R6, #17
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R7 xor (R6 or (not R4))) + Data[13] + $4e0811a1), 21);
  LDR   R3, [R1, #52]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R4
  ADD   R9, R9, R3
  ORR   R8, R6, R8
  ADD   R5, R5, R9
  EOR   R8, R7, R8
  ADD   R5, R5, R8
  ROR   R5, R5, #11
  ADD   R5, R5, R6

//R4 := R5 + roldword(dword(R4 + (R6 xor (R5 or (not R7))) + Data[4]  + $f7537e82),  6);
  LDR   R3, [R1, #16]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R7
  ADD   R9, R9, R3
  ORR   R8, R5, R8
  ADD   R4, R4, R9
  EOR   R8, R6, R8
  ADD   R4, R4, R8
  ROR   R4, R4, #26
  ADD   R4, R4, R5

//R7 := R4 + roldword(dword(R7 + (R5 xor (R4 or (not R6))) + Data[11] + $bd3af235), 10);
  LDR   R3, [R1, #44]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R6
  ADD   R9, R9, R3
  ORR   R8, R4, R8
  ADD   R7, R7, R9
  EOR   R8, R5, R8
  ADD   R7, R7, R8
  ROR   R7, R7, #22
  ADD   R7, R7, R4

//R6 := R7 + roldword(dword(R6 + (R4 xor (R7 or (not R5))) + Data[2]  + $2ad7d2bb), 15);
  LDR   R3, [R1, #8]
  LDR   R9, [R2], #4 // Increment round constant pointer
  BIC   R8, R10,R5
  ADD   R9, R9, R3
  ORR   R8, R7, R8
  ADD   R6, R6, R9
  EOR   R8, R4, R8
  ADD   R6, R6, R8
  ROR   R6, R6, #17
  ADD   R6, R6, R7

//R5 := R6 + roldword(dword(R5 + (R7 xor (R6 or (not R4))) + Data[9]  + $eb86d391), 21);
  LDR   R3, [R1, #36]
  LDR   R9, [R2]
  BIC   R8, R10,R4
  ADD   R9, R9, R3
  ORR   R8, R6, R8
  ADD   R5, R5, R9
  EOR   R8, R7, R8
  ADD   R5, R5, R8
  ROR   R5, R5, #11
  ADD   R5, R5, R6

  ADD   R1, R0, TMDContext.Length - TMDContext.State

  // Update length
  LDM   R1, {R2, R3}

  ADDS  R2, R2, #64
  ADC   R3, R3, #0

  STM   R1, {R2, R3}

  // Add the final state to the input
  LDM   R0, {R1, R2, R3, R8}

  ADD   R4, R4, R1
  ADD   R5, R5, R2
  ADD   R6, R6, R3
  ADD   R7, R7, R8

  STM   R0, {R4, R5, R6, R7}

  // Pop registers and return
  LDMFD SP!, {R4, R5, R6, R7, R8, R9, R10, PC}

.LMD5RoundConstants:
  .long 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
  .long 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
  .long 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
  .long 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
  .long 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
  .long 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
  .long 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
  .long 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
end;

