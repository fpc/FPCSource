// x86_64 (Windows) assembly optimized version
{$ifdef CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// RDI = Context, RSI = Buffer
{$asmmode intel}
asm
  // R8D = A, R9D = B, ECX = C, EDX = D
  MOV  R8D, TMDContext.State[RDI + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV  R9D, TMDContext.State[RDI + 4*1]
  MOV  ECX, TMDContext.State[RDI + 4*2]
  MOV  EDX, TMDContext.State[RDI + 4*3]

// Round 1
//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[0] + $d76aa478), 7);
  ADD  R8D, [RSI + 4*0]
  MOV  EAX, R9D
  ANDN R10D,R9D, EDX
  ADD  R8D, $d76aa478
  AND  EAX, ECX
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[1] + $e8c7b756), 12);
  ADD  EDX, [RSI + 4*1]
  ANDN R10D,R8D, ECX
  AND  EAX, R9D
  ADD  EDX, $e8c7b756
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 12
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[2] + $242070db), 17);
  ADD  ECX, [RSI + 4*2]
  ANDN R10D,EDX, R9D
  AND  EAX, R8D
  ADD  ECX, $242070db
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 17
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[3] + $c1bdceee), 22);
  ADD  R9D, [RSI + 4*3]
  ANDN R10D,ECX, R8D
  AND  EAX, EDX
  ADD  R9D, $c1bdceee
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[4] + $f57c0faf), 7);
  ADD  R8D, [RSI + 4*4]
  ANDN R10D,R9D, EDX
  AND  EAX, ECX
  ADD  R8D, $f57c0faf
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[5] + $4787c62a), 12);
  ADD  EDX, [RSI + 4*5]
  ANDN R10D,R8D, ECX
  AND  EAX, R9D
  ADD  EDX, $4787c62a
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 12
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[6] + $a8304613), 17);
  ADD  ECX, [RSI + 4*6]
  ANDN R10D,EDX, R9D
  AND  EAX, R8D
  ADD  ECX, $a8304613
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 17
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[7] + $fd469501), 22);
  ADD  R9D, [RSI + 4*7]
  ANDN R10D,ECX, R8D
  AND  EAX, EDX
  ADD  R9D, $fd469501
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[8] + $698098d8), 7);
  ADD  R8D, [RSI + 4*8]
  ANDN R10D,R9D, EDX
  AND  EAX, ECX
  ADD  R8D, $698098d8
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[9] + $8b44f7af), 12);
  ADD  EDX, [RSI + 4*9]
  ANDN R10D,R8D, ECX
  AND  EAX, R9D
  ADD  EDX, $8b44f7af
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 12
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[10] + $ffff5bb1), 17);
  ADD  ECX, [RSI + 4*10]
  ANDN R10D,EDX, R9D
  AND  EAX, R8D
  ADD  ECX, $ffff5bb1
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 17
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[11] + $895cd7be), 22);
  ADD  R9D, [RSI + 4*11]
  ANDN R10D,ECX, R8D
  AND  EAX, EDX
  ADD  R9D, $895cd7be
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[12] + $6b901122), 7);
  ADD  R8D, [RSI + 4*12]
  ANDN R10D,R9D, EDX
  AND  EAX, ECX
  ADD  R8D, $6b901122
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[13] + $fd987193), 12);
  ADD  EDX, [RSI + 4*13]
  ANDN R10D,R8D, ECX
  AND  EAX, R9D
  ADD  EDX, $fd987193
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 12
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[14] + $a679438e), 17);
  ADD  ECX, [RSI + 4*14]
  ANDN R10D,EDX, R9D
  AND  EAX, R8D
  ADD  ECX, $a679438e
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 17
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[15] + $49b40821), 22);
  ADD  R9D, [RSI + 4*15]
  ANDN R10D,ECX, R8D
  AND  EAX, EDX
  ADD  R9D, $49b40821
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

// Round 2
//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or ((not EDX) and ECX)) + Data[1] + $f61e2562), 5);
  ADD  R8D, [RSI + 4*1]
  ANDN R10D,EDX, ECX
  AND  EAX, EDX
  ADD  R8D, $f61e2562
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or ((not ECX) and R9D)) + Data[6] + $c040b340), 9);
  ADD  EDX, [RSI + 4*6]
  ANDN R10D,ECX, R9D
  AND  EAX, ECX
  ADD  EDX, $c040b340
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 9
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or ((not R9D) and R8D)) + Data[11] + $265e5a51), 14);
  ADD  ECX, [RSI + 4*11]
  ANDN R10D,R9D, R8D
  AND  EAX, R9D
  ADD  ECX, $265e5a51
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 14
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or ((not R8D) and EDX)) + Data[0] + $e9b6c7aa), 20);
  ADD  R9D, [RSI + 4*0]
  ANDN R10D,R8D, EDX
  AND  EAX, R8D
  ADD  R9D, $e9b6c7aa
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or ((not EDX) and ECX)) + Data[5] + $d62f105d), 5);
  ADD  R8D, [RSI + 4*5]
  ANDN R10D,EDX, ECX
  AND  EAX, EDX
  ADD  R8D, $d62f105d
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or ((not ECX) and R9D)) + Data[10] + $02441453), 9);
  ADD  EDX, [RSI + 4*10]
  ANDN R10D,ECX, R9D
  AND  EAX, ECX
  ADD  EDX, $02441453
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 9
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or ((not R9D) and R8D)) + Data[15] + $d8a1e681), 14);
  ADD  ECX, [RSI + 4*15]
  ANDN R10D,R9D, R8D
  AND  EAX, R9D
  ADD  ECX, $d8a1e681
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 14
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or ((not R8D) and EDX)) + Data[4] + $e7d3fbc8), 20);
  ADD  R9D, [RSI + 4*4]
  ANDN R10D,R8D, EDX
  AND  EAX, R8D
  ADD  R9D, $e7d3fbc8
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[9] + $21e1cde6), 5);
  ADD  R8D, [RSI + 4*9]
  ANDN R10D,EDX, ECX
  AND  EAX, EDX
  ADD  R8D, $21e1cde6
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[14] + $c33707d6), 9);
  ADD  EDX, [RSI + 4*14]
  ANDN R10D,ECX, R9D
  AND  EAX, ECX
  ADD  EDX, $c33707d6
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 9
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[3] + $f4d50d87), 14);
  ADD  ECX, [RSI + 4*3]
  ANDN R10D,R9D, R8D
  AND  EAX, R9D
  ADD  ECX, $f4d50d87
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 14
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[8] + $455a14ed), 20);
  ADD  R9D, [RSI + 4*8]
  ANDN R10D,R8D, EDX
  AND  EAX, R8D
  ADD  R9D, $455a14ed
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + ECX]
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[13] + $a9e3e905), 5);
  ADD  R8D, [RSI + 4*13]
  ANDN R10D,EDX, ECX
  AND  EAX, EDX
  ADD  R8D, $a9e3e905
  OR   EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[2] + $fcefa3f8), 9);
  ADD  EDX, [RSI + 4*2]
  ANDN R10D,ECX, R9D
  AND  EAX, ECX
  ADD  EDX, $fcefa3f8
  OR   EAX, R10D
  ADD  EDX, EAX
  ROL  EDX, 9
  LEA  EAX, [EDX + R8D]
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[7] + $676f02d9), 14);
  ADD  ECX, [RSI + 4*7]
  ANDN R10D,R9D, R8D
  AND  EAX, R9D
  ADD  ECX, $676f02d9
  OR   EAX, R10D
  ADD  ECX, EAX
  ROL  ECX, 14
  LEA  EAX, [ECX + EDX]
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[12] + $8d2a4c8a), 20);
  ADD  R9D, [RSI + 4*12]
  ANDN R10D,R8D, EDX
  AND  EAX, R8D
  ADD  R9D, $8d2a4c8a
  OR   EAX, R10D
  ADD  R9D, EAX
  ROL  R9D, 20
  ADD  R9D, ECX

// Round 3
//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[5] + $fffa3942), 4);
  ADD R8D, [RSI + 4*5]
  MOV EAX, R9D
  XOR EAX, ECX
  ADD R8D, $fffa3942
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[8] + $8771f681), 11);
  ADD EDX, [RSI + 4*8]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD EDX, $8771f681
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[11] + $6d9d6122), 16);
  ADD ECX, [RSI + 4*11]
  MOV EAX, EDX
  XOR EAX, R8D
  ADD ECX, $6d9d6122
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[14] + $fde5380c), 23);
  ADD R9D, [RSI + 4*14]
  MOV EAX, ECX
  XOR EAX, EDX
  ADD R9D, $fde5380c
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[1] + $a4beea44), 4);
  ADD R8D, [RSI + 4*1]
  MOV EAX, R9D
  XOR EAX, ECX
  ADD R8D, $a4beea44
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[4] + $4bdecfa9), 11);
  ADD EDX, [RSI + 4*4]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD EDX, $4bdecfa9
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[7] + $f6bb4b60), 16);
  ADD ECX, [RSI + 4*7]
  MOV EAX, EDX
  XOR EAX, R8D
  ADD ECX, $f6bb4b60
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[10] + $bebfbc70), 23);
  ADD R9D, [RSI + 4*10]
  MOV EAX, ECX
  XOR EAX, EDX
  ADD R9D, $bebfbc70
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[13] + $289b7ec6), 4);
  ADD R8D, [RSI + 4*13]
  MOV EAX, R9D
  XOR EAX, ECX
  ADD R8D, $289b7ec6
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[0] + $eaa127fa), 11);
  ADD EDX, [RSI + 4*0]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD EDX, $eaa127fa
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[3] + $d4ef3085), 16);
  ADD ECX, [RSI + 4*3]
  MOV EAX, EDX
  XOR EAX, R8D
  ADD ECX, $d4ef3085
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[6] + $04881d05), 23);
  ADD R9D, [RSI + 4*6]
  MOV EAX, ECX
  XOR EAX, EDX
  ADD R9D, $04881d05
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[9] + $d9d4d039), 4);
  ADD R8D, [RSI + 4*9]
  MOV EAX, R9D
  XOR EAX, ECX
  ADD R8D, $d9d4d039
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[12] + $e6db99e5), 11);
  ADD EDX, [RSI + 4*12]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD EDX, $e6db99e5
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[15] + $1fa27cf8), 16);
  ADD ECX, [RSI + 4*15]
  MOV EAX, EDX
  XOR EAX, R8D
  ADD ECX, $1fa27cf8
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  MOV R10D,-1 // Prepare a register of all 1s for Round 4.
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[2] + $c4ac5665), 23);
  ADD R9D, [RSI + 4*2]
  MOV EAX, ECX
  XOR EAX, EDX
  ADD R9D, $c4ac5665
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

// Round 4 (throughout this round, "ANDN EAX, reg, R10D" stands in for "EAX := not reg")
//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[0] + $f4292244), 6);
  ADD  R8D, [RSI + 4*0]
  ANDN EAX, EDX, R10D
  ADD  R8D, $f4292244
  OR   EAX, R9D
  XOR  EAX, ECX
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[7] + $432aff97), 10);
  ADD  EDX, [RSI + 4*7]
  ANDN EAX, ECX, R10D
  ADD  EDX, $432aff97
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  EDX, EAX
  ROL  EDX, 10
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[14] + $ab9423a7), 15);
  ADD  ECX, [RSI + 4*14]
  ANDN EAX, R9D, R10D
  ADD  ECX, $ab9423a7
  OR   EAX, EDX
  XOR  EAX, R8D
  ADD  ECX, EAX
  ROL  ECX, 15
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[5] + $fc93a039), 21);
  ADD  R9D, [RSI + 4*5]
  ANDN EAX, R8D, R10D
  ADD  R9D, $fc93a039
  OR   EAX, ECX
  XOR  EAX, EDX
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[12] + $655b59c3), 6);
  ADD  R8D, [RSI + 4*12]
  ANDN EAX, EDX, R10D
  ADD  R8D, $655b59c3
  OR   EAX, R9D
  XOR  EAX, ECX
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[3] + $8f0ccc92), 10);
  ADD  EDX, [RSI + 4*3]
  ANDN EAX, ECX, R10D
  ADD  EDX, $8f0ccc92
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  EDX, EAX
  ROL  EDX, 10
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[10] + $ffeff47d), 15);
  ADD  ECX, [RSI + 4*10]
  ANDN EAX, R9D, R10D
  ADD  ECX, $ffeff47d
  OR   EAX, EDX
  XOR  EAX, R8D
  ADD  ECX, EAX
  ROL  ECX, 15
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[1] + $85845dd1), 21);
  ADD  R9D, [RSI + 4*1]
  ANDN EAX, R8D, R10D
  ADD  R9D, $85845dd1
  OR   EAX, ECX
  XOR  EAX, EDX
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[8] + $6fa87e4f), 6);
  ADD  R8D, [RSI + 4*8]
  ANDN EAX, EDX, R10D
  ADD  R8D, $6fa87e4f
  OR   EAX, R9D
  XOR  EAX, ECX
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[15] + $fe2ce6e0), 10);
  ADD  EDX, [RSI + 4*15]
  ANDN EAX, ECX, R10D
  ADD  EDX, $fe2ce6e0
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  EDX, EAX
  ROL  EDX, 10
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[6] + $a3014314), 15);
  ADD  ECX, [RSI + 4*6]
  ANDN EAX, R9D, R10D
  ADD  ECX, $a3014314
  OR   EAX, EDX
  XOR  EAX, R8D
  ADD  ECX, EAX
  ROL  ECX, 15
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[13] + $4e0811a1), 21);
  ADD  R9D, [RSI + 4*13]
  ANDN EAX, R8D, R10D
  ADD  R9D, $4e0811a1
  OR   EAX, ECX
  XOR  EAX, EDX
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[4] + $f7537e82), 6);
  ADD  R8D, [RSI + 4*4]
  ANDN EAX, EDX, R10D
  ADD  R8D, $f7537e82
  OR   EAX, R9D
  XOR  EAX, ECX
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[11] + $bd3af235), 10);
  ADD  EDX, [RSI + 4*11]
  ANDN EAX, ECX, R10D
  ADD  EDX, $bd3af235
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  EDX, EAX
  ROL  EDX, 10
  ADD  EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[2] + $2ad7d2bb), 15);
  ADD  ECX, [RSI + 4*2]
  ANDN EAX, R9D, R10D
  ADD  ECX, $2ad7d2bb
  OR   EAX, EDX
  XOR  EAX, R8D
  ADD  ECX, EAX
  ROL  ECX, 15
  ADD  ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[9] + $eb86d391), 21);
  ADD  R9D, [RSI + 4*9]
  ANDN EAX, R8D, R10D
  ADD  R9D, $eb86d391
  OR   EAX, ECX
  XOR  EAX, EDX
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, ECX

  ADD TMDContext.State[RDI + 4*0], R8D //  Context.State[0 .. 3] += A, B, C, D
  ADD TMDContext.State[RDI + 4*1], R9D
  ADD TMDContext.State[RDI + 4*2], ECX
  ADD TMDContext.State[RDI + 4*3], EDX

//Inc(Context.Length,64);
  ADD QWORD PTR TMDContext.Length[RDI],64
end;
{$else CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// RDI = Context, RSI = Buffer
{$asmmode intel}
asm
  // R8D = A, R9D = B, ECX = C, EDX = D
  MOV R8D, TMDContext.State[RDI + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV R9D, TMDContext.State[RDI + 4*1]
  MOV ECX, TMDContext.State[RDI + 4*2]
  MOV EDX, TMDContext.State[RDI + 4*3]
// Round 1
//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[0] + $d76aa478), 7);
  MOV EAX, ECX
  ADD R8D, $d76aa478
  XOR EAX, EDX
  ADD R8D, [RSI + 4*0]
  AND EAX, R9D
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[1] + $e8c7b756), 12);
  MOV EAX, R9D
  ADD EDX, $e8c7b756
  XOR EAX, ECX
  ADD EDX, [RSI + 4*1]
  AND EAX, R8D
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 12
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[2] + $242070db), 17);
  MOV EAX, R8D
  ADD ECX, $242070db
  XOR EAX, R9D
  ADD ECX, [RSI + 4*2]
  AND EAX, EDX
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 17
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[3] + $c1bdceee), 22);
  MOV EAX, EDX
  ADD R9D, $c1bdceee
  XOR EAX, R8D
  ADD R9D, [RSI + 4*3]
  AND EAX, ECX
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[4] + $f57c0faf), 7);
  MOV EAX, ECX
  ADD R8D, $f57c0faf
  XOR EAX, EDX
  ADD R8D, [RSI + 4*4]
  AND EAX, R9D
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[5] + $4787c62a), 12);
  MOV EAX, R9D
  ADD EDX, $4787c62a
  XOR EAX, ECX
  ADD EDX, [RSI + 4*5]
  AND EAX, R8D
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 12
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[6] + $a8304613), 17);
  MOV EAX, R8D
  ADD ECX, $a8304613
  XOR EAX, R9D
  ADD ECX, [RSI + 4*6]
  AND EAX, EDX
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 17
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[7] + $fd469501), 22);
  MOV EAX, EDX
  ADD R9D, $fd469501
  XOR EAX, R8D
  ADD R9D, [RSI + 4*7]
  AND EAX, ECX
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[8] + $698098d8), 7);
  MOV EAX, ECX
  ADD R8D, $698098d8
  XOR EAX, EDX
  ADD R8D, [RSI + 4*8]
  AND EAX, R9D
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[9] + $8b44f7af), 12);
  MOV EAX, R9D
  ADD EDX, $8b44f7af
  XOR EAX, ECX
  ADD EDX, [RSI + 4*9]
  AND EAX, R8D
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 12
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[10] + $ffff5bb1), 17);
  MOV EAX, R8D
  ADD ECX, $ffff5bb1
  XOR EAX, R9D
  ADD ECX, [RSI + 4*10]
  AND EAX, EDX
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 17
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[11] + $895cd7be), 22);
  MOV EAX, EDX
  ADD R9D, $895cd7be
  XOR EAX, R8D
  ADD R9D, [RSI + 4*11]
  AND EAX, ECX
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and ECX) or ((not R9D) and EDX)) + Data[12] + $6b901122), 7);
  MOV EAX, ECX
  ADD R8D, $6b901122
  XOR EAX, EDX
  ADD R8D, [RSI + 4*12]
  AND EAX, R9D
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and R9D) or ((not R8D) and ECX)) + Data[13] + $fd987193), 12);
  MOV EAX, R9D
  ADD EDX, $fd987193
  XOR EAX, ECX
  ADD EDX, [RSI + 4*13]
  AND EAX, R8D
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 12
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R8D) or ((not EDX) and R9D)) + Data[14] + $a679438e), 17);
  MOV EAX, R8D
  ADD ECX, $a679438e
  XOR EAX, R9D
  ADD ECX, [RSI + 4*14]
  AND EAX, EDX
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 17
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and EDX) or ((not ECX) and R8D)) + Data[15] + $49b40821), 22);
  MOV EAX, EDX
  ADD R9D, $49b40821
  XOR EAX, R8D
  ADD R9D, [RSI + 4*15]
  AND EAX, ECX
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, ECX

// Round 2
//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[1] + $f61e2562), 5);
  MOV EAX, R9D
  ADD R8D, $f61e2562
  XOR EAX, ECX
  ADD R8D, [RSI + 4*1]
  AND EAX, EDX
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[6] + $c040b340), 9);
  MOV EAX, R8D
  ADD EDX, $c040b340
  XOR EAX, R9D
  ADD EDX, [RSI + 4*6]
  AND EAX, ECX
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 9
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[11] + $265e5a51), 14);
  MOV EAX, EDX
  ADD ECX, $265e5a51
  XOR EAX, R8D
  ADD ECX, [RSI + 4*11]
  AND EAX, R9D
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 14
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[0] + $e9b6c7aa), 20);
  MOV EAX, ECX
  ADD R9D, $e9b6c7aa
  XOR EAX, EDX
  ADD R9D, [RSI + 4*0]
  AND EAX, R8D
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[5] + $d62f105d), 5);
  MOV EAX, R9D
  ADD R8D, $d62f105d
  XOR EAX, ECX
  ADD R8D, [RSI + 4*5]
  AND EAX, EDX
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[10] + $02441453), 9);
  MOV EAX, R8D
  ADD EDX, $02441453
  XOR EAX, R9D
  ADD EDX, [RSI + 4*10]
  AND EAX, ECX
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 9
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[15] + $d8a1e681), 14);
  MOV EAX, EDX
  ADD ECX, $d8a1e681
  XOR EAX, R8D
  ADD ECX, [RSI + 4*15]
  AND EAX, R9D
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 14
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[4] + $e7d3fbc8), 20);
  MOV EAX, ECX
  ADD R9D, $e7d3fbc8
  XOR EAX, EDX
  ADD R9D, [RSI + 4*4]
  AND EAX, R8D
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[9] + $21e1cde6), 5);
  MOV EAX, R9D
  ADD R8D, $21e1cde6
  XOR EAX, ECX
  ADD R8D, [RSI + 4*9]
  AND EAX, EDX
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[14] + $c33707d6), 9);
  MOV EAX, R8D
  ADD EDX, $c33707d6
  XOR EAX, R9D
  ADD EDX, [RSI + 4*14]
  AND EAX, ECX
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 9
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[3] + $f4d50d87), 14);
  MOV EAX, EDX
  ADD ECX, $f4d50d87
  XOR EAX, R8D
  ADD ECX, [RSI + 4*3]
  AND EAX, R9D
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 14
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[8] + $455a14ed), 20);
  MOV EAX, ECX
  ADD R9D, $455a14ed
  XOR EAX, EDX
  ADD R9D, [RSI + 4*8]
  AND EAX, R8D
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + ((R9D and EDX) or (ECX and (not EDX))) + Data[13] + $a9e3e905), 5);
  MOV EAX, R9D
  ADD R8D, $a9e3e905
  XOR EAX, ECX
  ADD R8D, [RSI + 4*13]
  AND EAX, EDX
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + ((R8D and ECX) or (R9D and (not ECX))) + Data[2] + $fcefa3f8), 9);
  MOV EAX, R8D
  ADD EDX, $fcefa3f8
  XOR EAX, R9D
  ADD EDX, [RSI + 4*2]
  AND EAX, ECX
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 9
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + ((EDX and R9D) or (R8D and (not R9D))) + Data[7] + $676f02d9), 14);
  MOV EAX, EDX
  ADD ECX, $676f02d9
  XOR EAX, R8D
  ADD ECX, [RSI + 4*7]
  AND EAX, R9D
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 14
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + ((ECX and R8D) or (EDX and (not R8D))) + Data[12] + $8d2a4c8a), 20);
  MOV EAX, ECX
  ADD R9D, $8d2a4c8a
  XOR EAX, EDX
  ADD R9D, [RSI + 4*12]
  AND EAX, R8D
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, ECX

// Round 3
//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[5] + $fffa3942), 4);
  MOV EAX, R9D
  ADD R8D, $fffa3942
  XOR EAX, ECX
  ADD R8D, [RSI + 4*5]
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[8] + $8771f681), 11);
  MOV EAX, R8D
  ADD EDX, $8771f681
  XOR EAX, R9D
  ADD EDX, [RSI + 4*8]
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[11] + $6d9d6122), 16);
  MOV EAX, EDX
  ADD ECX, $6d9d6122
  XOR EAX, R8D
  ADD ECX, [RSI + 4*11]
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[14] + $fde5380c), 23);
  MOV EAX, ECX
  ADD R9D, $fde5380c
  XOR EAX, EDX
  ADD R9D, [RSI + 4*14]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[1] + $a4beea44), 4);
  MOV EAX, R9D
  ADD R8D, $a4beea44
  XOR EAX, ECX
  ADD R8D, [RSI + 4*1]
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[4] + $4bdecfa9), 11);
  MOV EAX, R8D
  ADD EDX, $4bdecfa9
  XOR EAX, R9D
  ADD EDX, [RSI + 4*4]
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[7] + $f6bb4b60), 16);
  MOV EAX, EDX
  ADD ECX, $f6bb4b60
  XOR EAX, R8D
  ADD ECX, [RSI + 4*7]
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[10] + $bebfbc70), 23);
  MOV EAX, ECX
  ADD R9D, $bebfbc70
  XOR EAX, EDX
  ADD R9D, [RSI + 4*10]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[13] + $289b7ec6), 4);
  MOV EAX, R9D
  ADD R8D, $289b7ec6
  XOR EAX, ECX
  ADD R8D, [RSI + 4*13]
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[0] + $eaa127fa), 11);
  MOV EAX, R8D
  ADD EDX, $eaa127fa
  XOR EAX, R9D
  ADD EDX, [RSI + 4*0]
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[3] + $d4ef3085), 16);
  MOV EAX, EDX
  ADD ECX, $d4ef3085
  XOR EAX, R8D
  ADD ECX, [RSI + 4*3]
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[6] + $04881d05), 23);
  MOV EAX, ECX
  ADD R9D, $04881d05
  XOR EAX, EDX
  ADD R9D, [RSI + 4*6]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (R9D xor ECX xor EDX) + Data[9] + $d9d4d039), 4);
  MOV EAX, R9D
  ADD R8D, $d9d4d039
  XOR EAX, ECX
  ADD R8D, [RSI + 4*9]
  XOR EAX, EDX
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R8D xor R9D xor ECX) + Data[12] + $e6db99e5), 11);
  MOV EAX, R8D
  ADD EDX, $e6db99e5
  XOR EAX, R9D
  ADD EDX, [RSI + 4*12]
  XOR EAX, ECX
  ADD EDX, EAX
  ROL EDX, 11
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (EDX xor R8D xor R9D) + Data[15] + $1fa27cf8), 16);
  MOV EAX, EDX
  ADD ECX, $1fa27cf8
  XOR EAX, R8D
  ADD ECX, [RSI + 4*15]
  XOR EAX, R9D
  ADD ECX, EAX
  ROL ECX, 16
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (ECX xor EDX xor R8D) + Data[2] + $c4ac5665), 23);
  MOV EAX, ECX
  ADD R9D, $c4ac5665
  XOR EAX, EDX
  ADD R9D, [RSI + 4*2]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, ECX

// Round 4
//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[0] + $f4292244), 6);
  MOV EAX, EDX
  ADD R8D, $f4292244
  NOT EAX
  ADD R8D, [RSI + 4*0]
  OR  EAX, R9D
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[7] + $432aff97), 10);
  MOV EAX, ECX
  ADD EDX, $432aff97
  NOT EAX
  ADD EDX, [RSI + 4*7]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 10
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[14] + $ab9423a7), 15);
  MOV EAX, R9D
  ADD ECX, $ab9423a7
  NOT EAX
  ADD ECX, [RSI + 4*14]
  OR  EAX, EDX
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 15
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[5] + $fc93a039), 21);
  MOV EAX, R8D
  ADD R9D, $fc93a039
  NOT EAX
  ADD R9D, [RSI + 4*5]
  OR  EAX, ECX
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[12] + $655b59c3), 6);
  MOV EAX, EDX
  ADD R8D, $655b59c3
  NOT EAX
  ADD R8D, [RSI + 4*12]
  OR  EAX, R9D
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[3] + $8f0ccc92), 10);
  MOV EAX, ECX
  ADD EDX, $8f0ccc92
  NOT EAX
  ADD EDX, [RSI + 4*3]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 10
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[10] + $ffeff47d), 15);
  MOV EAX, R9D
  ADD ECX, $ffeff47d
  NOT EAX
  ADD ECX, [RSI + 4*10]
  OR  EAX, EDX
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 15
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[1] + $85845dd1), 21);
  MOV EAX, R8D
  ADD R9D, $85845dd1
  NOT EAX
  ADD R9D, [RSI + 4*1]
  OR  EAX, ECX
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[8] + $6fa87e4f), 6);
  MOV EAX, EDX
  ADD R8D, $6fa87e4f
  NOT EAX
  ADD R8D, [RSI + 4*8]
  OR  EAX, R9D
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[15] + $fe2ce6e0), 10);
  MOV EAX, ECX
  ADD EDX, $fe2ce6e0
  NOT EAX
  ADD EDX, [RSI + 4*15]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 10
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[6] + $a3014314), 15);
  MOV EAX, R9D
  ADD ECX, $a3014314
  NOT EAX
  ADD ECX, [RSI + 4*6]
  OR  EAX, EDX
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 15
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[13] + $4e0811a1), 21);
  MOV EAX, R8D
  ADD R9D, $4e0811a1
  NOT EAX
  ADD R9D, [RSI + 4*13]
  OR  EAX, ECX
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, ECX

//R8D := R9D + roldword(dword(R8D + (ECX xor (R9D or (not EDX))) + Data[4] + $f7537e82), 6);
  MOV EAX, EDX
  ADD R8D, $f7537e82
  NOT EAX
  ADD R8D, [RSI + 4*4]
  OR  EAX, R9D
  XOR EAX, ECX
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//EDX := R8D + roldword(dword(EDX + (R9D xor (R8D or (not ECX))) + Data[11] + $bd3af235), 10);
  MOV EAX, ECX
  ADD EDX, $bd3af235
  NOT EAX
  ADD EDX, [RSI + 4*11]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD EDX, EAX
  ROL EDX, 10
  ADD EDX, R8D

//ECX := EDX + roldword(dword(ECX + (R8D xor (EDX or (not R9D))) + Data[2] + $2ad7d2bb), 15);
  MOV EAX, R9D
  ADD ECX, $2ad7d2bb
  NOT EAX
  ADD ECX, [RSI + 4*2]
  OR  EAX, EDX
  XOR EAX, R8D
  ADD ECX, EAX
  ROL ECX, 15
  ADD ECX, EDX

//R9D := ECX + roldword(dword(R9D + (EDX xor (ECX or (not R8D))) + Data[9] + $eb86d391), 21);
  MOV EAX, R8D
  ADD R9D, $eb86d391
  NOT EAX
  ADD R9D, [RSI + 4*9]
  OR  EAX, ECX
  XOR EAX, EDX
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, ECX

  ADD TMDContext.State[RDI + 4*0], R8D //  Context.State[0 .. 3] += A, B, C, D
  ADD TMDContext.State[RDI + 4*1], R9D
  ADD TMDContext.State[RDI + 4*2], ECX
  ADD TMDContext.State[RDI + 4*3], EDX

//Inc(Context.Length,64);
  ADD QWORD PTR TMDContext.Length[RDI],64
end;
{$endif CPUX86_HAS_BMI1}
