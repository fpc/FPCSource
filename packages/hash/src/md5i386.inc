// i386 assembler optimized version
{$ifdef CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// eax = Context, edx = Buffer
{$asmmode intel}
asm
  PUSH EBP
  PUSH EBX
  PUSH ESI
  PUSH EDI
  PUSH EAX // save Context

  // EBX = A, ECX = B, ESI = C, EDI = D
  MOV  EBX, TMDContext.State[EAX + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV  ECX, TMDContext.State[EAX + 4*1]
  MOV  ESI, TMDContext.State[EAX + 4*2]
  MOV  EDI, TMDContext.State[EAX + 4*3]

// Round 1
//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[0] + $d76aa478), 7);
  ADD  EBX, [EDX + 4*0]
  MOV  EAX, ECX
  ANDN EBP, ECX, EDI
  ADD  EBX, $d76aa478
  AND  EAX, ESI
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 7
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[1] + $e8c7b756), 12);
  ADD  EDI, [EDX + 4*1]
  ANDN EBP, EBX, ESI
  AND  EAX, ECX
  ADD  EDI, $e8c7b756
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI, 12
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[2] + $242070db), 17);
  ADD  ESI, [EDX + 4*2]
  ANDN EBP, EDI, ECX
  AND  EAX, EBX
  ADD  ESI, $242070db
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 17
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[3] + $c1bdceee), 22);
  ADD  ECX, [EDX + 4*3]
  ANDN EBP, ESI, EBX
  AND  EAX, EDI
  ADD  ECX, $c1bdceee
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 22
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[4] + $f57c0faf), 7);
  ADD  EBX, [EDX + 4*4]
  ANDN EBP, ECX, EDI
  AND  EAX, ESI
  ADD  EBX, $f57c0faf
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 7
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[5] + $4787c62a), 12);
  ADD  EDI, [EDX + 4*5]
  ANDN EBP, EBX, ESI
  AND  EAX, ECX
  ADD  EDI, $4787c62a
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI, 12
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[6] + $a8304613), 17);
  ADD  ESI, [EDX + 4*6]
  ANDN EBP, EDI, ECX
  AND  EAX, EBX
  ADD  ESI, $a8304613
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 17
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[7] + $fd469501), 22);
  ADD  ECX, [EDX + 4*7]
  ANDN EBP, ESI, EBX
  AND  EAX, EDI
  ADD  ECX, $fd469501
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 22
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[8] + $698098d8), 7);
  ADD  EBX, [EDX + 4*8]
  ANDN EBP, ECX, EDI
  AND  EAX, ESI
  ADD  EBX, $698098d8
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 7
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[9] + $8b44f7af), 12);
  ADD  EDI, [EDX + 4*9]
  ANDN EBP, EBX, ESI
  AND  EAX, ECX
  ADD  EDI, $8b44f7af
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI, 12
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[10] + $ffff5bb1), 17);
  ADD  ESI, [EDX + 4*10]
  ANDN EBP, EDI, ECX
  AND  EAX, EBX
  ADD  ESI, $ffff5bb1
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 17
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[11] + $895cd7be), 22);
  ADD  ECX, [EDX + 4*11]
  ANDN EBP, ESI, EBX
  AND  EAX, EDI
  ADD  ECX, $895cd7be
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 22
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[12] + $6b901122), 7);
  ADD  EBX, [EDX + 4*12]
  ANDN EBP, ECX, EDI
  AND  EAX, ESI
  ADD  EBX, $6b901122
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 7
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[13] + $fd987193), 12);
  ADD  EDI, [EDX + 4*13]
  ANDN EBP, EBX, ESI
  AND  EAX, ECX
  ADD  EDI, $fd987193
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI, 12
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[14] + $a679438e), 17);
  ADD  ESI, [EDX + 4*14]
  ANDN EBP, EDI, ECX
  AND  EAX, EBX
  ADD  ESI, $a679438e
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 17
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[15] + $49b40821), 22);
  ADD  ECX, [EDX + 4*15]
  ANDN EBP, ESI, EBX
  AND  EAX, EDI
  ADD  ECX, $49b40821
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 22
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

// Round 2
//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or ((not EDI) and ESI)) + Data[1] + $f61e2562), 5);
  ADD  EBX, [EDX + 4*1]
  ANDN EBP, EDI, ESI
  AND  EAX, EDI
  ADD  EBX, $f61e2562
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 5
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or ((not ESI) and ECX)) + Data[6] + $c040b340), 9);
  ADD  EDI, [EDX + 4*6]
  ANDN EBP, ESI, ECX
  AND  EAX, ESI
  ADD  EDI, $c040b340
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI,9
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or ((not ECX) and EBX)) + Data[11] + $265e5a51), 14);
  ADD  ESI, [EDX + 4*11]
  ANDN EBP, ECX, EBX
  AND  EAX, ECX
  ADD  ESI, $265e5a51
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 14
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or ((not EBX) and EDI)) + Data[0] + $e9b6c7aa), 20);
  ADD  ECX, [EDX + 4*0]
  ANDN EBP, EBX, EDI
  AND  EAX, EBX
  ADD  ECX, $e9b6c7aa
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 20
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or ((not EDI) and ESI)) + Data[5] + $d62f105d), 5);
  ADD  EBX, [EDX + 4*5]
  ANDN EBP, EDI, ESI
  AND  EAX, EDI
  ADD  EBX, $d62f105d
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 5
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or ((not ESI) and ECX)) + Data[10] + $02441453), 9);
  ADD  EDI, [EDX + 4*10]
  ANDN EBP, ESI, ECX
  AND  EAX, ESI
  ADD  EDI, $02441453
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI,9
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or ((not ECX) and EBX)) + Data[15] + $d8a1e681), 14);
  ADD  ESI, [EDX + 4*15]
  ANDN EBP, ECX, EBX
  AND  EAX, ECX
  ADD  ESI, $d8a1e681
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 14
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or ((not EBX) and EDI)) + Data[4] + $e7d3fbc8), 20);
  ADD  ECX, [EDX + 4*4]
  ANDN EBP, EBX, EDI
  AND  EAX, EBX
  ADD  ECX, $e7d3fbc8
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 20
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[9] + $21e1cde6), 5);
  ADD  EBX, [EDX + 4*9]
  ANDN EBP, EDI, ESI
  AND  EAX, EDI
  ADD  EBX, $21e1cde6
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 5
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[14] + $c33707d6), 9);
  ADD  EDI, [EDX + 4*14]
  ANDN EBP, ESI, ECX
  AND  EAX, ESI
  ADD  EDI, $c33707d6
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI,9
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[3] + $f4d50d87), 14);
  ADD  ESI, [EDX + 4*3]
  ANDN EBP, ECX, EBX
  AND  EAX, ECX
  ADD  ESI, $f4d50d87
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 14
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[8] + $455a14ed), 20);
  ADD  ECX, [EDX + 4*8]
  ANDN EBP, EBX, EDI
  AND  EAX, EBX
  ADD  ECX, $455a14ed
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 20
  LEA  EAX, [ECX + ESI]
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[13] + $a9e3e905), 5);
  ADD  EBX, [EDX + 4*13]
  ANDN EBP, EDI, ESI
  AND  EAX, EDI
  ADD  EBX, $a9e3e905
  OR   EAX, EBP
  ADD  EBX, EAX
  ROL  EBX, 5
  LEA  EAX, [EBX + ECX]
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[2] + $fcefa3f8), 9);
  ADD  EDI, [EDX + 4*2]
  ANDN EBP, ESI, ECX
  AND  EAX, ESI
  ADD  EDI, $fcefa3f8
  OR   EAX, EBP
  ADD  EDI, EAX
  ROL  EDI,9
  LEA  EAX, [EDI + EBX]
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[7] + $676f02d9), 14);
  ADD  ESI, [EDX + 4*7]
  ANDN EBP, ECX, EBX
  AND  EAX, ECX
  ADD  ESI, $676f02d9
  OR   EAX, EBP
  ADD  ESI, EAX
  ROL  ESI, 14
  LEA  EAX, [ESI + EDI]
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[12] + $8d2a4c8a), 20);
  ADD  ECX, [EDX + 4*12]
  ANDN EBP, EBX, EDI
  AND  EAX, EBX
  ADD  ECX, $8d2a4c8a
  OR   EAX, EBP
  ADD  ECX, EAX
  ROL  ECX, 20
  ADD  ECX, ESI

// Round 3
//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[5] + $fffa3942), 4);
  ADD EBX, [EDX + 4*5]
  MOV EAX, ECX
  XOR EAX, ESI
  ADD EBX, $fffa3942
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[8] + $8771f681), 11);
  ADD EDI, [EDX + 4*8]
  MOV EAX, EBX
  XOR EAX, ECX
  ADD EDI, $8771f681
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[11] + $6d9d6122), 16);
  ADD ESI, [EDX + 4*11]
  MOV EAX, EDI
  XOR EAX, EBX
  ADD ESI, $6d9d6122
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[14] + $fde5380c), 23);
  ADD ECX, [EDX + 4*14]
  MOV EAX, ESI
  XOR EAX, EDI
  ADD ECX, $fde5380c
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[1] + $a4beea44), 4);
  ADD EBX, [EDX + 4*1]
  MOV EAX, ECX
  XOR EAX, ESI
  ADD EBX, $a4beea44
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[4] + $4bdecfa9), 11);
  ADD EDI, [EDX + 4*4]
  MOV EAX, EBX
  XOR EAX, ECX
  ADD EDI, $4bdecfa9
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[7] + $f6bb4b60), 16);
  ADD ESI, [EDX + 4*7]
  MOV EAX, EDI
  XOR EAX, EBX
  ADD ESI, $f6bb4b60
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[10] + $bebfbc70), 23);
  ADD ECX, [EDX + 4*10]
  MOV EAX, ESI
  XOR EAX, EDI
  ADD ECX, $bebfbc70
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[13] + $289b7ec6), 4);
  ADD EBX, [EDX + 4*13]
  MOV EAX, ECX
  XOR EAX, ESI
  ADD EBX, $289b7ec6
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[0] + $eaa127fa), 11);
  ADD EDI, [EDX + 4*0]
  MOV EAX, EBX
  XOR EAX, ECX
  ADD EDI, $eaa127fa
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[3] + $d4ef3085), 16);
  ADD ESI, [EDX + 4*3]
  MOV EAX, EDI
  XOR EAX, EBX
  ADD ESI, $d4ef3085
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[6] + $04881d05), 23);
  ADD ECX, [EDX + 4*6]
  MOV EAX, ESI
  XOR EAX, EDI
  ADD ECX, $04881d05
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[9] + $d9d4d039), 4);
  ADD EBX, [EDX + 4*9]
  MOV EAX, ECX
  XOR EAX, ESI
  ADD EBX, $d9d4d039
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[12] + $e6db99e5), 11);
  ADD EDI, [EDX + 4*12]
  MOV EAX, EBX
  XOR EAX, ECX
  ADD EDI, $e6db99e5
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[15] + $1fa27cf8), 16);
  ADD ESI, [EDX + 4*15]
  MOV EAX, EDI
  XOR EAX, EBX
  ADD ESI, $1fa27cf8
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  MOV EBP, -1 // Prepare a register of all 1s for Round 4.
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[2] + $c4ac5665), 23);
  ADD ECX, [EDX + 4*2]
  MOV EAX, ESI
  XOR EAX, EDI
  ADD ECX, $c4ac5665
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

// Round 4 (throughout this round, "ANDN EAX, reg, EBP" stands in for "EAX := not reg")
//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[0] + $f4292244), 6);
  ADD  EBX, [EDX + 4*0]
  ANDN EAX, EDI, EBP
  ADD  EBX, $f4292244
  OR   EAX, ECX
  XOR  EAX, ESI
  ADD  EBX, EAX
  ROL  EBX, 6
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[7] + $432aff97), 10);
  ADD  EDI, [EDX + 4*7]
  ANDN EAX, ESI, EBP
  ADD  EDI, $432aff97
  OR   EAX, EBX
  XOR  EAX, ECX
  ADD  EDI, EAX
  ROL  EDI, 10
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[14] + $ab9423a7), 15);
  ADD  ESI, [EDX + 4*14]
  ANDN EAX, ECX, EBP
  ADD  ESI, $ab9423a7
  OR   EAX, EDI
  XOR  EAX, EBX
  ADD  ESI, EAX
  ROL  ESI, 15
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[5] + $fc93a039), 21);
  ADD  ECX, [EDX + 4*5]
  ANDN EAX, EBX, EBP
  ADD  ECX, $fc93a039
  OR   EAX, ESI
  XOR  EAX, EDI
  ADD  ECX, EAX
  ROL  ECX, 21
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[12] + $655b59c3), 6);
  ADD  EBX, [EDX + 4*12]
  ANDN EAX, EDI, EBP
  ADD  EBX, $655b59c3
  OR   EAX, ECX
  XOR  EAX, ESI
  ADD  EBX, EAX
  ROL  EBX, 6
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[3] + $8f0ccc92), 10);
  ADD  EDI, [EDX + 4*3]
  ANDN EAX, ESI, EBP
  ADD  EDI, $8f0ccc92
  OR   EAX, EBX
  XOR  EAX, ECX
  ADD  EDI, EAX
  ROL  EDI, 10
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[10] + $ffeff47d), 15);
  ADD  ESI, [EDX + 4*10]
  ANDN EAX, ECX, EBP
  ADD  ESI, $ffeff47d
  OR   EAX, EDI
  XOR  EAX, EBX
  ADD  ESI, EAX
  ROL  ESI, 15
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[1] + $85845dd1), 21);
  ADD  ECX, [EDX + 4*1]
  ANDN EAX, EBX, EBP
  ADD  ECX, $85845dd1
  OR   EAX, ESI
  XOR  EAX, EDI
  ADD  ECX, EAX
  ROL  ECX, 21
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[8] + $6fa87e4f), 6);
  ADD  EBX, [EDX + 4*8]
  ANDN EAX, EDI, EBP
  ADD  EBX, $6fa87e4f
  OR   EAX, ECX
  XOR  EAX, ESI
  ADD  EBX, EAX
  ROL  EBX, 6
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[15] + $fe2ce6e0), 10);
  ADD  EDI, [EDX + 4*15]
  ANDN EAX, ESI, EBP
  ADD  EDI, $fe2ce6e0
  OR   EAX, EBX
  XOR  EAX, ECX
  ADD  EDI, EAX
  ROL  EDI, 10
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[6] + $a3014314), 15);
  ADD  ESI, [EDX + 4*6]
  ANDN EAX, ECX, EBP
  ADD  ESI, $a3014314
  OR   EAX, EDI
  XOR  EAX, EBX
  ADD  ESI, EAX
  ROL  ESI, 15
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[13] + $4e0811a1), 21);
  ADD  ECX, [EDX + 4*13]
  ANDN EAX, EBX, EBP
  ADD  ECX, $4e0811a1
  OR   EAX, ESI
  XOR  EAX, EDI
  ADD  ECX, EAX
  ROL  ECX, 21
  ADD  ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[4] + $f7537e82), 6);
  ADD  EBX, [EDX + 4*4]
  ANDN EAX, EDI, EBP
  ADD  EBX, $f7537e82
  OR   EAX, ECX
  XOR  EAX, ESI
  ADD  EBX, EAX
  ROL  EBX, 6
  ADD  EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[11] + $bd3af235), 10);
  ADD  EDI, [EDX + 4*11]
  ANDN EAX, ESI, EBP
  ADD  EDI, $bd3af235
  OR   EAX, EBX
  XOR  EAX, ECX
  ADD  EDI, EAX
  ROL  EDI, 10
  ADD  EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[2] + $2ad7d2bb), 15);
  ADD  ESI, [EDX + 4*2]
  ANDN EAX, ECX, EBP
  ADD  ESI, $2ad7d2bb
  OR   EAX, EDI
  XOR  EAX, EBX
  ADD  ESI, EAX
  ROL  ESI, 15
  ADD  ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[9] + $eb86d391), 21);
  ADD  ECX, [EDX + 4*9]
  ANDN EAX, EBX, EBP
  ADD  ECX, $eb86d391
  OR   EAX, ESI
  XOR  EAX, EDI
  ADD  ECX, EAX
  ROL  ECX, 21
  ADD  ECX, ESI

  POP  EAX // EAX = Context

  ADD  TMDContext.State[EAX + 4*0], EBX //  Context.State[0 .. 3] += A, B, C, D
  ADD  TMDContext.State[EAX + 4*1], ECX
  ADD  TMDContext.State[EAX + 4*2], ESI
  ADD  TMDContext.State[EAX + 4*3], EDI

//Inc(Context.Length,64);
  ADD  DWORD PTR TMDContext.Length[EAX], 64
  ADC  DWORD PTR TMDContext.Length[EAX + 4], 0

  POP  EDI
  POP  ESI
  POP  EBX
  POP  EBP
end;
{$else CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// EAX = Context, EDX = Buffer
{$asmmode intel}
asm
  PUSH EBX
  PUSH ESI
  PUSH EDI
  PUSH EAX // save Context

  // EBX = A, ECX = B, ESI = C, EDI = D
  MOV EBX, TMDContext.State[EAX + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV ECX, TMDContext.State[EAX + 4*1]
  MOV ESI, TMDContext.State[EAX + 4*2]
  MOV EDI, TMDContext.State[EAX + 4*3]
// Round 1
//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[0] + $d76aa478), 7);
  MOV EAX, ESI
  ADD EBX, $d76aa478
  XOR EAX, EDI
  ADD EBX, [EDX + 4*0]
  AND EAX, ECX
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 7
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[1] + $e8c7b756), 12);
  MOV EAX, ECX
  ADD EDI, $e8c7b756
  XOR EAX, ESI
  ADD EDI, [EDX + 4*1]
  AND EAX, EBX
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 12
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[2] + $242070db), 17);
  MOV EAX, EBX
  ADD ESI, $242070db
  XOR EAX, ECX
  ADD ESI, [EDX + 4*2]
  AND EAX, EDI
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 17
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[3] + $c1bdceee), 22);
  MOV EAX, EDI
  ADD ECX, $c1bdceee
  XOR EAX, EBX
  ADD ECX, [EDX + 4*3]
  AND EAX, ESI
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 22
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[4] + $f57c0faf), 7);
  MOV EAX, ESI
  ADD EBX, $f57c0faf
  XOR EAX, EDI
  ADD EBX, [EDX + 4*4]
  AND EAX, ECX
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 7
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[5] + $4787c62a), 12);
  MOV EAX, ECX
  ADD EDI, $4787c62a
  XOR EAX, ESI
  ADD EDI, [EDX + 4*5]
  AND EAX, EBX
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 12
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[6] + $a8304613), 17);
  MOV EAX, EBX
  ADD ESI, $a8304613
  XOR EAX, ECX
  ADD ESI, [EDX + 4*6]
  AND EAX, EDI
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 17
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[7] + $fd469501), 22);
  MOV EAX, EDI
  ADD ECX, $fd469501
  XOR EAX, EBX
  ADD ECX, [EDX + 4*7]
  AND EAX, ESI
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 22
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[8] + $698098d8), 7);
  MOV EAX, ESI
  ADD EBX, $698098d8
  XOR EAX, EDI
  ADD EBX, [EDX + 4*8]
  AND EAX, ECX
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 7
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[9] + $8b44f7af), 12);
  MOV EAX, ECX
  ADD EDI, $8b44f7af
  XOR EAX, ESI
  ADD EDI, [EDX + 4*9]
  AND EAX, EBX
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 12
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[10] + $ffff5bb1), 17);
  MOV EAX, EBX
  ADD ESI, $ffff5bb1
  XOR EAX, ECX
  ADD ESI, [EDX + 4*10]
  AND EAX, EDI
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 17
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[11] + $895cd7be), 22);
  MOV EAX, EDI
  ADD ECX, $895cd7be
  XOR EAX, EBX
  ADD ECX, [EDX + 4*11]
  AND EAX, ESI
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 22
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[12] + $6b901122), 7);
  MOV EAX, ESI
  ADD EBX, $6b901122
  XOR EAX, EDI
  ADD EBX, [EDX + 4*12]
  AND EAX, ECX
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 7
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[13] + $fd987193), 12);
  MOV EAX, ECX
  ADD EDI, $fd987193
  XOR EAX, ESI
  ADD EDI, [EDX + 4*13]
  AND EAX, EBX
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 12
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[14] + $a679438e), 17);
  MOV EAX, EBX
  ADD ESI, $a679438e
  XOR EAX, ECX
  ADD ESI, [EDX + 4*14]
  AND EAX, EDI
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 17
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[15] + $49b40821), 22);
  MOV EAX, EDI
  ADD ECX, $49b40821
  XOR EAX, EBX
  ADD ECX, [EDX + 4*15]
  AND EAX, ESI
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 22
  ADD ECX, ESI

// Round 2
//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[1] + $f61e2562), 5);
  MOV EAX, ECX
  ADD EBX, $f61e2562
  XOR EAX, ESI
  ADD EBX, [EDX + 4*1]
  AND EAX, EDI
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 5
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[6] + $c040b340), 9);
  MOV EAX, EBX
  ADD EDI, $c040b340
  XOR EAX, ECX
  ADD EDI, [EDX + 4*6]
  AND EAX, ESI
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI,9
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[11] + $265e5a51), 14);
  MOV EAX, EDI
  ADD ESI, $265e5a51
  XOR EAX, EBX
  ADD ESI, [EDX + 4*11]
  AND EAX, ECX
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 14
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[0] + $e9b6c7aa), 20);
  MOV EAX, ESI
  ADD ECX, $e9b6c7aa
  XOR EAX, EDI
  ADD ECX, [EDX + 4*0]
  AND EAX, EBX
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 20
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[5] + $d62f105d), 5);
  MOV EAX, ECX
  ADD EBX, $d62f105d
  XOR EAX, ESI
  ADD EBX, [EDX + 4*5]
  AND EAX, EDI
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 5
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[10] + $02441453), 9);
  MOV EAX, EBX
  ADD EDI, $02441453
  XOR EAX, ECX
  ADD EDI, [EDX + 4*10]
  AND EAX, ESI
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI,9
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[15] + $d8a1e681), 14);
  MOV EAX, EDI
  ADD ESI, $d8a1e681
  XOR EAX, EBX
  ADD ESI, [EDX + 4*15]
  AND EAX, ECX
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 14
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[4] + $e7d3fbc8), 20);
  MOV EAX, ESI
  ADD ECX, $e7d3fbc8
  XOR EAX, EDI
  ADD ECX, [EDX + 4*4]
  AND EAX, EBX
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 20
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[9] + $21e1cde6), 5);
  MOV EAX, ECX
  ADD EBX, $21e1cde6
  XOR EAX, ESI
  ADD EBX, [EDX + 4*9]
  AND EAX, EDI
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 5
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[14] + $c33707d6), 9);
  MOV EAX, EBX
  ADD EDI, $c33707d6
  XOR EAX, ECX
  ADD EDI, [EDX + 4*14]
  AND EAX, ESI
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI,9
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[3] + $f4d50d87), 14);
  MOV EAX, EDI
  ADD ESI, $f4d50d87
  XOR EAX, EBX
  ADD ESI, [EDX + 4*3]
  AND EAX, ECX
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 14
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[8] + $455a14ed), 20);
  MOV EAX, ESI
  ADD ECX, $455a14ed
  XOR EAX, EDI
  ADD ECX, [EDX + 4*8]
  AND EAX, EBX
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 20
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[13] + $a9e3e905), 5);
  MOV EAX, ECX
  ADD EBX, $a9e3e905
  XOR EAX, ESI
  ADD EBX, [EDX + 4*13]
  AND EAX, EDI
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 5
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[2] + $fcefa3f8), 9);
  MOV EAX, EBX
  ADD EDI, $fcefa3f8
  XOR EAX, ECX
  ADD EDI, [EDX + 4*2]
  AND EAX, ESI
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI,9
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[7] + $676f02d9), 14);
  MOV EAX, EDI
  ADD ESI, $676f02d9
  XOR EAX, EBX
  ADD ESI, [EDX + 4*7]
  AND EAX, ECX
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 14
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[12] + $8d2a4c8a), 20);
  MOV EAX, ESI
  ADD ECX, $8d2a4c8a
  XOR EAX, EDI
  ADD ECX, [EDX + 4*12]
  AND EAX, EBX
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 20
  ADD ECX, ESI

// Round 3
//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[5] + $fffa3942), 4);
  MOV EAX, ECX
  ADD EBX, $fffa3942
  XOR EAX, ESI
  ADD EBX, [EDX + 4*5]
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[8] + $8771f681), 11);
  MOV EAX, EBX
  ADD EDI, $8771f681
  XOR EAX, ECX
  ADD EDI, [EDX + 4*8]
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[11] + $6d9d6122), 16);
  MOV EAX, EDI
  ADD ESI, $6d9d6122
  XOR EAX, EBX
  ADD ESI, [EDX + 4*11]
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[14] + $fde5380c), 23);
  MOV EAX, ESI
  ADD ECX, $fde5380c
  XOR EAX, EDI
  ADD ECX, [EDX + 4*14]
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[1] + $a4beea44), 4);
  MOV EAX, ECX
  ADD EBX, $a4beea44
  XOR EAX, ESI
  ADD EBX, [EDX + 4*1]
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[4] + $4bdecfa9), 11);
  MOV EAX, EBX
  ADD EDI, $4bdecfa9
  XOR EAX, ECX
  ADD EDI, [EDX + 4*4]
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[7] + $f6bb4b60), 16);
  MOV EAX, EDI
  ADD ESI, $f6bb4b60
  XOR EAX, EBX
  ADD ESI, [EDX + 4*7]
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[10] + $bebfbc70), 23);
  MOV EAX, ESI
  ADD ECX, $bebfbc70
  XOR EAX, EDI
  ADD ECX, [EDX + 4*10]
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[13] + $289b7ec6), 4);
  MOV EAX, ECX
  ADD EBX, $289b7ec6
  XOR EAX, ESI
  ADD EBX, [EDX + 4*13]
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[0] + $eaa127fa), 11);
  MOV EAX, EBX
  ADD EDI, $eaa127fa
  XOR EAX, ECX
  ADD EDI, [EDX + 4*0]
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[3] + $d4ef3085), 16);
  MOV EAX, EDI
  ADD ESI, $d4ef3085
  XOR EAX, EBX
  ADD ESI, [EDX + 4*3]
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[6] + $04881d05), 23);
  MOV EAX, ESI
  ADD ECX, $04881d05
  XOR EAX, EDI
  ADD ECX, [EDX + 4*6]
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[9] + $d9d4d039), 4);
  MOV EAX, ECX
  ADD EBX, $d9d4d039
  XOR EAX, ESI
  ADD EBX, [EDX + 4*9]
  XOR EAX, EDI
  ADD EBX, EAX
  ROL EBX, 4
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[12] + $e6db99e5), 11);
  MOV EAX, EBX
  ADD EDI, $e6db99e5
  XOR EAX, ECX
  ADD EDI, [EDX + 4*12]
  XOR EAX, ESI
  ADD EDI, EAX
  ROL EDI, 11
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[15] + $1fa27cf8), 16);
  MOV EAX, EDI
  ADD ESI, $1fa27cf8
  XOR EAX, EBX
  ADD ESI, [EDX + 4*15]
  XOR EAX, ECX
  ADD ESI, EAX
  ROL ESI, 16
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[2] + $c4ac5665), 23);
  MOV EAX, ESI
  ADD ECX, $c4ac5665
  XOR EAX, EDI
  ADD ECX, [EDX + 4*2]
  XOR EAX, EBX
  ADD ECX, EAX
  ROL ECX, 23
  ADD ECX, ESI

// Round 4
//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[0] + $f4292244), 6);
  MOV EAX, EDI
  ADD EBX, $f4292244
  NOT EAX
  ADD EBX, [EDX + 4*0]
  OR  EAX, ECX
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 6
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[7] + $432aff97), 10);
  MOV EAX, ESI
  ADD EDI, $432aff97
  NOT EAX
  ADD EDI, [EDX + 4*7]
  OR  EAX, EBX
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI, 10
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[14] + $ab9423a7), 15);
  MOV EAX, ECX
  ADD ESI, $ab9423a7
  NOT EAX
  ADD ESI, [EDX + 4*14]
  OR  EAX, EDI
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 15
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[5] + $fc93a039), 21);
  MOV EAX, EBX
  ADD ECX, $fc93a039
  NOT EAX
  ADD ECX, [EDX + 4*5]
  OR  EAX, ESI
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 21
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[12] + $655b59c3), 6);
  MOV EAX, EDI
  ADD EBX, $655b59c3
  NOT EAX
  ADD EBX, [EDX + 4*12]
  OR  EAX, ECX
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 6
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[3] + $8f0ccc92), 10);
  MOV EAX, ESI
  ADD EDI, $8f0ccc92
  NOT EAX
  ADD EDI, [EDX + 4*3]
  OR  EAX, EBX
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI, 10
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[10] + $ffeff47d), 15);
  MOV EAX, ECX
  ADD ESI, $ffeff47d
  NOT EAX
  ADD ESI, [EDX + 4*10]
  OR  EAX, EDI
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 15
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[1] + $85845dd1), 21);
  MOV EAX, EBX
  ADD ECX, $85845dd1
  NOT EAX
  ADD ECX, [EDX + 4*1]
  OR  EAX, ESI
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 21
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[8] + $6fa87e4f), 6);
  MOV EAX, EDI
  ADD EBX, $6fa87e4f
  NOT EAX
  ADD EBX, [EDX + 4*8]
  OR  EAX, ECX
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 6
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[15] + $fe2ce6e0), 10);
  MOV EAX, ESI
  ADD EDI, $fe2ce6e0
  NOT EAX
  ADD EDI, [EDX + 4*15]
  OR  EAX, EBX
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI, 10
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[6] + $a3014314), 15);
  MOV EAX, ECX
  ADD ESI, $a3014314
  NOT EAX
  ADD ESI, [EDX + 4*6]
  OR  EAX, EDI
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 15
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[13] + $4e0811a1), 21);
  MOV EAX, EBX
  ADD ECX, $4e0811a1
  NOT EAX
  ADD ECX, [EDX + 4*13]
  OR  EAX, ESI
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 21
  ADD ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[4] + $f7537e82), 6);
  MOV EAX, EDI
  ADD EBX, $f7537e82
  NOT EAX
  ADD EBX, [EDX + 4*4]
  OR  EAX, ECX
  XOR EAX, ESI
  ADD EBX, EAX
  ROL EBX, 6
  ADD EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[11] + $bd3af235), 10);
  MOV EAX, ESI
  ADD EDI, $bd3af235
  NOT EAX
  ADD EDI, [EDX + 4*11]
  OR  EAX, EBX
  XOR EAX, ECX
  ADD EDI, EAX
  ROL EDI, 10
  ADD EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[2] + $2ad7d2bb), 15);
  MOV EAX, ECX
  ADD ESI, $2ad7d2bb
  NOT EAX
  ADD ESI, [EDX + 4*2]
  OR  EAX, EDI
  XOR EAX, EBX
  ADD ESI, EAX
  ROL ESI, 15
  ADD ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[9] + $eb86d391), 21);
  MOV EAX, EBX
  ADD ECX, $eb86d391
  NOT EAX
  ADD ECX, [EDX + 4*9]
  OR  EAX, ESI
  XOR EAX, EDI
  ADD ECX, EAX
  ROL ECX, 21
  ADD ECX, ESI
  
  POP EAX // EAX = Context

  ADD TMDContext.State[EAX + 4*0], EBX //  Context.State[0 .. 3] += A, B, C, D
  ADD TMDContext.State[EAX + 4*1], ECX
  ADD TMDContext.State[EAX + 4*2], ESI
  ADD TMDContext.State[EAX + 4*3], EDI

//Inc(Context.Length,64);
  ADD DWORD PTR TMDContext.Length[EAX], 64
  ADC DWORD PTR TMDContext.Length[EAX + 4], 0

  POP EDI
  POP ESI
  POP EBX
end;
{$endif CPUX86_HAS_BMI1}
