// x86_64 (Windows) assembly optimized version
{$ifdef CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// RCX = Context, RDX = Buffer
{$asmmode intel}
asm
.seh_pushreg RBX
  PUSH RBX
.seh_endprologue

  // R8D = A, R9D = B, R10D = C, R11D = D
  MOV  R8D, TMDContext.State[RCX + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV  R9D, TMDContext.State[RCX + 4*1]
  MOV  R10D,TMDContext.State[RCX + 4*2]
  MOV  R11D,TMDContext.State[RCX + 4*3]

// Round 1
//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[0] + $d76aa478), 7);
  ADD  R8D, [RDX + 4*0]
  MOV  EAX, R9D
  ANDN EBX, R9D, R11D
  ADD  R8D, $d76aa478
  AND  EAX, R10D
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[1] + $e8c7b756), 12);
  ADD  R11D,[RDX + 4*1]
  ANDN EBX, R8D, R10D
  AND  EAX, R9D
  ADD  R11D,$e8c7b756
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,12
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[2] + $242070db), 17);
  ADD  R10D,[RDX + 4*2]
  ANDN EBX, R11D,R9D
  AND  EAX, R8D
  ADD  R10D,$242070db
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,17
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[3] + $c1bdceee), 22);
  ADD  R9D, [RDX + 4*3]
  ANDN EBX, R10D,R8D
  AND  EAX, R11D
  ADD  R9D, $c1bdceee
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[4] + $f57c0faf), 7);
  ADD  R8D, [RDX + 4*4]
  ANDN EBX, R9D, R11D
  AND  EAX, R10D
  ADD  R8D, $f57c0faf
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[5] + $4787c62a), 12);
  ADD  R11D,[RDX + 4*5]
  ANDN EBX, R8D, R10D
  AND  EAX, R9D
  ADD  R11D,$4787c62a
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,12
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[6] + $a8304613), 17);
  ADD  R10D,[RDX + 4*6]
  ANDN EBX, R11D,R9D
  AND  EAX, R8D
  ADD  R10D,$a8304613
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,17
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[7] + $fd469501), 22);
  ADD  R9D, [RDX + 4*7]
  ANDN EBX, R10D,R8D
  AND  EAX, R11D
  ADD  R9D, $fd469501
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[8] + $698098d8), 7);
  ADD  R8D, [RDX + 4*8]
  ANDN EBX, R9D, R11D
  AND  EAX, R10D
  ADD  R8D, $698098d8
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[9] + $8b44f7af), 12);
  ADD  R11D,[RDX + 4*9]
  ANDN EBX, R8D, R10D
  AND  EAX, R9D
  ADD  R11D,$8b44f7af
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,12
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[10] + $ffff5bb1), 17);
  ADD  R10D,[RDX + 4*10]
  ANDN EBX, R11D,R9D
  AND  EAX, R8D
  ADD  R10D,$ffff5bb1
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,17
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[11] + $895cd7be), 22);
  ADD  R9D, [RDX + 4*11]
  ANDN EBX, R10D,R8D
  AND  EAX, R11D
  ADD  R9D, $895cd7be
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[12] + $6b901122), 7);
  ADD  R8D, [RDX + 4*12]
  ANDN EBX, R9D, R11D
  AND  EAX, R10D
  ADD  R8D, $6b901122
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 7
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[13] + $fd987193), 12);
  ADD  R11D,[RDX + 4*13]
  ANDN EBX, R8D, R10D
  AND  EAX, R9D
  ADD  R11D,$fd987193
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,12
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[14] + $a679438e), 17);
  ADD  R10D,[RDX + 4*14]
  ANDN EBX, R11D,R9D
  AND  EAX, R8D
  ADD  R10D,$a679438e
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,17
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[15] + $49b40821), 22);
  ADD  R9D, [RDX + 4*15]
  ANDN EBX, R10D,R8D
  AND  EAX, R11D
  ADD  R9D, $49b40821
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 22
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

// Round 2
//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or ((not R11D) and R10D)) + Data[1] + $f61e2562), 5);
  ADD  R8D, [RDX + 4*1]
  ANDN EBX, R11D,R10D
  AND  EAX, R11D
  ADD  R8D, $f61e2562
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or ((not R10D) and R9D)) + Data[6] + $c040b340), 9);
  ADD  R11D,[RDX + 4*6]
  ANDN EBX, R10D,R9D
  AND  EAX, R10D
  ADD  R11D,$c040b340
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,9
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or ((not R9D) and R8D)) + Data[11] + $265e5a51), 14);
  ADD  R10D,[RDX + 4*11]
  ANDN EBX, R9D, R8D
  AND  EAX, R9D
  ADD  R10D,$265e5a51
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,14
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or ((not R8D) and R11D)) + Data[0] + $e9b6c7aa), 20);
  ADD  R9D, [RDX + 4*0]
  ANDN EBX, R8D, R11D
  AND  EAX, R8D
  ADD  R9D, $e9b6c7aa
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or ((not R11D) and R10D)) + Data[5] + $d62f105d), 5);
  ADD  R8D, [RDX + 4*5]
  ANDN EBX, R11D,R10D
  AND  EAX, R11D
  ADD  R8D, $d62f105d
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or ((not R10D) and R9D)) + Data[10] + $02441453), 9);
  ADD  R11D,[RDX + 4*10]
  ANDN EBX, R10D,R9D
  AND  EAX, R10D
  ADD  R11D,$02441453
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,9
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or ((not R9D) and R8D)) + Data[15] + $d8a1e681), 14);
  ADD  R10D,[RDX + 4*15]
  ANDN EBX, R9D, R8D
  AND  EAX, R9D
  ADD  R10D,$d8a1e681
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,14
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or ((not R8D) and R11D)) + Data[4] + $e7d3fbc8), 20);
  ADD  R9D, [RDX + 4*4]
  ANDN EBX, R8D, R11D
  AND  EAX, R8D
  ADD  R9D, $e7d3fbc8
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[9] + $21e1cde6), 5);
  ADD  R8D, [RDX + 4*9]
  ANDN EBX, R11D,R10D
  AND  EAX, R11D
  ADD  R8D, $21e1cde6
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[14] + $c33707d6), 9);
  ADD  R11D,[RDX + 4*14]
  ANDN EBX, R10D,R9D
  AND  EAX, R10D
  ADD  R11D,$c33707d6
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,9
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[3] + $f4d50d87), 14);
  ADD  R10D,[RDX + 4*3]
  ANDN EBX, R9D, R8D
  AND  EAX, R9D
  ADD  R10D,$f4d50d87
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,14
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[8] + $455a14ed), 20);
  ADD  R9D, [RDX + 4*8]
  ANDN EBX, R8D, R11D
  AND  EAX, R8D
  ADD  R9D, $455a14ed
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 20
  LEA  EAX, [R9D + R10D]
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[13] + $a9e3e905), 5);
  ADD  R8D, [RDX + 4*13]
  ANDN EBX, R11D,R10D
  AND  EAX, R11D
  ADD  R8D, $a9e3e905
  OR   EAX, EBX
  ADD  R8D, EAX
  ROL  R8D, 5
  LEA  EAX, [R8D + R9D]
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[2] + $fcefa3f8), 9);
  ADD  R11D,[RDX + 4*2]
  ANDN EBX, R10D,R9D
  AND  EAX, R10D
  ADD  R11D,$fcefa3f8
  OR   EAX, EBX
  ADD  R11D,EAX
  ROL  R11D,9
  LEA  EAX, [R11D + R8D]
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[7] + $676f02d9), 14);
  ADD  R10D,[RDX + 4*7]
  ANDN EBX, R9D, R8D
  AND  EAX, R9D
  ADD  R10D,$676f02d9
  OR   EAX, EBX
  ADD  R10D,EAX
  ROL  R10D,14
  LEA  EAX, [R10D + R11D]
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[12] + $8d2a4c8a), 20);
  ADD  R9D, [RDX + 4*12]
  ANDN EBX, R8D, R11D
  AND  EAX, R8D
  ADD  R9D, $8d2a4c8a
  OR   EAX, EBX
  ADD  R9D, EAX
  ROL  R9D, 20
  ADD  R9D, R10D

// Round 3
//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[5] + $fffa3942), 4);
  ADD R8D, [RDX + 4*5]
  MOV EAX, R9D
  XOR EAX, R10D
  ADD R8D, $fffa3942
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[8] + $8771f681), 11);
  ADD R11D,[RDX + 4*8]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD R11D,$8771f681
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[11] + $6d9d6122), 16);
  ADD R10D,[RDX + 4*11]
  MOV EAX, R11D
  XOR EAX, R8D
  ADD R10D,$6d9d6122
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[14] + $fde5380c), 23);
  ADD R9D, [RDX + 4*14]
  MOV EAX, R10D
  XOR EAX, R11D
  ADD R9D, $fde5380c
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[1] + $a4beea44), 4);
  ADD R8D, [RDX + 4*1]
  MOV EAX, R9D
  XOR EAX, R10D
  ADD R8D, $a4beea44
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[4] + $4bdecfa9), 11);
  ADD R11D,[RDX + 4*4]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD R11D,$4bdecfa9
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[7] + $f6bb4b60), 16);
  ADD R10D,[RDX + 4*7]
  MOV EAX, R11D
  XOR EAX, R8D
  ADD R10D,$f6bb4b60
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[10] + $bebfbc70), 23);
  ADD R9D, [RDX + 4*10]
  MOV EAX, R10D
  XOR EAX, R11D
  ADD R9D, $bebfbc70
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[13] + $289b7ec6), 4);
  ADD R8D, [RDX + 4*13]
  MOV EAX, R9D
  XOR EAX, R10D
  ADD R8D, $289b7ec6
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[0] + $eaa127fa), 11);
  ADD R11D,[RDX + 4*0]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD R11D,$eaa127fa
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[3] + $d4ef3085), 16);
  ADD R10D,[RDX + 4*3]
  MOV EAX, R11D
  XOR EAX, R8D
  ADD R10D,$d4ef3085
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[6] + $04881d05), 23);
  ADD R9D, [RDX + 4*6]
  MOV EAX, R10D
  XOR EAX, R11D
  ADD R9D, $04881d05
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[9] + $d9d4d039), 4);
  ADD R8D, [RDX + 4*9]
  MOV EAX, R9D
  XOR EAX, R10D
  ADD R8D, $d9d4d039
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[12] + $e6db99e5), 11);
  ADD R11D,[RDX + 4*12]
  MOV EAX, R8D
  XOR EAX, R9D
  ADD R11D,$e6db99e5
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[15] + $1fa27cf8), 16);
  ADD R10D,[RDX + 4*15]
  MOV EAX, R11D
  XOR EAX, R8D
  ADD R10D,$1fa27cf8
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  MOV EBX, -1 // Prepare a register of all 1s for Round 4.
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[2] + $c4ac5665), 23);
  ADD R9D, [RDX + 4*2]
  MOV EAX, R10D
  XOR EAX, R11D
  ADD R9D, $c4ac5665
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

// Round 4 (throughout this round, "ANDN EAX, reg, EBX" stands in for "EAX := not reg")
//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[0] + $f4292244), 6);
  ADD  R8D, [RDX + 4*0]
  ANDN EAX, R11D,EBX
  ADD  R8D, $f4292244
  OR   EAX, R9D
  XOR  EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[7] + $432aff97), 10);
  ADD  R11D,[RDX + 4*7]
  ANDN EAX, R10D,EBX
  ADD  R11D,$432aff97
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  R11D,EAX
  ROL  R11D,10
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[14] + $ab9423a7), 15);
  ADD  R10D,[RDX + 4*14]
  ANDN EAX, R9D, EBX
  ADD  R10D,$ab9423a7
  OR   EAX, R11D
  XOR  EAX, R8D
  ADD  R10D,EAX
  ROL  R10D,15
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[5] + $fc93a039), 21);
  ADD  R9D, [RDX + 4*5]
  ANDN EAX, R8D, EBX
  ADD  R9D, $fc93a039
  OR   EAX, R10D
  XOR  EAX, R11D
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[12] + $655b59c3), 6);
  ADD  R8D, [RDX + 4*12]
  ANDN EAX, R11D,EBX
  ADD  R8D, $655b59c3
  OR   EAX, R9D
  XOR  EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[3] + $8f0ccc92), 10);
  ADD  R11D,[RDX + 4*3]
  ANDN EAX, R10D,EBX
  ADD  R11D,$8f0ccc92
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  R11D,EAX
  ROL  R11D,10
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[10] + $ffeff47d), 15);
  ADD  R10D,[RDX + 4*10]
  ANDN EAX, R9D, EBX
  ADD  R10D,$ffeff47d
  OR   EAX, R11D
  XOR  EAX, R8D
  ADD  R10D,EAX
  ROL  R10D,15
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[1] + $85845dd1), 21);
  ADD  R9D, [RDX + 4*1]
  ANDN EAX, R8D, EBX
  ADD  R9D, $85845dd1
  OR   EAX, R10D
  XOR  EAX, R11D
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[8] + $6fa87e4f), 6);
  ADD  R8D, [RDX + 4*8]
  ANDN EAX, R11D,EBX
  ADD  R8D, $6fa87e4f
  OR   EAX, R9D
  XOR  EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[15] + $fe2ce6e0), 10);
  ADD  R11D,[RDX + 4*15]
  ANDN EAX, R10D,EBX
  ADD  R11D,$fe2ce6e0
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  R11D,EAX
  ROL  R11D,10
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[6] + $a3014314), 15);
  ADD  R10D,[RDX + 4*6]
  ANDN EAX, R9D, EBX
  ADD  R10D,$a3014314
  OR   EAX, R11D
  XOR  EAX, R8D
  ADD  R10D,EAX
  ROL  R10D,15
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[13] + $4e0811a1), 21);
  ADD  R9D, [RDX + 4*13]
  ANDN EAX, R8D, EBX
  ADD  R9D, $4e0811a1
  OR   EAX, R10D
  XOR  EAX, R11D
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[4] + $f7537e82), 6);
  ADD  R8D, [RDX + 4*4]
  ANDN EAX, R11D,EBX
  ADD  R8D, $f7537e82
  OR   EAX, R9D
  XOR  EAX, R10D
  ADD  R8D, EAX
  ROL  R8D, 6
  ADD  R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[11] + $bd3af235), 10);
  ADD  R11D,[RDX + 4*11]
  ANDN EAX, R10D,EBX
  ADD  R11D,$bd3af235
  OR   EAX, R8D
  XOR  EAX, R9D
  ADD  R11D,EAX
  ROL  R11D,10
  ADD  R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[2] + $2ad7d2bb), 15);
  ADD  R10D,[RDX + 4*2]
  ANDN EAX, R9D, EBX
  ADD  R10D,$2ad7d2bb
  OR   EAX, R11D
  XOR  EAX, R8D
  ADD  R10D,EAX
  ROL  R10D,15
  ADD  R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[9] + $eb86d391), 21);
  ADD  R9D, [RDX + 4*9]
  ANDN EAX, R8D, EBX
  ADD  R9D, $eb86d391
  OR   EAX, R10D
  XOR  EAX, R11D
  ADD  R9D, EAX
  ROL  R9D, 21
  ADD  R9D, R10D

  ADD TMDContext.State[RCX + 4*0], R8D //  Context.State[0 .. 3] += A, B, C, D
  ADD TMDContext.State[RCX + 4*1], R9D
  ADD TMDContext.State[RCX + 4*2], R10D
  ADD TMDContext.State[RCX + 4*3], R11D

//Inc(Context.Length,64);
  ADD QWORD PTR TMDContext.Length[RCX],64

  POP  RBX
end;
{$else CPUX86_HAS_BMI1}
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// RCX = Context, RDX = Buffer
{$asmmode intel}
asm
  // R8D = A, R9D = B, R10D = C, R11D = D
  MOV R8D, TMDContext.State[RCX + 4*0] // A, B, C, D := Context.State[0 .. 3];
  MOV R9D, TMDContext.State[RCX + 4*1]
  MOV R10D,TMDContext.State[RCX + 4*2]
  MOV R11D,TMDContext.State[RCX + 4*3]
// Round 1
//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[0] + $d76aa478), 7);
  MOV EAX, R10D
  ADD R8D, $d76aa478
  XOR EAX, R11D
  ADD R8D, [RDX + 4*0]
  AND EAX, R9D
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[1] + $e8c7b756), 12);
  MOV EAX, R9D
  ADD R11D,$e8c7b756
  XOR EAX, R10D
  ADD R11D,[RDX + 4*1]
  AND EAX, R8D
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,12
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[2] + $242070db), 17);
  MOV EAX, R8D
  ADD R10D,$242070db
  XOR EAX, R9D
  ADD R10D,[RDX + 4*2]
  AND EAX, R11D
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,17
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[3] + $c1bdceee), 22);
  MOV EAX, R11D
  ADD R9D, $c1bdceee
  XOR EAX, R8D
  ADD R9D, [RDX + 4*3]
  AND EAX, R10D
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[4] + $f57c0faf), 7);
  MOV EAX, R10D
  ADD R8D, $f57c0faf
  XOR EAX, R11D
  ADD R8D, [RDX + 4*4]
  AND EAX, R9D
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[5] + $4787c62a), 12);
  MOV EAX, R9D
  ADD R11D,$4787c62a
  XOR EAX, R10D
  ADD R11D,[RDX + 4*5]
  AND EAX, R8D
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,12
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[6] + $a8304613), 17);
  MOV EAX, R8D
  ADD R10D,$a8304613
  XOR EAX, R9D
  ADD R10D,[RDX + 4*6]
  AND EAX, R11D
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,17
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[7] + $fd469501), 22);
  MOV EAX, R11D
  ADD R9D, $fd469501
  XOR EAX, R8D
  ADD R9D, [RDX + 4*7]
  AND EAX, R10D
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[8] + $698098d8), 7);
  MOV EAX, R10D
  ADD R8D, $698098d8
  XOR EAX, R11D
  ADD R8D, [RDX + 4*8]
  AND EAX, R9D
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[9] + $8b44f7af), 12);
  MOV EAX, R9D
  ADD R11D,$8b44f7af
  XOR EAX, R10D
  ADD R11D,[RDX + 4*9]
  AND EAX, R8D
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,12
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[10] + $ffff5bb1), 17);
  MOV EAX, R8D
  ADD R10D,$ffff5bb1
  XOR EAX, R9D
  ADD R10D,[RDX + 4*10]
  AND EAX, R11D
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,17
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[11] + $895cd7be), 22);
  MOV EAX, R11D
  ADD R9D, $895cd7be
  XOR EAX, R8D
  ADD R9D, [RDX + 4*11]
  AND EAX, R10D
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R10D) or ((not R9D) and R11D)) + Data[12] + $6b901122), 7);
  MOV EAX, R10D
  ADD R8D, $6b901122
  XOR EAX, R11D
  ADD R8D, [RDX + 4*12]
  AND EAX, R9D
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 7
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R9D) or ((not R8D) and R10D)) + Data[13] + $fd987193), 12);
  MOV EAX, R9D
  ADD R11D,$fd987193
  XOR EAX, R10D
  ADD R11D,[RDX + 4*13]
  AND EAX, R8D
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,12
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R8D) or ((not R11D) and R9D)) + Data[14] + $a679438e), 17);
  MOV EAX, R8D
  ADD R10D,$a679438e
  XOR EAX, R9D
  ADD R10D,[RDX + 4*14]
  AND EAX, R11D
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,17
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R11D) or ((not R10D) and R8D)) + Data[15] + $49b40821), 22);
  MOV EAX, R11D
  ADD R9D, $49b40821
  XOR EAX, R8D
  ADD R9D, [RDX + 4*15]
  AND EAX, R10D
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 22
  ADD R9D, R10D

// Round 2
//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[1] + $f61e2562), 5);
  MOV EAX, R9D
  ADD R8D, $f61e2562
  XOR EAX, R10D
  ADD R8D, [RDX + 4*1]
  AND EAX, R11D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[6] + $c040b340), 9);
  MOV EAX, R8D
  ADD R11D,$c040b340
  XOR EAX, R9D
  ADD R11D,[RDX + 4*6]
  AND EAX, R10D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,9
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[11] + $265e5a51), 14);
  MOV EAX, R11D
  ADD R10D,$265e5a51
  XOR EAX, R8D
  ADD R10D,[RDX + 4*11]
  AND EAX, R9D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,14
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[0] + $e9b6c7aa), 20);
  MOV EAX, R10D
  ADD R9D, $e9b6c7aa
  XOR EAX, R11D
  ADD R9D, [RDX + 4*0]
  AND EAX, R8D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[5] + $d62f105d), 5);
  MOV EAX, R9D
  ADD R8D, $d62f105d
  XOR EAX, R10D
  ADD R8D, [RDX + 4*5]
  AND EAX, R11D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[10] + $02441453), 9);
  MOV EAX, R8D
  ADD R11D,$02441453
  XOR EAX, R9D
  ADD R11D,[RDX + 4*10]
  AND EAX, R10D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,9
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[15] + $d8a1e681), 14);
  MOV EAX, R11D
  ADD R10D,$d8a1e681
  XOR EAX, R8D
  ADD R10D,[RDX + 4*15]
  AND EAX, R9D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,14
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[4] + $e7d3fbc8), 20);
  MOV EAX, R10D
  ADD R9D, $e7d3fbc8
  XOR EAX, R11D
  ADD R9D, [RDX + 4*4]
  AND EAX, R8D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[9] + $21e1cde6), 5);
  MOV EAX, R9D
  ADD R8D, $21e1cde6
  XOR EAX, R10D
  ADD R8D, [RDX + 4*9]
  AND EAX, R11D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[14] + $c33707d6), 9);
  MOV EAX, R8D
  ADD R11D,$c33707d6
  XOR EAX, R9D
  ADD R11D,[RDX + 4*14]
  AND EAX, R10D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,9
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[3] + $f4d50d87), 14);
  MOV EAX, R11D
  ADD R10D,$f4d50d87
  XOR EAX, R8D
  ADD R10D,[RDX + 4*3]
  AND EAX, R9D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,14
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[8] + $455a14ed), 20);
  MOV EAX, R10D
  ADD R9D, $455a14ed
  XOR EAX, R11D
  ADD R9D, [RDX + 4*8]
  AND EAX, R8D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + ((R9D and R11D) or (R10D and (not R11D))) + Data[13] + $a9e3e905), 5);
  MOV EAX, R9D
  ADD R8D, $a9e3e905
  XOR EAX, R10D
  ADD R8D, [RDX + 4*13]
  AND EAX, R11D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 5
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + ((R8D and R10D) or (R9D and (not R10D))) + Data[2] + $fcefa3f8), 9);
  MOV EAX, R8D
  ADD R11D,$fcefa3f8
  XOR EAX, R9D
  ADD R11D,[RDX + 4*2]
  AND EAX, R10D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,9
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + ((R11D and R9D) or (R8D and (not R9D))) + Data[7] + $676f02d9), 14);
  MOV EAX, R11D
  ADD R10D,$676f02d9
  XOR EAX, R8D
  ADD R10D,[RDX + 4*7]
  AND EAX, R9D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,14
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + ((R10D and R8D) or (R11D and (not R8D))) + Data[12] + $8d2a4c8a), 20);
  MOV EAX, R10D
  ADD R9D, $8d2a4c8a
  XOR EAX, R11D
  ADD R9D, [RDX + 4*12]
  AND EAX, R8D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 20
  ADD R9D, R10D

// Round 3
//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[5] + $fffa3942), 4);
  MOV EAX, R9D
  ADD R8D, $fffa3942
  XOR EAX, R10D
  ADD R8D, [RDX + 4*5]
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[8] + $8771f681), 11);
  MOV EAX, R8D
  ADD R11D,$8771f681
  XOR EAX, R9D
  ADD R11D,[RDX + 4*8]
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[11] + $6d9d6122), 16);
  MOV EAX, R11D
  ADD R10D,$6d9d6122
  XOR EAX, R8D
  ADD R10D,[RDX + 4*11]
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[14] + $fde5380c), 23);
  MOV EAX, R10D
  ADD R9D, $fde5380c
  XOR EAX, R11D
  ADD R9D, [RDX + 4*14]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[1] + $a4beea44), 4);
  MOV EAX, R9D
  ADD R8D, $a4beea44
  XOR EAX, R10D
  ADD R8D, [RDX + 4*1]
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[4] + $4bdecfa9), 11);
  MOV EAX, R8D
  ADD R11D,$4bdecfa9
  XOR EAX, R9D
  ADD R11D,[RDX + 4*4]
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[7] + $f6bb4b60), 16);
  MOV EAX, R11D
  ADD R10D,$f6bb4b60
  XOR EAX, R8D
  ADD R10D,[RDX + 4*7]
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[10] + $bebfbc70), 23);
  MOV EAX, R10D
  ADD R9D, $bebfbc70
  XOR EAX, R11D
  ADD R9D, [RDX + 4*10]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[13] + $289b7ec6), 4);
  MOV EAX, R9D
  ADD R8D, $289b7ec6
  XOR EAX, R10D
  ADD R8D, [RDX + 4*13]
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[0] + $eaa127fa), 11);
  MOV EAX, R8D
  ADD R11D,$eaa127fa
  XOR EAX, R9D
  ADD R11D,[RDX + 4*0]
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[3] + $d4ef3085), 16);
  MOV EAX, R11D
  ADD R10D,$d4ef3085
  XOR EAX, R8D
  ADD R10D,[RDX + 4*3]
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[6] + $04881d05), 23);
  MOV EAX, R10D
  ADD R9D, $04881d05
  XOR EAX, R11D
  ADD R9D, [RDX + 4*6]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R9D xor R10D xor R11D) + Data[9] + $d9d4d039), 4);
  MOV EAX, R9D
  ADD R8D, $d9d4d039
  XOR EAX, R10D
  ADD R8D, [RDX + 4*9]
  XOR EAX, R11D
  ADD R8D, EAX
  ROL R8D, 4
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R8D xor R9D xor R10D) + Data[12] + $e6db99e5), 11);
  MOV EAX, R8D
  ADD R11D,$e6db99e5
  XOR EAX, R9D
  ADD R11D,[RDX + 4*12]
  XOR EAX, R10D
  ADD R11D,EAX
  ROL R11D,11
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R11D xor R8D xor R9D) + Data[15] + $1fa27cf8), 16);
  MOV EAX, R11D
  ADD R10D,$1fa27cf8
  XOR EAX, R8D
  ADD R10D,[RDX + 4*15]
  XOR EAX, R9D
  ADD R10D,EAX
  ROL R10D,16
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R10D xor R11D xor R8D) + Data[2] + $c4ac5665), 23);
  MOV EAX, R10D
  ADD R9D, $c4ac5665
  XOR EAX, R11D
  ADD R9D, [RDX + 4*2]
  XOR EAX, R8D
  ADD R9D, EAX
  ROL R9D, 23
  ADD R9D, R10D

// Round 4
//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[0] + $f4292244), 6);
  MOV EAX, R11D
  ADD R8D, $f4292244
  NOT EAX
  ADD R8D, [RDX + 4*0]
  OR  EAX, R9D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[7] + $432aff97), 10);
  MOV EAX, R10D
  ADD R11D,$432aff97
  NOT EAX
  ADD R11D,[RDX + 4*7]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,10
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[14] + $ab9423a7), 15);
  MOV EAX, R9D
  ADD R10D,$ab9423a7
  NOT EAX
  ADD R10D,[RDX + 4*14]
  OR  EAX, R11D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,15
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[5] + $fc93a039), 21);
  MOV EAX, R8D
  ADD R9D, $fc93a039
  NOT EAX
  ADD R9D, [RDX + 4*5]
  OR  EAX, R10D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[12] + $655b59c3), 6);
  MOV EAX, R11D
  ADD R8D, $655b59c3
  NOT EAX
  ADD R8D, [RDX + 4*12]
  OR  EAX, R9D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[3] + $8f0ccc92), 10);
  MOV EAX, R10D
  ADD R11D,$8f0ccc92
  NOT EAX
  ADD R11D,[RDX + 4*3]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,10
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[10] + $ffeff47d), 15);
  MOV EAX, R9D
  ADD R10D,$ffeff47d
  NOT EAX
  ADD R10D,[RDX + 4*10]
  OR  EAX, R11D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,15
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[1] + $85845dd1), 21);
  MOV EAX, R8D
  ADD R9D, $85845dd1
  NOT EAX
  ADD R9D, [RDX + 4*1]
  OR  EAX, R10D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[8] + $6fa87e4f), 6);
  MOV EAX, R11D
  ADD R8D, $6fa87e4f
  NOT EAX
  ADD R8D, [RDX + 4*8]
  OR  EAX, R9D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[15] + $fe2ce6e0), 10);
  MOV EAX, R10D
  ADD R11D,$fe2ce6e0
  NOT EAX
  ADD R11D,[RDX + 4*15]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,10
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[6] + $a3014314), 15);
  MOV EAX, R9D
  ADD R10D,$a3014314
  NOT EAX
  ADD R10D,[RDX + 4*6]
  OR  EAX, R11D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,15
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[13] + $4e0811a1), 21);
  MOV EAX, R8D
  ADD R9D, $4e0811a1
  NOT EAX
  ADD R9D, [RDX + 4*13]
  OR  EAX, R10D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, R10D

//R8D := R9D + roldword(dword(R8D + (R10D xor (R9D or (not R11D))) + Data[4] + $f7537e82), 6);
  MOV EAX, R11D
  ADD R8D, $f7537e82
  NOT EAX
  ADD R8D, [RDX + 4*4]
  OR  EAX, R9D
  XOR EAX, R10D
  ADD R8D, EAX
  ROL R8D, 6
  ADD R8D, R9D

//R11D := R8D + roldword(dword(R11D + (R9D xor (R8D or (not R10D))) + Data[11] + $bd3af235), 10);
  MOV EAX, R10D
  ADD R11D,$bd3af235
  NOT EAX
  ADD R11D,[RDX + 4*11]
  OR  EAX, R8D
  XOR EAX, R9D
  ADD R11D,EAX
  ROL R11D,10
  ADD R11D,R8D

//R10D := R11D + roldword(dword(R10D + (R8D xor (R11D or (not R9D))) + Data[2] + $2ad7d2bb), 15);
  MOV EAX, R9D
  ADD R10D,$2ad7d2bb
  NOT EAX
  ADD R10D,[RDX + 4*2]
  OR  EAX, R11D
  XOR EAX, R8D
  ADD R10D,EAX
  ROL R10D,15
  ADD R10D,R11D

//R9D := R10D + roldword(dword(R9D + (R11D xor (R10D or (not R8D))) + Data[9] + $eb86d391), 21);
  MOV EAX, R8D
  ADD R9D, $eb86d391
  NOT EAX
  ADD R9D, [RDX + 4*9]
  OR  EAX, R10D
  XOR EAX, R11D
  ADD R9D, EAX
  ROL R9D, 21
  ADD R9D, R10D

  ADD TMDContext.State[RCX + 4*0], R8D //  Context.State[0 .. 3] += A, B, C, D
  ADD TMDContext.State[RCX + 4*1], R9D
  ADD TMDContext.State[RCX + 4*2], R10D
  ADD TMDContext.State[RCX + 4*3], R11D

//Inc(Context.Length,64);
  ADD QWORD PTR TMDContext.Length[RCX],64
end;
{$endif CPUX86_HAS_BMI1}
