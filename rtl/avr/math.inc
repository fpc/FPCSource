{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

// Based on restoring division algorithm
// Algorithm source document: Lecture notes by S. Galal and D. Pham, Division algorithms and hardware implementations.
// Link to documentation http://www.seas.ucla.edu/~ingrid/ee213a/lectures/division_presentV2.pdf
// Also refer to description on Wikipedia: https://en.wikipedia.org/wiki/Division_algorithm#Restoring_division

// Note that the algorithm automatically yields the following results for special cases:
// z div 0 = MAX(type)
// 0 div 0 = MAX(type)
// 0 div n = 0
// Checks for z = 0; n = [0,1]; n = z and n > z could shortcut the algorithm for speed-ups
// but would add extra code
// Perhaps add the checks depending on optimization settings?

// z (dividend) = q(quotient) x n(divisor) + p(remainder)

{$ifndef FPC_SYSTEM_HAS_DIV_BYTE}
{$define FPC_SYSTEM_HAS_DIV_BYTE}

// z in Ra, n in Rb, 0 in Rp
function fpc_div_byte(n, z: byte): byte; assembler; nostackframe;
{$ifdef FPC_IS_SYSTEM}[public,alias: 'FPC_DIV_BYTE'];{$endif}
label
  start, div1, div2, div3, finish;
asm
// Symbol  Name        Register(s)
// z (A)   dividend    R22
// n (B)   divisor     R24
// p (P)   remainder   R20
// i	   counter     R18

  cp R24, R1
  brne start
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_pc_addr
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_pc_addr
{$endif CPUAVR_HAS_JMP_CALL}
  movw R20, R24
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_frame
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_frame
{$endif CPUAVR_HAS_JMP_CALL}
  movw R18, R24
  ldi R22, 200
  clr R23
  clr R24
  clr R25
{$ifdef CPUAVR_HAS_JMP_CALL}
  call HandleErrorAddrFrameInd
{$else  CPUAVR_HAS_JMP_CALL}
  rcall HandleErrorAddrFrameInd
{$endif CPUAVR_HAS_JMP_CALL}


start:
  clr R20         // clear remainder
  ldi R18, 8      // iterate over 8 bits

div1:
  lsl R22         // shift left A
  rol R20         // shift left P with carry from A shift
  sub R20, R24    // Subtract B from P, P <= P - B
  brlo div2
  ori R22, 1      // Set A[0] = 1
  rjmp div3
div2:             // negative branch, A[0] = 0 (default after shift), restore P
  add R20, R24    // restore old value of P

div3:
  dec R18
  brne div1

finish:
  mov R24, R22    // Move result from R22 to R24
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
{$ifdef FPC_IS_SYSTEM}
function fpc_div_byte(n, z: byte): byte; external name 'FPC_DIV_BYTE';
{$endif FPC_IS_SYSTEM}
{$endif FPC_SYSTEM_HAS_DIV_BYTE}

{$ifndef FPC_SYSTEM_HAS_DIV_WORD}
{$define FPC_SYSTEM_HAS_DIV_WORD}

// z in Ra, n in Rb, 0 in Rp
function fpc_div_word(n, z: word): word; assembler; nostackframe;
{$ifdef FPC_IS_SYSTEM}[public,alias: 'FPC_DIV_WORD'];{$endif}
label
  start, div1, div2, div3, finish;
asm
// Symbol  Name        Register(s)
// z (A)   dividend    R23, R22
// n (B)   divisor     R25, R24
// p (P)   remainder   R21, R20
// i	   counter     R18

  cp R24, R1
  brne start
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_pc_addr
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_pc_addr
{$endif CPUAVR_HAS_JMP_CALL}
  movw R20, R24
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_frame
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_frame
{$endif CPUAVR_HAS_JMP_CALL}
  movw R18, R24
  ldi R22, 200
  clr R23
  clr R24
  clr R25
{$ifdef CPUAVR_HAS_JMP_CALL}
  call HandleErrorAddrFrameInd
{$else  CPUAVR_HAS_JMP_CALL}
  rcall HandleErrorAddrFrameInd
{$endif CPUAVR_HAS_JMP_CALL}

start:            // Start of division...
  clr R20         // clear remainder low
  clr R21         // clear remainder hi
  ldi R18, 16     // iterate over 16 bits

div1:
  lsl R22         // shift left A_L
  rol R23
  rol R20         // shift left P with carry from A shift
  rol R21
  sub R20, R24    // Subtract B from P, P <= P - B
  sbc R21, R25
  brlo div2
  ori R22, 1      // Set A[0] = 1
  rjmp div3
div2:             // negative branch, A[0] = 0 (default after shift), restore P
  add R20, R24    // restore old value of P
  adc R21, R25

div3:
  dec R18
  brne div1

finish:
  movw R24, R22    // Move result from R22:R23 to R24:R25
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
{$ifdef FPC_IS_SYSTEM}
function fpc_div_word(n, z: word): word; external name 'FPC_DIV_WORD';
{$endif FPC_IS_SYSTEM}
{$endif FPC_SYSTEM_HAS_DIV_WORD}


{$ifndef FPC_SYSTEM_HAS_DIV_DWORD}
{$define FPC_SYSTEM_HAS_DIV_DWORD}

// z in Ra, n in Rb, 0 in Rp
function fpc_div_dword(n, z: dword): dword; assembler; nostackframe;
{$ifdef FPC_IS_SYSTEM}[public,alias: 'FPC_DIV_DWORD'];{$endif}
label
  start, div1, div2, div3, finish;
asm
// Symbol  Name        Register(s)
// z (A)   dividend    R21, R20, R19, R18
// n (B)   divisor     R25, R24, R23, R22
// p (P)   remainder   R17, R16, R15, R14
// i	   counter     R26

  cp R24, R1
  cpc R25, R1
  cpc R22, R1
  cpc R23, R1
  brne .LNonZero
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_pc_addr
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_pc_addr
{$endif CPUAVR_HAS_JMP_CALL}
  movw R20, R24
{$ifdef CPUAVR_HAS_JMP_CALL}
  call get_frame
{$else  CPUAVR_HAS_JMP_CALL}
  rcall get_frame
{$endif CPUAVR_HAS_JMP_CALL}
  movw R18, R24
  ldi R22, 200
  clr R23
  clr R24
  clr R25
{$ifdef CPUAVR_HAS_JMP_CALL}
  call HandleErrorAddrFrameInd
{$else  CPUAVR_HAS_JMP_CALL}
  rcall HandleErrorAddrFrameInd
{$endif CPUAVR_HAS_JMP_CALL}

.LNonZero:
  push R17
  push R16
  push R15
  push R14

start:            // Start of division...
  clr R14         // clear remainder
  clr R15         // clear remainder
  clr R16
  clr R17
  ldi R26, 32     // iterate over 32 bits

div1:
  lsl R18         // shift left A_L
  rol R19
  rol R20
  rol R21
  rol R14         // shift left P with carry from A shift
  rol R15
  rol R16
  rol R17
  sub R14, R22    // Subtract B from P, P <= P - B
  sbc R15, R23
  sbc R16, R24
  sbc R17, R25
  brlo div2
  ori R18, 1      // Set A[0] = 1
  rjmp div3
div2:             // negative branch, A[0] = 0 (default after shift), restore P
  add R14, R22    // restore old value of P
  adc R15, R23
  adc R16, R24
  adc R17, R25

div3:
  dec R26
  brne div1

finish:
  movw R22, R18    // Move result from R18:R21 to R22:R25
  movw R24, R20

  pop R14
  pop R15
  pop R16
  pop R17
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
{$ifdef FPC_IS_SYSTEM}
function fpc_div_dword(n, z: dword): dword; external name 'FPC_DIV_DWORD';
{$endif FPC_IS_SYSTEM}
{$endif FPC_SYSTEM_HAS_DIV_DWORD}

