{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    ARM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_FPC_MOVE}
{$endif FPC_SYSTEM_HAS_MOVE}

{$ifdef FPC_SYSTEM_FPC_MOVE}
const
  cpu_has_edsp : boolean = false;
  in_edsp_test : boolean = false;
{$endif FPC_SYSTEM_FPC_MOVE}

{$if not(defined(wince)) and not(defined(gba)) and not(defined(nds)) and not(defined(FPUSOFT)) and not(defined(FPULIBGCC))}

{$define FPC_SYSTEM_HAS_SYSINITFPU}
{$if not defined(darwin) and not defined(FPUVFPV2) and not defined(FPUVFPV3) and not defined(FPUVFPV3_D16)}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { Enable FPU exceptions, but disable INEXACT, UNDERFLOW, DENORMAL }
  asm
    rfs r0
    and r0,r0,#0xffe0ffff
    orr r0,r0,#0x00070000
    wfs r0
  end;
end;
{$else}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { Enable FPU exceptions, but disable INEXACT, UNDERFLOW, DENORMAL }
  asm
    fmrx r0,fpscr
    // set "round to nearest" mode
    and  r0,r0,#0xff3fffff
    // mask "exception happened" and overflow flags
    and  r0,r0,#0xffffff20
    // mask exception flags
    and  r0,r0,#0xffff40ff    
{$ifndef darwin}
    // Floating point exceptions cause kernel panics on iPhoneOS 2.2.1...

    // disable flush-to-zero mode (IEEE math compliant)
    and  r0,r0,#0xfeffffff
    // enable invalid operation, div-by-zero and overflow exceptions
    orr  r0,r0,#0x00000700
{$endif}
    fmxr fpscr,r0
  end;
end;
{$endif}
{$endif}

procedure fpc_cpuinit;
begin
  { don't let libraries influence the FPU cw set by the host program }
  if not IsLibrary then
    SysInitFPU;
end;

{$ifdef wince}
function _controlfp(new: DWORD; mask: DWORD): DWORD; cdecl; external 'coredll';

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_flags:=0;
end;

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
  { Enable FPU exceptions, but disable INEXACT, UNDERFLOW, DENORMAL }
  { FPU precision 64 bit, rounding to nearest, affine infinity }
  _controlfp($000C0003, $030F031F);
end;
{$endif wince}

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
asm
  mov    r0,r11
end;
{$ENDIF not INTERNAL_BACKTRACE}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;nostackframe;
asm
  cmp r0,#0
  ldrne r0,[r0,#-4]
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;nostackframe;
asm
  cmp r0,#0
  ldrne r0,[r0,#-12]
end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
asm
  mov    r0,sp
end;


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);assembler;nostackframe;
asm
        // less than 0?
        cmp r1,#0
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
        movlt pc,lr
{$else}
        bxlt  lr
{$endif}
        mov     r3,r0
        cmp     r1,#8           // at least 8 bytes to do?
        blt     .LFillchar2
        orr r2,r2,r2,lsl #8
        orr r2,r2,r2,lsl #16
.LFillchar0:
        tst     r3,#3           // aligned yet?
        strneb r2,[r3],#1
        subne   r1,r1,#1
        bne     .LFillchar0
        mov     ip,r2
.LFillchar1:
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        stmgeia r3!,{r2,ip}
        subge   r1,r1,#8
        bge     .LFillchar1
.LFillchar2:
        movs r1,r1              // anything left?
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
        moveq   pc,lr
{$else}
        bxeq    lr
{$endif}
        rsb     r1,r1,#7
        add     pc,pc,r1,lsl #2
        mov     r0,r0
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
        mov pc,lr
{$else}
        bx  lr
{$endif}
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move_pld(const source;var dest;count:longint);assembler;nostackframe;
asm
  pld [r0]
  // count <=0 ?
  cmp r2,#0
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  movle pc,lr
{$else}
  bxle  lr
{$endif}
  // overlap?
  cmp r1,r0
  bls .Lnooverlap
  add r3,r0,r2
  cmp r3,r1
  bls .Lnooverlap
  // overlap, copy backward
.Loverlapped:
  subs r2,r2,#1
  ldrb r3,[r0,r2]
  strb r3,[r1,r2]
  bne .Loverlapped
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  mov pc,lr
{$else}
  bx  lr
{$endif}
.Lnooverlap:
  // less then 16 bytes to copy?
  cmp r2,#8
  // yes, the forget about the whole optimizations
  // and do a bytewise copy
  blt .Lbyteloop

  // both aligned?
  orr r3,r0,r1
  tst r3,#3

  bne .Lbyteloop
(*
  // yes, then align
  // alignment to 4 byte boundries is enough
  ldrb ip,[r0],#1
  sub r2,r2,#1
  stb ip,[r1],#1
  tst r3,#2
  bne .Ldifferentaligned
  ldrh ip,[r0],#2
  sub r2,r2,#2
  sth ip,[r1],#2

.Ldifferentaligned
  // qword aligned?
  orrs r3,r0,r1
  tst r3,#7
  bne .Ldwordloop
*)
  pld [r0,#32]
.Ldwordloop:
  sub r2,r2,#4
  ldr r3,[r0],#4
  // preload
  pld [r0,#64]
  cmp r2,#4
  str r3,[r1],#4
  bcs .Ldwordloop
  cmp r2,#0
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  moveq pc,lr
{$else}
  bxeq  lr
{$endif}
.Lbyteloop:
  subs r2,r2,#1
  ldrb r3,[r0],#1
  strb r3,[r1],#1
  bne .Lbyteloop
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  mov pc,lr
{$else}
  bx  lr
{$endif}
end;

procedure Move_blended(const source;var dest;count:longint);assembler;nostackframe;
asm
  // count <=0 ?
  cmp r2,#0
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  movle pc,lr
{$else}
  bxle  lr
{$endif}
  // overlap?
  cmp r1,r0
  bls .Lnooverlap
  add r3,r0,r2
  cmp r3,r1
  bls .Lnooverlap
  // overlap, copy backward
.Loverlapped:
  subs r2,r2,#1
  ldrb r3,[r0,r2]
  strb r3,[r1,r2]
  bne .Loverlapped
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  mov pc,lr
{$else}
  bx  lr
{$endif}
.Lnooverlap:
  // less then 16 bytes to copy?
  cmp r2,#8
  // yes, the forget about the whole optimizations
  // and do a bytewise copy
  blt .Lbyteloop

  // both aligned?
  orr r3,r0,r1
  tst r3,#3

  bne .Lbyteloop
(*
  // yes, then align
  // alignment to 4 byte boundries is enough
  ldrb ip,[r0],#1
  sub r2,r2,#1
  stb ip,[r1],#1
  tst r3,#2
  bne .Ldifferentaligned
  ldrh ip,[r0],#2
  sub r2,r2,#2
  sth ip,[r1],#2

.Ldifferentaligned
  // qword aligned?
  orrs r3,r0,r1
  tst r3,#7
  bne .Ldwordloop
*)
.Ldwordloop:
  sub r2,r2,#4
  ldr r3,[r0],#4
  cmp r2,#4
  str r3,[r1],#4
  bcs .Ldwordloop
  cmp r2,#0
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  moveq pc,lr
{$else}
  bxeq  lr
{$endif}
.Lbyteloop:
  subs r2,r2,#1
  ldrb r3,[r0],#1
  strb r3,[r1],#1
  bne .Lbyteloop
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
  mov pc,lr
{$else}
  bx  lr
{$endif}
end;


const
  moveproc : pointer = @move_blended;

procedure Move(const source;var dest;count:longint);[public, alias: 'FPC_MOVE'];assembler;nostackframe;
asm
  ldr ip,.Lmoveproc
  ldr pc,[ip]
.Lmoveproc:
  .long moveproc
end;

{$endif FPC_SYSTEM_HAS_MOVE}

{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_shortstr_to_shortstr(len:longint;const sstr:shortstring):shortstring;assembler;nostackframe;[public,alias: 'FPC_SHORTSTR_TO_SHORTSTR'];compilerproc;
{$else}
procedure fpc_shortstr_to_shortstr(out res:shortstring;const sstr:shortstring);assembler;nostackframe;[public,alias: 'FPC_SHORTSTR_TO_SHORTSTR'];compilerproc;
{$endif}
{r0: __RESULT
 r1: len
 r2: sstr}

asm
    ldrb r12,[r2],#1
    cmp  r12,r1
    movgt r12,r1
    strb r12,[r0],#1
    cmp  r12,#6 (* 6 seems to be the break even point. *)
    blt  .LStartTailCopy
    (* Align destination on 32bits. This is the only place where unrolling
       really seems to help, since in the common case, sstr is aligned on
       32 bits, therefore in the common case we need to copy 3 bytes to
       align, i.e. in the case of a loop, you wouldn't branch out early.*)
    rsb  r3,r0,#0
    ands  r3,r3,#3
    sub  r12,r12,r3
    ldrneb r1,[r2],#1
    strneb r1,[r0],#1
    subnes  r3,r3,#1
    ldrneb r1,[r2],#1
    strneb r1,[r0],#1
    subnes  r3,r3,#1
    ldrneb r1,[r2],#1
    strneb r1,[r0],#1
    subnes  r3,r3,#1
.LDoneAlign:
    (* Destination should be aligned now, but source might not be aligned,
       if this is the case, do a byte-per-byte copy. *)
    tst r2,#3
    bne .LStartTailCopy
    (* Start the main copy, 32 bit at a time. *)
    movs r3,r12,lsr #2
    and r12,r12,#3
    beq  .LStartTailCopy
.LNext4bytes:
    (* Unrolling this loop would save a little bit of time for long strings
       (>20 chars), but alas, it hurts for short strings and they are the
       common case.*)
    ldrne r1,[r2],#4
    strne r1,[r0],#4
    subnes  r3,r3,#1
    bne .LNext4bytes
.LStartTailCopy:
    (* Do remaining bytes. *)
    cmp r12,#0
    beq .LDoneTail
.LNextChar3:
    ldrb r1,[r2],#1
    strb r1,[r0],#1
    subs  r12,r12,#1
    bne .LNextChar3
.LDoneTail:
end;

procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);assembler;nostackframe;[public,alias:'FPC_SHORTSTR_ASSIGN'];compilerproc;

{r0: len
 r1: sstr
 r2: dstr}

asm
    ldrb r12,[r1],#1
    cmp  r12,r0
    movgt r12,r0
    strb r12,[r2],#1
    cmp  r12,#6 (* 6 seems to be the break even point. *)
    blt  .LStartTailCopy
    (* Align destination on 32bits. This is the only place where unrolling
       really seems to help, since in the common case, sstr is aligned on
       32 bits, therefore in the common case we need to copy 3 bytes to
       align, i.e. in the case of a loop, you wouldn't branch out early.*)
    rsb  r3,r2,#0
    ands  r3,r3,#3
    sub  r12,r12,r3
    ldrneb r0,[r1],#1
    strneb r0,[r2],#1
    subnes  r3,r3,#1
    ldrneb r0,[r1],#1
    strneb r0,[r2],#1
    subnes  r3,r3,#1
    ldrneb r0,[r1],#1
    strneb r0,[r2],#1
    subnes  r3,r3,#1
.LDoneAlign:
    (* Destination should be aligned now, but source might not be aligned,
       if this is the case, do a byte-per-byte copy. *)
    tst r1,#3
    bne .LStartTailCopy
    (* Start the main copy, 32 bit at a time. *)
    movs r3,r12,lsr #2
    and r12,r12,#3
    beq  .LStartTailCopy
.LNext4bytes:
    (* Unrolling this loop would save a little bit of time for long strings
       (>20 chars), but alas, it hurts for short strings and they are the
       common case.*)
    ldrne r0,[r1],#4
    strne r0,[r2],#4
    subnes  r3,r3,#1
    bne .LNext4bytes
.LStartTailCopy:
    (* Do remaining bytes. *)
    cmp r12,#0
    beq .LDoneTail
.LNextChar3:
    ldrb r0,[r1],#1
    strb r0,[r2],#1
    subs  r12,r12,#1
    bne .LNextChar3
.LDoneTail:
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_Pchar_length(p:Pchar):sizeint;assembler;nostackframe;[public,alias:'FPC_PCHAR_LENGTH'];compilerproc;

asm
    cmp r0,#0
    mov r1,r0
    beq .Ldone
.Lnextchar:
    (*Are we aligned?*)
    tst r1,#3
    bne .Ltest_unaligned    (*No, do byte per byte.*)
    ldr r3,.L01010101
.Ltest_aligned:
    (*Aligned, load 4 bytes at a time.*)
    ldr r12,[r1],#4
    (*Check wether r12 contains a 0 byte.*)
    sub r2,r12,r3
    mvn r12,r12
    and r2,r2,r12
    ands r2,r2,r3,lsl #7    (*r3 lsl 7 = $80808080*)
    beq .Ltest_aligned      (*No 0 byte, repeat.*)
    sub r1,r1,#4
.Ltest_unaligned:
    ldrb r12,[r1],#1
    cmp r12,#1              (*r12<1 same as r12=0, but result in carry flag*)
    bcs .Lnextchar
    (*Dirty trick: we need to subtract 1 extra because we have counted the
      terminating 0, due to the known carry flag sbc can do this.*)
    sbc r0,r1,r0
.Ldone:
{$if defined(cpuarmv3) or defined(cpuarmv4) or defined(cpuarmv5)}
    mov pc,lr
{$else}
    bx  lr
{$endif}
.L01010101:
    .long 0x01010101
end;
{$endif}


var
  fpc_system_lock: longint; export name 'fpc_system_lock';

function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
asm
{$if defined(cpuarmv6) or defined(cpuarmv7m) or defined(cpucortexm3)}
.Lloop:
  ldrex r1, [r0]
  sub   r1, r1, #1
  strex r2, r1, [r0]
  cmp r2, #0
  bne .Lloop
  mov r0, r1
  bx  lr
{$else}
{$if defined(LINUX) and defined(CPUARMEL)}

  stmfd r13!, {lr}
  mov r2, r0   // kuser_cmpxchg does not clobber r2 by definition
.Latomic_dec_loop:
  ldr r0, [r2]   // Load the current value

  // We expect this to work without looping most of the time
  // R3 gets clobbered in kuser_cmpxchg so in the unlikely case that we have to
  // loop here again, we have to reload the value. Normaly this just fills the
  // load stall-cycles from the above ldr so in reality we'll not get any additional
  // delays because of this
  // Don't use ldr to load r3 to avoid cacheline trashing
  // Load 0xffff0fff into r3 and substract to 0xffff0fc0,
  // the kuser_cmpxchg entry point
  mvn r3, #0x0000f000
  sub r3, r3, #0x3F

  sub r1, r0, #1 // Decrement value
  blx r3	 // Call kuser_cmpxchg, sets C-Flag on success

  movcs r0, r1	 // We expect that to work most of the time so keep it pipeline friendly
  ldmcsfd r13!, {pc}
  b .Latomic_dec_loop // kuser_cmpxchg sets C flag on error

{$else}
// lock
  ldr r3, .Lfpc_system_lock
  mov r1, #1
.Lloop:
  swp r2, r1, [r3]
  cmp r2, #0
  bne .Lloop
// do the job
  ldr r1, [r0]
  sub r1, r1, #1
  str r1, [r0]
  mov r0, r1
// unlock and return
  str r2, [r3]
  bx  lr

.Lfpc_system_lock:
  .long fpc_system_lock
{$endif}
{$endif}
end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
asm
{$if defined(cpuarmv6) or defined(cpuarmv7m) or defined(cpucortexm3)}
.Lloop:
  ldrex r1, [r0]
  add   r1, r1, #1
  strex r2, r1, [r0]
  cmp r2, #0
  bne .Lloop
  mov r0, r1
  bx  lr
{$else}
{$if defined(LINUX) and defined(CPUARMEL)}

  stmfd r13!, {lr}
  mov r2, r0   // kuser_cmpxchg does not clobber r2 by definition
.Latomic_inc_loop:
  ldr r0, [r2]   // Load the current value

  // We expect this to work without looping most of the time
  // R3 gets clobbered in kuser_cmpxchg so in the unlikely case that we have to
  // loop here again, we have to reload the value. Normaly this just fills the
  // load stall-cycles from the above ldr so in reality we'll not get any additional
  // delays because of this
  // Don't use ldr to load r3 to avoid cacheline trashing
  // Load 0xffff0fff into r3 and substract to 0xffff0fc0,
  // the kuser_cmpxchg entry point
  mvn r3, #0x0000f000
  sub r3, r3, #0x3F

  add r1, r0, #1 // Increment value
  blx r3	 // Call kuser_cmpxchg, sets C-Flag on success

  movcs r0, r1	 // We expect that to work most of the time so keep it pipeline friendly
  ldmcsfd r13!, {pc}
  b .Latomic_inc_loop // kuser_cmpxchg sets C flag on error

{$else}
// lock
  ldr r3, .Lfpc_system_lock
  mov r1, #1
.Lloop:
  swp r2, r1, [r3]
  cmp r2, #0
  bne .Lloop
// do the job
  ldr r1, [r0]
  add r1, r1, #1
  str r1, [r0]
  mov r0, r1
// unlock and return
  str r2, [r3]
  bx  lr

.Lfpc_system_lock:
  .long fpc_system_lock
{$endif}
{$endif}
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
{$if defined(cpuarmv6) or defined(cpuarmv7m) or defined(cpucortexm3)}
// swp is deprecated on ARMv6 and above
.Lloop:
  ldrex r2, [r0]
  strex r3, r1, [r0]
  cmp r3, #0
  bne .Lloop
  mov r0, r2
  bx  lr
{$else}
  swp r1, r1, [r0]
  mov r0,r1
{$endif}
end;

function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
{$if defined(cpuarmv6) or defined(cpuarmv7m) or defined(cpucortexm3)}
.Lloop:
  ldrex r2, [r0]
  add   r12, r1, r2
  strex r3, r12, [r0]
  cmp r3, #0
  bne .Lloop
  mov  r0, r2
  bx  lr
{$else}
{$if defined(LINUX) and defined(CPUARMEL)}

  stmfd r13!, {r4, lr}
  mov r2, r0   // kuser_cmpxchg does not clobber r2 by definition
  mov r4, r1   // Save addend
.Latomic_add_loop:
  ldr r0, [r2]   // Load the current value

  // We expect this to work without looping most of the time
  // R3 gets clobbered in kuser_cmpxchg so in the unlikely case that we have to
  // loop here again, we have to reload the value. Normaly this just fills the
  // load stall-cycles from the above ldr so in reality we'll not get any additional
  // delays because of this
  // Don't use ldr to load r3 to avoid cacheline trashing
  // Load 0xffff0fff into r3 and substract to 0xffff0fc0,
  // the kuser_cmpxchg entry point
  mvn r3, #0x0000f000
  sub r3, r3, #0x3F

  add r1, r0, r4 // Add to value
  blx r3	 // Call kuser_cmpxchg, sets C-Flag on success
  // r1 does not get clobbered, so just get back the original value
  // Otherwise we would have to allocate one more register and store the
  // temporary value
  subcs   r0, r1, r4
  ldmcsfd r13!, {r4, pc}

  b .Latomic_add_loop // kuser_cmpxchg failed, loop back

{$else}
// lock
  ldr r3, .Lfpc_system_lock
  mov r2, #1
.Lloop:
  swp r2, r2, [r3]
  cmp r2, #0
  bne .Lloop
// do the job
  ldr r2, [r0]
  add r1, r1, r2
  str r1, [r0]
  mov r0, r2
// unlock and return
  mov r2, #0
  str r2, [r3]
  bx  lr

.Lfpc_system_lock:
  .long fpc_system_lock
{$endif}
{$endif}
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
asm
{$if defined(cpuarmv6) or defined(cpuarmv7m) or defined(cpucortexm3)}
.Lloop:
  ldrex    r3, [r0]
  mov      r12, #0
  cmp      r3, r2
  strexeq  r12, r1, [r0]
  cmp      r12, #0
  bne      .Lloop
  mov      r0, r3
  bx       lr
{$else}
{$if defined(LINUX) and defined(CPUARMEL)}

  stmfd r13!, {r4, lr}
  mvn   r3, #0x0000f000
  sub   r3, r3, #0x3F

  mov   r4, r2 // Swap parameters around
  mov   r2, r0
  mov   r0, r4 // Use r4 because we'll need the new value for later

  // r1 and r2 will not be clobbered by kuser_cmpxchg
  // If we have to loop, r0 will be set to the original Comperand
  .Linterlocked_compare_exchange_loop:

  blx   r3       // Call kuser_cmpxchg sets C-Flag on success
  movcs r0, r4   // Return the previous value on success
  ldmcsfd r13!, {r4, pc}
  // The error case is a bit tricky, kuser_cmpxchg does not return the current value
  // So we may need to loop to avoid race conditions
  // The loop case is HIGHLY unlikely, it would require that we got rescheduled between
  // calling kuser_cmpxchg and the ldr. While beeing rescheduled another process/thread
  // would have the set the value to our comperand
  ldr	r0, [r2] // Load the currently set value
  cmp   r0, r4   // Return if Comperand != current value, otherwise loop again
  ldmnefd r13!, {r4, pc}
  // If we need to loop here, we have to
  b	.Linterlocked_compare_exchange_loop

{$else}
// lock
  ldr r12, .Lfpc_system_lock
  mov r3, #1
.Lloop:
  swp r3, r3, [r12]
  cmp r3, #0
  bne .Lloop
// do the job
  ldr r3, [r0]
  cmp r3, r2
  streq r1, [r0]
  mov r0, r3
// unlock and return
  mov r3, #0
  str r3, [r12]
  bx  lr

.Lfpc_system_lock:
  .long fpc_system_lock
{$endif}
{$endif}
end;

{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l: longint) : boolean; inline;
begin
  Result:=InterLockedDecrement(l) = 0;
end;

{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l: longint); inline;
begin
  InterLockedIncrement(l);
end;

procedure fpc_cpucodeinit;
begin
{$ifdef FPC_SYSTEM_FPC_MOVE}
  cpu_has_edsp:=true;
  in_edsp_test:=true;
  asm
    bic r0,sp,#7
    ldrd r0,[r0]
  end;
  in_edsp_test:=false;
  if cpu_has_edsp then
    moveproc:=@move_pld
  else
    moveproc:=@move_blended;
{$endif FPC_SYSTEM_FPC_MOVE}
end;

{include hand-optimized assembler division code}
{$i divide.inc}

