{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  // This is a BSS variable, so it will be initialized to #0
  // This HAS to be larger than the amount if FPC_SETJMP call in the binary
  // To avoid excessive linear searches use at least Calls*1.5 rounded up to the next power of 2
  // If the size gets changed, the code below also needs adjustment
  setjmp_counter: array[0..$2000-1] of TSetJmpCounter;

// This function MUST return 0
function fpc_count_setjmp: LongInt; assembler; nostackframe;
asm
  // lr = Original Address
  // r0 = HashValue = (Address >> 2) and $1FFF
  // r1 = HashBase
  // r2 = CallerAddress in hashtable
  // r3 = Counter

  // Those two shifts are the "hashfunction"
  // The result can be used as an index into the hashtable
  // These required shift amount can be calculated like this
  // lshift = 32-clz($hashmask)-2
  // rshift = rshift - 1
  // This only works on ARM-Code where the lower two lr bits are always 0
  ldr r1, .LCounterBuffer
  mov r0, lr, lsl #17
  mov r0, r0, lsr #16

.LSlotLoop:
  // Did we wrap?
  tst   r0, #0x10000
  // If so, reset to #0
  movne r0, #0

  // Load Address and counter
  ldrd  r2, r3, [r1, r0]

  // If the Address is 0, create a new slot
  cmp   r2, #0
  beq   .LNewSlot

  // A once set Address is not going to change!
  cmp   r2, lr
  // Address did not match? Next!
  addne r0, r0, #8
  bne   .LSlotLoop

  // We'll not increment atomicaly here, because that has a lot of overhead
  // and very little gain, we might miss a small amount of calls, but thats not a big issue
  // Increment counter
  add r3, r3, #1
  // Adjust base to be on the counter
  add r0, r0, #4
  str r3, [r1, r0]
  mov r0, #0
  bx lr

.LNewSlot:
  stmfd sp!, {r0, r1, r2, lr}
  add   r0, r0, r1 // Address of the address ...
  mov   r1, lr     // New value
  mov   r2, #0     // OldValue
  blx   InterlockedCompareExchange
  ldmfd sp!, {r0, r1, r2, lr}
  b     .LSlotLoop

.LCounterBuffer:
  .long setjmp_counter
end;

procedure fpc_dump_setjmp;
var cnt: LongInt;
begin
  for cnt:=Low(setjmp_counter) to High(setjmp_counter) do
    begin
      if setjmp_counter[cnt].counter > 0 then
        begin
          writeln('Address: [$',hexstr(setjmp_counter[cnt].Address - 4,8), '] Count:',setjmp_counter[cnt].Counter);
        end;
    end;
end;

function fpc_setjmp_table_entry(idx: LongWord): PSetJmpCounter;
begin
  fpc_setjmp_table_entry:=nil;
  if (idx < $2000) then fpc_setjmp_table_entry:=@setjmp_counter[idx];
end;

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];nostackframe; compilerproc;
  asm
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    {$if defined(CPUARMV3) or defined(CPUARMV4) or defined(CPUARMV5)}
    fstmiax r0!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // fstmiad increases the address register always by 2n+1 words, so fix this
    sub r0,r0,#4
    {$else}
    fstmiad r0!, {d8-d15}
    {$endif}
    {$endif}

    {$if defined(CPUTHUMB2)}
    stmia	r0!, {v1-v6, sl, fp}
    mov	        r2, sp
    stmia	r0!, {r2, lr}

    mov     r0,#0
    mov     pc,lr
    {$elseif defined(CPUTHUMB)}
    stmia	r0!,{v1-v4}
    mov         r1,v5
    str         r1,[r0]
    mov         r1,v6
    str         r1,[r0,#4]
    mov         r1,sl
    str         r1,[r0,#8]
    mov         r1,fp
    str         r1,[r0,#12]
    mov         r1,sp
    str         r1,[r0,#16]
    mov         r1,lr
    str         r1,[r0,#20]
    mov         r0,#0
    bx          lr
    {$else}
    stmia   r0,{v1-v6, sl, fp, sp, lr}
    b       fpc_count_setjmp
    {$endif}
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc;
  asm
    {$if defined(CPUTHUMB2)}
    mov     ip, r0
    movs    r0, r1
    it eq
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    fldmiad ip!, {d8-d15}
    {$endif}
    ldmia   ip!, {v1-v6, sl, fp}
    ldr     sp, [ip]
    add     ip, ip, #4
    ldr     pc, [ip]

    {$elseif defined(CPUTHUMB)}
    mov     r3, r0
    movs    r0, r1
    bne     .L1
    mov     r0, #1
.L1:
    ldmia   r3!,{v1-v4}
    ldr     r2,[r3]
    mov     v5,r2
    ldr     r2,[r3,#4]
    mov     v6,r2
    ldr     r2,[r3,#8]
    mov     sl,r2
    ldr     r2,[r3,#12]
    mov     fp,r2
    ldr     r2,[r3,#16]
    mov     sp,r2
    ldr     r2,[r3,#20]
    mov     pc,r2

    {$else}
    mov     ip, r0
    movs    r0, r1
    moveq   r0, #1
    {$if defined(FPUVFPV2) or defined(FPUVFPV3) or defined(FPUVFPV3_D16)}
    {$if defined(CPUARMV3) or defined(CPUARMV4) or defined(CPUARMV5)}
    fldmiax ip!, {d8-d15}
    // according to the ARM Developer Suite Assembler Guide Version 1.2
    // increases fldmiax the address register always by 2n+1 words, so fix this
    sub ip,ip,#4
    {$else}
    fldmiad ip!, {d8-d15}
    {$endif}
    {$endif}
    ldmia   ip,{v1-v6, sl, fp, sp, pc}
    {$endif}
  end;


