{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000-2001 by the Free Pascal development team.

    Portions Copyright (c) 2000 by Casey Duncan (casey.duncan@state.co.us)

    Processor dependent implementation for the system unit for
    PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                           PowerPC specific stuff
****************************************************************************}

{ This function is never called directly, it's a dummy to hold the register save/
  load subroutines
}
procedure saverestorereg;assembler;
asm
{ exit }
.global _restfpr_14_x
_restfpr_14_x:  lfd     f14, -144(r11)
.global _restfpr_15_x
_restfpr_15_x:  lfd     f15, -136(r11)
.global _restfpr_16_x
_restfpr_16_x:  lfd     f16, -128(r11)
.global _restfpr_17_x
_restfpr_17_x:  lfd     f17, -120(r11)
.global _restfpr_18_x
_restfpr_18_x:  lfd     f18, -112(r11)
.global _restfpr_19_x
_restfpr_19_x:  lfd     f19, -104(r11)
.global _restfpr_20_x
_restfpr_20_x:  lfd     f20, -96(r11)
.global _restfpr_21_x
_restfpr_21_x:  lfd     f21, -88(r11)
.global _restfpr_22_x
_restfpr_22_x:  lfd     f22, -80(r11)
.global _restfpr_23_x
_restfpr_23_x:  lfd     f23, -72(r11)
.global _restfpr_24_x
_restfpr_24_x:  lfd     f24, -64(r11)
.global _restfpr_25_x
_restfpr_25_x:  lfd     f25, -56(r11)
.global _restfpr_26_x
_restfpr_26_x:  lfd     f26, -48(r11)
.global _restfpr_27_x
_restfpr_27_x:  lfd     f27, -40(r11)
.global _restfpr_28_x
_restfpr_28_x:  lfd     f28, -32(r11)
.global _restfpr_29_x
_restfpr_29_x:  lfd     f29, -24(r11)
.global _restfpr_30_x
_restfpr_30_x:  lfd     f30, -16(r11)
.global _restfpr_31_x
_restfpr_31_x:  lwz     r0, 4(r11)
                lfd     f31, -8(r11)
                mtlr    r0
                ori     r1, r11, 0
                blr

{ exit with restoring lr }
.global _restfpr_14_l
_restfpr_14_l:  lfd     f14, -144(r11)
.global _restfpr_15_l
_restfpr_15_l:  lfd     f15, -136(r11)
.global _restfpr_16_l
_restfpr_16_l:  lfd     f16, -128(r11)
.global _restfpr_17_l
_restfpr_17_l:  lfd     f17, -120(r11)
.global _restfpr_18_l
_restfpr_18_l:  lfd     f18, -112(r11)
.global _restfpr_19_l
_restfpr_19_l:  lfd     f19, -104(r11)
.global _restfpr_20_l
_restfpr_20_l:  lfd     f20, -96(r11)
.global _restfpr_21_l
_restfpr_21_l:  lfd     f21, -88(r11)
.global _restfpr_22_l
_restfpr_22_l:  lfd     f22, -80(r11)
.global _restfpr_23_l
_restfpr_23_l:  lfd     f23, -72(r11)
.global _restfpr_24_l
_restfpr_24_l:  lfd     f24, -64(r11)
.global _restfpr_25_l
_restfpr_25_l:  lfd     f25, -56(r11)
.global _restfpr_26_l
_restfpr_26_l:  lfd     f26, -48(r11)
.global _restfpr_27_l
_restfpr_27_l:  lfd     f27, -40(r11)
.global _restfpr_28_l
_restfpr_28_l:  lfd     f28, -32(r11)
.global _restfpr_29_l
_restfpr_29_l:  lfd     f29, -24(r11)
.global _restfpr_30_l
_restfpr_30_l:  lfd     f30, -16(r11)
.global _restfpr_31_l
_restfpr_31_l:  lwz     r0, 4(r11)
                lfd     f31, -8(r11)
                mtlr    r0
                ori     r1, r11, 0
                blr
end;


{****************************************************************************
                                Move / Fill
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(const source;var dest;count:longint);assembler;
asm
          {  count <= 0 ?  }
          cmpwi   cr0,r5,0
          {  check if we have to do the move backwards because of overlap  }
          sub     r10,r4,r3
          {  carry := boolean(dest-source < count) = boolean(overlap) }
          subc    r10,r10,r5

          {  count < 15 ? (to decide whether we will move dwords or bytes  }
          cmpwi   cr1,r5,15

          {  if overlap, then r10 := -1 else r10 := 0  }
          subfe   r10,r10,r10

          {  count < 39 ? (32 + max. alignment (7) }
          cmpwi   cr7,r5,39

          {  if count <= 0, stop  }
          ble     cr0,LMoveDone

          {  load the begin of the source in the data cache }
          dcbt    0,r3
          { and the dest as well }
          dcbst   0,r4

          {  if overlap, then r0 := count else r0 := 0  }
          and     r0,r5,r10
          {  if overlap, then point source and dest to the end  }
          add     r3,r3,r0
          add     r4,r4,r0
          {  if overlap, then r0 := 0, else r0 := -1  }
          not     r0,r10
          {  if overlap, then r10 := -2, else r10 := 0  }
          slwi    r10,r10,1
          {  if overlap, then r10 := -1, else r10 := 1  }
          addi    r10,r10,1
          {  if overlap, then source/dest += -1, otherwise they stay }
          {  After the next instruction, r3/r4 + r10 = next position }
          {  to load/store from/to                                   }
          add     r3,r3,r0
          add     r4,r4,r0

          {  if count < 15, copy everything byte by byte  }
          blt     cr1,LMoveBytes

          {  otherwise, guarantee 4 byte alignment for dest for starters  }
LMove4ByteAlignLoop:
          lbzux   r0,r3,r10
          stbux   r0,r4,r10
          {  is dest now 4 aligned?  }
          andi.   r0,r4,3
          subi    r5,r5,1
          {  while not aligned, continue  }
          bne     cr0,LMove4ByteAlignLoop

          { check for 8 byte alignment }
          andi.   r0,r4,7
          { we are going to copy one byte again (the one at the newly }
          { aligned address), so increase count byte 1                }
          addi    r5,r5,1
          { count div 4 for number of dwords to copy }
          srwi    r0,r5,2
          {  if 11 <= count < 39, copy using dwords }
          blt     cr7,LMoveDWords

          { multiply the update count with 4 }
          slwi    r10,r10,2

          beq     cr0,L8BytesAligned

          {  count >= 39 -> align to 8 byte boundary and then use the FPU  }
          {  since we're already at 4 byte alignment, use dword store      }
          lwzux   r0,r3,r10
          stwux   r0,r4,r10
          subi    r5,r5,4
L8BytesAligned:
          { count div 32 ( >= 1, since count was >=39 }
          srwi    r0,r5,5
          { remainder }
          andi.   r5,r5,31
          { to decide if we will do some dword stores (instead of only }
          { byte stores) afterwards or not                             }
          cmpwi   cr1,r5,11
          mtctr   r0

          {  r0 := count div 4, will be moved to ctr when copying dwords  }
          srwi    r0,r5,2

          {  adjust the update count: it will now be 8 or -8 depending on overlap  }
          slwi    r10,r10,1

          {  adjust source and dest pointers: because of the above loop, dest is now   }
          {  aligned to 8 bytes. So if we substract r10 we will still have an 8 bytes  }
          { aligned address)                                                           }
          sub     r3,r3,r10
          sub     r4,r4,r10

LMove32ByteLoop:
          lfdux   f0,r3,r10
          lfdux   f1,r3,r10
          lfdux   f2,r3,r10
          lfdux   f3,r3,r10
          stfdux  f0,r4,r10
          stfdux  f1,r4,r10
          stfdux  f2,r4,r10
          stfdux  f3,r4,r10
          bdnz    LMove32ByteLoop

          { cr0*4+eq is true if "count and 31" = 0 }
          beq     cr0,LMoveDone

          {  make r10 again -1 or 1, but first adjust source/dest pointers }
          add     r3,r3,r10
          add     r4,r4,r10
          srawi   r10,r10,3
          sub     r3,r3,r10
          sub     r4,r4,r10

          { cr1 contains whether count <= 11 }
          ble     cr1,LMoveBytes
          add     r3,r3,r10
          add     r4,r4,r10

LMoveDWords:
          mtctr   r0
          andi.   r5,r5,3
          {  r10 * 4  }
          slwi    r10,r10,2
          sub     r3,r3,r10
          sub     r4,r4,r10

LMoveDWordsLoop:
          lwzux   r0,r3,r10
          stwux   r0,r4,r10
          bdnz    LMoveDWordsLoop

          beq     cr0,LMoveDone
          {  make r10 again -1 or 1  }
          add     r3,r3,r10
          add     r4,r4,r10
          srawi   r10,r10,2
          sub     r3,r3,r10
          sub     r4,r4,r10
LMoveBytes:
          mtctr   r5
LMoveBytesLoop:
          lbzux   r0,r3,r10
          stbux   r0,r4,r10
          bdnz    LMoveBytesLoop
LMoveDone:
end ['R0','R3','R4','R5','R10','F0','F11','F12','F13','CTR','CR0','CR1','CR7'];


{$define FPC_SYSTEM_HAS_FILLCHAR}

Procedure FillChar(var x;count:longint;value:byte);assembler;
{ input: x in r3, count in r4, value in r5 }

{$ifndef ABI_AIX}
{ in the AIX ABI, we can use te red zone for temp storage, otherwise we have }
{ to explicitely allocate room                                               }
var
  temp : packed record
    case byte of
      0: (l1,l2: longint);
      1: (d: double);
    end;
{$endif ABI_AIX}
asm
        { no bytes? }
        cmpwi     cr6,r4,0
        { less than 15 bytes? }
        cmpwi     cr7,r4,15
        { less than 63 bytes? }
        cmpwi     cr1,r4,63
        { fill r5 with ValueValueValueValue }
        rlwimi    r5,r5,8,16,23
        { setup for aligning x to multiple of 4}
        rlwinm    r10,r3,0,31-2+1,31
        rlwimi    r5,r5,16,0,15
        beq       cr6,LFillCharDone
        { get the start of the data in the cache (and mark it as "will be }
        { modified")                                                      }
        dcbst     0,r3
        subfic    r10,r10,4
        blt       cr7,LFillCharVerySmall
        { just store 4 bytes instead of using a loop to align (there are }
        { plenty of other instructions now to keep the processor busy    }
        { while it handles the (possibly unaligned) store)               }
        stw       r5,0(r3)
        { r3 := align(r3,4) }
        add       r3,r3,r10
        { decrease count with number of bytes already stored }
        sub       r4,r4,r10
        blt       cr1,LFillCharSmall
        { if we have to fill with 0 (which happens a lot), we can simply use }
        { dcbz for the most part, which is very fast, so make a special case }
        { for that                                                           }
        cmplwi    cr1,r5,0
        { align to a multiple of 32 (and immediately check whether we aren't }
        { already 32 byte aligned)                                           }
        rlwinm.   r10,r3,0,31-5+1,31
        { setup r3 for using update forms of store instructions }
        subi      r3,r3,4
        { get number of bytes to store }
        subfic    r10,r10,32
        { if already 32byte aligned, skip align loop }
        beq       L32ByteAlignLoopDone
        { substract from the total count }
        sub       r4,r4,r10
L32ByteAlignLoop:
        { we were already aligned to 4 byres, so this will count down to }
        { exactly 0                                                      }
        subic.    r10,r10,4
        stwu      r5,4(r3)
        bne       L32ByteAlignLoop
L32ByteAlignLoopDone:
        { get the amount of 32 byte blocks }
        srwi      r10,r4,5
        { and keep the rest in r4 (recording whether there is any rest) }
        rlwinm.   r4,r4,0,31-5+2,31
        { move to ctr }
        mtctr     r10
        { check how many rest there is (to decide whether we'll use }
        { FillCharSmall or FillCharVerySmall)                       }
        cmpl      cr7,r4,11
        { if filling with zero, only use dcbz }
        bne       cr1, LFillCharNoZero
        { make r3 point again to the actual store position }
        addi      r3,r3,4
LFillCharDCBZLoop:
        dcbz      0,r3
        addi      r3,r3,32
        bdnz      LFillCharDCBZLoop
        { if there was no rest, we're finished }
        beq       LFillCharDone
        b         LFillCharSmall
LFillCharNoZero:
{$ifdef ABI_AIX}
        stw       r5,0(sp)
        stw       r5,4(sp)
        lfd       f0,0(sp)
{$else ABI_AIX}
        stw       r5,temp
        stw       r5,4+temp
        lfd       f0,temp
{$endif ABI_AIX}
        { make r3 point to address-8, so we're able to use fp double stores }
        { with update (it's already -4 now)                                 }
        subi      r3,r3,4
        { load r10 with 8, so that dcbz uses the correct address }
LFillChar32ByteLoop:
        dcbz      r3,r10
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        bdnz      LFillChar32ByteLoop
        { if there was no rest, we're finished }
        beq       LFillCharDone
LFillCharSmall:
        { when we arrive here, we're already 4 byte aligned }
        { get count div 4 to store dwords }
        srwi      r10,r4,2
        { get ready for use of update stores }
        subi      r3,r3,4
        mtctr     r10
        rlwinm.   r4,r4,0,31-2+1,31
LFillCharSmallLoop:
        stwu      r5,4(r3)
        bdnz      LFillCharSmallLoop
        { if nothing left, stop }
        beq       LFillCharDone
        { get ready to store bytes }
        addi      r3,r3,4
LFillCharVerySmall:
        mtctr     r4
        subi      r3,r3,1
LFillCharVerySmallLoop:
        stbu      r5,1(r3)
        bdnz      LFillCharVerySmallLoop
LFillCharDone:
end;


{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : longint;value : dword);
assembler;
asm
{       registers:
        r3              x
        r4              count
        r5              value
        r13             value.value
        r14             ptr to dest word
        r15             increment 1
        r16             increment 2
        r17             scratch
        r18             scratch
        f1              value.value.value.value
}
                cmpwi   cr0,r3,0
                mtctr   r4
                subi    r3,r3,4
                ble    .FillWordEnd    //if count<=0 Then Exit
.FillWordLoop:
                stwu    r5,4(r3)
                bdnz    .FillWordLoop
.FillWordEnd:
end ['R3','R4','R5','CTR'];


{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(const buf;len:longint;b:byte):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,1
                mr      r0,r3
                { assume not found }
                li      r3,-1
                beq     LIndexByteDone
LIndexByteLoop:
                lbzu    r9,1(r10)
                cmplw   r9,r5
                bdnzf   cr0*4+eq,LIndexByteLoop
                { r3 still contains -1 here }
                bne     LIndexByteDone
                sub     r3,r10,r0
LIndexByteDone:
end ['R0','R3','R9','R10','CR0','CTR'];


{$define FPC_SYSTEM_HAS_INDEXWORD}
function IndexWord(const buf;len:longint;b:word):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,2
                mr      r0,r3
                { assume not found }
                li      r3,-1
                beq     LIndexWordDone
LIndexWordLoop:
                lhzu    r9,2(r10)
                cmplw   r9,r5
                bdnzf   cr0*4+eq,LIndexWordLoop
                { r3 still contains -1 here }
                bne     LIndexWordDone
                sub     r3,r10,r0
LIndexWordDone:
end ['R0','R3','R9','R10','CR0','CTR'];


{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(const buf;len:longint;b:DWord):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,4
                mr      r0,r3
                { assume not found }
                li      r3,-1
                beq     LIndexDWordDone
LIndexDWordLoop:
                lwzu    r9,4(r30)
                cmplw   r9,r5
                bdnzf   cr0*4+eq, LIndexDWordLoop
                { r3 still contains -1 here }
                bne     LIndexDWordDone
                sub     r3,r10,r0
LIndexDWordDone:
end ['R0','R3','R9','R10','CR0','CTR'];

{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(const buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,1
        subi    r4,r4,1
        li      r3,0
        beq     LCompByteDone
LCompByteLoop:
        { load next chars }
        lbzu    r9,1(r11)
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompByteLoop
LCompByteDone:
end ['R0','R3','R4','R9','R10','R11','CR0','CTR'];

{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(const buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,2
        subi    r4,r4,2
        li      r3,0
        beq     LCompWordDone
LCompWordLoop:
        { load next chars }
        lhzu    r9,2(r11)
        lhzu    r10,2(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompWordLoop
LCompWordDone:
end ['R0','R3','R4','R9','R10','R11','CR0','CTR'];


{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(const buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,4
        subi    r4,r4,4
        li      r3,0
        beq     LCompDWordDone
LCompDWordLoop:
        { load next chars }
        lwzu    r9,4(r11)
        lwzu    r10,4(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompDWordLoop
LCompDWordDone:
end ['R0','R3','R4','R9','R10','R11','CR0','CTR'];

{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(const buf;len:longint;b:Char):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                         }
{ output: r3 = position of found position (-1 if not found) }
asm
        {  load the begin of the buffer in the data cache }
        dcbt    0,r3
        { length = 0? }
        cmplwi  r4,0
        mtctr   r4
        subi    r9,r3,1
        mr      r0,r9
        { assume not found }
        li      r3,-1
        { if yes, do nothing }
        beq     LIndexChar0Done
        subi    r3,r3,1
LIndexChar0Loop:
        lbzu    r10,1(r9)
        cmplwi  cr1,r10,0
        cmplw   r10,r5
        beq     cr1,LIndexChar0Done
        bdnzf   cr0*4+eq, LIndexChar0Loop
        bne     LIndexChar0Done
        sub     r3,r9,r0
LIndexChar0Done:
end ['R0','R3','R4','R9','R10','CR0','CTR'];


{****************************************************************************
                              Object Helpers
****************************************************************************}

{ use generic implementation for now }
{ that's a problem currently, the generic has a another prototyp than this defined in compproc.inc (FK) }

{$define FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}
procedure fpc_help_constructor; assembler;compilerproc;
asm
end;

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL}
procedure fpc_help_fail;assembler;[public,alias:'FPC_HELP_FAIL']; compilerproc;
assembler;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;


{$define FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
{ use generic implementation for now }
{ that's a problem currently, the generic has a another prototyp than this defined in compproc.inc (FK) }

procedure fpc_help_destructor;assembler; compilerproc;
asm
end;

{$define FPC_SYSTEM_HAS_FPC_NEW_CLASS}
procedure fpc_new_class;assembler;[public,alias:'FPC_NEW_CLASS']; compilerproc;
assembler;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;


{$define FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
procedure fpc_dispose_class;assembler;[public,alias:'FPC_DISPOSE_CLASS']; compilerproc;
assembler;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}
procedure fpc_help_fail_class;assembler;[public,alias:'FPC_HELP_FAIL_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
assembler;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;



{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}
{ use generic implementation for now }
{ that's a problem currently, the generic has a another prototy than this defined in compproc.inc (FK) }
procedure fpc_check_object(obj : pointer);assembler; compilerproc;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;

{ use generic implementation for now }
{ that's a problem currently, the generic has a another prototy than this defined in compproc.inc (FK) }
{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
procedure fpc_check_object_ext; compilerproc;assembler;
asm
{$warning FIX ME!}
// !!!!!!!!!!!
end;

{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}
function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; [public,alias: 'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
assembler;
{ input: r3: pointer to result, r4: len, r5: sstr }
asm
        { load length source }
        lbz     r10,0(r5)
        {  load the begin of the dest buffer in the data cache }
        dcbtst  r0,r3

        { put min(length(sstr),len) in r3 }
        subc    r0,r4,r10     { r0 := r3 - r10                               }
        subfme  r4,r4         { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r4,r0,r4      { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r4,r4,r10     { if r3 >= r4 then r3' := r10 else r3' := r3   }

        cmplwi  r4,0
        { put length in ctr }
        mtctr   r4
        stb     r4,0(r3)
        beq     LShortStrCopyDone
LShortStrCopyLoop:
        lbzu    r0,1(r5)
        stbu    r0,1(r3)
        bdnz    LShortStrCopyLoop
LShortStrCopyDone:
end ['R0','R3','R4','R5','R10','CR0','CTR'];


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
{$ifdef interncopy}
procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN'];
{$else}
procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
{$endif}
assembler;
{ input: r3: len, r4: sstr, r5: dstr }
asm
        { load length source }
        lbz     r10,0(r4)
        {  load the begin of the dest buffer in the data cache }
        dcbtst  r0,r5

        { put min(length(sstr),len) in r3 }
        subc    r0,r3,r10    { r0 := r3 - r10                               }
        subfme  r3,r3        { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r3,r0,r3     { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r3,r3,r10    { if r3 >= r4 then r3' := r10 else r3' := r3   }

        cmplwi  r3,0
        { put length in ctr }
        mtctr   r3
        stb     r3,0(r5)
        beq     LShortStrCopyDone2
LShortStrCopyLoop2:
        lbzu    r0,1(r4)
        stbu    r0,1(r5)
        bdnz    LShortStrCopyLoop2
LShortStrCopyDone2:
end ['R0','R3','R4','R5','R10','CR0','CTR'];

{define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}
(*

BUGGY!!

function fpc_shortstr_concat(const s1,s2: shortstring): shortstring; compilerproc;
{ expects that results (r3) contains a pointer to the current string and s1 }
{ (r4) a pointer to the one that has to be concatenated                     }
assembler;
asm
      { load length s1 }
      lbz     r9, 0(r4)
      { load length result }
      lbz     r10, 0(r3)
      { length 0? }
      cmplwi  r10,0
      { go to last current character of result }
      add     r4,r9,r4

      { calculate min(length(s1),255-length(result)) }
      subfic  r9,r9,255
      subc    r8,r9,r10    { r8 := r9 - r10                               }
      subfme  r9,r9        { if r9 >= r10 then r9' := 0 else r9' := -1    }
      and     r9,r8,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r8 }
      add     r9,r9,r10    { if r9 >= r10 then r9' := r10 else r9' := r9  }

      { and concatenate }
      mtctr   r9
      beq     LShortStrConcatDone
LShortStrConcatLoop:
      lbzu    r10,1(r4)
      stbu    r10,1(r3)
      bdnz    LShortStrConcatLoop
LShortStrConcatDone:
end ['R3','R4','R8','R9','R10','CTR'];
*)

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const dstr,sstr:shortstring): longint; [public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
assembler;
asm
      { load length sstr }
      lbz     r9,0(r4)
      { load length dstr }
      lbz     r10,0(r3)
      { save their difference for later and      }
      { calculate min(length(sstr),length(dstr)) }
      subc    r0,r9,r10    { r0 := r9 - r10                               }
      subfme  r9,r9        { if r9 >= r10 then r9' := 0 else r9' := -1    }
      and     r9,r0,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r8 }
      add     r9,r9,r10    { if r9 >= r10 then r9' := r10 else r9' := r9  }

      { first compare dwords (length/4) }
      srwi.   r8,r9,2
      { keep length mod 4 for the ends }
      rlwinm  r9,r9,0,30,31
      { already check whether length mod 4 = 0 }
      cmplwi  cr1,r9,0
      { length div 4 in ctr for loop }
      mtctr   r8
      { if length < 3, goto byte comparing }
      beq     LShortStrCompare1
      { setup for use of update forms of load/store with dwords }
      subi    r4,r4,3
      subi    r8,r3,3
LShortStrCompare4Loop:
      lwzu    r3,4(r4)
      lwzu    r10,4(r8)
      sub.    r3,r3,r10
      bdnzt   cr0+eq,LShortStrCompare4Loop
      { r3 contains result if we stopped because of "ne" flag }
      bne     LShortStrCompareDone
      { setup for use of update forms of load/store with bytes }
      addi    r4,r4,3
      addi    r8,r8,3
LShortStrCompare1:
      { if comparelen mod 4 = 0, skip this and return the difference in }
      { lengths                                                         }
      beq     cr1,LShortStrCompareLen
LShortStrCompare1Loop:
      lbzu    r3,1(r4)
      lbzu    r10,1(r8)
      sub.    r3,r3,r10
      bdnzt   cr0+eq,LShortStrCompare4Loop
      bne     LShortStrCompareDone
LShortStrCompareLen:
      { also return result in flags, maybe we can use this in the CG }
      mr.     r3,r0
LShortStrCompareDone:
end ['R0','R3','R4','R8','R9','R10','CR0','CR1','CTR'];


{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
assembler;
{$include strpas.inc}


{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):longint;assembler;[public,alias:'FPC_PCHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$include strlen.inc}


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:longint;assembler;
asm
    {$warning FIX ME!}
    //    !!!!!!! depends on ABI !!!!!!!!
end ['R3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:longint):longint;assembler;
asm
   {$warning FIX ME!}
    //     !!!!!!! depends on ABI !!!!!!!!
end ['R3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:longint):longint;assembler;
asm
    {$warning FIX ME!}
   //     !!!!!!! depends on ABI !!!!!!!!
end ['R3'];

{$define FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint; assembler;[internconst:in_const_abs];
asm
        srawi   r0,r3,31
        add     r3,r0,r3
        xor     r3,r3,r0
end ['R0','R3'];


{****************************************************************************
                                 Math
****************************************************************************}

{$define FPC_SYSTEM_HAS_ODD_LONGINT}
function odd(l:longint):boolean;assembler;[internconst:in_const_odd];
asm
        rlwinm  r3,r3,0,31,31
end ['R3'];


{$define FPC_SYSTEM_HAS_SQR_LONGINT}
function sqr(l:longint):longint;assembler;[internconst:in_const_sqr];
asm
        mullw   r3,r3,r3
end ['R3'];


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Longint;assembler;
asm
        mr    r3,r1
end ['R3'];


{****************************************************************************
                                 Str()
****************************************************************************}

{ int_str: generic implementation is used for now }


{****************************************************************************
                             Multithreading
****************************************************************************}

{ do a thread save inc/dec }

function declocked(var l : longint) : boolean;assembler;
{ input:  address of l in r3                                      }
{ output: boolean indicating whether l is zero after decrementing }
asm
LDecLockedLoop:
{$ifdef MT}
    lwarx   r10,0,r3
    subi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LDecLockedLoop
{$else MT}
    lwzx    r10,0,r3
    subi    r10,r10,1
    stw     r10,0(r3)
{$endif MT}
    mr.     r3,r10
end ['R3','R10'];

procedure inclocked(var l : longint);assembler;
asm
LIncLockedLoop:
{$ifdef MT}
    lwarx   r10,0,r3
    addi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LDecLockedLoop
{$else MT}
    lwzx    r10,0,r3
    addi    r10,r10,1
    stw     r10,0(r3)
{$endif MT}
end ['R3','R10'];


{
  $Log$
  Revision 1.22  2002-10-05 14:20:16  peter
    * fpc_pchar_length compilerproc and strlen alias

  Revision 1.21  2002/10/02 18:21:52  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.20  2002/09/10 21:30:34  jonas
    * disabled powerpc-specific fpc_shortstr_concat for now, it was
      completely wrong

  Revision 1.19  2002/09/10 17:47:20  jonas
    * fixed bug with concatting 0-length shortstrings

  Revision 1.18  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.17  2002/08/31 21:29:57  florian
    * several PC related fixes

  Revision 1.16  2002/08/31 16:08:36  florian
    * fixed undefined labels

  Revision 1.15  2002/08/31 13:11:11  florian
    * several fixes for Linux/PPC compilation

  Revision 1.14  2002/08/18 22:11:10  florian
    * fixed remaining assembler errors

  Revision 1.13  2002/08/18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.12  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.11  2002/07/30 17:29:53  florian
    + dummy setjmp and longjmp added
    + dummy implemtation of the destructor helper

  Revision 1.10  2002/07/28 21:39:29  florian
    * made abs a compiler proc if it is generic

  Revision 1.9  2002/07/28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.8  2002/07/26 15:45:56  florian
    * changed multi threading define: it's MT instead of MTRTL

}
