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
{$ifndef MACOS}
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
{$endif MACOS}

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

          {  count < 63 ? (32 + max. alignment (31) }
          cmpwi   cr7,r5,63

          {  if count <= 0, stop  }
          ble     cr0,LMoveDone

          {  load the begin of the source in the data cache }
          dcbt    0,r3
          { and the dest as well }
          dcbtst  0,r4

          {  if overlap, then r0 := count else r0 := 0  }
          and     r0,r5,r10
          {  if overlap, then point source and dest to the end  }
          add     r3,r3,r0
          add     r4,r4,r0
          {  if overlap, then r6 := 0, else r6 := -1  }
          not     r6,r10
          {  if overlap, then r10 := -2, else r10 := 0  }
          slwi    r10,r10,1
          {  if overlap, then r10 := -1, else r10 := 1  }
          addi    r10,r10,1

          {  if count < 15, copy everything byte by byte  }
          blt     cr1,LMoveBytes

          {  if no overlap, then source/dest += -1, otherwise they stay }
          {  After the next instruction, r3/r4 + r10 = next position to }
          {  load/store from/to                                         }
          add     r3,r3,r6
          add     r4,r4,r6

          {  otherwise, guarantee 4 byte alignment for dest for starters  }
LMove4ByteAlignLoop:
          lbzux   r0,r3,r10
          stbux   r0,r4,r10
          {  is dest now 4 aligned?  }
          andi.   r0,r4,3
          subi    r5,r5,1
          {  while not aligned, continue  }
          bne     cr0,LMove4ByteAlignLoop

{$ifndef ppc603}
          { check for 32 byte alignment }
          andi.   r7,r4,31
{$endif non ppc603}
          { we are going to copy one byte again (the one at the newly }
          { aligned address), so increase count byte 1                }
          addi    r5,r5,1
          { count div 4 for number of dwords to copy }
          srwi    r0,r5,2
          {  if 11 <= count < 63, copy using dwords }
          blt     cr7,LMoveDWords

{$ifndef ppc603}
          { # of dwords to copy to reach 32 byte alignment (*4) }
          { (depends on forward/backward copy)                  }

          { if forward copy, r6 = -1 -> r8 := 32 }
          { if backward copy, r6 = 0 -> r8 := 0  }
          rlwinm  r8,r6,0,31-6+1,31-6+1
          { if forward copy, we have to copy 32 - unaligned count bytes }
          { if backward copy unaligned count bytes                      }
          sub     r7,r8,r7
          { if backward copy, the calculated value is now negate -> }
          { make it positive again                                 }
          not     r8, r6
          add     r7, r7, r8
          xor     r7, r7, r8
{$endif not ppc603}

          { multiply the update count with 4 }
          slwi    r10,r10,2
          slwi    r6,r6,2
          { and adapt the source and dest }
          add     r3,r3,r6
          add     r4,r4,r6

{$ifndef ppc603}
          beq     cr0,LMove32BytesAligned
L32BytesAlignMoveLoop:
          {  count >= 39 -> align to 8 byte boundary and then use the FPU  }
          {  since we're already at 4 byte alignment, use dword store      }
          subic.  r7,r7,4
          lwzux   r0,r3,r10
          subi    r5,r5,4
          stwux   r0,r4,r10
          bne     L32BytesAlignMoveLoop

LMove32BytesAligned:
          { count div 32 ( >= 1, since count was >=63 }
          srwi    r0,r5,5
          { remainder }
          andi.   r5,r5,31
          { to decide if we will do some dword stores (instead of only }
          { byte stores) afterwards or not                             }
{$else not ppc603}
          srwi    r0,r5,4
          andi.   r5,r5,15
{$endif not ppc603}
          cmpwi   cr1,r5,11
          mtctr   r0

          {  r0 := count div 4, will be moved to ctr when copying dwords  }
          srwi    r0,r5,2

{$ifndef ppc603}
          {  adjust the update count: it will now be 8 or -8 depending on overlap  }
          slwi    r10,r10,1

          {  adjust source and dest pointers: because of the above loop, dest is now   }
          {  aligned to 8 bytes. So if we add r6 we will still have an 8 bytes         }
          { aligned address)                                                           }
          add     r3,r3,r6
          add     r4,r4,r6

          slwi    r6,r6,1

          { the dcbz offset must give a 32 byte aligned address when added   }
          { to the current dest address and its address must point to the    }
          { bytes that will be overwritten in the current iteration. In case }
          { of a forward loop, the dest address has currently an offset of   }
          { -8 compared to the bytes that will be overwritten (and r6 = -8). }
          { In case of a backward of a loop, the dest address currently has  }
          { an offset of +32 compared to the bytes that will be overwritten  }
          { (and r6 = 0). So the forward dcbz offset must become +8 and the  }
          { backward -32 -> (-r6 * 5) - 32 gives the correct offset          }
          slwi    r7,r6,2
          add     r7,r7,r6
          neg     r7,r7
          subi    r7,r7,32

LMove32ByteDcbz:
          lfdux   f0,r3,r10
          lfdux   f1,r3,r10
          lfdux   f2,r3,r10
          lfdux   f3,r3,r10
          { must be done only now, in case source and dest are less than }
          { 32 bytes apart!                                              }
          dcbz    r4,r7
          stfdux  f0,r4,r10
          stfdux  f1,r4,r10
          stfdux  f2,r4,r10
          stfdux  f3,r4,r10
          bdnz    LMove32ByteDcbz
LMove32ByteLoopDone:
{$else not ppc603}
LMove16ByteLoop:
          lwzux   r11,r3,r10
          lwzux   r7,r3,r10
          lwzux   r8,r3,r10
          lwzux   r9,r3,r10
          stwux   r11,r4,r10
          stwux   r7,r4,r10
          stwux   r8,r4,r10
          stwux   r9,r4,r10
          bdnz    LMove16ByteLoop
{$endif not ppc603}

          { cr0*4+eq is true if "count and 31" = 0 }
          beq     cr0,LMoveDone

          {  make r10 again -1 or 1, but first adjust source/dest pointers }
          sub     r3,r3,r6
          sub     r4,r4,r6
{$ifndef ppc603}
          srawi   r10,r10,3
          srawi   r6,r6,3
{$else not ppc603}
          srawi   r10,r10,2
          srawi   r6,r6,2
{$endif not ppc603}

          { cr1 contains whether count <= 11 }
          ble     cr1,LMoveBytes

LMoveDWords:
          mtctr   r0
          andi.   r5,r5,3
          {  r10 * 4  }
          slwi    r10,r10,2
          slwi    r6,r6,2
          add     r3,r3,r6
          add     r4,r4,r6

LMoveDWordsLoop:
          lwzux   r0,r3,r10
          stwux   r0,r4,r10
          bdnz    LMoveDWordsLoop

          beq     cr0,LMoveDone
          {  make r10 again -1 or 1  }
          sub     r3,r3,r6
          sub     r4,r4,r6
          srawi   r10,r10,2
          srawi   r6,r6,2
LMoveBytes:
          add     r3,r3,r6
          add     r4,r4,r6
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
        ble       cr6,LFillCharDone
        { get the start of the data in the cache (and mark it as "will be }
        { modified")                                                      }
        dcbtst    0,r3
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
        rlwinm.   r4,r4,0,31-5+1,31
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
        b         LFillCharVerySmall
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
        li        r10, 8
LFillChar32ByteLoop:
        dcbz      r3,r10
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        stfdu     f0,8(r3)
        bdnz      LFillChar32ByteLoop
        { if there was no rest, we're finished }
        beq       LFillCharDone
        { make r3 point again to the actual next byte that must be written }
        addi      r3,r3,8
        b         LFillCharVerySmall
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
}
                cmpwi   cr0,r4,0
                mtctr   r4
                subi    r3,r3,4
                ble    LFillDWordEnd    //if count<=0 Then Exit
LFillDWordLoop:
                stwu    r5,4(r3)
                bdnz    LFillDWordLoop
LFillDWordEnd:
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
                ble     LIndexByteDone
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
                ble     LIndexWordDone
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
                ble     LIndexDWordDone
LIndexDWordLoop:
                lwzu    r9,4(r10)
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
        ble     LCompByteDone
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
        ble     LCompWordDone
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
        ble     LCompDWordDone
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
        subi    r0,r3,1
        { assume not found }
        li      r3,-1
        { if yes, do nothing }
        ble     LIndexChar0Done
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
        dcbtst  0,r3

        { put min(length(sstr),len) in r4 }
        subfc   r7,r10,r4     { r0 := r4 - r10                               }
        subfe   r4,r4,r4      { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r7,r7,r4      { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r4,r10,r7     { if r3 >= r4 then r3' := r10 else r3' := r3   }

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
end ['R0','R3','R4','R5','R6','R7','R10','CR0','CTR'];


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
        dcbtst  0,r5

        { put min(length(sstr),len) in r3 }
        subc    r0,r3,r10    { r0 := r3 - r10                               }
        subfe   r3,r3,r3     { if r3 >= r4 then r3' := 0 else r3' := -1     }
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


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}

procedure fpc_shortstr_append_shortstr(var s1: shortstring; const s2: shortstring); compilerproc;
{ expects that results (r3) contains a pointer to the current string s1, r4 }
{ high(s1) and (r5) a pointer to the one that has to be concatenated        }
assembler;
asm
      { load length s1 }
      lbz     r6, 0(r3)
      { load length s2 }
      lbz     r10, 0(r5)
      { length 0? }
      cmplwi  r10,0

      { calculate min(length(s2),high(result)-length(result)) }
      sub     r9,r4,r6
      subc    r8,r9,r10    { r8 := r9 - r10                                }
      subfe   r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1     }
      and     r9,r8,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r10 }
      add     r9,r9,r10    { if r9 >= r10 then r9' := r10 else r9' := r9   }

      { calculate new length }
      add     r10,r6,r9
      { load value to copy in ctr }
      mtctr   r9
      { store new length }
      stb     r10,0(r3)
      { go to last current character of result }
      add     r3,r6,r3
      
      { if nothing to do, exit }
      beq     LShortStrAppendDone
      { and concatenate }
LShortStrAppendLoop:
      lbzu    r10,1(r5)
      stbu    r10,1(r3)
      bdnz    LShortStrAppendLoop
LShortStrAppendDone:
end ['R3','R4','R8','R9','R10','CTR'];

(*
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
      subfc    r7,r10,r9    { r0 := r9 - r10                               }
      subfe    r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1    }
      and      r7,r7,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r8 }
      add      r9,r10,r7    { if r9 >= r10 then r9' := r10 else r9' := r9  }

      { first compare dwords (length/4) }
      srwi.   r5,r9,2
      { keep length mod 4 for the ends }
      rlwinm  r9,r9,0,30,31
      { already check whether length mod 4 = 0 }
      cmplwi  cr1,r9,0
      { so we can load r3 with 0, in case the strings both have length 0 }
      mr      r8,r3
      li      r3, 0
      { length div 4 in ctr for loop }
      mtctr   r5
      { if length < 3, goto byte comparing }
      beq     LShortStrCompare1
      { setup for use of update forms of load/store with dwords }
      subi    r4,r4,3
      subi    r8,r8,3
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
      mtctr   r9
LShortStrCompare1Loop:
      lbzu    r3,1(r4)
      lbzu    r10,1(r8)
      sub.    r3,r3,r10
      bdnzt   cr0+eq,LShortStrCompare1Loop
      bne     LShortStrCompareDone
LShortStrCompareLen:
      { also return result in flags, maybe we can use this in the CG }
      mr.     r3,r3
LShortStrCompareDone:
end ['R0','R3','R4','R5','R6','R7','R8','R9','R10','CR0','CR1','CTR'];
*)

{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
assembler;
{$include strpas.inc}


{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):longint;assembler;[public,alias:'FPC_PCHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$include strlen.inc}


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;
asm
  { all abi's I know use r1 as stack pointer }
  mr r3, r1
end ['R3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;
asm
   cmplwi  r3,0
   beq     Lcaller_addr_frame_null
   lwz r3,0(r3)
   cmplwi  r3,0
   beq     Lcaller_addr_frame_null
   lwz r3,4(r3)
Lcaller_addr_frame_null:
    //     !!!!!!! depends on ABI !!!!!!!!
end ['R3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;
asm
    cmplwi  r3,0
    beq     Lcaller_frame_null
    lwz  r3,0(r3)
Lcaller_frame_null:
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

{$define FPC_SYSTEM_HAS_DECLOCKED}
function declocked(var l : longint) : boolean;assembler;
{ input:  address of l in r3                                      }
{ output: boolean indicating whether l is zero after decrementing }
asm
LDecLockedLoop:
    lwarx   r10,0,r3
    subi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LDecLockedLoop
    cntlzw  r3,r10
    srwi    r3,r3,5
end ['R3','R10'];

{$define FPC_SYSTEM_HAS_INCLOCKED}
procedure inclocked(var l : longint);assembler;
asm
LIncLockedLoop:
    lwarx   r10,0,r3
    addi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LIncLockedLoop
end ['R3','R10'];


{
  $Log$
  Revision 1.50  2003-06-01 14:50:17  jonas
    * fpc_shortstr_append_shortstr has to use high(s1) instead of 255 as
      maxlen
    + ppc version of fpc_shortstr_append_shortstr

  Revision 1.49  2003/05/29 21:17:27  jonas
    * compile with -dppc603 to not use unaligned float loads in move() and
      g_concatcopy, because the 603 and 604 take an exception for those
      (and netbsd doesn't even handle those in the kernel). There are
      still some of those left that could cause problems though (e.g.
      in the set helpers)

  Revision 1.48  2003/05/29 14:32:54  jonas
    * changed dcbst to dcbtst (former means "flush cache block to memory,
      the latter means "I will soon store something to that cache block")

  Revision 1.47  2003/05/29 12:14:02  jonas
    * move() now uses dcbz if possible

  Revision 1.46  2003/05/17 00:19:51  jonas
    * fixed inclocked

  Revision 1.45  2003/05/14 19:47:35  jonas
    * fixed stupid bug in filldword

  Revision 1.44  2003/05/13 20:39:26  florian
    * uncommented shortstring compare, buggy

  Revision 1.43  2003/05/12 19:39:33  jonas
    * fixed final fillchar error (tfillchr passes now)

  Revision 1.42  2003/05/12 19:00:50  jonas
    * fixed bug in fillchar

  Revision 1.41  2003/05/10 20:33:39  jonas
    * fixed get_caller_frame and get_caller_addr

  Revision 1.40  2003/05/10 17:33:06  jonas
    * final (? :) fix to move, passes new tests/test/tmove test

  Revision 1.39  2003/05/02 19:03:25  jonas
    * fixed some bugs in move()

  Revision 1.38  2003/04/27 16:24:44  jonas
    - disabled fpc_shortstr_concat because it's called differently than that
      routine is declared

  Revision 1.37  2003/04/26 20:37:17  jonas
    * fixed and re-enabled routines commented out by Florian :)

  Revision 1.36  2003/04/26 17:46:49  florian
    * commented out not working routines
    * reactivated assembler fillchar

  Revision 1.35  2003/04/26 17:35:15  jonas
    * fixed FillChar

  Revision 1.34  2003/04/26 12:05:10  florian
    * removed object/class helpers, the compiler uses the generic ones

  Revision 1.33  2003/04/26 11:55:52  florian
    * fixed newlines

  Revision 1.32  2003/04/23 21:04:48  florian
    * fixed fpc_shortstr_to_shortstr

  Revision 1.31  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.30  2003/03/12 19:21:29  jonas
    + implemented get_frame()
    * fixed bug in IndexDWord()

  Revision 1.29  2003/01/09 20:14:35  florian
    * fixed helper declarations

  Revision 1.28  2003/01/09 13:38:56  florian
    * dec/inclocked got defines

  Revision 1.27  2002/11/07 15:23:13  jonas
    * always use code that was between 'ifdef mt', since that define is
      deprecated now

  Revision 1.26  2002/11/01 13:27:55  jonas
    * changed "dcbtst r0,x" to "dcbtst 0,x"

  Revision 1.25  2002/10/23 15:26:00  olle
    * excluded saverestorereg for target macos

  Revision 1.24  2002/10/20 13:40:55  jonas
    * move/fill*/index*/comp* routines immediately exit if length is negative

  Revision 1.23  2002/10/17 10:12:50  jonas
    * fixed return value of declocked()

  Revision 1.22  2002/10/05 14:20:16  peter
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
