{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team.

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
                                Move / Fill
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(var source;var dest;count:longint);
begin
{ register usage:
  r3    source
  r4    dest
  r5    count
  r13   ptr to end of source
  r14   ptr to end of dest
  r15   counter 1
  r16   counter 2
  r17   addr increment
  r18   ptr to current source block
  r19   ptr to current dest block
  r20-24        buffer
  f1-4  buffer
  ctr   Loop counter
  notes:
  Move uses FPRs for increased bandwidth
}
        asm
        { do some param checking, initialization }
        cmplwi  cr2,r3,0
        cmplwi  cr3,r4,0
        cmplw   cr4,r3,r4
        add             r13,r3,r5
        add             r14,r4,r5
        bt              cr2,.MoveEnd    //end if source=nil
        bt              cr3,.MoveEnd    //end if dest=nil
        bt              cr4,.MoveEnd    //end if source=dest
        { see if source and dest overlap }
        cmplw   cr2,r13,r4
        cmplw   cr3,r4,r3
        srawi.  r15,r5,$5               //r15 := count div 32
        andi    r16,r5,$1F              //r16 := count mod 32
        crand   cr3,cr2,cr3
        mtctr   r15                             //Load loop counter
        bgt             cr3,.MoveRL             //dest overlaps source on right
        li              r17,$8                  //Offset 8 bytes per doubleword copy
        sub             r18,r17,r3              //calculate the starting source
        sub             r19,r17,r4              //                                      and dest ptrs
        beq             .MoveByByte             //If count<32 skip 32 byte block copy
        srawi.  r15,r16,$2              //r15 := r16 div 4
        andi    r16,r15,$3              //r16 := r15 mod 4
        cmpwi   cr2,r16,0               //r16 = 0 ?
        crand   cr3,cr2,cr0             //r15 = 0 AND r16 = 0 ?
.MoveBlockLoop:                                 //32 Byte block copy (fully optimized)
                lfdux   f1,r18,r17
                lfdux   f2,r18,r17
                lfdux   f3,r18,r17
                lfdux   f4,r18,r17
                stfdux  f1,r19,r17
                stfdux  f2,r19,r17
                stfdux  f3,r19,r17
                stfdux  f4,r19,r17
                bdnz    .MoveBlockLoop

                bt              cr3,MoveEnd             //Nothing left to do...
                mtspr   1,r16                   //XER := r16
                beq             .MoveBytes              //There are fewer than 4 bytes left
                mtctr   r15                             //load counter
                andi    r15,r15,$3              //r15 := r15 mod 4
                srawi   r17,$1                  //Offset := Offset div 2
.MoveWordLoop:                                  //4 byte copy
                lwzux   r20,r18,r17
                stwux   r20,r19,r17
                bdnz    .WordCopyLoop

                bt              cr2,MoveEnd             //Nothing left to do...
.MoveBytes:                                             //Copy remaining stragglers
                lswx    r20,r0,r18
                stswx   r20,r0,r19
.MoveEnd:
                End;
End;


{$define FPC_SYSTEM_HAS_FILLCHAR}

Procedure FillChar(var x;count:longint;value:byte);
begin
        asm
{ Register Usage:
        r3      x
        r4      count
        r5      value
        r13     value.value.value.value
        r14     ptr to current dest char
        r15     byte increment, Scratch
        r16     Block count
        r17 misalignment byte count
}
                cmpwi   cr2,r4,12
                mr              r14,r3
                andi.   r17,r3,3
                sub             r14,r3,r17              //32 bit align
                blt             cr2,.FillBytes  //if count<12 then fill byte by byte
                sub             r16,r4,r17
                andi    r17,r16,3
                cmpwi   cr2,r17,0
                srwi    r16,r16,2               //r16:=count div 4
                subi    r16,r16,2
                mtctr   r16                             //counter:=r16
                mr              r13,r5                  //insert
                insrwi  r13,r5,8,16             //              value into all four bytes
                insrwi  r13,r13,16,0    //                                                                      of r13
                li              r15,4
                stw             r13,0(r3)               //fill first few bytes
.FillWordLoop:
                stwux   r13,r14,r15
                bdnz    .FillWordLoop
                beq             cr2,FillEnd             //No trailing bytes, so exit
                add             r14,r3,r4
                stw             r13,-4(r14)             //fill last few bytes
                b               .FillEnd

.FillBytes:
                mtctr   r4                              //counter:=count
                li              r15,1
                subi    r14,r3,1
.FillByteLoop:
                stbux   r13,r14,r15
                bdnz    .FillByteLoop
.FillEnd:
        end [r13,r14,r15,r16,r17,ctr];
end;


{$define FPC_SYSTEM_HAS_FILLWORD}

procedure fillword(var x;count : longint;value : word);
begin
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
        asm
                cmpwi   cr0,r3,0
                andi    r17,r4,$3
                srwi    r18,r4,1        //r18:=count div 2
                mr              r13,r3
                li              r14,4
                ble             .FillWordEnd    //if count<=0 Then Exit
.FillWordLoop:
                stwux   r5,r13,r14
                bdnz    .FillWordLoop
.FillWordEnd:
        end [r13,r14,ctr]
end;


{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(var buf;len:longint;b:byte):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                cmpli   r4,0
                mtctr   r4
                subi    r30,r3,1
                { assume not found }
                li      r3,-1
                beq     LIndexByteNotFound
LIndexByteLoop:
                lbzu    r29,1(r30)
                cmpl    r29,r5
                bdnzne  LIndexByteLoop
                { r3 still contains -1 here }
                bne     LIndexByteDone
                sub     r3,r29,r3
LIndexByteDone:
end ['r3','r29','r30','cr0','ctr'];


{$define FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(var buf;len:longint;b:word):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                cmpli   r4,0
                mtctr   r4
                subi    r30,r3,2
                { assume not found }
                li      r3,-1
                beq     LIndexWordNotFound
LIndexWordLoop:
                lhzu    r29,2(r30)
                cmpl    r29,r5
                bdnzne  LIndexWordLoop
                { r3 still contains -1 here }
                bne     LIndexWordDone
                sub     r3,r29,r3
LIndexWordDone:
end ['r3','r29','r30','cr0','ctr'];


{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(var buf;len:longint;b:DWord):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                cmpli   r4,0
                mtctr   r4
                subi    r30,r3,4
                { assume not found }
                li      r3,-1
                beq     LIndexDWordNotFound
LIndexDWordLoop:
                lwzu    r29,4(r30)
                cmpl    r29,r5
                bdnzne  LIndexDWordLoop
                { r3 still contains -1 here }
                bne     LIndexDWordDone
                sub     r3,r29,r3
LIndexDWordDone:
end ['r3','r29','r30','cr0','ctr'];

{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        { use r28 instead of r3 for buf1 since r3 contains result }
        cmpl    r5,0
        subi    r28,r3,1
        li      r3,0
        beq     LCompByteDone
        mtctr   r5
        subi    r4,r4,1
LCompByteLoop:
        { load next chars }
        lbzu    r29,1(r28)
        lbzu    r30,1(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars not equal or at the end, we're ready }
        bdnze     LCompByteDone
LCompByteDone:
end ['r3','r4','r28','r29','r30','cr0','ctr'];

{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        { use r28 instead of r3 for buf1 since r3 contains result }
        cmpl    r5,0
        subi    r28,r3,2
        li      r3,0
        beq     LCompWordDone
        mtctr   r5
        subi    r4,r4,2
LCompWordLoop:
        { load next chars }
        lhzu    r29,2(r28)
        lhzu    r30,2(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars not equal or at the end, we're ready }
        bdnze     LCompWordDone
LCompWordDone:
end ['r3','r4','r28','r29','r30','cr0','ctr'];


{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        { use r28 instead of r3 for buf1 since r3 contains result }
        cmpl    r5,0
        subi    r28,r3,4
        li      r3,0
        beq     LCompDWordDone
        mtctr   r5
        subi    r4,r4,4
LCompDWordLoop:
        { load next chars }
        lwzu    r29,4(r28)
        lwzu    r30,4(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars not equal or at the end, we're ready }
        bdnze     LCompDWordDone
LCompDWordDone:
end ['r3','r4','r28','r29','r30','cr0','ctr'];

{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(var buf;len:longint;b:Char):longint; assembler;
{ input: r3 = buf, r4 = len, r5 = b                         }
{ output: r3 = position of found position (-1 if not found) }
asm
        { length = 0? }
        cmpli   r5,0
        subi    r29,r3,1
        { assume not found }
        li      r3,-1
        mtctr   r5
        { if yes, do nothing }
        beq     LIndexChar0Done
        subi    r3,r3,1
LIndexChar0Loop:
        lbzu    r30,1(r29)
        cmpli   cr1,r30,0
        cmpl    r30,r4
        beq     cr1,LIndexChar0Done
        bdnzne  LIndexChar0Loop
        bne     LIndexChar0Done
        sub     r3,r29,r3
LIndexCharDone:
end ['r3','r4','r29','r30','cr0','ctr'];

{ all FPC_HELP_* are still missing (JM) }


{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}
procedure int_strcopy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
assembler;
{ input: r3: len, sstr: r4, dstr: r5 }
asm
        { load length source }
        lbz     r30,0(r4)

        { put min(length(sstr),len) in r3 }
        subc    r29,r3,r30    { r29 := r3 - r30                              }
        subme   r3,r3,r3      { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r3,r29,r3     { if r3 >= r4 then r3' := 0 else r3' := r3-r30 }
        add     r3,r3,r30     { if r3 >= r4 then r3' := r30 else r3' := r3   }
        
        cmpli   r3,0
        { put length in ctr }
        mtctr   r3
        stb     r3,0(r5)
        beq     LShortStrCopyDone
LShortStrCopyLoop:
        lbzu    r29,1(r4)
        stbu    r29,1(r5)
        bdnz    LShortStrCopyLoop
end ['r3','r4','r5','r29','r30','cr0','ctr'];


{
  $Log$
  Revision 1.2  2001-02-11 17:59:46  jonas
    * implemented several more procedures

  Revision 1.1  2000/07/27 07:32:12  jonas
    + initial version by Casey Duncan (not yet thoroughly debugged or complete)

}