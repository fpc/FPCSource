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
                                Move / Fill
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(var source;var dest;count:longint);assembler;
asm
          {  load the begin of the source in the data cache }
          dcbt    0,r3
          {  count <= 0 ?  }
          cmpwi   cr0,r5,0
          {  check if we have to do the move backwards because of overlap  }
          sub     r10,r4,r3
          {  carry := boolean(dest-source < count) = boolean(overlap) }
          subc    r10,r10,r5

          {  count < 11 ? (to decide whether we will move dwords or bytes  }
          cmpwi   cr1,r5,11

          {  if overlap, then r10 := -1 else r10 := 0  }
          subfe   r10,r10,r10

          {  count < 39 ? (32 + max. alignment (7) }
          cmpwi   cr7,r5,39

          {  if count <= 0, stop  }
          ble     cr0,LMoveDone

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

          {  if count < 11, copy everything byte by byte  }
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
          lfdux   f13,r3,r10
          lfdux   f12,r3,r10
          lfdux   f11,r3,r10
          lfdux   f0,r3,r10
          stfdux  f13,r4,r10
          stfdux  f12,r4,r10
          stfdux  f11,r4,r10
          stfdux  f0,r4,r10
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
end ['r0','r3','r9','r10','cr0','ctr'];


{$define FPC_SYSTEM_HAS_INDEXWORD}
function IndexWord(var buf;len:longint;b:word):longint; assembler;
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
end ['r0','r3','r9','r10','cr0','ctr'];


{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(var buf;len:longint;b:DWord):longint; assembler;
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
end ['r0','r3','r9','r10','cr0','ctr'];

{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r0,r3,1
        subi    r4,r4,1
        li      r3,0
        beq     LCompByteDone
LCompByteLoop:
        { load next chars }
        lbzu    r9,1(r0)
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompByteLoop
LCompByteDone:
end ['r0','r3','r4','r9','r10','cr0','ctr'];

{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r0,r3,2
        subi    r4,r4,2
        li      r3,0
        beq     LCompWordDone
LCompWordLoop:
        { load next chars }
        lhzu    r9,2(r0)
        lhzu    r10,2(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompWordLoop
LCompWordDone:
end ['r0','r3','r4','r9','r10','cr0','ctr'];


{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(var buf1,buf2;len:longint):longint; assembler;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r0,r3,4
        subi    r4,r4,4
        li      r3,0
        beq     LCompDWordDone
LCompDWordLoop:
        { load next chars }
        lwzu    r9,4(r0)
        lwzu    r10,4(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, LCompDWordLoop
LCompDWordDone:
end ['r0','r3','r4','r9','r10','cr0','ctr'];

{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(var buf;len:longint;b:Char):longint; assembler;
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
end ['r0','r3','r4','r9','r10','cr0','ctr'];


{****************************************************************************
                              Object Helpers
****************************************************************************}

{define FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}
(*
use generic implementation for now
procedure fpc_help_constructor; assembler; [public,alias:'FPC_HELP_CONSTRUCTOR']; {$ifdef hascompilerproc} compilerproc; {$endif}
*)

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL}
procedure fpc_help_fail;assembler;[public,alias:'FPC_HELP_FAIL']; {$ifdef hascompilerproc} compilerproc; {$endif}
assembler
asm
!!!!!!!!!!!
end;


{$define FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
(*
use generic implementation for now
procedure fpc_help_destructor;assembler;[public,alias:'FPC_HELP_DESTRUCTOR']; {$ifdef hascompilerproc} compilerproc; {$endif}
*)

{$define FPC_SYSTEM_HAS_FPC_NEW_CLASS}
procedure fpc_new_class;assembler;[public,alias:'FPC_NEW_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
assembler;
asm
!!!!!!!!!!!
end;


{$define FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
procedure fpc_dispose_class;assembler;[public,alias:'FPC_DISPOSE_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
assembler;
asm
!!!!!!!!!!!
end;

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}
procedure fpc_help_fail_class;assembler;[public,alias:'FPC_HELP_FAIL_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
assembler;
asm
!!!!!!!!!!!
end;



{define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}
{ we want the stack for debugging !! PM }
(*
use generic implementation for now
procedure fpc_check_object(obj : pointer);[public,alias:'FPC_CHECK_OBJECT']; {$ifdef hascompilerproc} compilerproc; {$endif}
*)

{define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
(*
use generic implementation for now
procedure fpc_check_object_ext;assembler;[public,alias:'FPC_CHECK_OBJECT_EXT']; {$ifdef hascompilerproc} compilerproc; {$endif}
*)

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
        subme   r4,r4,r4      { if r3 >= r4 then r3' := 0 else r3' := -1     }
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
end ['r0','r3','r4','r5','r10','cr0','ctr'];


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}
procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
assembler;
{ input: r3: len, r4: sstr, r5: dstr }
asm
        { load length source }
        lbz     r10,0(r4)
        {  load the begin of the dest buffer in the data cache }
        dcbtst  r0,r5

        { put min(length(sstr),len) in r3 }
        subc    r0,r3,r10    { r0 := r3 - r10                               }
        subme   r3,r3,r3     { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r3,r0,r3     { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r3,r3,r10    { if r3 >= r4 then r3' := r10 else r3' := r3   }

        cmplwi  r3,0
        { put length in ctr }
        mtctr   r3
        stb     r3,0(r5)
        beq     LShortStrCopyDone
LShortStrCopyLoop:
        lbzu    r0,1(r4)
        stbu    r0,1(r5)
        bdnz    LShortStrCopyLoop
end ['r0','r3','r4','r5','r10','cr0','ctr'];


function fpc_shortstr_concat(const s1: shortstring): shortstring; compilerproc;
{ expects that results (r3) contains a pointer to the current string and s1 }
{ (r4) a pointer to the one that has to be concatenated                     }
assembler;
asm
      { load length s1 }
      lbz     r9, 0(r4)
      { load length result }
      lbz     r10, 0(r3)
      { go to last current character of result }
      add     r4,r9,r4

      { calculate min(length(s1),255-length(result)) }
      subfic  r9,r9,255
      subc    r8,r9,r10    { r8 := r9 - r10                               }
      subme   r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1    }
      and     r9,r8,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r8 }
      add     r9,r9,r10    { if r9 >= r10 then r9' := r10 else r9' := r9  }

      { and concatenate }
      mtctr   r9
LShortStrConcatLoop:
      lbzu    r10,1(r4)
      stbu    r10,1(r3)
      bdnz    LShortStrConcatLoop
end ['r3','r4','r8','r9','r10','ctr'];


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
      subme   r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1    }
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
end ['r0','r3','r4','r8','r9','r10','cr0','cr1','ctr'];


{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
assembler;
{$include strpas.inc}


{$define FPC_SYSTEM_HAS_STRLEN}
function strlen(p:pchar):longint;assembler;
{$include strlen.inc}


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:longint;assembler;
asm
        !!!!!!! depends on ABI !!!!!!!!
end ['r3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:longint):longint;assembler;
asm
        !!!!!!! depends on ABI !!!!!!!!
end ['r3'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:longint):longint;assembler;
asm
        !!!!!!! depends on ABI !!!!!!!!
end ['r3'];

{$define FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint; assembler;[internconst:in_const_abs];
asm
        srawi   r0,r3,31
        add     r3,r0,r3
        xor     r3,r3,r0
end ['r0','r3'];


{****************************************************************************
                                 Math
****************************************************************************}

{$define FPC_SYSTEM_HAS_ODD_LONGINT}
function odd(l:longint):boolean;assembler;[internconst:in_const_odd];
asm
        rlwinm  r3,r3,0,31,31
end ['r3'];


{$define FPC_SYSTEM_HAS_SQR_LONGINT}
function sqr(l:longint):longint;assembler;[internconst:in_const_sqr];
asm
        mullw   r3,r3,r3
end ['r3'];


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Longint;assembler;
asm
        mr    r3,sp
end ['r3'];


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
{$ifdef MTRTL}
    lwarx   r10,0,r3
    subi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LDecLockedLoop
{$else MTRTL}
    lwzx    r10,0,r3
    subi    r10,r10,1
    stw     r10,0,r3
{$endif MTRTL}
    mr.     r3,r10
end ['r3','r10'];

procedure inclocked(var l : longint);assembler;
LIncLockedLoop:   
{$ifdef MTRTL}
    lwarx   r10,0,r3
    addi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    LDecLockedLoop
{$else MTRTL}
    lwzx    r10,0,r3
    addi    r10,r10,1
    stw     r10,0,r3
{$endif MTRTL}
end ['r3','r10'];


{
  $Log$
  Revision 1.6  2001-09-27 15:30:29  jonas
    * conversion to compilerproc and to structure used by i386 rtl
    * some bugfixes
    * powerpc.inc is almost complete (only fillchar/word/dword, get_frame etc
      and the class helpers are still needed
    - removed unnecessary register saving in set.inc (thanks to compilerproc)
    * use registers reserved for parameters as much as possible instead of
      those reserved for local vars (since those have to be saved by the
      called anyway, while the ones for local vars have to be saved by the
      callee)

  Revision 1.5  2001/07/07 12:46:12  jonas
    * some small bugfixes and cache optimizations

  Revision 1.4  2001/03/03 13:53:36  jonas
    * fixed small bug in move

  Revision 1.3  2001/03/02 13:24:10  jonas
    + new, complete implementation of move procedure (including support for
      overlapping regions)

  Revision 1.2  2001/02/11 17:59:46  jonas
    * implemented several more procedures

  Revision 1.1  2000/07/27 07:32:12  jonas
    + initial version by Casey Duncan (not yet thoroughly debugged or complete)

}