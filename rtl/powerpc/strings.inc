{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Note: the implementation of these routines is for BIG ENDIAN only!! (JM) }

function strcopy(dest,source : pchar) : pchar;assembler;
{ in: dest in r3, source in r4 }
{ out: result (dest) in r3     }
asm
{  in: dest in r3, source in r4  }
{  out: result (dest) in r3      }
        {  load the begin of the source string in the data cache }
        dcbt    0,r4
        {  get # of misaligned bytes  }
        rlwinm. r10,r4,0,31-2+1,31
        subfic  r10,r10,4
        mtctr   r10
        {  since we have to return dest intact, use another register for  }
        {  dest in the copy loop                                          }
        subi    r9,r3,1
        subi    r4,r4,1
        beq     LStrCopyAligned
LStrCopyAlignLoop:
        {  load next byte  }
        lbzu    r0,1(r4)
        {  end of string?  }
        cmplwi  cr0,r0,0
        {  store byte  }
        stbu    r0,1(r9)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   eq,LStrCopyAlignLoop
        beq     LStrCopyDone
LStrCopyAligned:
        subi    r4,r4,3
        subi    r9,r9,3
        { setup magic constants }
        lis     r8,(0xfefefeff)@ha
        addi    r8,r8,(0xfefefeff)@l
        lis     r7,(0x80808080)@ha
        addi    r7,r7,(0x80808080)@l
        { load first 4 bytes  }
        lwzu    r0,4(r4)

LStrCopyAlignedLoop:
        { test for zero byte }
        add     r10,r0,r8
        andc    r10,r10,r0
        and.    r10,r10,r7
        bne     LStrCopyEndFound
        stwu    r0,4(r9)
        { load next 4 bytes (do it here so the load can begin while the }
        { the branch is processed)                                      }
        lwzu    r0,4(r4)
        b       LStrCopyAlignedLoop
LStrCopyEndFound:
        { adjust for possible $01 bytes coming before the terminating 0 byte }
	rlwinm	r8,r0,7,0,31
	andc	r10,r10,r8
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r9,r9,3
LStrCopyWrapUpLoop:
        subic.  r10,r10,8
        rlwinm  r0,r0,8,0,31
        stbu    r0,1(r9)
        bge     LStrCopyWrapUpLoop
LStrCopyDone:
        {  r3 still contains dest here  }
end ['R4','R7','R8','R0','R9','R10','CR0','CTR'];


function strecopy(dest,source : pchar) : pchar;assembler;
{ in: dest in r3, source in r4        }
{ out: result (end of new dest) in r3 }
asm
        {  load the begin of the source string in the data cache }
        dcbt    0,r4
        {  get # of misaligned bytes  }
        rlwinm. r10,r4,0,31-2+1,31
        subfic  r10,r10,4
        mtctr   r10
        subi    r3,r3,1
        subi    r4,r4,1
        beq     LStrECopyAligned
LStrECopyAlignLoop:
        {  load next byte  }
        lbzu    r0,1(r4)
        {  end of string?  }
        cmplwi  cr0,r0,0
        {  store byte  }
        stbu    r0,1(r3)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   eq,LStrECopyAlignLoop
        beq     LStrECopyDone
LStrECopyAligned:
        subi    r4,r4,3
        subi    r3,r3,3
        { setup magic constants }
        lis     r8,(0xfefefeff)@ha
        addi    r8,r8,(0xfefefeff)@l
        lis     r7,(0x80808080)@ha
        addi    r7,r7,(0x80808080)@l
{
        li      r8,-257        { 0x0feff }
        andis.  r8,r8,0x0fefe
        li      r9,-32640      { 0x08080 }
        andis.  r9,r9,0x08080
}
LStrECopyAlignedLoop:

        {  load next 4 bytes  }
        lwzu    r0,4(r4)

        { test for zero byte }
        add     r10,r0,r8
        andc    r10,r10,r0
        and.    r10,r10,r7
        bne     LStrECopyEndFound
        stwu    r0,4(r3)
        b       LStrECopyAlignedLoop
LStrECopyEndFound:
        { adjust for possible $01 bytes coming before the terminating 0 byte }
	rlwinm	r8,r0,7,0,31
	andc	r10,r10,r8
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r3,r3,3
LStrECopyWrapUpLoop:
        subic.  r10,r10,8
        rlwinm  r0,r0,8,0,31
        stbu    r0,1(r3)
        bge     LStrECopyWrapUpLoop
LStrECopyDone:
        {  r3 contains new dest here  }
end ['R3','R4','R8','R0','R3','R9','R10','CR0','CTR'];


function strlcopy(dest,source : pchar;maxlen : longint) : pchar;assembler;
{ in: dest in r3, source in r4, maxlen in r5 }
{ out: result (dest) in r3                   }
asm
        {  load the begin of the source string in the data cache }
        dcbt    0,r4
        mtctr   r5
        subi    r4,r4,1
        subi    r10,r3,1
LStrlCopyLoop:
        lbzu    r0,1(r4)
        cmplwi  r0,0
        stbu    r0,1(r10)
        bdnzf   cr0*4+eq, LStrlCopyLoop
        { if we stopped because we copied a #0, we're done }
        beq     LStrlCopyDone
        { otherwise add the #0 }
        li      r0,0
        stb     r0,1(r10)
LStrlCopyDone:
end ['R0','R4','R10','CR0'];


function strlen(p : pchar) : longint;assembler;
{$i strlen.inc}

function strend(p : pchar) : pchar;assembler;
{ in: p in r3                  }
{ out: result (end of p) in r3 }
asm
        {  load the begin of the string in the data cache }
        dcbt    0,r3
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     LStrEndDone
        subi    r3,r3,1
LStrEndLoop:
        lbzu    r0,1(r3)
        cmplwi  r0,0
        bne     LStrEndLoop
LStrEndDone:
end ['R0','R3','R4','CR0'];


function strcomp(str1,str2 : pchar) : longint;assembler;
{ in: str1 in r3, str2 in r4                                                }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
        { use r0 instead of r3 for str1 since r3 contains result }
        subi    r9,r3,1
        subi    r4,r4,1
LStrCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars not equal, we're ready }
        bne     LStrCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop                                                    }
        bne     cr1,LStrCompLoop
LStrCompDone:
end ['R0','R3','R4','R9','R10','CR0','CR1'];


function strlcomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as strcomp, but maximally compare until l'th character)             }
{ in: str1 in r3, str2 in r4, l in r5                                       }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
        { load the begin of one of the strings in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for str1 since r3 contains result }
        cmplwi  r5,0
        subi    r9,r3,1
        li      r3,0
        beq     LStrlCompDone
        mtctr   r5
        subi    r4,r4,1
LStrlCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars not equal, we're ready }
        bne     LStrlCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzf  cr1*4+eq,LStrlCompLoop
LStrlCompDone:
end ['R0','R3','R4','R9','R10','CR0','CR1','CTR'];


function stricomp(str1,str2 : pchar) : longint;assembler;
{ in: str1 in r3, str2 in r4                                 }
{ out: result of case insensitive comparison (< 0, = 0, > 0) }
asm
        { use r28 instead of r3 for str1 since r3 contains result }
        subi    r28,r3,1
        subi    r4,r4,1
LStriCompLoop:
        { load next chars }
        lbzu    r29,1(r28)
        { check if one is zero }
        cmplwi  cr1,r29,0
        lbzu    r30,1(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars are equal, no further test is necessary }
        beq+    LStriCompEqual

        { make both lowercase, no branches }
        li       r27,0
        li       r25,0

        { r3 := r29 - 'A' }
        subic    r3,r29,'A'
        { if r29 < 'A' then r27 := 0 else r27 := $ffffffff }
        addme    r27,r27
        { same for r30 }
        subic    r3,r30,'A'
        addme    r25,r25

        { r3 := 'Z' - r29 }
        subfic   r3,r29,'Z'
        { if r29 < 'A' then r27 := 0 else r27 := $20 }
        andi.    r27,r27,0x020
        { if r29 > Z then r26 := 0 else r26 := $ffffffff }
        subfe    r26,r26,r26
        { same for r30 }
        subfic   r3,r30,'Z'
        andi.    r25,r25,0x020
        subfe    r24,r24,r24

        { if (r29 in ['A'..'Z'] then r27 := $20 else r27 := 0 }
        and      r27,r27,r26
        { same for r30 }
        and      r25,r25,r24

        { make lowercase }
        add      r29,r29,r27
        { same for r30 }
        add      r30,r30,r25

        { compare again }
        sub.     r3,r29,r30
        bne      LStriCompDone
LStriCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop                                                    }
        bne     cr1,LStriCompLoop
LStriCompDone:
end ['R3','R4','R26','R27','R28','R29','R30','CR0','CR1'];


function strlicomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as stricomp, but maximally compare until l'th character) }
{ in: str1 in r3, str2 in r4, l in r5                            }
{ out: result of case insensitive comparison (< 0, = 0, > 0)     }
asm
        {  load the begin of one of the string in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for str1 since r3 contains result }
        cmplwi  r5,0
        subi    r9,r3,1
        li      r3,0
        beq-    LStrliCompDone
        mtctr   r5
        subi    r4,r4,1
LStrliCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars are equal, no further test is necessary }
        beq     LStrliCompEqual

        { see stricomp for explanation }
        li       r8,0
        li       r5,0

        subic    r3,r0,'A'
        addme    r8,r8
        subic    r3,r10,'A'
        addme    r5,r5

        subfic   r3,r0,'Z'
        andi.    r8,r8,0x020
        subfe    r7,r7,r7
        subfic   r3,r10,'Z'
        andi.    r5,r5,0x020
        subfe    r24,r24,r24

        and      r8,r8,r7
        and      r5,r5,r24
        add      r0,r0,r8
        add      r10,r10,r5

        { compare again }
        sub.     r3,r0,r10
        bne      LStrliCompDone
LStrliCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzf    cr1*4+eq,LStrliCompLoop
LStrliCompDone:
end ['R0','R3','R4','R5','R7','R8','R9','R10','CR0','CR1','CTR'];


function strscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     LStrScanDone
        subi    r3,r3,1
LStrScanLoop:
        lbzu    r0,1(r3)
        cmplwi  r0,0
        cmplw   cr1,r0,r4
        bne     LStrScanLoop
        beq     cr1,LStrScanDone
        li      r3, 0
LStrScanDone:
end ['R0','R3','R4','CR0','CR1'];


function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     LStrrScanDone
        { make r5 will be walking through the string }
        subi    r5,r3,1
        { assume not found }
        li      r3,0
LStrrScanLoop:
        lbzu    r10,1(r5)
        cmplw   cr1,r10,r4
        cmplwi  cr0,r10,0
        bne+    cr1,LStrrScanNotFound
        { store address of found position }
        mr      r3,r5
LStrrScanNotFound:
        bne     LStrrScanLoop
LStrrScanDone:
end ['R0','R3','R4','R10','CR0','CR1'];


function strupper(p : pchar) : pchar;assembler;
asm
        cmplwi  r3,0
        beq     LStrUpperNil
        subi    r9,r3,1
LStrUpperLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,97
        cmplwi  r0,122-97
        cmplwi  cr1,r10,0
        subi    r10,r10,0x20
        bgt     LStrUpper1
        stb     r10,0(r9)
LStrUpper1:
        bne     cr1,LStrUpperLoop
LStrUpperNil:
end ['R0','R9','R10','CR0','CR1'];


function strlower(p : pchar) : pchar;assembler;
asm
        cmplwi  r3,0
        beq     LStrLowerNil
        subi    r9,r3,1
LStrLowerLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,65
        cmplwi  r0,90-65
        cmplwi  cr1,r10,0
        addi    r10,r10,0x20
        bgt     LStrLower1
        stb     r10,0(r9)
LStrLower1:
        bne     cr1,LStrLowerLoop
LStrLowerNil:
end ['R0','R9','R10','CR0','CR1'];


{
  $Log$
  Revision 1.18  2003-05-28 19:18:10  jonas
    * fixed strcopy and strecopy if there are #1 chars right before the end
      of the string to copied

  Revision 1.17  2003/05/24 10:16:24  jonas
    * fixed strscan and strrscan

  Revision 1.16  2003/05/17 12:55:30  florian
    * fixed copy&paste bug in strecopy

  Revision 1.15  2003/05/17 00:01:13  jonas
    * fixed strcopy

  Revision 1.14  2002/09/11 07:49:40  jonas
    * fixed assembler errors

  Revision 1.13  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.12  2002/09/06 16:58:43  jonas
    * fixed wrong references (used r0 as base register)

  Revision 1.11  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

}
