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
        cmpli   cr0,r0,0
        {  store byte  }
        stbu    r0,1(r9)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   eq,LStrCopyAlignLoop
        beq     LStrCopyDone
LStrCopyAligned:
        subi    r4,r4,3
        subi    r9,r9,3
        { setup magic constants }
        li      r8,0x0feff
        addis   r8,r8,0x0feff
        li      r7,0x08080
        addis    r7,r7,0x08081

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
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r9,r9,3
LStrCopyWrapUpLoop:
        subi    r10,r10,8
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
        beq     LStrCopyAligned
LStrCopyAlignLoop:
        {  load next byte  }
        lbzu    r0,1(r4)
        {  end of string?  }
        cmpli   cr0,r0,0
        {  store byte  }
        stbu    r0,1(r3)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   eq,LStrCopyAlignLoop
        beq     LStrCopyDone
LStrCopyAligned:
        subi    r4,r4,3
        subi    r3,r3,3
        { setup magic constants }
        li      r8,0x0feff
        addis   r8,r8,0x0feff
        li      r9,0x08080
        addis    r9,r9,0x08081
LStrCopyAlignedLoop:

        {  load next 4 bytes  }
        lwzu    r0,4(r4)

        { test for zero byte }
        add     r10,r0,r8
        andc    r10,r10,r0
        and.    r10,r10,r9
        bne     LStrCopyEndFound
        stwu    r0,4(r3)
        b       LStrCopyAlignedLoop
LStrCopyEndFound:
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r3,r3,3
LStrCopyWrapUpLoop:
        subic.  r10,r10,8
        rlwinm  r0,r0,8,0,31
        stbu    r0,1(r3)
        bge     LStrCopyWrapUpLoop
LStrCopyDone:
        {  r3 contains new dest here  }
end ['R3','R4','R8','R0','R3','R10','CR0','CTR'];


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
        cmpli   r0,0
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
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrEndDone
        subi    r3,r3,1
LStrEndLoop:
        lbzu    r0,1(r3)
        cmpli   r0,0
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
        cmpli   cr1,r0,0
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
        cmpl    r5,0
        subi    r9,r3,1
        li      r3,0
        beq     LStrlCompDone
        mtctr   r5
        subi    r4,r4,1
LStrlCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmpli   cr1,r0,0
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
        cmpli   cr1,r29,0
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
        andi     r27,r27,0x020
        { if r29 > Z then r26 := 0 else r26 := $ffffffff }
        subfe    r26,r26,r26
        { same for r30 }
        subfic   r3,r30,'Z'
        andi     r25,r25,0x020
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
        bne      LStrCompDone
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
        cmpl    r5,0
        subi    r9,r3,1
        li      r3,0
        beq-    LStrlCompDone
        mtctr   r5
        subi    r4,r4,1
LStriCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmpli   cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars are equal, no further test is necessary }
        beq     LStriCompEqual

        { see stricomp for explanation }
        li       r8,0
        li       r5,0

        subic    r3,r0,'A'
        addme    r8,r8
        subic    r3,r10,'A'
        addme    r5,r5

        subfic   r3,r0,'Z'
        andi     r8,r8,0x020
        subfe    r7,r7,r7
        subfic   r3,r10,'Z'
        andi     r5,r5,0x020
        subfe    r24,r24,r24

        and      r8,r8,r7
        and      r5,r5,r24
        add      r0,r0,r8
        add      r10,r10,r5

        { compare again }
        sub.     r3,r0,r10
        bne      LStrCompDone
LStriCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzf    cr1*4+eq,LStriCompLoop
LStriCompDone:
end ['R0','R3','R4','R5','R7','R8','R9','R10','CR0','CR1','CTR'];


function strscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrScanDone
        subi    r3,r3,1
LStrScanLoop:
        lbzu    r0,1(r3)
        cmpl    cr1,r0,r4
        cmpli   r0,0
        beq     cr1,LStrScanDone
        bne     LStrScanLoop
LStrScanDone:
end ['R0','R3','R4','CR0','CR1'];


function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrrScanDone
        { make r0 $ffffffff, later on we take min(r0,r3) }
        li      r0,0x0ffff
        subi    r3,r3,1
LStrrScanLoop:
        lbzu    r10,1(r3)
        cmpl    cr1,r10,r4
        cmpli   cr0,r10,0
        bne+    cr1,LStrrScanNotFound
        { store address of found position }
        mr      r0,r3
LStrrScanNotFound:
        bne     LStrrScanLoop
        { Select min of r3 and r0 -> end of string or found position    }
        { From the PPC compiler writer's guide, not sure if I could ever }
        { come up with something like this :)                            }

        subfc   r10,r3,r0   { r10 = r0 - r3, CA = (r0 >= r3) ? 1 : 0 }
        subfe   r0,r0,r0    { r0' = (r0 >= r3) ? 0 : -1              }
        and     r10,r10,r0  { r10 = (r0 >= r3) ? 0 : r0 - r3         }
        add     r3,r10,r3   { r3  = (r0 >= r3) ?  r3 : r0            }
LStrrScanDone:
end ['R0','R3','R4','R10','CR0','CR1'];


function strupper(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrUpperNil
        subi    r9,r3,1
LStrUpperLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,97
        cmpli   r0,122-97
        cmpli   cr1,r10,0
        subi    r10,r10,0x20
        bgt     LStrUpper1
        stb     r10,0(r9)
LStrUpper1:
        bne     cr1,LStrUpperLoop
LStrUpperNil:
end ['R0','R9','R10','CR0','CR1'];


function strlower(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrLowerNil
        subi    r9,r3,1
LStrLowerLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,65
        cmpli   r0,90-65
        cmpli   cr1,r10,0
        addi    r10,r10,0x20
        bgt     LStrLower1
        stb     r10,0(r9)
LStrLower1:
        bne     cr1,LStrLowerLoop
LStrLowerNil:
end ['R0','R9','R10','CR0','CR1'];


{
  $Log$
  Revision 1.12  2002-09-06 16:58:43  jonas
    * fixed wrong references (used r0 as base register)

  Revision 1.11  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.10  2001/09/28 13:25:04  jonas
    * fixed wrong alignment code (sometimes we aligned to multiple of 8
      instead of the desired multiple of 4)

  Revision 1.9  2001/09/27 15:30:29  jonas
    * conversion to compilerproc and to structure used by i386 rtl
    * some bugfixes
    * powerpc.inc is almost complete (only fillchar/word/dword, get_frame etc
      and the class helpers are still needed
    - removed unnecessary register saving in set.inc (thanks to compilerproc)
    * use registers reserved for parameters as much as possible instead of
      those reserved for local vars (since those have to be saved by the
      called anyway, while the ones for local vars have to be saved by the
      callee)

  Revision 1.8  2001/07/21 15:51:50  jonas
    * fixed small bug in stricomp

  Revision 1.7  2001/07/07 12:46:12  jonas
    * some small bugfixes and cache optimizations

  Revision 1.6  2001/02/23 14:05:33  jonas
    * optimized strcopy/strecopy

  Revision 1.5  2001/02/11 17:59:14  jonas
    * fixed bug in strscan

  Revision 1.4  2001/02/11 12:15:03  jonas
    * some small optimizations and bugfixes

  Revision 1.3  2001/02/10 16:09:43  jonas
   + implemented all missing routines and changed reg allocation to follow ABI

  Revision 1.2  2001/02/10 12:28:22  jonas
    * fixed some bugs, simplified/optimized already implemented routines and code some more

  Revision 1.1  2000/11/05 17:17:08  jonas
    + first implementation, not yet finished

}
