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
        subi    r4,r4,1
        subi    r29,r3,1
LStrCopyLoop:
        lbzu    r30,1(r4)
        cmpli   r30,0
        stbu    r30,1(r29)
        bne     LStrCopyLoop
end ['r4','r29','r30','cr0'];


function strecopy(dest,source : pchar) : pchar;assembler;
{ in: dest in r3, source in r4        }
{ out: result (end of new dest) in r3 }
asm
        subi    r4,r4,1
        subi    r3,r3,1
LStreCopyLoop:
        lbzu    r30,1(r4)
        cmpli   r30,0
        stbu    r30,1(r3)
        bne     LStreCopyLoop
end ['r3','r4','r30','cr0'];


function strlcopy(dest,source : pchar;maxlen : longint) : pchar;assembler;
{ in: dest in r3, source in r4, maxlen in r5 }
{ out: result (dest) in r3                   }
asm
        mtctr   r5
        subi    r4,r4,1
        subi    r29,r3,1
LStrlCopyLoop:
        lbzu    r30,1(r4)
        cmpli   r30,0
        stbu    r30,1(r29)
        bdnzne  LStrlCopyLoop
        { if we stopped because we copied a #0, we're done }
        beq     LStrlCopyDone
        { otherwise add the #0 }
        li      r30,0
        stb     r30,1(r29)
LStrlCopyDone:
end ['r4','r29','r30','cr0'];


function strlen(p : pchar) : longint;assembler;
{ in: p in r3                }
{ out: result (length) in r3 }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrLenDone
        subi    r29,r3,1
LStrLenLoop:
        lbzu    r30,1(r29)
        cmpli   r30,0
        bne     LStrLenLoop
        sub     r3,r29,r3
LStrLenDone:
end ['r3','r4','r29','r30','cr0'];


function strend(p : pchar) : pchar;assembler;
{ in: p in r3                  }
{ out: result (end of p) in r3 }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrEndDone
        subi    r3,r3,1
LStrEndLoop:
        lbzu    r30,1(r3)
        cmpli   r30,0
        bne     LStrEndLoop
LStrEndDone:
end ['r3','r4','r30','cr0'];


function strcomp(str1,str2 : pchar) : longint;assembler;
{ in: str1 in r3, str2 in r4                                                }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
        { use r28 instead of r3 for str1 since r3 contains result }
        subi    r28,r3,1
        subi    r4,r4,1
LStrCompLoop:
        { load next chars }
        lbzu    r29,1(r28)
        { check if one is zero }
        cmpli   cr1,r29,0
        lbzu    r30,1(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars not equal, we're ready }
        bne     LStrCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop                                                    }
        bne     cr1,LStrCompLoop
LStrCompDone:
end ['r3','r4','r28','r29','r30','cr0','cr1'];


function strlcomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as strcomp, but maximally compare until l'th character)             }
{ in: str1 in r3, str2 in r4, l in r5                                       }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
        { use r28 instead of r3 for str1 since r3 contains result }
        cmpl    r5,0
        subi    r28,r3,1
        li      r3,0
        beq     LStrlCompDone
        mtctr   r5
        subi    r4,r4,1
LStrlCompLoop:
        { load next chars }
        lbzu    r29,1(r28)
        { check if one is zero }
        cmpli   cr1,r29,0
        lbzu    r30,1(r4)
        { calculate difference }
        sub.    r3,r29,r30
        { if chars not equal, we're ready }
        bne     LStrlCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzne  cr1,LStrlCompLoop
LStrlCompDone:
end ['r3','r4','r28','r29','r30','cr0','cr1','ctr'];


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
        { if r29 > Z then r26 := 0 else r26 := $ffffffff
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
        bne     LStriCompLoop
LStriCompDone:
end ['r3','r4','r26','r27','r28','r29','r30','cr0','cr1'];


function strlicomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as stricomp, but maximally compare until l'th character) }
{ in: str1 in r3, str2 in r4, l in r5                            }
{ out: result of case insensitive comparison (< 0, = 0, > 0)     }
asm
        { use r28 instead of r3 for str1 since r3 contains result }
        cmpl    r5,0
        subi    r28,r3,1
        li      r3,0
        beq     LStrlCompDone
        mtctr   r5
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

        { see stricomp for explanation }
        li       r27,0
        li       r25,0

        subic    r3,r29,'A'
        addme    r27,r27
        subic    r3,r30,'A'
        addme    r25,r25

        subfic   r3,r29,'Z'
        andi     r27,r27,0x020
        subfe    r26,r26,r26
        subfic   r3,r30,'Z'
        andi     r25,r25,0x020
        subfe    r24,r24,r24

        and      r27,r27,r26
        and      r25,r25,r24
        add      r29,r29,r27
        add      r30,r30,r25

        { compare again }
        sub.     r3,r29,r30
        bne      LStrCompDone
LStriCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzne  cr1,LStriCompLoop
LStriCompDone:
end ['r3','r4','r26','r27','r28','r29','r30','cr0','cr1','ctr'];


function strscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrScanDone
        subi    r3,r3,1
LStrScanLoop:
        lbzu    r30,1(r3)
        cmpl    cr1,r30,r4
        cmpli   r30,0
        beq     cr1,LStrScanDone
        bne     LStrScanLoop
LStrScanDone:
end ['r3','r4','r30','cr0','cr1'];


function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrrScanDone
        { make r29 $ffffffff, later on we take min(r29,r3) }
        li      r29,0x0ffff
        subi    r3,r3,1
LStrrScanLoop:
        lbzu    r30,1(r3)
        cmpl    cr1,r30,r4
        cmpli   cr0,r30,0
        bne+    cr1,LStrrScanNotFound
        { store address of found position }
        mr      r29,r3
LStrrScanNotFound:
        bne     LStrrScanLoop
        { Select min of r3 and r29 -> end of string or found position    }
        { From the PPC compiler writer's guide, not sure if I could ever }
        { come up with something like this :)                            }

        subfc   r30,r3,r29   { r30 = r29 - r3, CA = (r29 >= r3) ? 1 : 0 }
        subfe   r29,r29,r29  { r29' = (r29 >= r3) ? 0 : -1              }
        and     r30,r30,r29  { r30 = (r29 >= r3) ? 0 : r29 - r3         }
        add     r3,r30,r3    { r3  = (r29 >= r3) ?  r3 : r29            }
LStrrScanDone:
end ['r3','r4','r29','r30','cr0','cr1'];


function strupper(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrUpperNil
        subi    r29,r3,1
LStrUpperLoop:
        lbzu    r30,1(r29)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r28,r30,97
        cmpli   r28,122-97
        cmpli   cr1,r30,0
        subi    r30,r30,0x20
        bgt     LStrUpper1
        stb     r30,0(r29)
LStrUpper1:
        bne     cr1,LStrUpperLoop
LStrUpperNil:
end ['r28','r29','r30','cr0','cr1'];


function strlower(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrLowerNil
        subi    r29,r3,1
LStrLowerLoop:
        lbzu    r30,1(r29)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r28,r30,65
        cmpli   r28,90-65
        cmpli   cr1,r30,0
        addi    r30,r30,0x20
        bgt     LStrLower1
        stb     r30,0(r29)
LStrLower1:
        bne     cr1,LStrLowerLoop
LStrLowerNil:
end ['r28','r29','r30','cr0','cr1'];


{
  $Log$
  Revision 1.5  2001-02-11 17:59:14  jonas
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
