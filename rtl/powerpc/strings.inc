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
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrCopyDone
        subi    r4,r4,1
        subi    r9,r3,1
LStrCopyLoop:
        lbzu    r10,1(r4)
        cmpli   r10,0
        stbu    r10,1(r9)
        bne     LStrCopyLoop
LStrCopyDone:
end ['r4','r9','r10','cr0'];


function strecopy(dest,source : pchar) : pchar;assembler;
{ in: dest in r3, source in r4        }
{ out: result (end of new dest) in r3 }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStreCopyDone
        subi    r4,r4,1
        subi    r3,r3,1
LStreCopyLoop:
        lbzu    r10,1(r4)
        cmpli   r10,0
        stbu    r10,1(r3)
        bne     LStreCopyLoop
LStreCopyDone:
end ['r3','r4','r10','cr0'];


function strlcopy(dest,source : pchar;maxlen : longint) : pchar;assembler;
{ in: dest in r3, source in r4, maxlen in r5 }
{ out: result (dest) in r3                   }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrCopyDone
        mtctr   r5
        subi    r4,r4,1
        subi    r9,r3,1
LStrlCopyLoop:
        lbzu    r10,1(r4)
        cmpli   r10,0
        stbu    r10,1(r9)
        bdnzne  LStrlCopyLoop
        beq     LStrlCopyDone
        li      r10,0
        stb     r10,1(r9)
LStrlCopyDone:
end ['r4','r9','r10','cr0'];


function strlen(p : pchar) : longint;assembler;
{ in: p in r3                }
{ out: result (length) in r3 }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrLenDone
        subi    r9,r3,1
LStrLenLoop:
        lbzu    r10,1(r9)
        cmpli   r10,0
        bne     LStrLenLoop
        sub     r3,r9,r3
LStrLenDone:
end ['r3','r4','r9','r10','cr0'];


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
        lbzu    r10,1(r3)
        cmpli   r10,0
        bne     LStrEndLoop
LStrEndDone:
end ['r3','r4','r10','cr0'];


function strcomp(str1,str2 : pchar) : longint;assembler;
{ in: str1 in r3, str2 in r4                                                }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
  { !!! }
end;


function strlcomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as strcomp, but maximally compare until l'th character)             }
{ in: str1 in r3, str2 in r4, l in r5                                       }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
  { !!! }
end;


function stricomp(str1,str2 : pchar) : longint;assembler;
{ in: str1 in r3, str2 in r4                               }
{ out: result (= index of first differing character) in r3 }
asm
  { !!! }
end;


function strlicomp(str1,str2 : pchar;l : longint) : longint;assembler;
{ (same as stricomp, but maximally compare until l'th character) }
{ in: str1 in r3, str2 in r4, l in r5                            }
{ out: result (= index of first differing character) in r3       }
asm
  { !!! }
end;


function strscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrScanDone
        subi    r3,r3,1
LStrScanLoop:
        lbzu    r10,1(r3)
        cmpl    r10,r4
        bne     LStrScanLoop
LStrScanDone:
end ['r3','r4','r10','cr0'];


function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrrScanDone
        { make r9 $ffffffff, later on we take min(r9,r3) }
        li      r9,0x0ffff
        subi    r3,r3,1
LStrrScanLoop:
        lbzu    r10,1(r3)
        cmpl    cr1,r10,r4
        cmpli   cr0,r10,0
        bne+    cr1,LStrrScanNotFound
        { store address of found position }
        mr      r9,r3
LStrrScanNotFound:
        bne     LStrrScanLoop
        { Select min of r3 and r9 -> end of string or found position     }
        { From the PPC compiler writer's guide, not sure if I could ever }
        { come up with something like this :)                            }

        subfc   r10,r3,r9   { r10 = r9 - r3, CA = (r9 >= r3) ? 1 : 0 }
        subfe   r9,r9,r9    { r9' = (r9 >= r3) ? 0 : -1              }
        and     r10,r10,r9  { r10 = (r9 >= r3) ? 0 : r9 - r3         }
        add     r3,r10,r3   { r3  = (r9 >= r3) ?  r3 : r9            }
LStrrScanDone:
end ['r3','r4','r9','r10','cr0','cr1'];


function strupper(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrUpperNil
        subi    r9,r3,1
LStrUpperLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r8,r10,97
        cmpli   r8,122-97
        cmpli   cr1,r10,0
        subi    r10,r10,0x20
        bgt     LStrUpper1
        stb     r10,0(r9)
LStrUpper1:
        bne     cr1,LStrUpperLoop
LStrUpperNil:
end ['r8','r9','r10','cr0','cr1'];


function strlower(p : pchar) : pchar;assembler;
asm
        cmpli   r3,0
        beq     LStrLowerNil
        subi    r9,r3,1
LStrLowerLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r8,r10,65
        cmpli   r8,90-65
        cmpli   cr1,r10,0
        addi    r10,r10,0x20
        bgt     LStrLower1
        stb     r10,0(r9)
LStrLower1:
        bne     cr1,LStrLowerLoop
LStrLowerNil:
end ['r8','r9','r10','cr0','cr1'];


{
  $Log$
  Revision 1.2  2001-02-10 12:28:22  jonas
    * fixed some bugs, simplified/optimized already implemented routines and code some more

  Revision 1.1  2000/11/05 17:17:08  jonas
    + first implementation, not yet finished

}
