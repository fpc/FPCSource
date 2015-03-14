{
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

{$ifdef FPC_BIG_ENDIAN}

{ Note: the implementation of these routines is for BIG ENDIAN only!! (JM) }

{$ifndef FPC_UNIT_HAS_STRCOPY}
{$define FPC_UNIT_HAS_STRCOPY}
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
        beq     .LStrCopyAligned
.LStrCopyAlignLoop:
        {  load next byte  }
        lbzu    r0,1(r4)
        {  end of string?  }
        cmplwi  cr0,r0,0
        {  store byte  }
        stbu    r0,1(r9)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   cr0*4+eq,.LStrCopyAlignLoop
        beq     .LStrCopyDone
.LStrCopyAligned:
        subi    r4,r4,3
        subi    r9,r9,3
        { setup magic constants }
        {$if defined(macos) or defined(aix)}
        {  load constant 0xfefefeff }
        lis     r8,0xfefe
        ori     r8,r8,0xfeff
        {  load constant 0x80808080}
        lis     r7,0x8080
        ori     r7,r7,0x8080
        {$else}
        lis     r8,(0xfefefeff)@ha
        addi    r8,r8,(0xfefefeff)@l
        lis     r7,(0x80808080)@ha
        addi    r7,r7,(0x80808080)@l
        {$endif}
        { load first 4 bytes  }
        lwzu    r0,4(r4)

.LStrCopyAlignedLoop:
        { test for zero byte }
        add     r10,r0,r8
        andc    r10,r10,r0
        and.    r10,r10,r7
        bne     .LStrCopyEndFound
        stwu    r0,4(r9)
        { load next 4 bytes (do it here so the load can begin while the }
        { the branch is processed)                                      }
        lwzu    r0,4(r4)
        b       .LStrCopyAlignedLoop
.LStrCopyEndFound:
        { adjust for possible $01 bytes coming before the terminating 0 byte }
        rlwinm  r8,r0,7,0,31
        andc    r10,r10,r8
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r9,r9,3
.LStrCopyWrapUpLoop:
        subic.  r10,r10,8
        rlwinm  r0,r0,8,0,31
        stbu    r0,1(r9)
        bge     .LStrCopyWrapUpLoop
.LStrCopyDone:
        {  r3 still contains dest here  }
end;
{$endif FPC_UNIT_HAS_STRCOPY}


{$ifndef FPC_UNIT_HAS_STRECOPY}
{$define FPC_UNIT_HAS_STRECOPY}
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
        beq     .LStrECopyAligned
.LStrECopyAlignLoop:
        {  load next byte  }
        lbzu    r0,1(r4)
        {  end of string?  }
        cmplwi  cr0,r0,0
        {  store byte  }
        stbu    r0,1(r3)
        {  loop if misaligned bytes left and not end of string found }
        bdnzf   cr0*4+eq,.LStrECopyAlignLoop
        beq     .LStrECopyDone
.LStrECopyAligned:
        subi    r4,r4,3
        subi    r3,r3,3
        { setup magic constants }
        {$if defined(macos) or defined(aix)}
        {  load constant 0xfefefeff }
        lis     r8,0xfefe
        ori     r8,r8,0xfeff
        {  load constant 0x80808080}
        lis     r7,0x8080
        ori     r7,r7,0x8080
        {$else}
        lis     r8,(0xfefefeff)@ha
        addi    r8,r8,(0xfefefeff)@l
        lis     r7,(0x80808080)@ha
        addi    r7,r7,(0x80808080)@l
        {$endif}
.LStrECopyAlignedLoop:

        {  load next 4 bytes  }
        lwzu    r0,4(r4)

        { test for zero byte }
        add     r10,r0,r8
        andc    r10,r10,r0
        and.    r10,r10,r7
        bne     .LStrECopyEndFound
        stwu    r0,4(r3)
        b       .LStrECopyAlignedLoop
.LStrECopyEndFound:
        { adjust for possible $01 bytes coming before the terminating 0 byte }
        rlwinm  r8,r0,7,0,31
        andc    r10,r10,r8
        { result is either 0, 8, 16 or 24 depending on which byte is zero }
        cntlzw  r10,r10
        addi    r3,r3,3
.LStrECopyWrapUpLoop:
        subic.  r10,r10,8
        rlwinm  r0,r0,8,0,31
        stbu    r0,1(r3)
        bge     .LStrECopyWrapUpLoop
.LStrECopyDone:
        {  r3 contains new dest here  }
end;
{$endif FPC_UNIT_HAS_STRECOPY}

{$endif FPC_BIG_ENDIAN}


{$ifndef FPC_UNIT_HAS_STRLCOPY}
{$define FPC_UNIT_HAS_STRLCOPY}
function strlcopy(dest,source : pchar;maxlen : int64) : pchar;assembler;
{ in: dest in r3, source in r4, maxlen in r5 }
{ out: result (dest) in r3                   }
asm
        {  load the begin of the source string in the data cache }
        dcbt    0,r4
        mtctr   r5
        subi    r4,r4,1
        subi    r10,r3,1
.LStrlCopyLoop:
        lbzu    r0,1(r4)
        cmplwi  r0,0
        stbu    r0,1(r10)
        bdnzf   cr0*4+eq, .LStrlCopyLoop
        { if we stopped because we copied a #0, we're done }
        beq     .LStrlCopyDone
        { otherwise add the #0 }
        li      r0,0
        stb     r0,1(r10)
.LStrlCopyDone:
end;
{$endif FPC_UNIT_HAS_STRLCOPY}


{$ifndef FPC_UNIT_HAS_STREND}
{$define FPC_UNIT_HAS_STREND}
function strend(p : pchar) : pchar;assembler;
{ in: p in r3                  }
{ out: result (end of p) in r3 }
asm
        {  load the begin of the string in the data cache }
        dcbt    0,r3
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     .LStrEndDone
        subi    r3,r3,1
.LStrEndLoop:
        lbzu    r0,1(r3)
        cmplwi  r0,0
        bne     .LStrEndLoop
.LStrEndDone:
end;
{$endif FPC_UNIT_HAS_STREND}


{$ifndef FPC_UNIT_HAS_STRCOMP}
{$define FPC_UNIT_HAS_STRCOMP}
function strcomp(str1,str2 : pchar) : int64;assembler;
{ in: str1 in r3, str2 in r4                                                }
{ out: result (= 0 if strings equal, < 0 if str1 < str2, > 0 if str1 > str2 }
{      in r3                                                                }
asm
        { use r0 instead of r3 for str1 since r3 contains result }
        subi    r9,r3,1
        subi    r4,r4,1
.LStrCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars not equal, we're ready }
        bne     .LStrCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop                                                    }
        bne     cr1,.LStrCompLoop
.LStrCompDone:
end;
{$endif FPC_UNIT_HAS_STRCOMP}


{$ifndef FPC_UNIT_HAS_STRLCOMP}
{$define FPC_UNIT_HAS_STRLCOMP}
function strlcomp(str1,str2 : pchar;l : int64) : int64;assembler;
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
        beq     .LStrlCompDone
        mtctr   r5
        subi    r4,r4,1
.LStrlCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars not equal, we're ready }
        bne     .LStrlCompDone
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzf  cr1*4+eq,.LStrlCompLoop
.LStrlCompDone:
end;
{$endif FPC_UNIT_HAS_STRLCOMP}


{$ifndef FPC_UNIT_HAS_STRICOMP}
{$define FPC_UNIT_HAS_STRICOMP}
function stricomp(str1,str2 : pchar) : int64;assembler;
{ in: str1 in r3, str2 in r4                                 }
{ out: result of case insensitive comparison (< 0, = 0, > 0) }
asm
        { use r5 instead of r3 for str1 since r3 contains result }
        subi    r5,r3,1
        subi    r4,r4,1
.LStriCompLoop:
        { load next chars }
        lbzu    r6,1(r5)
        { check if one is zero }
        cmplwi  cr1,r6,0
        lbzu    r7,1(r4)
        { calculate difference }
        sub.    r3,r6,r7
        { if chars are equal, no further test is necessary }
        beq+    .LStriCompEqual

        { make both lowercase, no branches }

        { r3 := pred('A') - r6 }
        subfic    r3,r6,64
        { if r6 < 'A' then r8 := 0 else r8 := $ffffffff }
        subfe    r8,r8,r8
        { same for r7 }
        subfic   r3,r7,64
        subfe    r9,r9,r9

        { r3 := r6 - succ('Z') }
        subic    r3,r6,91
        { if r6 < 'A' then r8 := 0 else r8 := $20 }
        andi.    r8,r8,0x020
        { if r6 > Z then r10 := 0 else r10 := $ffffffff }
        subfe    r10,r10,r10
        { same for r7 }
        subic    r3,r7,91
        andi.    r9,r9,0x020
        subfe    r11,r11,r11

        { if (r6 in ['A'..'Z'] then r8 := $20 else r8 := 0 }
        and      r8,r8,r10
        { same for r7 }
        and      r9,r9,r11

        { make lowercase }
        add      r6,r6,r8
        { same for r7 }
        add      r7,r7,r9

        { compare again }
        sub.     r3,r6,r7
        bne-      .LStriCompDone
.LStriCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop                                                    }
        bne     cr1,.LStriCompLoop
.LStriCompDone:
end;
{$endif FPC_UNIT_HAS_STRICOMP}


{$ifndef FPC_UNIT_HAS_STRLICOMP}
{$define FPC_UNIT_HAS_STRLICOMP}
function strlicomp(str1,str2 : pchar;l : int64) : int64;assembler;
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
        beq-    .LStrliCompDone
        mtctr   r5
        subi    r4,r4,1
.LStrliCompLoop:
        { load next chars }
        lbzu    r0,1(r9)
        { check if one is zero }
        cmplwi  cr1,r0,0
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r0,r10
        { if chars are equal, no further test is necessary }
        beq     .LStrliCompEqual

        { see stricomp for explanation }

        subfic   r3,r0,64
        subfe    r8,r8,r8
        subfic   r3,r10,64
        subfe    r5,r5,r5

        subic    r3,r0,91
        andi.    r8,r8,0x020
        subfe    r7,r7,r7
        subic    r3,r10,91
        andi.    r5,r5,0x020
        subfe    r11,r11,r11

        and      r8,r8,r7
        and      r5,r5,r11
        add      r0,r0,r8
        add      r10,r10,r5

        { compare again }
        sub.     r3,r0,r10
        bne      .LStrliCompDone
.LStrliCompEqual:
        { if they are equal and one is zero, then the other one is zero too }
        { and we're done as well (r3 also contains 0 then)                  }
        { otherwise loop (if ctr <> 0)                                      }
        bdnzf    cr1*4+eq,.LStrliCompLoop
.LStrliCompDone:
end;
{$endif FPC_UNIT_HAS_STRLICOMP}


{$ifndef FPC_UNIT_HAS_STRSCAN}
{$define FPC_UNIT_HAS_STRSCAN}
function strscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     .LStrScanDone
        subi    r3,r3,1
.LStrScanLoop:
        lbzu    r0,1(r3)
        cmplw   cr1,r0,r4
        cmplwi  r0,0
        beq     cr1,.LStrScanDone
        bne     .LStrScanLoop
        li      r3, 0
.LStrScanDone:
end;
{$endif FPC_UNIT_HAS_STRSCAN}


{$ifndef FPC_UNIT_HAS_STRRSCAN}
{$define FPC_UNIT_HAS_STRRSCAN}
function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        { empty/invalid string? }
        cmplwi  r3,0
        { if yes, do nothing }
        beq     .LStrrScanDone
        { make r5 will be walking through the string }
        subi    r5,r3,1
        { assume not found }
        li      r3,0
.LStrrScanLoop:
        lbzu    r10,1(r5)
        cmplw   cr1,r10,r4
        cmplwi  cr0,r10,0
        bne+    cr1,.LStrrScanNotFound
        { store address of found position }
        mr      r3,r5
.LStrrScanNotFound:
        bne     .LStrrScanLoop
.LStrrScanDone:
end;
{$endif FPC_UNIT_HAS_STRRSCAN}


{$ifndef FPC_UNIT_HAS_STRUPPER}
{$define FPC_UNIT_HAS_STRUPPER}
function strupper(p : pchar) : pchar;assembler;
asm
        cmplwi  r3,0
        beq     .LStrUpperNil
        subi    r9,r3,1
.LStrUpperLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,97
        cmplwi  r0,122-97
        cmplwi  cr1,r10,0
        subi    r10,r10,0x20
        bgt     .LStrUpper1
        stb     r10,0(r9)
.LStrUpper1:
        bne     cr1,.LStrUpperLoop
.LStrUpperNil:
end;
{$endif FPC_UNIT_HAS_STRUPPER}


{$ifndef FPC_UNIT_HAS_STRLOWER}
{$define FPC_UNIT_HAS_STRLOWER}
function strlower(p : pchar) : pchar;assembler;
asm
        cmplwi  r3,0
        beq     .LStrLowerNil
        subi    r9,r3,1
.LStrLowerLoop:
        lbzu    r10,1(r9)
        { a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
        subi    r0,r10,65
        cmplwi  r0,90-65
        cmplwi  cr1,r10,0
        addi    r10,r10,0x20
        bgt     .LStrLower1
        stb     r10,0(r9)
.LStrLower1:
        bne     cr1,.LStrLowerLoop
.LStrLowerNil:
end;
{$endif FPC_UNIT_HAS_STRLOWER}

