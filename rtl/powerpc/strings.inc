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
        beq     .LStrCopyDone
        { clear two lowest bits of source address }
        rlwminm r28,r4,0,0,31-2
        { get # of misaligned bytes }
        sub.    r28,r28,r4
        { since we have to return dest intact, use another register for }
        { dest in the copy loop                                         }
        mr      r29,r3
        beq     .LStrCopyAligned
.LStrCopyAlignLoop:
        { decrease misaligned bytes counter (do it here already to improve }
        { jump prediction)                                                 }
        subic.  r28,1
        { load next byte }
        lbz     r27,(r4)
        { end of string? }
        cmpli   cr1,r27,0
        { point to next source byte }
        addi    r4,r4,1
        { store byte }
        stb     r27,(r29)
        { point to next dest address }
        addi    r29,r29,1
        { stop if end of string }
        beq     cr1,.LStrCopyDone
        bne     .LStrCopyAlignLoop
        .balign  16
.LStrCopyAligned:
        { load next 4 bytes }
        lwz     r27,(r4)
        { first/highest byte zero? (big endian!) }
        andis.  r28,r27,0x0ff00
        addi    r4,r4,4
        beq     .LStrCopyByte
        { second byte zero? }
        andis.  r28,r27,0x00ff
        beq     .LStrCopyWord
        { third byte zero? }
        andi.   r28,r27,0xff00
        beq     .LStrCopy3Bytes
        { fourth byte zero? }
        andi.   r28,r27,0x00ff
        { store next 4 bytes }
        stw     r27,(r29)
        { increase dest address }
        addi    r29,r29,4
        beq     .LStrCopyDone
        b       .LStrCopyAligned
{ store left-overs }
.LStrCopy3Bytes:
        sth      r27,(r29)
        li       r27,0
        stb      r27,2(r29)
        b        .LStrCopyDone
.LStrCopyWord:
        sth      r27,(r29)
        b        .LStrCopyDone
.LStrCopyByte:
        stb      r27,(r29)
.LStrCopyDone:
        { r3 still contains dest here }
end ['r4','r27','r28','r29','cr0','cr1'];


function strecopy(dest,source : pchar) : pchar;assembler;
{ in: dest in r3, source in r4        }
{ out: result (end of new dest) in r3 }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     .LStreCopyDone
        { clear two lowest bits of source address }
        rlwminm r28,r4,0,0,31-2
        { get # of misaligned bytes }
        sub.    r28,r28,r4
        beq     .LStreCopyAligned
.LStreCopyAlignLoop:
        { decrease misaligned bytes counter (do it here already to improve }
        { jump prediction)                                                 }
        subic.  r28,1
        { load next byte }
        lbz     r27,(r4)
        { end of string? }
        cmpli   cr1,r27,0
        { point to next source byte }
        addi    r4,r4,1
        { store byte }
        stb     r27,(r3)
        { stop if end of string }
        beq     cr1,.LStreCopyDone
        { point to next dest address }
        addi    r3,r3,1
        { loop if misaligned bytes left }
        bne     .LStreCopyAlignLoop
        .balign  16
.LStreCopyAligned:
        { load next 4 bytes }
        lwz     r27,(r4)
        { first/highest byte zero? (big endian!) }
        andis.  r28,r27,0x0ff00
        addi    r4,r4,4
        beq     .LStreCopyByte
        { second byte zero? }
        andis.  r28,r27,0x00ff
        beq     .LStreCopyWord
        { third byte zero? }
        andi.   r28,r27,0xff00
        beq     .LStreCopy3Bytes
        { fourth byte zero? }
        andi.   r28,r27,0x00ff
        { store next 4 bytes }
        stw     r27,(r3)
        { increase dest address                                      }
        { the result must point to the terminating #0, so only add 3 }
        addi    r3,r3,3
        beq     .LStreCopyDone
        { add another 1 for next char }
        addi    r3,r3,1
        b       .LStreCopyAligned
{ store left-overs }
.LStreCopy3Bytes:
        sth      r27,(r3)
        li       r27,0
        stbu     r27,2(r3)
        b        .LStrCopyDone
.LStreCopyWord:
        sth      r27,(r3)
        addi     r3,r3,1
        b        .LStrCopyDone
.LStreCopyByte:
        stb      r27,(r3)
.LStreCopyDone:
        { r3 contains end of new string now }
end ['r3','r4','r27','r28','cr0','cr1'];


function strlcopy(dest,source : pchar;maxlen : longint) : pchar;assembler;
asm
{ in: dest in r3, source in r4, maxlen in r5 }
{ out: result (dest) in r3                   }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     .LStrlCopyDone
        { maxlen in counter }
        mtctr   r5
        { clear two lowest bits of source address }
        rlwminm r28,r4,0,0,31-2
        { get # of misaligned bytes }
        sub.    r28,r28,r4
        { since we have to return dest intact, use another register for }
        { dest in the copy loop                                         }
        mr      r29,r3
        beq     .LStrlCopyAligned
.LStrlCopyAlignLoop:
        { if decreased maxlen counter = 0 (dz), stop }
        bdz     .LStrlCopyByte
        { decrease misaligned bytes counter (do it here already to improve }
        { jump prediction)                                                 }
        subic.  r28,1
        { load next byte }
        lbz     r27,(r4)
        { end of string? }
        cmpli   cr1,r27,0
        { point to next source byte }
        addi    r4,r4,1
        { store byte }
        stb     r27,(r29)
        { point to next dest address }
        addi    r29,r29,1
        { stop if end of string }
        beq     cr1,.LStrlCopyDone
        { loop while unaligned byte counter <> 0 }
        bne  .LStrlCopyAlignLoop
        .balign  16
.LStrlCopyAligned:
        { load next 4 bytes }
        lwz     r27,(r4)
        { first/highest byte zero? (big endian!) }
        andis.  r28,r27,0x0ff00
        addi    r4,r4,4
        { if decremented maxlen counter not zero (dnz) and no #0 (ne), }
        { continue (and hint that the most likely case is jump taken)  }
        bdnzne+ .LNoStrlCopyByte
        b       .LStrlCopyByte
.LNoStrlCopyByte:
        { second byte zero? }
        andis.  r28,r27,0x00ff
        bdnzne+ .LNoStrlCopyWord
        b       .LStrlCopyWord
.LNoStrlCopyWord:
        { third byte zero? }
        andi.   r28,r27,0xff00
        bdnzne+ .LNoStrlCopy3Bytes
        b       .LStrlCopy3Bytes
.LNoStrlCopy3Bytes:
        { fourth byte zero? }
        andi.   r28,r27,0x00ff
        { store next 4 bytes }
        stw     r27,(r29)
        { increase dest address }
        addi    r29,r29,4
        bdnzne  .LStrlCopyAligned
        { replace last char with a #0 in case we stopped because the maxlen }
        { was reached                                                       }
        li      r27,0
        stb     r27,-1(r29)
        b       .LStrlCopyDone
{ store left-overs }
.LStrlCopy3Bytes:
        { big endian! So move upper 16bits to lower 16bits}
        srwi     r27,r27,16
        sth      r27,(r29)
        li       r27,0
        stb      r27,2(r29)
        b        .LStrlCopyDone
.LStrlCopyWord:
        { clear lower 8 bits of low 16 bits }
        andi     r27,r27,0x0ff00
        sth      r27,(r29)
        b        .LStrlCopyDone
.LStrlCopyByte:
        li       r27,0
        stb      r27,(r29)
.LStrlCopyDone:
        { r3 still contains dest here }
end ['r4','r27','r28','r29','cr0','cr1','ctr'];


function strlen(p : pchar) : longint;assembler;
{ in: p in r3                                                           }
{ out: result (length) in r3                                            }
{ WARNING: if the used registers change here, also change strend!! (JM) }
asm
        { empty/invalid string? }
        cmpli   r3,0
        { if yes, do nothing }
        beq     .LStrLenNil
        { clear two lowest bits of source address }
        rlwminm r28,r3,0,0,31-2
        { get # of misaligned bytes }
        sub.    r28,r28,r3
        { at the end, we substract r29 from r3 to get the length }
        mr      r29,r3
        beq     .LStrLenAligned
.LStrLenAlignLoop:
        { decrease misaligned bytes counter (do it here already to improve }
        { jump prediction)                                                 }
        subic.  r28,1
        { load next byte }
        lbz     r27,(r3)
        { end of string? }
        cmpli   cr1,r27,0
        { stop if end of string }
        beq     cr1,.LStrLenDone
        { point to next source byte }
        addi    r3,r3,1
        bne     .LStrLenAlignLoop
        .balign  16
.LStrLenAligned:
        { load next 4 bytes }
        lwz     r27,(r3)
        { first/highest byte zero? (big endian!) }
        andis.  r28,r27,0x0ff00
        beq     .LStrLenDone
        { second byte zero? }
        andis.  r28,r27,0x00ff
        { increase length }
        addi    r3,r3,1
        beq     .LStrLenDone
        { third byte zero? }
        andi.  r28,r27,0xff00
        addi    r3,r3,1
        beq     .LStrLenDone
        { fourth byte zero? }
        andi.  r28,r27,0x00ff
        addi    r3,r3,1
        beq     .LStrLenDone
        addi    r3,r3,1
        b       .LStrLenAligned
.LStrLenDone:
        sub      r3,r29,r3
.LStrLenNil:
end ['r3','r27','r28','r29','cr0','cr1'];


function strend(p : pchar) : pchar;assembler;
asm
        mr      r26,r3
        mflr    r25
        bl      strlen
        mtlr    r25
        add     r3,r26,r3
end ['r3','r25','r26','r27','r28','r29','cr0','cr1'];


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
        movl    p,%eax
        xorl    %ecx,%ecx
        testl   %eax,%eax
        jz      .LSTRSCAN
// align
        movb    c,%cl
        movl    %eax,%esi
        andl    $0xfffffff8,%eax
        movl    $0xff,%edx
        movl    p,%edi
        subl    %eax,%esi
        jz      .LSTRSCANLOOP
        xorl    %eax,%eax
.LSTRSCANALIGNLOOP:
        movb    (%edi),%al
// at .LSTRSCANFOUND, one is substracted from edi to calculate the position,
// so add 1 here already (not after .LSTRSCAN, because then the test/jz and
// cmp/je can't be paired)
        incl    %edi
        testb   %al,%al
        jz      .LSTRSCAN
        cmpb    %cl,%al
        je      .LSTRSCANFOUND
        decl    %esi
        jnz     .LSTRSCANALIGNLOOP
        jmp     .LSTRSCANLOOP
        .balign  16
.LSTRSCANLOOP:
        movl    (%edi),%eax
        movl    %eax,%esi
// first char
        andl    %edx,%eax
// end of string -> stop
        jz      .LSTRSCAN
        shrl    $8,%esi
        cmpl    %ecx,%eax
        movl    %esi,%eax
        je      .LSTRSCANFOUND1
// second char
        andl    %edx,%eax
        jz      .LSTRSCAN
        shrl    $8,%esi
        cmpl    %ecx,%eax
        movl    %esi,%eax
        je      .LSTRSCANFOUND2
// third char
        andl    %edx,%eax
        jz      .LSTRSCAN
        shrl    $8,%esi
        cmpl    %ecx,%eax
        movl    %esi,%eax
        je      .LSTRSCANFOUND3
// fourth char
// all upper bits have already been cleared
        testl   %eax,%eax
        jz      .LSTRSCAN
        addl    $4,%edi
        cmpl    %ecx,%eax
        je      .LSTRSCANFOUND
        jmp     .LSTRSCANLOOP
.LSTRSCANFOUND3:
        leal    2(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND2:
        leal    1(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND1:
        movl    %edi,%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND:
        leal    -1(%edi),%eax
.LSTRSCAN:
end ['EAX','ECX','ESI','EDI','EDX'];


function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        xorl    %eax,%eax
        movl    p,%edi
        orl     %edi,%edi
        jz      .LSTRRSCAN
        movl    $0xffffffff,%ecx
        cld
        xorb    %al,%al
        repne
        scasb
        not     %ecx
        movb    c,%al
        movl    p,%edi
        addl    %ecx,%edi
        decl    %edi
        std
        repne
        scasb
        cld
        movl    $0,%eax
        jnz     .LSTRRSCAN
        movl    %edi,%eax
        incl    %eax
.LSTRRSCAN:
end ['EAX','ECX','EDI'];


function strupper(p : pchar) : pchar;assembler;
asm
        movl    p,%esi
        orl     %esi,%esi
        jz      .LStrUpperNil
        movl    %esi,%edi
.LSTRUPPER1:
        lodsb
        cmpb    $97,%al
        jb      .LSTRUPPER3
        cmpb    $122,%al
        ja      .LSTRUPPER3
        subb    $0x20,%al
.LSTRUPPER3:
        stosb
        orb     %al,%al
        jnz     .LSTRUPPER1
.LStrUpperNil:
        movl    p,%eax
end ['EAX','ESI','EDI'];


function strlower(p : pchar) : pchar;assembler;
asm
        movl    p,%esi
        orl     %esi,%esi
        jz      .LStrLowerNil
        movl    %esi,%edi
.LSTRLOWER1:
        lodsb
        cmpb    $65,%al
        jb      .LSTRLOWER3
        cmpb    $90,%al
        ja      .LSTRLOWER3
        addb    $0x20,%al
.LSTRLOWER3:
        stosb
        orb     %al,%al
        jnz     .LSTRLOWER1
.LStrLowerNil:
        movl    p,%eax
end ['EAX','ESI','EDI'];

{
  $Log$
  Revision 1.1  2000-11-05 17:17:08  jonas
    + first implementation, not yet finished

}
