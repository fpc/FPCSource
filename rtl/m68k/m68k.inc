{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************

      m68k.inc : Processor dependent implementation of system unit
                 For Motorola 680x0 Processor.

*****************************************************************************}

{****************************************************************************}
{   Credit where credit is due:                                              }
{   -Some of the copy routines taken from the Atari dlib source code:        }
{     Dale Schumacher (alias: Dalnefre')  dal@syntel.uucp                    }
{     399 Beacon Ave. St. Paul, MN 55104,USA                                 }
{   -Some of the routines taken from the freeware ATARI Sozobon C compiler   }
{      1988 by Sozobon, Limited.  Author: Johann Ruegg (freeware)            }
{  Thanks to all these people wherever they maybe today!                     }
{****************************************************************************}


{$IF DEFINED(FPU68881) OR DEFINED(FPUCOLDFIRE)}
function GetFPCR: DWord; assembler; nostackframe;
asm
  fmove.l fpcr,d0
end;

function GetFPSR: DWord; assembler; nostackframe;
asm
  fmove.l fpsr, d0
end;

procedure SetFPCR(x: DWord); assembler; nostackframe;
asm
  fmove.l x, fpcr
end;

procedure SetFPSR(x: DWord); assembler; nostackframe;
asm
  fmove.l x, fpsr
end;

{$DEFINE FPC_SYSTEM_HAS_SYSRESETFPU}
procedure SysResetFPU;
begin
  SetFPCR(Default68KFPCR);
  SetFPSR(0);
end;

{$DEFINE FPC_SYSTEM_HAS_SYSINITFPU}
procedure SysInitFPU;
begin
end;

procedure fpc_cpuinit;
  begin
    if IsLibrary then
      begin
        Default68kFPCR:=GetFPCR;
      end;
    SysResetFPU;
  end;

{$ELSE}

procedure fpc_cpuinit;
  begin
{$IFNDEF FPUNONE}
    SysResetFPU;
    if (not IsLibrary) then
      SysInitFPU;
{$ENDIF}
  end;

{$ENDIF}

{$ifndef INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame : pointer; assembler;nostackframe;
  asm
    move.l fp,d0
  end;
{$endif not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp : pointer;addr:pointer=nil) : pointer; assembler;
asm
    move.l framebp,d0
    tst.l  d0
    beq @Lnul_address
    move.l d0,a0
    move.l 4(a0),d0
@Lnul_address:
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp : pointer;addr:pointer=nil) : pointer; assembler;
asm
    move.l framebp,d0
    tst.l  d0
    beq    @Lnul_frame
    move.l d0,a0
    move.l (a0),d0
@Lnul_frame:
end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr : pointer; assembler;nostackframe;
asm
  move.l sp,d0
end;

{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
function get_pc_addr : pointer;assembler;nostackframe;
asm
  move.l (sp),d0
end;


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure FillChar(var x; count : longint; value : byte); assembler; register; nostackframe;
asm
{$ifdef VER3_0}
  move.l   x,a0
  move.l   count,d0
  clr.l    d1
  move.b   value,d1
{$endif VER3_0}
  { a0 is x, d0 is count, d1 is value }
  tst.l    d0             { anything to fill at all?      }
  ble      @Lquit
  cmp.l    #32,d0         { limits were tested against real hardware on various CPU }
  blt      @LfillByte
  cmp.l    #128,d0        { limits were tested against real hardware on various CPU }
  blt      @LfillWord
  bra      @LfillDWord

{$ifndef CPUM68K_HAS_DBRA}
@LfillByte:
{$endif}
@LfillByteLoop:
  move.b   d1,(a0)+
{$ifdef CPUM68K_HAS_DBRA}
@LfillByte:
  dbra     d0,@LfillByteLoop
{$else}
  subq.l   #1,d0
  bne      @LfillByteLoop
{$endif}
  rts

@LfillWord:
  move.l   d2,-(sp)
  move.l   a0,d2
  btst     #0,d2
  beq      @Leven
  subq.l   #1,d0
  move.b   d1,(a0)+
@Leven:
  move.b   d1,d2  // copy value to upper byte
{$ifdef CPUCOLDFIRE}
  lsl.l    #8,d1
{$else}
  lsl.w    #8,d1
{$endif}
  move.b   d2,d1
  move.l   d0,d2  // adjust d0 for leftover copy
  bclr     #0,d2
  sub.l    d2,d0
  lsr.l    #1,d2
{$ifdef CPUM68K_HAS_DBRA}
  subq.l   #1,d2
{$endif}
@LfillWordLoop:
  move.w   d1,(a0)+
{$ifdef CPUM68K_HAS_DBRA}
  dbra     d2,@LFillWordLoop
{$else}
  subq.l   #1,d2
  bne      @LfillWordLoop
{$endif}
  move.l   (sp)+,d2
  tst.l    d0
  bne      @LfillByte
  rts

@LfillDWord:
  move.l   d2,-(sp)
  move.b   d1,d2  // copy value to upper bytes
{$ifdef CPUCOLDFIRE}
  lsl.l    #8,d1
{$else}
  lsl.w    #8,d1
{$endif}
  move.b   d2,d1
  move.w   d1,d2
  swap     d1
  move.w   d2,d1

  move.l   a0,d2  // do initial byte and word fill, if the address is unaligned
  btst     #0,d2
  beq      @Ldeven
  subq.l   #1,d0
  move.b   d1,(a0)+
@Ldeven:
  move.l   a0,d2
  btst     #1,d2
  beq      @Ldquad
  subq.l   #2,d0
  move.w   d1,(a0)+
@Ldquad:
  move.l   d0,d2  // adjust d0 for leftover copy
{$ifdef CPUCOLDFIRE}
  and.l    #$fffffffc,d2
{$else}
  and.b    #$fc,d2
{$endif}
  sub.l    d2,d0
  lsr.l    #2,d2
  bra      @LfillLongLoopStart

@LfillLongLoop:
  move.l   d1,(a0)+
  move.l   d1,(a0)+
  move.l   d1,(a0)+
  move.l   d1,(a0)+
  subq.l   #4,d2
@LfillLongLoopStart:
  cmp.l    #4,d2
  bgt      @LfillLongLoop

{$ifdef CPUM68K_HAS_DBRA}
  subq.l   #1,d2
{$endif}
@LfillDWordLoop:
  move.l   d1,(a0)+
{$ifdef CPUM68K_HAS_DBRA}
  dbra     d2,@LFillDWordLoop
{$else}
  subq.l   #1,d2
  bne      @LfillDWordLoop
{$endif}
  move.l   (sp)+,d2
  tst.l    d0
  bne      @LfillByte
@Lquit:
end;
{$endif ndef FPC_SYSTEM_HAS_FILLCHAR}

{$ifdef dummy}
{    procedure strcopy(dstr,sstr : pointer;len : longint);[public,alias: 'STRCOPY'];}
procedure strcopy; assembler;[public,alias: 'FPC_STRCOPY'];
{---------------------------------------------------}
{ Low-level routine to copy a string to another     }
{ string with maximum length. Never call directly!  }
{ On Entry:                                         }
{     a1.l = string to copy to                      }
{     a0.l = source string                          }
{     d0.l = maximum length of copy                 }
{ registers destroyed: a0,a1,d0,d1                  }
{---------------------------------------------------}
asm
{  move.l sstr,a0
   move.l dstr,a1
   move.l len,d1 }
   move.l d0,d1

   move.b (a0)+,d0     { Get source length }
   and.w  #$ff,d0
   cmp.w  d1,d0        { This is a signed comparison! }
   ble    @LM4
   move.b d1,d0        { If longer than maximum size of target, cut
                         source length }
@LM4:
   andi.l #$ff,d0     { zero extend d0-byte }
   move.l d0,d1       { save length to copy }
   move.b d0,(a1)+    { save new length     }
   { Check if copying length is zero - if so then }
   { exit without copying anything.               }
   tst.b  d1
   beq    @Lend
   bra    @LMSTRCOPY55
@LMSTRCOPY56:         { 68010 Fast loop mode }
   move.b (a0)+,(a1)+
@LMSTRCOPY55:
{$ifndef CPUM68K_HAS_DBRA}
   sub.l #1,d1
   bpl @LMSTRCOPY56
{$else CPUM68K_HAS_DBRA}
   dbra  d1,@LMSTRCOPY56
{$endif CPUM68K_HAS_DBRA}
@Lend:
end;


{ Concatenate Strings }
{ PARAMETERS ARE REVERSED COMPARED TO NORMAL! }
{ therefore online assembler may not parse the params as normal }
procedure strconcat(s1,s2 : pointer);[public,alias: 'STRCONCAT'];
  begin
     asm
      move.b  #255,d0
      move.l  s1,a0                { a0 = destination }
      move.l  s2,a1                { a1 = source      }
      sub.b   (a0),d0              {  copyl:= 255 -length(s1)    }
      move.b  (a1),d6
      and.w   #$ff,d0              { Sign flags are checked!     }
      and.w   #$ff,d6
      cmp.w   d6,d0                { if copyl > length(s2) then  }
      ble     @Lcontinue
      move.b  (a1),d0              {  copyl:=length(s2)          }
@Lcontinue:
      move.b  (a0),d6
      and.l   #$ff,d6
      lea     1(a0,d6),a0          { s1[length(s1)+1]            }
      add.l   #1,a1                { s2[1]                       }
      move.b  d0,d6
      { Check if copying length is zero - if so then }
      { exit without copying anything.               }
      tst.b  d6
      beq    @Lend
      bra    @ALoop
@Loop:
      move.b  (a1)+,(a0)+          { s1[i] := s2[i];             }
@ALoop:
{$ifndef CPUM68K_HAS_DBRA}
      sub.l   #1,d6
      bpl     @Loop
{$else CPUM68K_HAS_DBRA}
      dbra    d6,@Loop
{$endif CPUM68K_HAS_DBRA}
      move.l  s1,a0
      add.b   d0,(a0)              { change to new string length }
@Lend:
     end ['d0','d1','a0','a1','d6'];
  end;

{ Compares strings }
{ DO NOT CALL directly.                                 }
{   a0 = pointer to first string to compare             }
{   a1 = pointer to second string to compare            }
{   ALL FLAGS are set appropriately.                    }
{    ZF = strings are equal                             }
{ REGISTERS DESTROYED: a0, a1, d0, d1, d6               }
procedure strcmp; assembler;[public,alias:'FPC_STRCMP'];
asm
       move.b (a0)+,d0     { Get length of first string  }
       move.b (a1)+,d6     { Get length of 2nd string    }

       move.b  d6,d1      { Save length of string for final compare   }

       cmp.b  d0,d6        { Get shortest string length   }
       ble    @LSTRCONCAT1
       move.b d0,d6       { Set length to shortest string }

    @LSTRCONCAT1:
       tst.b  d6          { Both strings have a length of zero, exit }
       beq    @LSTRCONCAT2

       andi.l  #$ff,d6


       subq.l  #1,d6      { subtract first attempt                    }
       { if value is -1 then don't loop and just compare lengths of   }
       { both strings before exiting.                                 }
       bmi     @LSTRCONCAT2
       or.l    d0,d0      { Make sure to set Zerfo flag to 0          }
    @LSTRCONCAT5:
       { Workaroung for GAS v.134 bug }
       {  old: cmp.b (a1)+,(a0)+      }
       cmpm.b  (a1)+,(a0)+
    @LSTRCONCAT4:
       dbne    d6,@LSTRCONCAT5   { Repeat until not equal }
       bne     @LSTRCONCAT3
     @LSTRCONCAT2:
       { If length of both string are equal }
       { Then set zero flag                 }
       cmp.b   d1,d0   { Compare length - set flag if equal length strings }
    @LSTRCONCAT3:
end;
{$endif dummy}


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure move(const source;var dest;count : longint); assembler;
{ base pointer+8 = source                  }
{ base pointer+12 = destination            }
{ base pointer+16 = number of bytes to move}
asm
    move.l   count, d0   {  number of bytes }
    ble    @LMOVE5       { anything to copy at all? }

    move.l   dest, a1    {  destination          }
    move.l   source, a0  {  source               }

{$ifdef CPUM68K_HAS_DBRA}
    cmpi.l #65535, d0     { check, if this is a word move }
    ble    @LMEMSET00     { use fast dbra mode 68010+     }
{$endif CPUM68K_HAS_DBRA}

    cmp.l      a0,a1         {  check copy direction }
    bls      @LMOVE3
    add.l      d0,a0         { move pointers to end  }
    add.l      d0,a1
  @LMOVE1:
    move.b   -(a0),-(a1)   {  (s < d) copy loop }
    subq.l    #1,d0
    bne       @LMOVE1
    bra       @LMOVE5
  @LMOVE3:
    move.b  (a0)+,(a1)+  { (s >= d) copy loop }
    subq.l    #1,d0
    bne       @LMOVE3
    bra       @LMOVE5

{$ifdef CPUM68K_HAS_DBRA}
  @LMEMSET00:            { use fast loop mode 68010+ }
    cmp.l      a0,a1         {  check copy direction }
    bls      @LMOVE04
    add.l      d0,a0         { move pointers to end  }
    add.l      d0,a1
    bra     @LMOVE02
  @LMOVE01:
    move.b   -(a0),-(a1)   {  (s < d) copy loop }
  @LMOVE02:
    dbra      d0,@LMOVE01
    bra       @LMOVE5
  @LMOVE03:
    move.b  (a0)+,(a1)+  { (s >= d) copy loop }
  @LMOVE04:
    dbra      d0,@LMOVE03
{$endif CPUM68K_HAS_DBRA}
  { end fast loop mode }
  @LMOVE5:
end;
{$endif ndef FPC_SYSTEM_HAS_MOVE}


{$ifdef CPUM68K_HAS_UNALIGNED}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord(var x; count : longint; value : word); assembler;
asm
  move.l x, a0         { destination              }
  move.w value, d1     { fill data                }
  move.l count, d0     { number of bytes to fill  }
  ble @LMEMSET3        { anything to fill at all? }
  bra @LMEMSET21
@LMEMSET11:
  move.w d1,(a0)+
@LMEMSET21:
  subq.l #1,d0
  bpl @LMEMSET11
@LMEMSET3:
end;
{$endif}

{$IFNDEF FPC_SYSTEM_HAS_INTERLOCKEDFUNCS}
function InterLockedDecrement (var Target: longint) : longint;
{$IFDEF CPUM68K_HAS_CAS}
  register; assembler;
  asm
    move.l  (a0), d0
    @loop:
    move.l  d0, d1
    subq.l  #1, d1
    cas.l   d0, d1, (a0)
    bne @loop
    move.l  d1, d0
  end;
{$ELSE}
  begin
  {$warning FIX ME}
    Dec(Target);
    Result := Target;
  end;
{$ENDIF}


function InterLockedIncrement (var Target: longint) : longint;
{$IFDEF CPUM68K_HAS_CAS}
  register; assembler;
  asm
    move.l  (a0), d0
    @loop:
    move.l  d0, d1
    addq.l  #1, d1
    cas.l   d0, d1, (a0)
    bne @loop
    move.l  d1, d0
  end;
{$ELSE}
  begin
  {$warning FIX ME}
    Inc(Target);
    Result := Target;
  end;
{$ENDIF}

function InterLockedExchange (var Target: longint;Source : longint) : longint;
{$IFDEF CPUM68K_HAS_CAS}
  register; assembler;
  asm
    move.l  Source, d1
    move.l  (a0), d0
    @loop:
    cas.l   d0, d1, (a0)
    bne @loop
  end;
{$ELSE}
  begin
  {$warning FIX ME}
    Result := Target;
    Target := Source;
  end;
{$ENDIF}


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
{$IFDEF CPUM68K_HAS_CAS}
  register; assembler;
  asm
    move.l  Source, a1
    move.l  (a0), d0
    @loop:
    move.l  a1, d1
    add.l   d0, d1
    cas.l   d0, d1, (a0)
    bne @loop
  end;
{$ELSE}
  begin
  {$warning FIX ME}
    Result := Target;
    Target := Target + Source;
  end;
{$ENDIF}


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$IFDEF CPUM68K_HAS_CAS}
  register; assembler;
  asm
    // Target = a0, NewValue = d0, Comperand = d1
    exg.l   d0, d1
    cas.l   d0, d1, (a0)
  end;
{$ELSE}
  begin
  {$warning FIX ME}
    Result := Target;
    if Target = Comperand then
      Target := NewValue;
  end;
{$ENDIF}
{$ENDIF FPC_SYSTEM_HAS_INTERLOCKEDFUNCS}

{$ifndef FPC_SYSTEM_HAS_TEST68K}
procedure Test68k(var CPU: byte; var FPU: byte);
begin
  {$warning Implement me!}
  CPU:=0;
  FPU:=0;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
  // d0 = shift
  lea.l   4(sp),a0
  move.l  d2,-(sp)
  move.l  d0,d2
{$ifndef CPUCOLDFIRE}
  and.w   #63,d2
  cmp.w   #32,d2
{$else}
  and.l   #63,d2
  cmp.l   #32,d2
{$endif}
  bge.s   @longshift

  move.l  (a0)+,d0
  move.l  (a0),d1

{$ifdef CPUM68K_HAS_ROLROR}
  cmp.w   #1,d2
  beq.s   @oneshift
{$endif}
{$ifdef CPU68000}
  cmp.w   #16,d2
  beq.s   @sixteenshift
{$endif}

  move.l  d3,a0
  move.l  d4,a1

  move.l  d0,d3
  moveq.l #32,d4
{$ifndef CPUCOLDFIRE}
  sub.w   d2,d4
{$else}
  sub.l   d2,d4
{$endif}
  asr.l   d2,d0
  lsl.l   d4,d3
  lsr.l   d2,d1
  or.l    d3,d1

  move.l  a0,d3
  move.l  a1,d4

  bra.s   @quit

{$ifdef CPU68000}
@sixteenshift:
  move.w  d0,d1
  swap    d1
  swap    d0
  ext.l   d0

  bra.s   @quit
{$endif}

{$ifdef CPUM68K_HAS_ROLROR}
@oneshift:
  asr.l   #1,d0
  roxr.l  #1,d1

  bra.s   @quit
{$endif}

@longshift:
  move.l  (a0),d0
  move.l  d0,d1
  smi     d0
{$if defined(CPU68020) or defined(CPUCOLDFIRE)}
  extb.l  d0
{$else}
  ext.w   d0
  ext.l   d0
{$endif}
{$ifndef CPUCOLDFIRE}
  sub.w   #32,d2
{$else}
  sub.l   #32,d2
{$endif}
  asr.l   d2,d1

@quit:
  move.l (sp)+,d2
end;
{$endif}

{$if defined(CPUM68K_HAS_BYTEREV) or defined(CPUM68K_HAS_ROLROR)}
{ Disabled for now, because not all cases below were tested. (KB) }
{.$define FPC_SYSTEM_HAS_SWAPENDIAN}
{$endif}

{$if defined(FPC_SYSTEM_HAS_SWAPENDIAN)}
function SwapEndian(const AValue: SmallInt): SmallInt; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.w   avalue, d0
    ror.w    #8, d0
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.w   avalue, d0
    byterev  d0
    swap     d0
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    clr.l    d0
    move.w   avalue, d0
    move.w   d0, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
{$endif}
end;


function SwapEndian(const AValue: Word): Word; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.w   avalue, d0
    ror.w    #8, d0
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.w   avalue, d0
    byterev  d0
    swap     d0
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    clr.l    d0
    move.w   avalue, d0
    move.w   d0, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
{$endif}
end;


function SwapEndian(const AValue: LongInt): LongInt; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.l   avalue, d0
    ror.w    #8, d0
    swap     d0
    ror.w    #8, d0
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.l   avalue, d0
    byterev  d0
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    move.l   avalue, d0
    move.l   d0, d1
    andi.l   #$ff00ff00, d0
    andi.l   #$00ff00ff, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
    swap     d0
{$endif}
end;

function SwapEndian(const AValue: DWord): DWord; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.l   avalue, d0
    ror.w    #8, d0
    swap     d0
    ror.w    #8, d0
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.l   avalue, d0
    byterev  d0
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    move.l   avalue, d0
    move.l   d0, d1
    andi.l   #$ff00ff00, d0
    andi.l   #$00ff00ff, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
    swap     d0
{$endif}
end;

function SwapEndian(const AValue: Int64): Int64; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.l   avalue+4, d0
    ror.w    #8, d0
    swap     d0
    ror.w    #8, d0
    move.l   avalue, d1
    ror.w    #8, d1
    swap     d1
    ror.w    #8, d1
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.l   avalue+4, d0
    move.l   avalue, d1
    byterev  d0
    byterev  d1
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    move.l   d2, -(sp)
    move.l   avalue+4, d0
    move.l   d0, d1
    andi.l   #$ff00ff00, d0
    andi.l   #$00ff00ff, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
    swap     d0
    move.l   avalue, d1
    move.l   d1, d2
    andi.l   #$ff00ff00, d1
    andi.l   #$00ff00ff, d2
    lsr.l    #8, d1
    lsl.l    #8, d2
    or.l     d2, d1
    swap     d1
    move.l   (sp)+, d2
{$endif}
end;

function SwapEndian(const AValue: QWord): QWord; assembler; nostackframe;
asm
{$if defined(CPUM68K_HAS_ROLROR)}
    move.l   avalue+4, d0
    ror.w    #8, d0
    swap     d0
    ror.w    #8, d0
    move.l   avalue, d1
    ror.w    #8, d1
    swap     d1
    ror.w    #8, d1
{$elseif defined(CPUM68K_HAS_BYTEREV)}
    move.l   avalue+4, d0
    move.l   avalue, d1
    byterev  d0
    byterev  d1
{$else}
    // only ISA A/B ColdFire can end in this branch, so use long ops everywhere
    move.l   d2, -(sp)
    move.l   avalue+4, d0
    move.l   d0, d1
    andi.l   #$ff00ff00, d0
    andi.l   #$00ff00ff, d1
    lsr.l    #8, d0
    lsl.l    #8, d1
    or.l     d1, d0
    swap     d0
    move.l   avalue, d1
    move.l   d1, d2
    andi.l   #$ff00ff00, d1
    andi.l   #$00ff00ff, d2
    lsr.l    #8, d1
    lsl.l    #8, d2
    or.l     d2, d1
    swap     d1
    move.l   (sp)+, d2
{$endif}
end;
{$endif FPC_SYSTEM_HAS_SWAPENDIAN}

procedure fpc_cpucodeinit;
  begin
    Test68k(Test68000,Test68881);
  end;
