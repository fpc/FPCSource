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


procedure fpc_cpuinit;
  begin
  end;

{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame : pointer; assembler;nostackframe;
  asm
{$if defined(amiga)}
    move.l a5,d0
{$else}
    move.l a6,d0
{$endif}
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp : pointer;addr:pointer=nil) : pointer;
  begin
     asm
        move.l framebp,a0
        cmp.l #0,a0
        beq @Lnul_address
        move.l 4(a0),a0
     @Lnul_address:
        move.l a0,@RESULT
     end ['a0'];
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp : pointer;addr:pointer=nil) : pointer;
  begin
     asm
        move.l FRAMEBP,a0
        cmp.l  #0,a0
        beq    @Lnul_frame
        move.l (a0),a0
     @Lnul_frame:
        move.l a0,@RESULT
     end ['a0'];
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



{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure FillChar(var x; count : longint; value : byte); assembler;
asm
  move.l x, a0          { destination                   }
  move.l count, d1      { number of bytes to fill       }
  move.b value, d0      { fill data                     }
  tst.l  d1             { anything to fill at all?      }
  ble    @LMEMSET5
{$ifdef CPUM68K_HAS_DBRA}
{ FIXME: Any reason why not always just use DBRA mode on
         CPUs which support it? (KB) }
  cmpi.l #65535, d1     { check, if this is a word move }
  ble    @LMEMSET3      { use fast dbra mode            }
{$endif CPUM68K_HAS_DBRA}
  bra    @LMEMSET2
@LMEMSET1:
  move.b d0,(a0)+
@LMEMSET2:
  subq.l #1,d1
  bpl @LMEMSET1
  bra @LMEMSET5         { finished slow mode , exit     }
{$ifdef CPUM68K_HAS_DBRA}
@LMEMSET4:              { fast loop mode section 68010+ }
  move.b d0,(a0)+
@LMEMSET3:
  dbra d1,@LMEMSET4
{$endif CPUM68K_HAS_DBRA}
@LMEMSET5:
end;


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


{$define FPC_SYSTEM_HAS_MOVE}
procedure move(const source;var dest;count : longint);
{ base pointer+8 = source                  }
{ base pointer+12 = destination            }
{ base pointer+16 = number of bytes to move}
begin
  asm
    clr.l      d0
    move.l   count, d0   {  number of bytes }
    tst.l  d0            { anything to copy at all? }
    ble    @LMOVE5
  @LMOVE0:
    move.l   dest, a1    {  destination          }
    move.l   source, a0  {  source               }

{$ifdef CPUM68K_HAS_DBRA}
    cmpi.l #65535, d0     { check, if this is a word move }
    ble    @LMEMSET00     { use fast dbra mode 68010+     }
{$endif CPUM68K_HAS_DBRA}

    cmp.l      a0,a1         {  check copy direction }
    bls      @LMOVE4
    add.l      d0,a0         { move pointers to end  }
    add.l      d0,a1
    bra     @LMOVE2
  @LMOVE1:
    move.b   -(a0),-(a1)   {  (s < d) copy loop }
  @LMOVE2:
    subq.l    #1,d0
    cmpi.l    #-1,d0
    bne       @LMOVE1
    bra       @LMOVE5
  @LMOVE3:
    move.b  (a0)+,(a1)+  { (s >= d) copy loop }
  @LMOVE4:
    subq.l    #1,d0
    cmpi.l    #-1,d0
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
  end ['d0','a0','a1'];
end;


{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord(var x; count : longint; value : word); assembler;
asm
  move.l x, a0         { destination              }
  move.l count, d1     { number of bytes to fill  }
  move.w value, d0     { fill data                }
  tst.l  d1            { anything to fill at all? }
  ble @LMEMSET3
  bra @LMEMSET21
@LMEMSET11:
  move.w d0,(a0)+
@LMEMSET21:
  subq.l #1,d1
  bpl @LMEMSET11
@LMEMSET3:
end;


function InterLockedDecrement (var Target: longint) : longint;
  begin
  {$warning FIX ME}
    Dec(Target);
    Result := Target;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
  {$warning FIX ME}
    Inc(Target);
    Result := Target;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
  {$warning FIX ME}
    Result := Target;
    Target := Source;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
  {$warning FIX ME}
    Result := Target;
    Target := Target + Source;
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  begin
  {$warning FIX ME}
    Result := Target;
    if Target = Comperand then
      Target := NewValue;
  end;
