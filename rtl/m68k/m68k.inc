{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Carl-Eric Codere,
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
{     This include file contains as little assembler as possible, to make    }
{     porting to other systems easier.                                       }
{   Port to the Motorola 680x0 compiler by:                                  }
{                                                                            }
{     Carl-Eric Codere - port of non-system specific stuff.                  }
{                                                                            }
{  Some routines taken from the Atari freeware dlib source code, created by: }
{     Dale Schumacher                         399 Beacon Ave.                }
{     (alias: Dalnefre')                      St. Paul, MN  55104            }
{     dal@syntel.UUCP                         United States of America       }
{  Some routines taken from the freeware Atari Sozobon C compiler, created by:}
{      1988 by Sozobon, Limited.  Author: Johann Ruegg (freeware)             }
{  Thanks to all these people wherever they maybe today!                     }
{      BUGS in sqr and abs for return values. Only longint seems supported.  }
{                                                                            }
{  Still left to do:                                                         }
{     mod_rr routine to convert to pascal format.                            }
{                                                                            }
{   ALL routines in set.inc, system.inc and real2str.inc are system independant.}
{****************************************************************************}


    { Don't call the following routines directly. }
 Procedure Hlt;[public,alias: 'HALT_ERROR'];
 { called by code generator on run-time errors. }
 { on entry contains d0 = error code.           }
 var
  b:byte; { only byte is used... }
 begin
  asm
     move.b d0,b
  end;
     HandleError(b);
 end;




   Procedure FillChar(var x; count: longint; value: byte);
   begin
     asm
      move.l 8(a6), a0      { destination                   }
      move.l 12(a6), d1     { number of bytes to fill       }
      move.b 16(a6),d0      { fill data                     }
      cmpi.l #65535, d1     { check, if this is a word move }
      ble    @LMEMSET3      { use fast dbra mode            }
      bra @LMEMSET2
    @LMEMSET1:
      move.b d0,(a0)+
    @LMEMSET2:
      subq.l #1,d1
      cmp.b #-1,d1
      bne  @LMEMSET1
      bra @LMEMSET5        { finished slow mode , exit     }

    @LMEMSET4:             { fast loop mode section 68010+ }
      move.b d0,(a0)+
    @LMEMSET3:
      dbra d1,@LMEMSET4

    @LMEMSET5:
     end ['d0','d1','a0'];
   end;

   Procedure FillObject(var x; count: longint; value: byte);
   begin
     asm
      move.l 8(a6), a0      { destination                   }
      move.l 12(a6), d1     { number of bytes to fill       }
      move.w 16(a6),d0      { fill data                     }
      cmp.l  #65535, d1     { check, if this is a word move }
      ble    @LMEMSET3      { use fast dbra mode            }
      bra @LMEMSET2
    @LMEMSET1:
      move.b d0,(a0)+
    @LMEMSET2:
      subq.l #1,d1
      cmp.b #-1,d1
      bne  @LMEMSET1
      bra @LMEMSET5        { finished slow mode , exit     }

    @LMEMSET4:             { fast loop mode section 68010+ }
      move.b d0,(a0)+
    @LMEMSET3:
      dbra d1,@LMEMSET4

    @LMEMSET5:
     end ['d0','d1','a0'];
   end;

    procedure int_help_constructor;

      begin
         asm
          XDEF HELP_CONSTRUCTOR
            { Entry without preamble, since we need the ESP of the
              constructor }
            { Stack (relative to %ebp):
                12 Self
                8 VMT-Address
                4 main programm-Addr
                0 %ebp
            }
            { do we have to initialize self  }
            { we just need to check for zero }
            move.l    a5,d0
            tst.l     d0      { set flags }
            bne       @LHC_4

            { get memory, but save register first }
            { temporary variable }
            subq.l #4,sp
            move.l sp,a5
            { Save Registers }
            movem.l d0-a7,-(sp)
            { Memory size }
            move.l 8(a6),a0
            move.l (a0),-(sp)
            { push method pointer }
            move.l a5,-(sp)
            jsr GETMEM
            { Restore all registers in the correct order }
            movem.l (sp)+,d0-a7
            { Memory size to a5 }
            move.l (a5),a5
            add.l  #4,sp
            { If no memory available : fail() }
            move.l a5,d0
            tst.l  d0         { set flags for a5 }
            beq    @LHC_5
            { init self for the constructor }
            move.l a5,12(a6)
         @LHC_4:
            { is there a VMT address ? }
            move.l 8(a6),d0
            or.l   d0,d0
            bne @LHC_7
            { In case the constructor doesn't do anything, the Zero-Flag }
            { can't be put, because this calls Fail() }
            add.l  #1,d0
            rts
         @LHC_7:
            { set zero inside the object }
            { Save Registers }
            movem.l d0-a7,-(sp)
            move.w  #0,-(sp)

            move.l  8(a6),a0
            move.l  (a0),-(sp)
            move.l  a5,-(sp)
            {                }
            jsr  FILLOBJECT
            { Restore all registers in the correct order }
            movem.l (sp)+,d0-a7
            { set the VMT address for the new created object }
            move.l 8(a6),d0
            move.l d0,(a5)
            or.l d0,d0
         @LHC_5:
            rts
         end;
      end;

    procedure help_fail;

      begin
         asm
         end;
      end;

    procedure int_help_destructor;

      begin
         asm
            { Stack (relative to %ebp):
                12 Self
                8 VMT-Address
                4 Main program-Addr
                0 %ebp
            }
            { temporary Variable }
          XDEF HELP_DESTRUCTOR
            subq.l #4,sp
            move.l sp,d6
            { Save Registers }
            movem.l d0-a7,-(sp)

            move.l 8(a6),d0         { Get the address of the vmt }
            or.l   d0,d0            { Check if there is a vmt    }
            beq    @LHD_3
            { Yes, get size from SELF! }
            move.l 12(a6),a0
            { get VMT-pointer (from Self) to %ebx }
            move.l (a0),a1
            { And put size on the Stack }
            move.l (a1),-(sp)
            { SELF }
            { I think for precaution }
            { that we should clear the VMT here }
            clr.l (a0)
            { get address of local variable into  }
            { address register                    }
            move.l d6,a1
            move.l a0,(a1)
            move.l a1,-(sp)
            jsr    FREEMEM
         @LHD_3:
            { Restore all registers in the correct order }
            movem.l (sp)+,d0-a7
            add.l #4,sp
            rts
         end;
      end;

  procedure new_class;assembler;

  asm
     XDEF NEW_CLASS
     { create class ? }
     move.l 8(a6), d0
     tst.l  d0
     { check for nil... }
     beq    @NEW_CLASS1

     { a5 contains vmt }
     move.l a5,-(sp)
     { call newinstance (class method!) }
     jsr 16(a5)
     { new instance returns a pointer to the new created }
     { instance in d0                                    }
     { load a5  and insert self                          }
     move.l d0,8(a6)
     move.l d0,a5
     bra    @end
  @NEW_CLASS1:
     move.l a5,8(a6)
  @end:
  end;



  procedure dispose_class;assembler;

  asm
    XDEF DISPOSE_CLASS
     { destroy class ? }
     move.l 8(a6),d0
     { save self }
     move.l a5,8(a6)
     tst.l  d0
     beq    @DISPOSE_CLASS
     { no inherited call }
     move.l (a5),d0
     { push self }
     move.l a5,-(sp)
     { call freeinstance }
     move.l d0,a0
     jsr    20(a0)
  @DISPOSE_CLASS:
     { load self }
     move.l 8(a6),a5
  end;

  { checks for a correct vmt pointer }
  procedure co;assembler;
  { ON ENTRY: a0 -> Pointer to the VMT                  }
  {   Nota: All registers must be preserved including   }
  {   A0 itself!                                        }
  asm
   XDEF CHECK_OBJECT
     move.l   d0,-(sp)
     tst.l    a0
     { z flag set if zero }
     beq      @co_re

     move.l   (a0),d0
     add.l    4(a0),d0
     bne      @co_re
     bra      @end
@co_re:
     move.l   (sp)+,d0
     move.b   #210,d0
     jsr      HALT_ERROR
@end:
     move.l   (sp)+,d0
  end;



    function get_addr(BP : longint) : longint;
      begin
         asm
            move.l BP,a0
            cmp.l #0,a0
            beq @Lnul_address
            move.l 4(a0),a0
         @Lnul_address:
            move.l a0,@RESULT
         end ['a0'];
      end;

    function get_next_frame(bp : longint) : longint;

      begin
         asm
            move.l bp,a0
            cmp.l  #0,a0
            beq    @Lnul_frame
            move.l (a0),a0
         @Lnul_frame:
            move.l a0,@RESULT
         end ['a0'];
      end;

Procedure HandleError (Errno : longint);[alias : 'handleerror'];
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
}
      function get_addr : pointer;

        begin
           asm
              move.l (a6),a0
              move.l 4(a0),a0
              move.l a0,@RESULT
           end ['a0'];
        end;
      function get_error_bp : longint;

        begin
           asm
              { get base pointer of error }
              move.l (a6),d0
              move.l d0,@RESULT
           end ['d0'];
        end;

begin
  If ErrorProc<>Nil then
    TErrorProc (ErrorProc)(Errno,get_addr);
  errorcode:=Errno;
  exitcode:=Errno;
  erroraddr:=Get_addr;
  DoError := TRUE;
  errorbase:=get_error_bp;
  halt(errorcode);
end;


    procedure runerror(w : word);

      function get_addr : longint;

        begin
           asm
              move.l (a6),a0
              move.l 4(a0),a0
              move.l a0,@RESULT
           end ['a0'];
        end;

      function get_error_bp : longint;

        begin
           asm
              { get base pointer of error }
              move.l (a6),d0
              move.l d0,@RESULT
           end ['d0'];
        end;

      begin
         errorcode:=w;
         exitcode:=w;
         erroraddr:=pointer(get_addr);
         DoError:=True;
         ErrorBase:=get_error_bp;
         halt(byte(errorcode));
      end;

    procedure io1(addr : longint);[public,alias: 'IOCHECK'];

      var
         l : longint;

      begin
         { Since IOCHECK is called directly and only later the optimiser }
         { Maybe also save global registers  }
         asm
            movem.l d0-a7,-(sp)
         end;
         l:=ioresult;
         if l<>0 then
           begin
              writeln('IO-Error ',l,' at 0x',HexStr(addr,8));
              halt(byte(l));
           end;
         asm
            { the register are put back in the correct order }
            movem.l (sp)+,d0-a7
         end;
      end;

    procedure re_overflow;[public,alias: 'RE_OVERFLOW'];

      var
         addr : longint;

      begin
         { Overflow was shortly before the return address }
         asm
            move.l 4(a6),d0
            move.l d0,addr
         end;
         writeln('Overflow at 0x',HexStr(addr,8));
         HandleError(215);
      end;

{    procedure strcopy(dstr,sstr : pointer;len : longint);[public,alias: 'STRCOPY'];}
     procedure strcopy; assembler;
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
           XDEF STRCOPY
{            move.l 12(a6),a0
            move.l 16(a6),a1
            move.l 8(a6),d1 }
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
            dbra  d1,@LMSTRCOPY56
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
          dbra    d6,@Loop
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
    procedure strcmp; assembler;
     asm
     XDEF STRCMP

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


  Function strpas(p: pchar): string;
  { only 255 first characters are actually copied. }
   var
    counter : byte;
    str: string;
  Begin
     counter := 0;
     str := '';
     while (ord(p[counter]) <> 0) and (counter < 255) do
     begin
        counter:=counter+1;
        str[counter] := char(p[counter-1]);
     end;
     str[0] := char(counter);
     strpas := str;
  end;

  function strlen(p : pchar) : longint;
  var
     counter : longint;
  Begin
       counter := 0;
       repeat
          counter:=counter+1;
       until ord(p[counter]) = 0;
       strlen := counter;
  end;


   procedure move(var source;var dest;count : longint);
   { base pointer+8 = source                  }
   { base pointer+12 = destination            }
   { base pointer+16 = number of bytes to move}
   begin
     asm
      clr.l      d0
      move.l   16(a6),d0   {  number of bytes }
    @LMOVE0:
      move.l   12(a6),a1   {  destination          }
      move.l   8(a6),a0      {  source               }

      cmpi.l #65535, d0     { check, if this is a word move }
      ble    @LMEMSET00     { use fast dbra mode 68010+     }

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
    { end fast loop mode }
    @LMOVE5:
    end ['d0','a0','a1'];
   end;


    procedure fillword(var x;count : longint;value : word);

      begin
     asm
      move.l 8(a6), a0      { destination             }
      move.l 12(a6), d1     { number of bytes to fill }
      move.w 16(a6),d0      { fill data               }
      bra @LMEMSET21
    @LMEMSET11:
      move.w d0,(a0)+
    @LMEMSET21:
      subq.l #1,d1
      cmp.b #-1,d1
      bne  @LMEMSET11
     end ['d0','d1','a0'];
   end;


    function abs(l : longint) : longint;

      begin
         asm
            move.l 8(a6),d0
            tst.l  d0
            bpl @LMABS1
            neg.l d0
         @LMABS1:
            move.l d0,@RESULT
         end ['d0'];
      end;

    function odd(l : longint) : boolean;

      begin
        if (l and $01) = $01 then
          odd := TRUE
        else
          odd := FALSE;
      end;

    function sqr(l : longint) : longint;

      begin
         sqr := l*l;
      end;

    procedure int_str(l : longint;var s : string);

      var
        value: longint;
        negative: boolean;

      begin
         negative := false;
         s:='';
         { Workaround: }
         if l=$80000000 then
           begin
              s:='-2147483648';
              exit;
           end;
        { handle case where l = 0 }
         if l = 0 then
         begin
           s:='0';
           exit;
         end;
         If l < 0 then
         begin
             negative := true;
             value:=abs(l);
         end
         else
             value:=l;
       { handle non-zero case }
       while value>0 do
         begin
            s:=char((value mod 10)+ord('0'))+s;
            value := value div 10;
         end;
         if negative then
           s := '-' + s;
      end;


{$IFNDEF NEW_READWRITE}
    procedure f1;[public,alias: 'FLUSH_STDOUT'];

      begin
         asm
            { Save Registers }
            movem.l d0-a7,-(sp)
         end;
         FileFunc(textrec(output).flushfunc)(textrec(output));
         asm
            { Restore all registers in the correct order }
            movem.l (sp)+,d0-a7
         end;
      end;
{$ENDIF NEW_READWRITE}

Function Sptr : Longint;
begin
  asm
    move.l sp,d0
    add.l  #8,d0
    move.l d0,@RESULT
  end ['d0'];
end;




 Procedure BoundsCheck;assembler;
 { called by code generator with R+ state to    }
 { determine if a range check occured.          }
 { Only in 68000 mode, in 68020 mode this is    }
 { inline.                                      }
 { On Entry:                                    }
 {   A1 = address contaning min and max indexes }
 {   D0 = value of current index to check.      }
 asm
XDEF RE_BOUNDS_CHECK
  cmp.l   (A1),D0        { lower bound ...    }
  bmi     @rebounderr    { is index lower ... }
  add.l   #4,A1
  cmp.l   (A1),D0
  bmi     @reboundend
  beq     @reboundend
@rebounderr:
  move.l  #201,d0
  jsr     HALT_ERROR
@reboundend:
 end;

{
  $Log$
  Revision 1.9  1998-07-30 13:26:14  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.8  1998/07/10 11:02:41  peter
    * support_fixed, becuase fixed is not 100% yet for the m68k

  Revision 1.7  1998/07/02 12:20:58  carl
    + Io-Error and overflow print erroraddr in hex now

  Revision 1.6  1998/07/01 14:25:57  carl
    * strconcat was copying one byte too much
    * strcopy bugfix was using signed comparison
    + STRCOPY uses register calling conventions
    * FillChar bugfix was loading a word instead of a byte

  Revision 1.2  1998/03/27 23:48:06  carl
    * bugfix of STRCONCAT alignment problem

  Revision 1.18  1998/03/02 04:17:24  carl
    * problem with CHECK_OBJECT fixed, will probably only work with
      GNU tools, as the VMT pointer is an .lcomm and might not be
      zeroed automatically by other loaders.
    * CHECK_OBJECT was not jumping on right condition

  Revision 1.17  1998/02/23 02:26:06  carl
    * bugfix to make it link without problems

  Revision 1.13  1998/02/06 16:35:35  carl
    * oops commited wrong file

  Revision 1.11  1998/01/26 12:01:32  michael
  + Added log at the end


  
  Working file: rtl/m68k/m68k.inc
  description:
  ----------------------------
  revision 1.10
  date: 1998/01/19 10:21:36;  author: michael;  state: Exp;  lines: +1 -12
  * moved Fillchar t(..,char) to system.inc
  ----------------------------
  revision 1.9
  date: 1998/01/13 03:47:39;  author: carl;  state: Exp;  lines: +3 -3
    * bugfix of BoundsCheck invalid opcodes
  ----------------------------
  revision 1.8
  date: 1998/01/13 03:24:58;  author: carl;  state: Exp;  lines: +2 -2
    * moveq.l #201 bugfix (This is of course an impossible opcode)
  ----------------------------
  revision 1.7
  date: 1998/01/12 15:24:47;  author: carl;  state: Exp;  lines: +1 -20
    * bugfix, a function was being duplicated.
  ----------------------------
  revision 1.6
  date: 1998/01/12 03:40:11;  author: carl;  state: Exp;  lines: +2 -2
    * bugfix of RE_OVERFLOW, now gives out a runerror(215)
  ----------------------------
  revision 1.5
  date: 1998/01/05 00:31:43;  author: carl;  state: Exp;  lines: +206 -119
  * Bugfix of syntax errors
  ----------------------------
  revision 1.4
  date: 1998/01/01 16:50:16;  author: michael;  state: Exp;  lines: +1 -21
  - Moved Do_exit to system.inc. Now processor independent.
  ----------------------------
  revision 1.3
  date: 1997/12/10 12:15:05;  author: michael;  state: Exp;  lines: +2 -2
  * changed dateifunc to FileFunc.
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:37:21;  author: michael;  state: Exp;  lines: +14 -0
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
