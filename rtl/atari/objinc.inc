{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Includefile for objects.pp implementing OS-dependent file routines
    for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o Implement SetfileSize                                            }
{--------------------------------------------------------------------}



{---------------------------------------------------------------------------}
{  FileClose -> Platforms Atari TOS        - Not checked                    }
{---------------------------------------------------------------------------}
FUNCTION FileClose(Handle: THandle): word;
begin
  asm
        movem.l d2/d3/a2/a3,-(sp)
        move.w  Handle,d0
        move.w  d0,-(sp)
        move.w  #$3e,-(sp)
        trap    #1
        add.l   #4,sp      { restore stack ... }
        movem.l (sp)+,d2/d3/a2/a3
  end;
   FileClose := 0;
end;

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms Atari TOS        - 08Jul98 CEC                     }
{  Returns 0 on failure                                                     }
{---------------------------------------------------------------------------}

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
var
 oflags : longint;
 AHandle : THandle;
begin
  AHandle:=0;
  { On opening reset error code }
  DosStreamError := 0;
  if Mode=stCreate then
      oflags:=4
  else
      { read/write access on existing file }
      oflags := Mode and 3;

      asm
         movem.l d2/d3/a2/a3,-(sp)    { save used registers }

         cmp.l   #4,oflags    { check if rewrite mode ... }
         bne     @opencont2
         { rewrite mode - create new file }
         move.w  #0,-(sp)
         move.l  FileName,-(sp)
         move.w  #$3c,-(sp)
         trap    #1
         add.l   #8,sp       { restore stack of os call }
         bra     @end
         { reset - open existing files     }
      @opencont2:
         move.w  oflags,d0    { use flag as source  ...    }
      @opencont1:
         move.w  d0,-(sp)
         move.l  FileName,-(sp)
         move.w  #$3d,-(sp)
         trap    #1
         add.l   #8,sp       { restore stack of os call }
       @end:
         movem.l (sp)+,d2/d3/a2/a3

         tst.w   d0
         bpl     @opennoerr  { if positive return values then ok }
         cmp.w   #-1,d0      { if handle is -1 CON:              }
         beq     @opennoerr
         cmp.w   #-2,d0      { if handle is -2 AUX:              }
         beq     @opennoerr
         cmp.w   #-3,d0      { if handle is -3 PRN:              }
         beq     @opennoerr
         move.w  d0,dosStreamError    { otherwise normal error            }
    @opennoerr:
         move.w  d0,AHandle  { get handle as SIGNED VALUE...     }
      end;
   FileOpen := AHandle;
end;


{***************************************************************************}
{  DosSetFilePtr -> Platforms Atari TOS    - 08Jul98 CEC                    }
{***************************************************************************}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
BEGIN
       asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.w MoveType,-(sp)     { seek from start of file    }
            move.w Handle,-(sp)
            move.l pos,-(sp)
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            move.l  Actual,a0
            move.l  d0,(a0)
       end;
   SetFilePos := DosStreamError;                   { Return any error }
END;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms Atari TOS        - 08Jul98 CEC                     }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l buf,-(sp)
            move.l Count,-(sp)
            move.w Handle,d0
            move.w d0,-(sp)
            move.w #$3f,-(sp)
            trap   #1
            lea    12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @dosrdend
            move.w  d0,DosStreamError    { error ... }
          @dosrdend:
  end;
  FileRead:=DosStreamError;
  Actual:=Count;
end;


{---------------------------------------------------------------------------}
{  FileWrite -> Platforms Atari TOS        - 08Jul98 CEC                    }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l  buf,-(sp)
            move.l  Count,-(sp)
            move.w  Handle,d0
            move.w  d0,-(sp)
            move.w  #$40,-(sp)
            trap    #1
            lea     12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @doswrend
            move.w  d0,DosStreamError    { error ... }
          @doswrend:
  end;
   Actual:=Count;
   FileWrite:=DosStreamError;
end;


{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms Atari TOS    - 08Jul98 CEC                      }
{---------------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
VAR Actual, Buf: LongInt;
BEGIN
   SetFilePos(Handle,FileSize,0,Actual);
   If (Actual = FileSize) Then
    Begin
      Actual := FileWrite(Handle, Pointer(@Buf), 0,Actual);   { Truncate the file }
      If (Actual <> -1) Then
       SetFileSize := 0
      Else
       SetFileSize := 103;                            { File truncate error }
    End
   Else
    SetFileSize := 103;                       { File truncate error }
END;

{
  $Log$
  Revision 1.3  2002-09-07 16:01:16  peter
    * old logs removed and tabs fixed

}
