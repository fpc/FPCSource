{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Includefile for objects.pp implementing OS-dependent file routines
    for AmigaOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

    Const

    _LVOFindTask          = -294;
    _LVOWaitPort          = -384;
    _LVOGetMsg            = -372;
    _LVOOpenLibrary       = -552;
    _LVOCloseLibrary      = -414;
    _LVOClose             = -36;
    _LVOOpen              = -30;
    _LVOIoErr             = -132;
    _LVOSeek              = -66;
    _LVODeleteFile        = -72;
    _LVORename            = -78;
    _LVOWrite             = -48;
    _LVORead              = -42;
    _LVOCreateDir         = -120;
    _LVOSetCurrentDirName = -558;
    _LVOGetCurrentDirName = -564;
    _LVOInput             = -54;
    _LVOOutput            = -60;
    _LVOUnLock            = -90;
    _LVOLock              = -84;
    _LVOCurrentDir        = -126;

    _LVONameFromLock      = -402;
    _LVONameFromFH        = -408;
    _LVOGetProgramName    = -576;
    _LVOGetProgramDir     = -600;
    _LVODupLock           =  -96;
    _LVOExamine           = -102;
    _LVOParentDir         = -210;
    _LVOSetFileSize       = -456;




{---------------------------------------------------------------------------}
{  FileClose -> Platforms AmigaOS          - Not checked                    }
{---------------------------------------------------------------------------}
FUNCTION FileClose(Handle: THandle): word;
begin
  asm
            move.l  handle,d1
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOClose(a6)
            move.l  d6,a6              { restore a6 }
  end;
   FileClose := 0;
end;

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms AmigaOS          - 08Jul98 CEC                     }
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
      { read/write file with creation of file }
      oflags := 1006
  else
      { read/write access on existing file }
      oflags := 1005;
      asm
             move.l  a6,d6                  { save a6 }

             move.l  FileName,d1
             move.l  oflags,d2               { MODE_READWRITE }
             move.l  _DOSBase,a6
             jsr     _LVOOpen(a6)
             tst.l   d0
             bne     @noopenerror           { on zero an error occured }
             jsr     _LVOIoErr(a6)
             move.w  d0,DosStreamError
             bra     @openend
          @noopenerror:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,AHandle            { we need the base pointer to access this variable }
             bra     @end
          @openend:
             move.l  d6,a6                 { restore a6 }
          @end:
         end;
   FileOpen := AHandle;
end;


{***************************************************************************}
{  DosSetFilePtr -> Platforms AmigaOS      - 08Jul98 CEC                    }
{***************************************************************************}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
Var
  Move_typ : longint;
BEGIN
  Move_typ := 0;
  { Move from beginning of file }
  if MoveType = 0 then
   Move_typ := -1;
  { Move from current position of file }
  If MoveType = 1 then
   Move_typ := 0;
  { Move from end of file              }
  If MoveType = 2 then
    Move_typ := 1;
       { We have to seek TWO times, if we wish to get the actual absolute }
       { file position normally.                                          }
       asm
             move.l  a6,d6                { Save base pointer            }

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers              }

             move.l  pos,d2
             move.l  Move_typ,d3           { Setup correct move type     }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr_one            { no, then seek a second time   }
             jsr     _LVOIoErr(a6)         { yes ,get error in d0 and jmp  }
             bra     @err
      @noerr_one:                          { Seek a second time            }
             move.l  d6,a6                 { Restore base pointer          }

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers                }

             move.l  pos,d2
             move.l  Move_typ,d3           { Setup correct move type     }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
      @err:
             move.w  d0,DosStreamError
             move.l  d6,a6                 { restore a6 }
             bra     @seekend
      @noerr:
             move.l  d6,a6                 { restore a6 }
             move.l  Actual,a0             { Get address of variable }
             move.l  d0,(a0)               { Set value of Actual     }
      @seekend:
       end;
   SetFilePos := DosStreamError;                   { Return any error }
END;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms AmigaOS          - 08Jul98 CEC                     }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  if Count <= 0 then
  Begin
     FileRead:=1;  { Return a non zero error }
     exit;
  end;
  asm
            move.l  a6,d6

            movem.l d2/d3,-(sp)
            move.l  handle,d1         { we must set up aparamters BEFORE }
            move.l  buf,d2            { setting up a6 for the OS call    }
            move.l  count,d3
            move.l  _DOSBase,a6
            jsr     _LVORead(a6)
            movem.l (sp)+,d2/d3

            cmp.l   #-1,d0
            bne     @doswrend              { if -1 = error }
            jsr     _LVOIoErr(a6)
            move.w  d0,DosStreamError
            bra     @doswrend2
          @doswrend:
            { to store a result for the function  }
            { we must of course first get back the}
            { base pointer!                       }
            move.l  d6,a6
            move.l  Actual,a0   { Actual is a pointer! }
            move.l  d0,(a0)
            bra     @end
          @doswrend2:
            move.l  d6,a6
          @end:
  end;
  FileRead:=DosStreamError;
end;


{---------------------------------------------------------------------------}
{  FileWrite -> Platforms AmigAOS          - 08Jul98 CEC                    }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
  if Count <= 0 then
   Begin
    FileWrite:=1;  { Return a non zero error code }
    exit;
   end;
  asm
            move.l  a6,d6

            movem.l d2/d3,-(sp)
            move.l  handle,d1             { we must of course set up the }
            move.l  buf,d2                { parameters BEFORE getting    }
            move.l  count,d3              { _DOSBase                     }
            move.l  _DOSBase,a6
            jsr     _LVOWrite(a6)
            movem.l (sp)+,d2/d3

            cmp.l   #-1,d0
            bne     @doswrend              { if -1 = error }
            jsr     _LVOIoErr(a6)
            move.w  d0,DosStreamError
            bra     @doswrend2
          @doswrend:
            { we must restore the base pointer before setting the result }
            move.l  d6,a6
            move.l  Actual,a0   { Actual is a pointer! }
            move.l  d0,(a0)
            bra     @end
          @doswrend2:
            move.l  d6,a6
          @end:
  end;
   Actual:=Count;
   FileWrite:=DosStreamError;
end;


{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms AmigaOS      - 08Jul98 CEC                      }
{---------------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
BEGIN
      { Point to the end of the file }
      { with the new size            }
      asm
      @noerr_one:                          { Seek a second time            }
             move.l  a6,d6                 { Save base pointer             }

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers                }

             move.l  FileSize,d2
             move.l  #-1,d3                { Setup correct move type     }
             move.l  _DOSBase,a6           { from beginning of file      }
             jsr    _LVOSetFileSize(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.w  d0,DosStreamError
      @noerr:
             move.l  d6,a6                 { restore a6 }
      end;
      SetFileSize:=DosStreamError;
END;

{
  $Log$
  Revision 1.3  2002-09-07 16:01:16  peter
    * old logs removed and tabs fixed

}
