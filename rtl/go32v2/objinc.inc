{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Includefile for objects.pp implementing OS-dependent file routines
    for Go32V2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

{---------------------------------------------------------------------------}
{  FileClose -> Platforms DOS              - Not checked                    }
{---------------------------------------------------------------------------}
FUNCTION FileClose (Handle: THandle): Word;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realeax:=$3e00;
  sysrealintr($21,regs);
  FileClose := 0;
end;


{---------------------------------------------------------------------------}
{  FileOpen -> Platforms DOS              - Checked 05May1998 CEC           }
{  Returns 0 on failure                                                     }
{---------------------------------------------------------------------------}
FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
Var
  regs : trealregs;
BEGIN
         DosStreamError:=0;
         syscopytodos(longint(@FileName),256);
         { get linear address from system unit }
         regs.realedx:=tb mod 16;
         regs.realds:=tb div 16;
         if LFNSupport then
           begin
             if (mode = stCreate) then
               begin
                 regs.realeax := $716C;
                 regs.realesi:=tb mod 16;
                 regs.realebx:=$2002;
                 regs.realecx:=$20;
                 regs.realedi:=0;
                 regs.realedx:=$12;
               end
             else if (mode = stOpenRead) then
               begin
                 regs.realeax := $716C;
                 regs.realesi:=tb mod 16;
                 regs.realebx:=$2000;
                 regs.realecx:=$20;
                 regs.realedi:=0;
                 regs.realedx:=$1;
               end
             else if (mode = stOpenWrite) then
               begin
                 regs.realeax := $716C;
                 regs.realesi:=tb mod 16;
                 regs.realebx:=$2001;
                 regs.realecx:=$20;
                 regs.realedi:=0;
                 regs.realedx:=$11;
               end
             else if (mode = stOpen) then
               begin
                 regs.realeax := $716C;
                 regs.realesi:=tb mod 16;
                 regs.realebx:=$2002;
                 regs.realecx:=$20;
                 regs.realedi:=0;
                 regs.realedx:=$11;
               end
             else
               regs.realeax := Mode;
           end
         else
           regs.realeax := Mode;
         regs.realecx:=0;
         sysrealintr($21,regs);
         if (regs.realflags and 1) <> 0 then
           begin
             DosStreamError:=lo(regs.realeax);
             FileOpen:=$0;
             exit;
           end
         else
            { word handle (under DOS) }
            FileOpen:=regs.realeax and $ffff;
END;


{---------------------------------------------------------------------------}
{  SetFilePos -> Platforms DOS             - Checked 05May1998 CEC          }
{---------------------------------------------------------------------------}
{
  Calls the operating system to move the file denoted by the handle to
  to the requested position. The move method can be: 0 = absolute offset;
  1 = offset from present location; 2 = offset from end of file;
  Any error is held in DosErrorStream and returned from the call.
  If the return is zero (ie no error) NewPos contains the new absolute
  file position.
}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;Var Actual: LongInt): Word;
Var
 regs: Trealregs;
const
 CarryFlag = $001;
BEGIN
  regs.realeax := ($42 shl 8) + Byte(MoveType);
  regs.realedx := pos and $ffff;   { keep low word }
  regs.realecx := pos shr 16;
  regs.realebx := longint(Handle);
  sysrealintr($21,regs);
  if (regs.RealFlags and CarryFlag = 0) then { no error }
     Actual:=(regs.realeax and $ffff) + ((regs.realedx and $ffff) shl 16)
  else
     DosStreamError:=word(regs.realeax);
  SetFilePos := DosStreamError;                   { Return any error }
END;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms DOS              - Checked 05May1998 CEC           }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
Var
  StoreInOutRes : word;
BEGIN
  StoreInOutRes:=InOutRes;
  InOutRes:=0;
  Actual:=system.do_read(longint(Handle),longint(@Buf),Count);
  DosStreamError:=InOutRes;
  InOutRes:=StoreInOutRes;
  FileRead:=DosStreamError;
End;


{---------------------------------------------------------------------------}
{  FileWrite -> Platforms DOS              - Checked 05May1998 CEC          }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
Var
  StoreInOutRes : word;
BEGIN
  StoreInOutRes:=InOutRes;
  InOutRes:=0;
 system.do_write(longint(Handle),longint(@Buf),Count);
 Actual:=Count;
 DosStreamError:=InOutRes;
 InOutRes:=StoreInOutRes;
 FileWrite:=DosStreamError;
End;


{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms DOS          - Not Checked                      }
{---------------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
VAR Actual: LongInt;
  regs : trealregs;
const
 CarryFlag = $001;
BEGIN
   SetFilePos(Handle,FileSize,0,Actual);
   If (Actual = FileSize) Then
    Begin
      regs.realecx:=0;
      regs.realedx:=tb mod 16;
      regs.realds:=tb div 16;
      regs.realebx:=handle;
      regs.realeax:=$4000;
      sysrealintr($21,regs);
      if (regs.RealFlags and CarryFlag = 0) then { no error }
       SetFileSize := 0
      Else
       SetFileSize := 103;                            { File truncate error }
    End
   Else
    SetFileSize := 103;                       { File truncate error }
END;

{
  $Log$
  Revision 1.3  2002-05-09 08:42:24  carl
  * Merges from Fixes branch

  Revision 1.1.2.1  2002/04/12 08:08:08  pierre
   * avoid that objects unit is sensitive to or changes system.InOutRes value

  Revision 1.1  2000/07/13 06:30:39  michael
  + Initial import

  Revision 1.7  2000/06/30 22:16:19  peter
    * fixed truncate

  Revision 1.6  2000/05/30 06:45:53  marco
   * fixed in line 164 pointer(@buf) to buf

  Revision 1.5  2000/03/20 19:19:44  pierre
   * LFN support in streams

  Revision 1.4  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.3  2000/01/07 16:41:32  daniel
    * copyright 2000

  Revision 1.2  2000/01/07 16:32:23  daniel
    * copyright 2000 added

}
