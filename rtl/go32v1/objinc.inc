{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Includefile for objects.pp implementing OS-dependent file routines
    for Go32V1

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
FUNCTION FileClose(Handle: THandle): word;
begin
   asm
      xor  %bx,%bx
      movw handle,%bx
      movb $0x3e,%ah
      pushl %ebp
      int $0x21
      popl %ebp
   end;
   FileClose := 0;
end;

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms DOS              - Checked 05May1998 CEC           }
{  Returns 0 on failure                                                     }
{---------------------------------------------------------------------------}

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
var
 AMode: word;
begin
  if Mode=stCreate then
    Begin
      AMode:=$8302;
    end
  else
    Begin
      Case (Mode and 3) of
       0 : AMode:=$8001;
       1 : AMode:=$8404;
       2 : AMode:=$8404;
      end;
    end;
   asm
     xorl  %eax, %eax
     movw  %ax, DosStreamError
     movl  FileName, %ebx
     movw  $0xff02, %ax
     movw  AMode, %cx
     pushl %ebp
     int   $0x21
     popl  %ebp
     jnc   .Lexit1
     movw  %ax, DosStreamError                        { Hold Error  }
     xorl  %eax, %eax                                 { Open Failed }
   .Lexit1:
     movw  %ax, __RESULT
   END;
end;


{***************************************************************************}
{  DosSetFilePtr -> Platforms DOS          - Checked 05May1998 CEC          }
{***************************************************************************}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
Var
 val : longint;
BEGIN
  asm
     movw MoveType, %ax;                              { Load move type }
     movb $0x42, %ah;
     movl pos, %edx;                              { Load file position }
     andl $0xffff,%edx                            { Only keep low word }
     movl pos, %ecx
     shrl $16,%ecx;
     movw Handle, %bx;                              { Load file handle }
     pushl %ebp;
     int $0x21;                                         { Position the file }
     popl %ebp;
     jc .Lexit4
     shll    $16,%edx
     movzwl  %ax,%eax
     orl     %edx,%eax
     movl    %eax,val                               { Update new position }
     xorl %eax, %eax;
   .Lexit4:
     movw %ax, DosStreamError                         { DOS error returned }
   .Lend:
   END;
   Actual := val;
   SetFilePos := DosStreamError;                   { Return any error }
END;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms DOS              - Checked 05May1998 CEC           }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  asm
     movl count,%ecx
     movl buf,%edx
     xorl %ebx,%ebx
     movw handle,%bx
     movb $0x3f,%ah
     int $0x21
     jnc .LDOSREAD1
     movw %ax,DosStreamError
     xorl %eax,%eax
  .LDOSREAD1:
  end;
  Actual:=Count;
  FileRead:=DosStreamError;
end;


{---------------------------------------------------------------------------}
{  FileWrite -> Platforms DOS              - Checked 05May1998 CEC          }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
  Actual:=0;
  asm
     movl Count,%ecx
     movl buf,%edx
     xorl %ebx,%ebx
     movw Handle,%bx
     movb $0x40,%ah
     pushl %ebp
     int $0x21
     pop   %ebp
     jnc .LDOSWRITE1
     movw %ax,DosStreamError
  .LDOSWRITE1:
  end;
  Actual:=Count;
  FileWrite:=DosStreamError;
end;


{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms DOS          - Not Checked                      }
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
  Revision 1.2  2000-07-13 11:33:38  michael
  + removed logs
 
}
