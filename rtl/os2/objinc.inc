{
	$Id$
	This file is part of the Free Pascal run time library.
	Copyright (c) 1999-2000 by the Free Pascal development team.

	Includefile for objects.pp implementing OS-dependent file routines
	for EMX (OS/2 & DOS)

	See the file COPYING.FPC, included in this distribution,
	for details about the copyright.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

{This is the correct way to call external assembler procedures.}
procedure syscall;external name '___SYSCALL';

FUNCTION FileClose(Handle: THandle): word;
begin
   asm
	  xor  %bx,%bx
	  movw handle,%bx
	  movb $0x3e,%ah
	  call syscall
   end;
   FileClose := 0;
end;

procedure AllowSlash (P: PChar);

{Allow slash as backslash.}

var I: longint;

begin
    for I := 0 to StrLen (P) do
        if P [I] = '/' then P [I] := '\';
end;

function FileOpen (var FileName: AsciiZ; Mode: word): THandle;
var AMode: longint;
begin
    if Mode = stCreate then
        AMode := $50000    (* Create / replace *)
    else
        AMode := Mode and $FF;
(* DenyAll if sharing not specified. *)
    if AMode and 112 = 0 then AMode := AMode or 16;
    asm
        xorl %eax, %eax
        movw %ax, DosStreamError
        movl FileName, %edx
        movl $0x7f0b, %eax
        movl AMode, %ecx
        call syscall
        cmpl $0xffffffff, %eax
        jnz .Lexit1
        movw  %cx, DosStreamError                        { Hold Error  }
        xorl  %eax, %eax                                 { Open Failed }
    .Lexit1:
        movw  %ax, __RESULT
    end;
end;


FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
Var
 val : longint;
BEGIN
  asm
	 movw MoveType, %ax;                              { Load move type }
	 movb $0x42, %ah;
	 movl pos, %edx;                              { Load file position }
	 movw Handle, %bx;                              { Load file handle }
	 call syscall
	 jc .Lexit4
	 movl    %eax,val                               { Update new position }
	 xorl %eax, %eax;
   .Lexit4:
	 movw %ax, DosStreamError                         { OS2 error returned }
   .Lend:
   END;
   Actual := val;
   SetFilePos := DosStreamError;                   { Return any error }
END;


FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  asm
	 movl count,%ecx
	 movl buf,%edx
	 xorl %ebx,%ebx
	 movw handle,%bx
	 movb $0x3f,%ah
	 call syscall
	 jnc .LDOSREAD1
	 movw %ax,DosStreamError
	 xorl %eax,%eax
  .LDOSREAD1:
  end;
  Actual:=Count;
  FileRead:=DosStreamError;
end;


FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
  Actual:=0;
  asm
	 movl Count,%ecx
	 movl buf,%edx
	 xorl %ebx,%ebx
	 movw Handle,%bx
	 movb $0x40,%ah
	 call syscall
	 jnc .LDOSWRITE1
	 movw %ax,DosStreamError
  .LDOSWRITE1:
  end;
  Actual:=Count;
  FileWrite:=DosStreamError;
end;


function SetFileSize (Handle: THandle; FileSize: longint): word; assembler;
asm
    movl $0x7F18, %eax
    movl Handle, %ebx
    movl FileSize,%edx
    call syscall
    jc .LSetFSize1
    movl $0x4202, %eax
    movl Handle, %ebx
    movl $0, %edx
    call syscall
    movl $0, %eax
    jnc .LSetFSize1
    decl %eax
.LSetFSize1:
end;

{
  $Log$
  Revision 1.5  2002-09-22 16:04:19  hajny
    * correction for THandle length change

  Revision 1.4  2002/09/07 16:01:24  peter
    * old logs removed and tabs fixed

}
