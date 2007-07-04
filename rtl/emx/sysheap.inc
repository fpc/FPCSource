{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************

                    Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):pointer;
{$IFDEF DUMPGROW}
var
  L: longword;
begin
  WriteLn ('Trying to grow heap by ', Size);
{$IFDEF CONTHEAP}
  WriteLn ('BrkLimit is ', BrkLimit);
{$ENDIF CONTHEAP}
  asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
    mov  %eax,L
  end ['eax', 'edx'];
  WriteLn ('New heap at ', L);
  Sbrk := pointer (L);
end;
{$ELSE DUMPGROW}
                                     assembler;
asm
{$IFDEF REGCALL}
    movl %eax,%edx
{$ELSE REGCALL}
    movl size,%edx
{$ENDIF REGCALL}
    movw $0x7f00,%ax
    call syscall
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
end {['eax', 'edx']};
{$ENDIF DUMPGROW}

function SysOSAlloc (Size: ptruint): pointer;
begin
 SysOSAlloc := Sbrk (Size);
end;

{.$define HAS_SYSOSFREE}

procedure SysOSFree (P: pointer; Size: ptruint);
begin
end;



