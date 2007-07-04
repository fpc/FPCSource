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


{*****************************************************************************
                              Heap Management
*****************************************************************************}

function __sbrk(size:longint):longint;cdecl;external;

function SysOSAlloc (size: ptruint): pointer; assembler;
asm
{$ifdef SYSTEMDEBUG}
        cmpb    $1,accept_sbrk
        je      .Lsbrk
        movl    $0,%eax
        jmp     .Lsbrk_fail
      .Lsbrk:
{$endif}
        movl    size,%eax
        pushl   %eax
        call    __sbrk
        addl    $4,%esp
{$ifdef SYSTEMDEBUG}
      .Lsbrk_fail:
{$endif}
end;

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

{function SysOSAlloc(size: ptrint): pointer;
begin
  result := sbrk(size);
end;
}
{.$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin
end;



