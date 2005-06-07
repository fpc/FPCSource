{******************************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000-2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This file was adapted from
Guardian:/usr/local/src/glibc-2.2.3/sysdeps/sparc/sparc32# more setjmp.S
Guardian:/usr/local/src/glibc-2.2.3/sysdeps/sparc/sparc32# more __longjmp.S
    Copyright (C) 1991, 93, 94, 96, 97, 98 Free Software Foundation, Inc.
    This file is part of the GNU C Library.

    The GNU C Library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    The GNU C Library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with the GNU C Library; see the file COPYING.LIB.  If not,
    write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

******************************************************************************}
{@Define the machine-dependent type `jmp_buf'.  SPARC version.}


type
  jmp_buf=packed record
    {stack pointer}
    JB_SP,
    {frame pointer}
    JB_FP,
    {program counter}
    JB_PV:Pointer;
  end;
  Pjmp_buf=^jmp_buf;

function setjmp(var S:jmp_buf):longint;
procedure longjmp(var S:jmp_buf;value:longint);
