{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl

    Contains SunOS specific errors for error.pp in rtl/unix

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  sys_errn = 128;

{ since we're libc-based, just just the libc version }
var
  sys_errlist:array[0..sys_errn-1] of pchar; cvar; external;

