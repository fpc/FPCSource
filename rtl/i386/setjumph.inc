{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 the Free Pascal development team

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef VER1_0}
  {$if (FPC_VERSION>1) or ((FPC_RELEASE>=9) and (FPC_PATCH>6))}
    {$ifdef linux}
      {$define needsigprocmask}
    {$endif}
  {$endif}
{$endif}

{$ifdef needsigprocmask}
const
{$ifdef FPC_USE_LIBC}
  jmpbufsigmaskwords = 1024 div 32;
{$else}
  jmpbufsigmaskwords = 128 div 32;
{$endif}

type
  tjmpsigset=array[0..jmpbufsigmaskwords-1] of longint;
  pjmpsigset=^tjmpsigset;
{$endif needsigprocmask}

Type
  jmp_buf = packed record
    ebx,esi,edi : Longint;
    bp,sp,pc : Pointer;
{$ifdef needsigprocmask}
    sigmask : tjmpsigset;
{$endif needsigprocmask}
    end;
  PJmp_buf = ^jmp_buf;

Function Setjmp (Var S : Jmp_buf) : longint;
Procedure longjmp (Var S : Jmp_buf; value : longint);

{
  $Log$
  Revision 1.6  2005-01-24 16:54:16  peter
    * no sigprocmask for 1.0.x bootstrapping

  Revision 1.5  2005/01/20 16:38:28  peter
    * restore sigprocmask for linux

  Revision 1.4  2002/09/15 10:04:31  carl
    * make the record packed (just in case)

  Revision 1.3  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
