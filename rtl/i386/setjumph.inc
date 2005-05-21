{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 the Free Pascal development team

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
  jmp_buf = packed record
    ebx,esi,edi : Longint;
    bp,sp,pc : Pointer;
    end;
  PJmp_buf = ^jmp_buf;

Function Setjmp (Var S : Jmp_buf) : longint;
Procedure longjmp (Var S : Jmp_buf; value : longint);

{
  $Log: setjumph.inc,v $
  Revision 1.8  2005/04/24 21:19:22  peter
    * unblock signal in signalhandler, remove the sigprocmask call
      from setjmp

  Revision 1.7  2005/02/14 17:13:22  peter
    * truncate log

  Revision 1.6  2005/01/24 16:54:16  peter
    * no sigprocmask for 1.0.x bootstrapping

  Revision 1.5  2005/01/20 16:38:28  peter
    * restore sigprocmask for linux

}
