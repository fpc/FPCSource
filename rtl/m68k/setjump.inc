{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
           Set_Jmp/Long_jmp
 **********************************************************************}

Function SetJmp (Var S : Jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];

asm
end;

Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];

asm
end;

{
  Revision 1.4  2005/02/14 17:13:30  peter
    * truncate log

}
