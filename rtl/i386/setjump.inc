{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function SetJmp (Var S : Jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
asm
  movl 8(%ebp),%eax
  movl %ebx,(%eax)
  movl %esi,4(%eax)
  movl %edi,8(%eax)
  movl 4(%ebp),%edi
  movl %edi,20(%eax)
  movl (%ebp),%edi
  movl %edi,12(%eax)
  leal 12(%ebp),%edi
  movl %edi,16(%eax)
  movl 8(%eax),%edi
  xorl %eax,%eax
end['EAX'];


Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
  movl 8(%ebp),%ecx
  movl 12(%ebp),%eax
  movl (%ecx),%ebx
  movl 4(%ecx),%esi
  movl 8(%ecx),%edi
  movl 12(%ecx),%ebp
  movl 16(%ecx),%esp
  // we should also clear the fpu
  // fninit no must be done elsewhere PM
  // or we should reset the control word also
  jmp 20(%ecx)
end;

{
  $Log$
  Revision 1.13  2000-05-04 09:47:40  pierre
    * Preserve all registers in SetJmp
    * set EAX to value filed in LongJmp instead of only 0 or 1

  Revision 1.12  2000/03/31 23:12:09  pierre
   * remove fninit in longjump

  Revision 1.11  2000/02/18 16:16:13  florian
    * we don't need to to finit twice ...

  Revision 1.10  2000/02/18 15:23:01  florian
    * fixed constants in rtti
    * longjmp does now a finit

  Revision 1.9  2000/02/09 22:12:54  florian
    * longjump does now a finit

  Revision 1.8  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.7  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.6  1999/08/18 10:42:13  pierre
   * loading of esp value corrected
}