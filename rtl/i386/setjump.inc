{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

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
  movl 4(%ebp),%edx
  movl %edx,20(%eax)
  movl (%ebp),%edx
  movl %edx,12(%eax)
  leal 8(%ebp),%edx
  movl %edx,16(%eax)
  xorl %eax,%eax
end;


Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
  movl 8(%ebp),%ecx
  movl 12(%ebp),%eax
  testl %eax,%eax
  jne .Ljnonzero
  movl $1,%eax
.Ljnonzero:
  movl (%ecx),%ebx
  movl 4(%ecx),%esi
  movl 8(%ecx),%edi
  movl 12(%ecx),%ebp
  movl 16(%ecx),%esp
  jmp 20(%ecx)
end;

{
  $Log$
  Revision 1.5  1999-03-01 15:41:00  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.4  1998/09/14 10:48:13  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.3  1998/08/11 00:04:52  peter
    * $ifdef ver0_99_5 updates

}
