{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_MUL_QWORD}
function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;assembler;[public,alias: 'FPC_MUL_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
asm
  mov r6,#0
  // lo(f1)*lo(f2)
  umull r4,r5,r0,r2
  // lo(f1)*hi(f2)
  umlal r5,r6,r0,r3
  // overflow?
  // hi(f1)*hi(f2)
  mul r0,r1,r3
  // hi(f1)*lo(f2)
  umlal r5,r6,r1,r2
  // check for overflow
  orrs r6,r6,r0
  mov r0,r4
  mov r1,r5
  // no overflow?
  beq .Lexit

  ldr r2,checkoverflow
  cmp r2,r2
  beq .Lexit

  mov r0,#215
  mov r1,fp
  bl HandleErrorFrame
.Lexit:
end ['r4','r5','r6'];


{
  $Log$
  Revision 1.3  2004-03-23 21:03:11  florian
    + assembler implementation of fpc_mul_qword
    * fpu exceptions are now generated

  Revision 1.2  2004/03/14 21:45:11  florian
    * draft for qword mul

  Revision 1.1  2003/11/03 17:28:21  florian
    * initial revision
}
