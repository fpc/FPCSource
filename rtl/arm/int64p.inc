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

{!!!!
function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
asm
  mov r6,#0
  umull r4,r5,r0,r2
  umlal r5,r6,r0,r3
  { overflow? }
  or r0,r1,r3
  umlal r5,r6,r1,r2
  { check for overflow }
  ors r6,r6,r0
  mov r0,r4
  mov r1,r5
end;
}


{
  $Log$
  Revision 1.2  2004-03-14 21:45:11  florian
    * draft for qword mul

  Revision 1.1  2003/11/03 17:28:21  florian
    * initial revision
}
