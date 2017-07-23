{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Karoly Balogh,
    member of the Free Pascal development team.

    Syscalls for *BSD/m68k

    Tested with NetBSD/m68k (ELF)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


function FpSysCall(sysnr:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL0'];
asm
  move.l  sysnr,d0
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL1'];
asm
  move.l  sysnr,d0
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL2'];
asm
  move.l  sysnr,d0
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL3'];
asm
  move.l  sysnr,d0
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL4'];
asm
  move.l  sysnr,d0
  move.l  param4,-(sp)
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL5'];
asm
  move.l  sysnr,d0
  move.l  param5,-(sp)
  move.l  param4,-(sp)
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL6'];
asm
  move.l  sysnr,d0
  move.l  param6,-(sp)
  move.l  param5,-(sp)
  move.l  param4,-(sp)
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL7'];
asm
  move.l  sysnr,d0
  move.l  param7,-(sp)
  move.l  param6,-(sp)
  move.l  param5,-(sp)
  move.l  param4,-(sp)
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6,param7,param8:TSysParam):TSysResult; assembler; stdcall; [public,alias:'FPC_SYSCALL8'];
asm
  move.l  sysnr,d0
  move.l  param8,-(sp)
  move.l  param7,-(sp)
  move.l  param6,-(sp)
  move.l  param5,-(sp)
  move.l  param4,-(sp)
  move.l  param3,-(sp)
  move.l  param2,-(sp)
  move.l  param1,-(sp)
  move.l  #0,-(sp)
  trap    #0
  bcc     @noerror
  jsr     seterrno
  moveq.l #-1,d0
@noerror:
end;
