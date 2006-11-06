{
    Copyright (c) 2002 by the Free Pascal development team

    Syscall implementation for linux m68k

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************

}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL0'];
  asm
  end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL1'];
  asm
  end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL2'];
  asm
  end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL3'];
  asm
  end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL4'];
  asm
  end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL5'];
  asm
  end;


{$ifdef notsupported}
function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL6'];
  asm
  end;


{$endif notsupported}
