{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    Syscall functions for i386 *BSD.

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

procedure actualsyscall; assembler;

asm
  sc
  b LSyscallError
  blr
LSyscallError:
  stw r3,errno
end;


function Do_SysCall(sysnr:LONGINT):longint; assembler; [public,alias:'FPC_DOSYS0'];

asm
  mr  r0,r3
  b   actualsyscall
end;


function Do_SysCall(sysnr,param1:longint):longint; assembler;[public,alias:'FPC_DOSYS1'];

 asm
  mr  r0,r3
  mr  r3,r4
  b   actualsyscall
 end;


function Do_SysCall(sysnr,param1:integer):longint; assembler;[public,alias:'FPC_DOSYS1w'];

 asm
  rlwinm r0,r3,0,0,15
  extsh  r3,r4
  b   actualsyscall
 end;


function Do_SysCall(sysnr,param1,param2:LONGINT):longint; assembler; [public,alias:'FPC_DOSYS2'];

 asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  b   actualsyscall
 end;


function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint; assembler;[public,alias:'FPC_DOSYS3'];

 asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  b   actualsyscall   
end;


function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint; assembler;[public,alias:'FPC_DOSYS4'];

asm
  mr  r0,r3
  mr  r3,r4   
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  b   actualsyscall
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;  assembler;[public,alias:'FPC_DOSYS5'];

 asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6           
  mr  r6,r7
  mr  r7,r8
  b   actualsyscall
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:LONGINT):int64;  assembler;[public,alias:'FPC_DOSYS6'];

asm
  mr  r0,r3
  mr  r3,r4           
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7   
  mr  r7,r8
  mr  r8,r9
  b   actualsyscall
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):int64;  assembler; [public,alias:'FPC_DOSYS7'];

asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5   
  mr  r5,r6   
  mr  r6,r7      
  mr  r7,r8
  mr  r8,r9
  mr  r9,r10
  b   actualsyscall
end;

{
  $Log$
  Revision 1.1  2002-10-26 14:33:09  jonas
    + initial version
:

}
