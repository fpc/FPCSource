{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    Header for syscall in system unit for i386 *BSD.

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

type
  TSysResult = int64;   // all platforms, cint=32-bit.
                        // On platforms with off_t =64-bit, people should
                        // use int64, and typecast all calls that don't
                        // return off_t to cint.

// I don't think this is going to work on several platforms
// 64-bit machines don't have only 64-bit params.

  TSysParam  = int64;

function Do_SysCall(sysnr:TSysParam):TSysResult;  external name 'FPC_SYSCALL0';
function Do_SysCall(sysnr,param1:TSysParam):TSysResult; external name 'FPC_SYSCALL1';
function Do_SysCall(sysnr,param1,param2:TSysParam):TSysResult;  external name 'FPC_SYSCALL2';
function Do_SysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; external name 'FPC_SYSCALL3';
function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; external name 'FPC_SYSCALL4';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult;  external name 'FPC_SYSCALL5';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult;  external name 'FPC_SYSCALL6';

{
  $Log$
  Revision 1.4  2004-04-22 17:17:23  peter
    * x86-64 fixes

  Revision 1.3  2004/02/06 15:58:21  florian
    * fixed x86-64 assembler problems

  Revision 1.2  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.1  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}