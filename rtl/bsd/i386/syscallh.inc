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

Type

  TSysResult = longint; // all platforms, cint=32-bit.
                        // On platforms with off_t =64-bit, people should
                        // use int64, and typecast all calls that don't
                        // return off_t to cint.

// I don't think this is going to work on several platforms
// 64-bit machines don't have only 64-bit params.

  TSysParam  = Longint;

function do_sysCall(sysnr:TSysParam):TSysResult;{$ifndef VER1_0} oldfpccall;{$endif}   external name 'FPC_DOSYS0';
function do_sysCall(sysnr,param1:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall;{$endif}external name 'FPC_DOSYS1';
//function do_sysCall(sysnr,param1:integer):TSysResult; {$ifndef VER1_0} oldfpccall;{$endif}external name 'FPC_DOSYS1w';
function do_sysCall(sysnr,param1,param2:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall;{$endif} external name 'FPC_DOSYS2';
function do_sysCall(sysnr,param1,param2,param3:TSysParam):TSysResult;{$ifndef VER1_0} oldfpccall;{$endif} external name 'FPC_DOSYS3';
function do_sysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult;{$ifndef VER1_0} oldfpccall;{$endif} external name 'FPC_DOSYS4';
function do_sysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall;{$endif}  external name 'FPC_DOSYS5';
function do_sysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):int64;{$ifndef VER1_0} oldfpccall;{$endif}  external name 'FPC_DOSYS6';
function do_sysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:TSysParam):int64; {$ifndef VER1_0} oldfpccall;{$endif}  external name 'FPC_DOSYS7';

{
  $Log$
  Revision 1.8  2003-09-15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.7  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.6  2003/05/30 19:58:40  marco
   * Getting NetBSD/i386 to compile.

  Revision 1.5  2003/01/05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.4  2002/10/16 18:44:00  marco
   * and again for ftruncate (sigh)

  Revision 1.3  2002/10/16 18:41:14  marco
   * the 7 param syscall (for lseek and truncate) now returns a int64.

  Revision 1.2  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/20 08:28:14  marco
   * Updates for new errno scheme.

}
