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

function Do_SysCall(sysnr:LONGINT):longint;  external name 'FPC_DOSYS0';
function Do_SysCall(sysnr,param1:longint):longint; external name 'FPC_DOSYS1';
function Do_SysCall(sysnr,param1:integer):longint; external name 'FPC_DOSYS1w';
function Do_SysCall(sysnr,param1,param2:LONGINT):longint;  external name 'FPC_DOSYS2';
function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint; external name 'FPC_DOSYS3';
function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint; external name 'FPC_DOSYS4';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;  external name 'FPC_DOSYS5';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:LONGINT):longint;  external name 'FPC_DOSYS6';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):int64;   external name 'FPC_DOSYS7';

{
  $Log$
  Revision 1.3  2002-10-16 18:41:14  marco
   * the 7 param syscall (for lseek and truncate) now returns a int64.

  Revision 1.2  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/20 08:28:14  marco
   * Updates for new errno scheme.


}
