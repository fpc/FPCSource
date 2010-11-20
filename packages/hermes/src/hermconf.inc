{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}


const
  {$IFDEF FPC_LITTLE_ENDIAN}
  A_32 = 3;                   { oh yes, hardcode it... tsk tsk tsk }
  R_32 = 2;                   { Position of R,G,B in a 32 bit dword (in ARGB8888 format) }
  G_32 = 1;
  B_32 = 0;
  R_24 = 2;                   { Position of R,G,B in a 24 bit pixel (in RGB888 format) }
  G_24 = 1;
  B_24 = 0;

  DWORD_BYTE0_SHL = 0;
  DWORD_BYTE1_SHL = 8;
  DWORD_BYTE2_SHL = 16;
  DWORD_BYTE3_SHL = 24;
  DWORD_SMALLINT0_SHL = 0;
  DWORD_SMALLINT1_SHL = 16;
  {$ELSE FPC_LITTLE_ENDIAN}
  A_32 = 0;
  R_32 = 1;
  G_32 = 2;
  B_32 = 3;
  R_24 = 0;
  G_24 = 1;
  B_24 = 2;

  DWORD_BYTE0_SHL = 24;
  DWORD_BYTE1_SHL = 16;
  DWORD_BYTE2_SHL = 8;
  DWORD_BYTE3_SHL = 0;
  DWORD_SMALLINT0_SHL = 16;
  DWORD_SMALLINT1_SHL = 0;
  {$ENDIF FPC_LITTLE_ENDIAN}
