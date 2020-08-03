{
    Copyright (c) 2016 by Free Pascal development team

    XBIOS interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit xbios;

interface

{ The API description of this file is based on the information available
  online at: http://toshyp.atari.org }

function xbios_physbase: pointer; syscall 14 2;
function xbios_logbase: pointer; syscall 14 3;
function xbios_getrez: longint; syscall 14 4;
procedure xbios_setscreen(laddr: pointer; paddr: pointer; rez: smallint); syscall 14 5;
procedure xbios_setpalette(palette: pointer); syscall 14 6;
function xbios_setcolor(colornum: smallint; color: smallint): smallint; syscall 14 7;

function xbios_random: longint; syscall 14 17;

procedure xbios_vsync; syscall 14 37;

implementation

end.
