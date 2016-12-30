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

function xbios_random: longint; syscall 14 17;

implementation

end.
