{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by Free Pascal development team

    Utility functions and constants unit for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit h68kutil;
{$ENDIF FPC_DOTTEDUNITS}

interface

const
  VERTICAL_BLANKING_DETECTION = longint($80000000);
  VERTICAL_BLANKING_NO_DETECT = 0;

const
  SP_DEFCG_8X8_TILE = 0;
  SP_DEFCG_16X16_TILE = 1;

implementation

end.
