{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by Free Pascal development team

    IOCS API unit for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit h68kiocs;
{$ENDIF FPC_DOTTEDUNITS}

interface

procedure _iocs_b_curon;
procedure _iocs_b_curoff;
function _iocs_crtmod(mode: longint): longint;
procedure _iocs_g_clr_on;
function _iocs_vpage(page: longint): longint;


implementation

procedure _iocs_b_curon; assembler; nostackframe;
asm
  moveq.l #$1e,d0
  trap #15
end;

procedure _iocs_b_curoff; assembler; nostackframe;
asm
  moveq.l #$1f,d0
  trap #15
end;

function _iocs_crtmod(mode: longint): longint; assembler; nostackframe;
asm
  move.l d0,d1
  moveq.l #$10,d0
  trap #15
end;

procedure _iocs_g_clr_on; assembler; nostackframe;
asm
  moveq.l #$ffffff90,d0
  trap #15
end;

function _iocs_vpage(page: longint): longint; assembler; nostackframe;
asm
  move.l d0,d1
  moveq.l #$ffffffb2,d0
  trap #15
end;

end.
