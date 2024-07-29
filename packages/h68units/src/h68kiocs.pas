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
function _iocs_gpalet(colnum: longint; color: word): longint;
function _iocs_vpage(page: longint): longint;
function _iocs_sp_init: longint;
procedure _iocs_sp_on;
procedure _iocs_sp_off;
function _iocs_sp_defcg(code: longint; size: longint; addr: pointer): longint;
function _iocs_sp_regst(spno: longint; mode: longint; x,y: longint; code: dword; prio: longint): longint;
function _iocs_spalet(mode: longint; block: longint; color: longint): longint;


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

function _iocs_gpalet(colnum: longint; color: word): longint; assembler; nostackframe;
asm
  move.l d2,-(sp)
  move.l d1,d2
  move.l d0,d1
  moveq.l #$ffffff94,d0
  trap #15
  move.l (sp)+,d2
end;

function _iocs_vpage(page: longint): longint; assembler; nostackframe;
asm
  move.l d0,d1
  moveq.l #$ffffffb2,d0
  trap #15
end;

function _iocs_sp_init: longint; assembler; nostackframe;
asm
  moveq.l #$ffffffc0,d0
  trap #15
end;

procedure _iocs_sp_on; assembler; nostackframe;
asm
  moveq.l #$ffffffc1,d0
  trap #15
end;

procedure _iocs_sp_off; assembler; nostackframe;
asm
  moveq.l #$ffffffc2,d0
  trap #15
end;

function _iocs_sp_defcg(code: longint; size: longint; addr: pointer): longint; assembler; nostackframe;
asm
  move.l d2,-(sp)
  move.l d1,d2
  move.l d0,d1
  move.l a0,a1
  moveq #$ffffffc4,d0
  trap #15
  move.l (sp)+,d2
end;

function _iocs_sp_regst(spno: longint; mode: longint; x,y: longint; code: dword; prio: longint): longint; assembler; nostackframe;
asm
  movem.l d2/d3/d4/d5,-(sp)
  movem.l 20(sp),d2/d3/d4/d5
  and.l #$80000000,d1
  move.b d0,d1
  moveq.l #$ffffffc6,d0
  trap #15
  movem.l (sp)+,d2/d3/d4/d5
end;

function _iocs_spalet(mode: longint; block: longint; color: longint): longint; assembler; nostackframe;
asm
  move.l d2,-(sp)
  move.l d3,-(sp)
  move.l 12(sp),d3
  move.l d1,d2
  move.l d0,d1
  moveq.l #$ffffffcf,d0
  trap #15
  move.l (sp)+,d3
  move.l (sp)+,d2
end;

end.
