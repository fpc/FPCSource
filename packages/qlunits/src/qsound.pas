{
    This file is part of the Free Pascal Sinclair QL support package.
    Copyright (c) 2024 by Karoly Balogh

    QSound ROM functions support unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ The QSound is a sound card for the Sinclair QL, based on the AY-3-8910 PSG
  (Programmable Sound Generator). In the QL's case this card has a ROM on it,
  that eases sound programming. This unit is an interface to the ROM functions.

  An Open Source replica board is available at:
  https://github.com/alvaroalea/QL_QsoundQprint_clone }

{ Note: this unit is incomplete, especially when it comes to multichip support,
  because apparently all versions of the QSound ROM have serious bugs, and
  the actual code mismatches the documentation, and I got fed up trying to
  reverse engineer the ROM what it actually does. Additionally some documented
  functions just can't be reasonably called from a high level language, because
  they expect certain registers to contain ROM-internal values and handles
  when called.
  (KB) }

{$IFNDEF FPC_DOTTEDUNITS}
unit qsound;
{$ENDIF FPC_DOTTEDUNITS}

interface

var
  ay_jump: pointer;

const
  _AY_PORTA = $8000;
  _AY_CTRLA = $8001;
  _AY_PORTB = $8002;
  _AY_CTRLB = $8003;

const
  _AY_RESET = $00;
  _AY_WRREG = $01;
  _AY_RDREG = $02;
  _AY_WRALL = $03;
  _AY_RDALL = $04;
  _AY_PLAY  = $05;
  _AY_TSTPL = $06;
  _AY_HOLD  = $07;
  _AY_RELSE = $08;
  _AY_NOISE = $09;
  _AY_SOUND = $0A;

  _AY_INFO  = $0B;
  _AY_CHIP_TYPE = $0C;
  _AY_CHIP_FREQ = $0D;
  _AY_STEREO = $0E;
  _AY_VOLUME = $0F;

const
  _AYST_MONO = 0;
  _AYST_ABC  = 1;
  _AYST_ACB  = 2;
  _AYST_BAC  = 3;
  _AYST_BCA  = 4;
  _AYST_CAB  = 5;
  _AYST_CBA  = 6;

  _AYST_QUERY = -1;

const
  _AYCT_AY   = 0;
  _AYCT_YM   = 1; 

  _AYCT_QUERY = -1;

const
  NOISE_EXPLOSION = 0;
  NOISE_SHOOT = 1;
  NOISE_BELL = 2;

type
  Tay_all = array[0..13] of byte;
  Pay_all = ^Tay_all;


{ "low level" programming, direct calls to ROM functions } 
procedure ay_reset;
function ay_wrreg(const reg: byte; const value: byte): smallint;
function ay_rdreg(const reg: byte): smallint;
procedure ay_wrall(const regs: Pay_all);
procedure ay_wrall(const chipid: byte; const regs: Pay_all);
procedure ay_rdall(const regs: Pay_all);
procedure ay_rdall(const chipid: byte; const regs: Pay_all);
function ay_play(const channel: byte; const str: pointer): smallint;
function ay_tstpl(const channel: byte): smallint;
function ay_hold(const channel: byte): smallint;
function ay_relse(const channel: byte): smallint;
function ay_noise(const noise: byte): smallint;
function ay_sound(const channel: byte; const frequency: word; const volume: byte): smallint;


{ "high level" functions, that mimic SuperBASIC additions }
function qs_peek_ay(const reg: byte): byte;
procedure qs_poke_ay(const reg: byte; const value: byte);
procedure qs_explode;
procedure qs_shoot;
procedure qs_bell;

implementation

uses
  qdos;

procedure ay_reset; assembler; nostackframe;
asm
  moveq.l #_AY_RESET,d0
  movem.l d2/a5,-(sp)
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

function ay_wrreg(const reg: byte; const value: byte): smallint; assembler; nostackframe;
asm
  move.l reg,a0
  { value is already in d1 }
  moveq.l #_AY_WRREG,d0
  movem.l d2/a5,-(sp)
  move.l a0,d2
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

function ay_rdreg(const reg: byte): smallint; assembler; nostackframe;
asm
  move.l reg,a0
  moveq.l #_AY_RDREG,d0
  movem.l d2/a5,-(sp)
  move.l a0,d2
  move.l ay_jump,a0
  jsr (a0)
  tst.w d0
  bne @exit
  move.w d1,d0
@exit:
  movem.l (sp)+,d2/a5
end;

procedure ay_wrall(const regs: Pay_all); assembler; nostackframe;
asm
  moveq.l #_AY_WRALL,d0
  movem.l d2/a5,-(sp)
  moveq.l #0,d2
  move.l a0,a1
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

procedure ay_wrall(const chipid: byte; const regs: Pay_all); assembler; nostackframe;
asm
  moveq.l #_AY_WRALL,d0
  movem.l d2/a5,-(sp)
  move.l d0,d2
  move.l a0,a1
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

procedure ay_rdall(const regs: Pay_all); assembler; nostackframe;
asm
  moveq.l #_AY_RDALL,d0
  movem.l d2/a5,-(sp)
  moveq.l #0,d2
  move.l a0,a1
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

procedure ay_rdall(const chipid: byte; const regs: Pay_all); assembler; nostackframe;
asm
  moveq.l #_AY_RDALL,d0
  movem.l d2/a5,-(sp)
  moveq.l #0,d2
  move.l a0,a1
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

function ay_play(const channel: byte; const str: pointer): smallint; assembler; nostackframe;
asm
  move.b channel,d1
  moveq.l #_AY_PLAY,d0
  { str is already in a0 }
  move.l a5,-(sp)
  move.l ay_jump,a0
  jsr (a0)
  move.l (sp)+,a5
end;

function ay_tstpl(const channel: byte): smallint; assembler; nostackframe;
asm
  move.b channel,d1
  moveq.l #_AY_TSTPL,d0
  { str is already in a0 }
  move.l a5,-(sp)
  move.l ay_jump,a0
  jsr (a0)
  tst.w d0
  bne @exit
  move.w d1,d0
@exit:
  move.l (sp)+,a5
end;

function ay_hold(const channel: byte): smallint; assembler; nostackframe;
asm
  move.b channel,d1
  moveq.l #_AY_HOLD,d0
  movem.l d2-d3/a5,-(sp)
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2-d3/a5
end;

function ay_relse(const channel: byte): smallint; assembler; nostackframe;
asm
  move.b channel,d1
  moveq.l #_AY_RELSE,d0
  movem.l d2-d3/a5,-(sp)
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2-d3/a5
end;

function ay_noise(const noise: byte): smallint; assembler; nostackframe;
asm
  move.b noise,d1
  moveq.l #_AY_NOISE,d0
  movem.l d2/a5,-(sp)
  moveq.l #0,d2  // this is a workaround of a ROM bug found in recent versions
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2/a5
end;

function ay_sound(const channel: byte; const frequency: word; const volume: byte): smallint; assembler; nostackframe;
asm
  move.l frequency,a0
  move.l volume,a1
  move.b channel,d1
  move.l #_AY_SOUND,d0
  movem.l d2-d3/a5,-(sp)
  move.l a0,d2
  move.l a1,d3
  move.l ay_jump,a0
  jsr (a0)
  movem.l (sp)+,d2-d3/a5
end;


function qs_peek_ay(const reg: byte): byte;
begin
  qs_peek_ay:=ay_rdreg(reg);
end;

procedure qs_poke_ay(const reg: byte; const value: byte);
begin
  ay_wrreg(reg,value);
end;

procedure qs_explode;
begin
  writeln(ay_noise(NOISE_EXPLOSION));
end;

procedure qs_shoot;
begin
  writeln(ay_noise(NOISE_SHOOT));
end;

procedure qs_bell;
begin
  writeln(ay_noise(NOISE_BELL));
end;


procedure qsound_init;
var
  system_variables: PSystemVariables;
  ver_ascii: array[0..3] of AnsiChar;
begin
  mt_inf(@system_variables,@ver_ascii);
  ay_jump:=ppointer(pbyte(system_variables)+$164)^;
end;

begin
  qsound_init;
end.
