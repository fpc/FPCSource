(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba bios sound functions

	Copyright (C) 2003-2005 by Dave Murphy.

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Library General Public
	License as published by the Free Software Foundation; either
	version 2 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Library General Public License for more details.

	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
	USA.

	Please report all bugs and problems through the bug tracker at
	"http://sourceforge.net/tracker/?group_id=114505&atid=668551".

  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$



*)


{$ifdef GBA_INTERFACE}
type
  WaveData = record
    typ: u16;
    stat: u16;
    freq: u32;
    loop: u32;
    size: u32;
    data: array [0..0] of s8;
  end;
  PWaveData = ^WaveData;

  SoundChannel = record
    Status: u8;
    reserved1: u8;
    RightVol: u8;
    LeftVol: u8;
    Attack: u8;
    Decay: u8;
    Sustain: u8;
    Release: u8;
    reserved2: array [0..23] of u8;
    fr: u32;
    wp: PWaveData;
    reserved3: array [0..5] of u32;
  end; 
  PSoundChannel = ^SoundChannel;

const
  PCM_DMA_BUF = 1584;
  MAX_DIRECTSOUND_CHANNELS  = 12;

type
  SoundArea = record
    ident: u32;
    DmaCount: u8;
    reverb: u8;
    maxchn: u8;
    masvol: u8;
    freq: u8;
    mode: u8;
    r2: array [0..5] of u8;
    r3: array [0..15] of u32;
    vchn: array [0..MAX_DIRECTSOUND_CHANNELS-1] of SoundChannel;
    pcmbuf: array [0..PCM_DMA_BUF*2 - 1] of s8;
  end;
  PSoundArea = ^SoundArea;

(*---------------------------------------------------------------------------------
	Control Registers
---------------------------------------------------------------------------------*)
const
  SND1_R_ENABLE	= (1 shl 8);		// Enable left & right speakers for each sound channel
  SND1_L_ENABLE = (1 shl 12);
  SND2_R_ENABLE = (1 shl 9);
  SND2_L_ENABLE = (1 shl 13);
  SND3_R_ENABLE = (1 shl 10);
  SND3_L_ENABLE = (1 shl 14);
  SND4_R_ENABLE = (1 shl 11);
  SND4_L_ENABLE = (1 shl 15);

  SNDA_VOL_50      = (0 shl 2);
  SNDA_VOL_100     = (1 shl 2);
  SNDB_VOL_50      = (0 shl 3);
  SNDB_VOL_100     = (1 shl 3);
  SNDA_R_ENABLE    = (1 shl 8);
  SNDA_L_ENABLE    = (1 shl 9);
  SNDA_RESET_FIFO  = (1 shl 11);
  SNDB_R_ENABLE    = (1 shl 12);
  SNDB_L_ENABLE    = (1 shl 13);
  SNDB_RESET_FIFO  = (1 shl 15);


  REG_SOUNDCNT_L	: pu16 = pointer(REG_BASE + $080);
  REG_SOUNDCNT_H	: pu16 = pointer(REG_BASE + $082);
  REG_SOUNDCNT_X	: pu16 = pointer(REG_BASE + $084);


  REG_SOUND1CNT_L	: pu16 = pointer(REG_BASE + $060);
  REG_SOUND1CNT_H	: pu16 = pointer(REG_BASE + $062);
  REG_SOUND1CNT_X	: pu16 = pointer(REG_BASE + $064);

  REG_SOUND2CNT_L	: pu16 = pointer(REG_BASE + $068);
  REG_SOUND2CNT_H	: pu16 = pointer(REG_BASE + $06C);

  REG_SOUND3CNT_L	: pu16 = pointer(REG_BASE + $070);
  REG_SOUND3CNT_H	: pu16 = pointer(REG_BASE + $072);
  REG_SOUND3CNT_X	: pu16 = pointer(REG_BASE + $074);

  REG_SOUND4CNT_L	: pu16 = pointer(REG_BASE + $078);
  REG_SOUND4CNT_H	: pu16 = pointer(REG_BASE + $07C);

  REG_SOUNDBIAS	: pu16 = pointer(REG_BASE + $088);

  REG_FIFO_A		: pu32 = pointer(REG_BASE + $0A0);
  REG_FIFO_B		: pu32 = pointer(REG_BASE + $0A4);

  WAVE_RAM      : pu16 = pointer(REG_BASE + $090);

  SOUND3_STEP32		= (0 shl 5);	// Use two banks of 32 steps each
  SOUND3_STEP64		= (1 shl 5);	// Use one bank of 64 steps

  
function SOUND3_SETBANK(n: integer): integer; inline;

const
  SOUND3_PLAY			= (1 shl 7);	// Output sound
  SOUND3_STOP			= (0 shl 7);	// Stop sound output

//---------------------------------------------------------------------------------
// pin8 compatible sound macros 
//---------------------------------------------------------------------------------

(*---------------------------------------------------------------------------------
	DMG Sound Control (0x04000080)
fedcba9876543210
|||||||| ||| |||
|||||||| ||| +++- DMG left volume
|||||||| +++----- DMG right volume
|||||||+--------- Enable sqr1 on left
||||||+---------- Enable sqr2 on left
|||||+----------- Enable triangle on left
||||+------------ Enable noise on left
|||+------------- Enable sqr1 on right
||+-------------- Enable sqr2 on right
|+--------------- Enable triangle on right
+---------------- 
---------------------------------------------------------------------------------*)
const
  DMGSNDCTRL         : pu16 = pointer($04000080);
  
function DMGSNDCTRL_LVOL(x: integer): integer; inline;

function DMGSNDCTRL_RVOL(x: integer): integer; inline;

const
  DMGSNDCTRL_LSQR1   = $0100;
  DMGSNDCTRL_LSQR2   = $0200;
  DMGSNDCTRL_LTRI    = $0400;
  DMGSNTCTRL_LNOISE  = $0800;
  DMGSNDCTRL_RSQR1   = $1000;
  DMGSNDCTRL_RSQR2   = $2000;
  DMGSNDCTRL_RTRI    = $4000;
  DMGSNDCTRL_RNOISE  = $8000;

(*---------------------------------------------------------------------------------
	Direct Sound Control (0x04000082)
-----------------------------------------------------------------------------------
fedcba9876543210
||||||||    ||||
||||||||    ||++- DMG sound output volume
||||||||    ||    (00: 25%; 01: 50%; 10: 100%)
||||||||    |+--- DSound A output volume (0: 50%; 1: 100%)
||||||||    +---- DSound B output volume (0: 50%; 1: 100%)
|||||||+--------- Enable DSound A on right
||||||+---------- Enable DSound A on left
|||||+----------- DSound A sample timer (0 or 1)
||||+------------ DSound A FIFO reset
|||+------------- Enable DSound B on right
||+-------------- Enable DSound B on left
|+--------------- DSound B sample timer (0 or 1)
+---------------- DSound B FIFO reset
---------------------------------------------------------------------------------*)
const
  DSOUNDCTRL           : pu16 = pointer($04000082);
  DSOUNDCTRL_DMG25     = $0000;
  DSOUNDCTRL_DMG50     = $0001;
  DSOUNDCTRL_DMG100    = $0002;
  DSOUNDCTRL_A50       = $0000;
  DSOUNDCTRL_A100      = $0004;
  DSOUNDCTRL_B50       = $0000;
  DSOUNDCTRL_B100      = $0008;
  DSOUNDCTRL_AR        = $0100;
  DSOUNDCTRL_AL        = $0200;
  
function DSOUNDCTRL_ATIMER(x: integer): integer; inline; 
  
const
  DSOUNDCTRL_ARESET    = $0400;
  DSOUNDCTRL_BR        = $1000;
  DSOUNDCTRL_BL        = $2000;
  
function  DSOUNDCTRL_BTIMER(x: integer): integer; inline;  

const
  DSOUNDCTRL_BRESET    = $8000;

(*---------------------------------------------------------------------------------
 Sound Status (0x04000084)
-----------------------------------------------------------------------------------
Note that unlike NES's $4014, bits 0 to 3 of this register are
read-only.  They do not enable sound.

fedcba9876543210
        |   ||||
        |   |||+- Square 1 playing
        |   ||+-- Square 2 playing
        |   |+--- Triangle playing
        |   +---- Noise playing
        +-------- 0: save 10% battery power by turning off ALL sound;
                  1: play sound
---------------------------------------------------------------------------------*)
const
  SNDSTAT        : pu16 = pointer($04000084);
  SNDSTAT_SQR1   = $0001;
  SNDSTAT_SQR2   = $0002;
  SNDSTAT_TRI    = $0004;
  SNDSTAT_NOISE  = $0008;
  SNDSTAT_ENABLE = $0080;

(*---------------------------------------------------------------------------------
	Sound Bias: will not be documented.
-----------------------------------------------------------------------------------
fedcba9876543210
||    ||||||||||
||    ++++++++++- PWM bias
++--------------- Amplitude resolution
                  00: 9-bit at 32768 Hz
                  01: 8-bit at 65536 Hz (most common)
                  10: 7-bit at 131072 Hz
                  11: 6-bit at 262144 Hz

Do NOT use SNDBIAS directly.  To set the resolution, use
  SETSNDRES(1);
---------------------------------------------------------------------------------*)
const
  SNDBIAS      : pu16 = pointer($04000088);
  
procedure SETSNDRES(x: integer); inline;

const
  DSOUND_FIFOA : pu32 = pointer($040000a0);
  DSOUND_FIFOB : pu32 = pointer($040000a4);


(*---------------------------------------------------------------------------------
 Square 1 Sweep Register
---------------------------------------------------------------------------------

fedcba9876543210
         |||||||
         ||||+++- Sweep shifts (1 fastest; 7 slowest)
         |||+---- 0: Sweep up; 1: Sweep down

Write 0x0040 into this register to disable sweep.
---------------------------------------------------------------------------------*)
const
  SQR1SWEEP   : pu16 = pointer($04000060);
  SQR1SWEEP_OFF = $0008;


(*---------------------------------------------------------------------------------
   Square 1 Control Register
   Square 2 Control Register
-----------------------------------------------------------------------------------
fedcba9876543210
||||||||||||||||
||||||||||++++++- Sound length (1 longest; 63: shortest)
||||||||++------- Duty cycle (00: 1/8; 01: 1/4; 10: 1/2; 11: 3/4)
|||||+++--------- Envelope step time (0: off; 1 fastest; 7 slowest)
||||+------------ Envelope direction (0: decrease; 1: increase)
++++------------- Volume
---------------------------------------------------------------------------------*)
const
  SQR1CTRL    : pu16 = pointer($04000062);
  SQR2CTRL    : pu16 = pointer($04000068);

function SQR_DUTY(n: integer): integer; inline;

function  SQR_VOL(n: integer): integer; inline;

(*---------------------------------------------------------------------------------
   Square 1 Frequency
   Square 2 Frequency
   Triangle Channel Frequency (0x04000074)
-----------------------------------------------------------------------------------
fedcba9876543210
||   |||||||||||
||   +++++++++++- frequency (131072 Hz/(2048-x)) (halved for tri channel)
|+--------------- 0: hold note; 1: use length
+---------------- 1: Reset channel
---------------------------------------------------------------------------------*)
const
  SQR1FREQ      : pu16 = pointer($04000064);
  SQR2FREQ      : pu16 = pointer($0400006c);
  TRIFREQ       : pu16 = pointer($04000074);
  TRIFREQ_HOLD  = $0000;
  TRIFREQ_TIMED = $4000;
  TRIFREQ_RESET = $8000;


(*---------------------------------------------------------------------------------
	Triangle Channel Control Register
-----------------------------------------------------------------------------------
fedcba9876543210
        |||
        ||+------ Bank mode (0: 2 banks of 32; 1: 1 bank of 64)
        |+------- Play this bank (and write other bank)
        +-------- Enable triangle channel
---------------------------------------------------------------------------------*)
const
  TRICTRL         : pu16 = pointer($04000070);
  TRICTRL_2X32    = $0000;
  TRICTRL_1X64    = $0020;
  
function TRICTRL_BANK(x: integer): integer; inline;

const
  TRICTRL_ENABLE  = $0080;

(*---------------------------------------------------------------------------------
	Triangle Channel Length/Volume (0x04000072)
-----------------------------------------------------------------------------------

fedcba9876543210
|||     ||||||||
|||     ++++++++- Length ((256-x)/256 seconds)
+++-------------- Volume (000: mute; 001: 100%; 010: 50%;
                          011: 25%; 100: 75%)
---------------------------------------------------------------------------------*)
const
  TRILENVOL        :pu16 = pointer($04000072);

function TRILENVOL_LEN(x: integer): integer; inline;

const
  TRILENVOL_MUTE   = $0000;
  TRILENVOL_25     = $6000;
  TRILENVOL_50     = $4000;
  TRILENVOL_75     = $8000;
  TRILENVOL_100    = $2000;

  TRIWAVERAM : pu32 = pointer($04000090);

//---------------------------------------------------------------------------------
//	Bios sound functions
//---------------------------------------------------------------------------------

procedure SoundDriverMain(); assembler; inline;
procedure SoundDriverVsync(); assembler; inline;
procedure SoundChannelClear(); assembler; inline;
procedure SoundDriverVsyncOff(); assembler; inline;
procedure SoundDriverVsyncOn(); assembler; inline;

procedure SoundDriverInit(sa: PSoundArea); cdecl; external;
procedure SoundDriverMode(mode: u32); cdecl; external;

function MidiKey2Freq(wa: PWaveData; mk, fp: u8): u32; cdecl; external;
{$endif GBA_INTERFACE}


{$ifdef GBA_IMPLEMENTATION}
function SOUND3_SETBANK(n: integer): integer; inline;
begin
	SOUND3_SETBANK := (n shl 6);	// Bank to play 0 or 1 (non set bank is written to)
end;
  
function DMGSNDCTRL_LVOL(x: integer): integer; inline;
begin 
  DMGSNDCTRL_LVOL := (x);
end;

function DMGSNDCTRL_RVOL(x: integer): integer; inline;
begin
  DMGSNDCTRL_RVOL := ((x) shl 4);
end;
  
function DSOUNDCTRL_ATIMER(x: integer): integer; inline; 
begin
  DSOUNDCTRL_ATIMER := ((x) shl 10);
end;
  
function  DSOUNDCTRL_BTIMER(x: integer): integer; inline;  
begin
  DSOUNDCTRL_BTIMER := ((x) shl 14);
end;

procedure SETSNDRES(x: integer); inline;
begin 
  SNDBIAS^ := (SNDBIAS^ and $3fff) or (x shl 14);
end;

function SQR_DUTY(n: integer): integer; inline;
begin 
  SQR_DUTY := ((n) shl 6);
end;

function  SQR_VOL(n: integer): integer; inline;
begin   
  SQR_VOL := ((n) shl 12);
end;

function TRICTRL_BANK(x: integer): integer; inline;
begin
  TRICTRL_BANK := ((x) shl 6);
end;

function TRILENVOL_LEN(x: integer): integer; inline;
begin 
  TRILENVOL_LEN := (256 - (x));
end;

procedure SoundDriverMain(); assembler; inline;
asm 
  SWI SC_ARM_28 
end;

procedure SoundDriverVsync(); assembler; inline;
asm 
  SWI SC_ARM_29
end;

procedure SoundChannelClear(); assembler; inline;
asm 
  SWI SC_ARM_30
end;

procedure SoundDriverVsyncOff(); assembler; inline;
asm 
  SWI SC_ARM_40
end;

procedure SoundDriverVsyncOn(); assembler; inline;
asm 
  SWI SC_ARM_41
end;
{$endif GBA_IMPLEMENTATION}
