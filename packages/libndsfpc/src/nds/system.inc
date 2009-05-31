(*
  $Id: system.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifdef NDS_INTERFACE}
const
  REG_DISPSTAT	: pcuint16 = pointer($04000004);
  
type
  DISP_BITS = cint;
const
  DISP_IN_VBLANK    : DISP_BITS = (1 shl 0);
  DISP_IN_HBLANK    : DISP_BITS = (1 shl 1);
  DISP_YTRIGGERED   : DISP_BITS = (1 shl 2);
  DISP_VBLANK_IRQ   : DISP_BITS = (1 shl 3);
  DISP_HBLANK_IRQ   : DISP_BITS = (1 shl 4);
  DISP_YTRIGGER_IRQ : DISP_BITS = (1 shl 5);

  REG_VCOUNT		: pcuint16 = pointer($04000006);
  HALT_CR       : pcuint16 = pointer($04000300);
  REG_POWERCNT  : pcuint16 = pointer($04000304);

procedure SetYtrigger(Yvalue: cint); inline;

const
  PM_ARM9_DIRECT = (1 shl 16);

type
  PM_Bits = cint;
const
	PM_SOUND_AMP         : PM_Bits = (1 shl 0);
	PM_SOUND_MUTE        : PM_Bits = (1 shl 1);
	PM_BACKLIGHT_BOTTOM  : PM_Bits = (1 shl 2);
	PM_BACKLIGHT_TOP     : PM_Bits = (1 shl 3);
	PM_SYSTEM_PWR        : PM_Bits = (1 shl 6);

	POWER_LCD      : PM_Bits = PM_ARM9_DIRECT or (1 shl 0);
	POWER_2D_A     : PM_Bits = PM_ARM9_DIRECT or (1 shl 1);
	POWER_MATRIX   : PM_Bits = PM_ARM9_DIRECT or (1 shl 2);
	POWER_3D_CORE  : PM_Bits = PM_ARM9_DIRECT or (1 shl 3);
	POWER_2D_B     : PM_Bits = PM_ARM9_DIRECT or (1 shl 9);
	POWER_SWAP_LCDS: PM_Bits = PM_ARM9_DIRECT or (1 shl 15);
	POWER_ALL_2D   : PM_Bits = PM_ARM9_DIRECT or (1 shl 0) or (1 shl 1) or (1 shl 9);
	POWER_ALL      : PM_Bits = PM_ARM9_DIRECT or (1 shl 0) or (1 shl 1) or (1 shl 9) or (1 shl 3) or (1 shl 2);

procedure sleep(); cdecl; external;

{$ifdef ARM9}
procedure powerOn(bits: PM_Bits); cdecl; external;
procedure powerOff(bits: PM_Bits); cdecl; external;
procedure ledBlink(bm: PM_LedBlinkMode); cdecl; external;

procedure systemMsgHandler(bytes: cint; user_data: pointer); cdecl; external;
procedure powerValueHandler(value: cuint32; data: pointer); cdecl; external;

procedure lcdSwap(); inline; 
procedure lcdMainOnTop(); inline; 
procedure lcdMainOnBottom(); inline; 


procedure setVectorBase(highVector: cint); cdecl; external;

type 
  sysVectors_t = packed record
    reset: cuint32;
    undefined: cuint32;
    swi: cuint32;
    prefetch_abort: cuint32;
    data_abort: cuint32;
    fiq: cuint32;
  end; 
  sysVectors = sysVectors_t;
  PsysVectors = ^sysVectors;

var
  SystemVectors: sysVectors; cvar; external;

{$endif ARM9}


{$ifdef ARM7}
type
  ARM7_power = cint;
const
  POWER_SOUND     : ARM7_power = (1 shl 0);
  PM_CONTROL_REG  : ARM7_power = 0;
  PM_BATTERY_REG  : ARM7_power = 1;
  PM_AMPLIFIER_REG: ARM7_power = 2;
  PM_READ_REGISTER: ARM7_power = (1 shl 7);
  PM_AMP_OFFSET   : ARM7_power = 2;
  PM_GAIN_OFFSET  : ARM7_power = 3;
  PM_GAIN_20      : ARM7_power = 0;
  PM_GAIN_40      : ARM7_power = 1;
  PM_GAIN_80      : ARM7_power = 2;
  PM_GAIN_160     : ARM7_power = 3;
  PM_AMP_ON       : ARM7_power = 1;
  PM_AMP_OFF      : ARM7_power = 0;

function PM_LED_CONTROL(m: cint): cint; inline;

procedure installSystemFIFO(); cdecl; external;
procedure systemSleep(); cdecl; external;
function sleepEnabled(): cint; cdecl; external;
function writePowerManagement(reg, command: cint): cint; cdecl; external;

function readPowerManagement(reg: cint): cint; inline; 

procedure powerOn(bits: PM_Bits); inline;
procedure powerOff(bits: PM_Bits); inline;

procedure readUserSettings(); cdecl; external;
{$endif ARM7}

type
  tPERSONAL_DATA = bitpacked record
    RESERVED0: array [0..1] of cuint8;
    
    theme: cuint8;
    birthMonth: cuint8;
    birthDay: cuint8;
    
    RESERVED1: array [0..0] of cuint8;
    
    name: array [0..9] of cint16;
    nameLen: cuint16;
    
    message: array [0..25] of cint16;
    messageLen: cuint16;
    
    alarmHour: cuint8;
    alarmMinute: cuint8;
    
    RESERVED2: array [0..3] of cuint8;
    
    calX1: cuint16;
    calY1: cuint16;
    calX1px: cuint8;
    calY1px: cuint8;
    
    calX2: cuint16;
    calY2: cuint16;
    calX2px: cuint8;
    calY2px: cuint8;
    
  
    _user_data: packed record
      language: 0..3;
      gbaScreen: 0..1;
      defaultBrightness: 0..2;
      autoMode: 0..1;
      RESERVED1: 0..2;
	    settingsLost: 0..1;	
	    RESERVED2: 0..6;
    end;
    RESERVED3: cuint16;
    rtcOffset: cuint32;
    RESERVED4: cuint32;
  end; 
  PERSONAL_DATA = tPERSONAL_DATA;
  PPERSONAL_DATA = ^tPERSONAL_DATA;

const
  REG_KEYINPUT	: pcuint16 = pointer($04000130);
  REG_KEYCNT		: pcuint16 = pointer($04000132);
  PersonalData  : PPERSONAL_DATA = pointer($27FFC80);


type 
  ppcchar = ^pcchar;
  __argv = record 
    argvMagic: cint;		  // argv magic number, set to 0x5f617267 ('_arg') if valid 
    commandLine: pcchar;	// base address of command line, set of null terminated strings
    length: cint;			    // total length of command line
    argc: integer;
    argv: ppcchar;    
  end;
  Targv = __argv;
  Pargv = ^Targv;

const
  __system_argv: Pargv = pointer($027FFF70);
  ARGV_MAGIC = $5f617267;
  

type
  RTCtime = packed record
    year: cuint8;        // add 2000 to get 4 digit year
    month: cuint8;       // 1 to 12
    day: cuint8;         // 1 to (days in month)
    weekday: cuint8;     // day of week
    hours: cuint8;       // 0 to 11 for AM, 52 to 63 for PM
    minutes: cuint8;     // 0 to 59
    seconds: cuint8;     // 0 to 59
    padding: cuint8;
  end;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}

procedure SetYtrigger(Yvalue: cint); inline;
begin
  REG_DISPSTAT^ := (REG_DISPSTAT^ and $007F ) or (Yvalue shl 8) or (( Yvalue and $100 ) shr 1);
end;

{$ifdef ARM9}
procedure lcdSwap(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ xor POWER_SWAP_LCDS; 
end;

procedure lcdMainOnTop(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ or cint(POWER_SWAP_LCDS); 
end;

procedure lcdMainOnBottom(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ and (not cint(POWER_SWAP_LCDS)); 
end;
{$endif ARM9}


{$ifdef ARM7}
function PM_LED_CONTROL(m: cint): cint; inline;
begin
  result := (m) shl 4;
end;

function readPowerManagement(reg: cint): cint; inline; 
begin
	result := writePowerManagement(reg or PM_READ_REGISTER, 0);
end;

procedure powerOn(bits: PM_Bits); inline;
begin
	REG_POWERCNT^ := REG_POWERCNT^ or bits;
end;

procedure powerOff(bits: PM_Bits); inline;
begin
	REG_POWERCNT^ := REG_POWERCNT^ and not bits;
end;
{$endif ARM7}
{$endif NDS_IMPLEMENTATION}

