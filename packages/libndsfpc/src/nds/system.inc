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
  DISP_IN_VBLANK  =  (1 shl 0);
  DISP_IN_HBLANK  =  (1 shl 1);
  DISP_YTRIGGERED =  (1 shl 2);
  DISP_VBLANK_IRQ =  (1 shl 3);
  DISP_HBLANK_IRQ =  (1 shl 4);
  DISP_YTRIGGER_IRQ = (1 shl 5);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure SetYtrigger(Yvalue: cint); inline;
begin
  REG_DISPSTAT^ := (REG_DISPSTAT^ and $007F ) or (Yvalue shl 8) or (( Yvalue and $100 ) shr 2);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  REG_VCOUNT		: pcuint16 = pointer($04000006);
  HALT_CR       : pcuint16 = pointer($04000300);
  REG_POWERCNT  : pcuint16 = pointer($04000304);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure powerON(Aon: cint); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ or Aon;
end;

procedure powerSET(Aon: cint); inline; 
begin
  REG_POWERCNT^ := Aon;
end;

procedure powerOFF(off: cint); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ and (not off);
end;
{$endif NDS_IMPLEMENTATION}


{$ifdef ARM9}
{$ifdef NDS_INTERFACE}
const
  POWER_LCD			= (1 shl 0);
  POWER_2D_A		= (1 shl 1);
  POWER_MATRIX	= (1 shl 2);
  POWER_3D_CORE	= (1 shl 3);
  POWER_2D_B		= (1 shl 9);
  POWER_SWAP_LCDS	= (1 shl 15);
  POWER_ALL_2D    = (POWER_LCD or POWER_2D_A or POWER_2D_B);
  POWER_ALL       = (POWER_ALL_2D or POWER_3D_CORE or POWER_MATRIX);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
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
{$endif NDS_IMPLEMENTATION}
{$endif ARM9}

{$ifdef ARM7}
{$ifdef NDS_INTERFACE}
const
  POWER_SOUND  = (1 shl 0);
  POWER_UNKNOWN  = (1 shl 1);

procedure readUserSettings(); cdecl; external;
{$endif NDS_INTERFACE}
{$endif ARM7}


{$ifdef NDS_INTERFACE}
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
      RESERVED4: 0..1;
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
  __argv = record 
    argvMagic: cint;		  // argv magic number, set to 0x5f617267 ('_arg') if valid 
    commandLine: pcchar;	// base address of command line, set of null terminated strings
    length: cint;			    // total length of command line
  end;
  Targv = __argv;
  Pargv = ^Targv;

const
  libnds_argv: Pargv = pointer($027FFF70);
  argvMagic = $5f617267;
{$endif NDS_INTERFACE}


{$ifdef NDS_INTERFACE}
procedure SetYtrigger(Yvalue: cint); inline;
procedure powerON(Aon: cint); inline; 
procedure powerSET(Aon: cint); inline; 
procedure powerOFF(off: cint); inline;
{$ifdef ARM9} 
procedure lcdSwap(); inline; 
procedure lcdMainOnTop(); inline; 
procedure lcdMainOnBottom(); inline; 
{$endif ARM9} 
{$endif NDS_INTERFACE}
