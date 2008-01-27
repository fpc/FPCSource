(*
  $Id: audio.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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

{$ifndef ARM7}
{$error Audio is only available on the ARM7}
{$endif ARM7}

{$ifdef NDS_IMPLEMENTATION}
function SOUND_VOL(n: cint): cint; inline;	
begin
  SOUND_VOL := (n);
end;

function SOUND_FREQ(n: cint): cint; inline;	
begin
  SOUND_FREQ := ((-$1000000 div (n)));
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  SOUND_ENABLE        = (1 shl 15);
  SOUND_REPEAT        = (1 shl 27);
  SOUND_ONE_SHOT      = (1 shl 28);
  SOUND_FORMAT_16BIT  = (1 shl 29);
  SOUND_FORMAT_8BIT   = (0 shl 29);
  SOUND_FORMAT_PSG    = (3 shl 29);
  SOUND_FORMAT_ADPCM  = (2 shl 29);
  SOUND_16BIT         = (1 shl 29);
  SOUND_8BIT          = (0);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function SOUND_PAN(n: cint): cint; inline;
begin
  SOUND_PAN := ((n) shl 16);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  SCHANNEL_ENABLE = (1 shl 31);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
//---------------------------------------------------------------------------------
// registers
//---------------------------------------------------------------------------------
function SCHANNEL_CR(n: cint): pcuint32; inline; 
begin
  SCHANNEL_CR := pcuint32($04000400 + ((n) shl 4));
end;

function SCHANNEL_VOL(n: cint): pcuint8; inline;				
begin
  SCHANNEL_VOL := pcuint8($04000400 + ((n) shl 4));
end;

function SCHANNEL_PAN(n: cint): pcuint8; inline;				 
begin
  SCHANNEL_PAN := pcuint8($04000402 + ((n) shl 4));
end;

function SCHANNEL_SOURCE(n: cint): pcuint32; inline;			 
begin
  SCHANNEL_SOURCE := pcuint32($04000404 + ((n) shl 4));
end;

function SCHANNEL_TIMER(n: cint): pcuint16; inline;
begin
  SCHANNEL_TIMER := pcuint16($04000408 + ((n) shl 4));
end;

function SCHANNEL_REPEAT_POINT(n: cint): pcuint16; inline;	 
begin
  SCHANNEL_REPEAT_POINT := pcuint16($0400040A + ((n) shl 4));
end;

function SCHANNEL_LENGTH(n: cint): pcuint32; inline;			 
begin
  SCHANNEL_LENGTH := pcuint32($0400040C + ((n) shl 4));
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  SOUND_CR          : pcuint16 = pointer($04000500);
  SOUND_MASTER_VOL  : pcuint8 = pointer($04000500);

//---------------------------------------------------------------------------------
// not sure on the following
//---------------------------------------------------------------------------------
  SOUND_BIAS        : pcuint16 = pointer($04000504);
  SOUND508          : pcuint16 = pointer($04000508);
  SOUND510          : pcuint16 = pointer($04000510);
  SOUND514		      : pcuint16 = pointer($04000514);
  SOUND518          : pcuint16 = pointer($04000518);
  SOUND51C          : pcuint16 = pointer($0400051C);


(*---------------------------------------------------------------------------------
	microphone code based on neimod's microphone example.
	See: http://neimod.com/dstek/ 
	Chris Double (chris.double@double.co.nz)
	http://www.double.co.nz/nintendo_ds
---------------------------------------------------------------------------------*)


(*---------------------------------------------------------------------------------
	Read a byte from the microphone
---------------------------------------------------------------------------------*)
function MIC_ReadData(): cuint8; cdecl; external;

(*---------------------------------------------------------------------------------
	Fill the buffer with data from the microphone. The buffer will be
	signed sound data at 16kHz. Once the length of the buffer is
	reached, no more data will be stored. Uses ARM7 timer 0.  
---------------------------------------------------------------------------------*)
procedure StartRecording(buffer: pcuint8; length: cint); cdecl; external;

(*---------------------------------------------------------------------------------
	Stop recording data, and return the length of data recorded.
---------------------------------------------------------------------------------*)
function StopRecording(): cint; cdecl; external;

(* This must be called during IRQ_TIMER0 *)
procedure ProcessMicrophoneTimerIRQ(); cdecl; external;

procedure PM_SetAmp(control: cuint8); cdecl; external;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
//---------------------------------------------------------------------------------
// Turn the microphone on 
//---------------------------------------------------------------------------------
procedure MIC_On(); inline; 
begin
  PM_SetAmp(PM_AMP_ON);
end;

//---------------------------------------------------------------------------------
// Turn the microphone off 
//---------------------------------------------------------------------------------
procedure MIC_Off(); inline; 
begin
  PM_SetAmp(PM_AMP_OFF);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
function SOUND_VOL(n: cint): cint; inline;	
function SOUND_FREQ(n: cint): cint; inline;	
function SOUND_PAN(n: cint): cint; inline;
function SCHANNEL_CR(n: cint): pcuint32; inline; 
function SCHANNEL_VOL(n: cint): pcuint8; inline;				
function SCHANNEL_PAN(n: cint): pcuint8; inline;				 
function SCHANNEL_SOURCE(n: cint): pcuint32; inline;			 
function SCHANNEL_TIMER(n: cint): pcuint16; inline;
function SCHANNEL_REPEAT_POINT(n: cint): pcuint16; inline;	 
function SCHANNEL_LENGTH(n: cint): pcuint32; inline;			 
procedure MIC_On(); inline; 
procedure MIC_Off(); inline; 
{$endif NDS_INTERFACE}
