(*
  $Id: timers.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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
type
  ClockDivider = cint;
const
  ClockDivider_1   : ClockDivider = 0;
  ClockDivider_64  : ClockDivider = 1;
  ClockDivider_256 : ClockDivider = 2;
  ClockDivider_1024: ClockDivider = 3;


procedure timerStart(channel: cint; divider: ClockDivider; ticks: cuint16; callback: fp); cdecl; external; 
function timerElapsed(channel: cint): cuint16; cdecl; external;

function timerFreqToTicks_1(freq: cint): cuint16; inline;
function timerFreqToTicks_64(freq: cint): cuint16; inline;
function timerFreqToTicks_256(freq: cint): cuint16; inline;
function timerFreqToTicks_1024(freq: cint): cuint16; inline;

function TIMER_FREQ(n: cint): cint; inline;
function TIMER_FREQ_64(n: cint): cint; inline;
function TIMER_FREQ_256(n: cint): cint; inline;
function TIMER_FREQ_1024(n: cint): cint; inline; 

const
  TIMER0_DATA    : pcuint16 = pointer($04000100);
  TIMER1_DATA    : pcuint16 = pointer($04000104);
  TIMER2_DATA    : pcuint16 = pointer($04000108);
  TIMER3_DATA    : pcuint16 = pointer($0400010C);
function TIMER_DATA(n: cuint): pcuint16; inline;


const
  TIMER0_CR   : pcuint16 = pointer($04000102);
  TIMER1_CR   : pcuint16 = pointer($04000106);
  TIMER2_CR   : pcuint16 = pointer($0400010A);
  TIMER3_CR   : pcuint16 = pointer($0400010E);
function TIMER_CR(n: cint): pcuint16; inline;

const
  TIMER_ENABLE    = (1 shl 7);
  TIMER_IRQ_REQ   = (1 shl 6);
  TIMER_CASCADE   = (1 shl 2);
  TIMER_DIV_1     = (0);
  TIMER_DIV_64    = (1);
  TIMER_DIV_256   = (2);
  TIMER_DIV_1024  = (3);

procedure timerStop(channel: cint); inline;

{$endif NDS_INTERFACE}



{$ifdef NDS_IMPLEMENTATION}

function timerFreqToTicks_1(freq: cint): cuint16; inline;
begin
  result := -$2000000 div freq;
end;

function timerFreqToTicks_64(freq: cint): cuint16; inline;
begin
  result := (-$2000000 shr 6) div freq;
end;

function timerFreqToTicks_256(freq: cint): cuint16; inline;
begin
  result := (-$2000000 shr 8) div freq;
end;

function timerFreqToTicks_1024(freq: cint): cuint16; inline;
begin
  result := (-$2000000 shr 10) div freq;
end;

function TIMER_FREQ(n: cint): cint; inline;
begin
  TIMER_FREQ := cint(-$2000000 div (n));
end;
	
function TIMER_FREQ_64(n: cint): cint; inline;
begin
  TIMER_FREQ_64 := cint(-($2000000 shr 6) div (n));
end;
	
function TIMER_FREQ_256(n: cint): cint; inline;
begin
  TIMER_FREQ_256 := cint(-($2000000 shr 8) div (n));
end;

function TIMER_FREQ_1024(n: cint): cint; inline; 
begin
  TIMER_FREQ_1024 := cint(-($2000000 shr 10) div (n));
end;

function TIMER_DATA(n: cuint): pcuint16; inline;
begin
  TIMER_DATA := pcuint16($04000100 + (n shl 2));
end;

function TIMER_CR(n: cint): pcuint16; inline;
begin
  TIMER_CR := pcuint16($04000102 + (n shl 2));
end;

procedure timerStop(channel: cint); inline;
begin
  TIMER_CR(channel)^ := TIMER_CR(channel)^ and not TIMER_ENABLE;
end;
{$endif NDS_IMPLEMENTATION}
