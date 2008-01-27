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

{$ifdef NDS_IMPLEMENTATION}
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
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  //!	Same as %TIMER_DATA(0).
  TIMER0_DATA    : pcuint16 = pointer($04000100);
  //!	Same as %TIMER_DATA(1).
  TIMER1_DATA    : pcuint16 = pointer($04000104);
  //!	Same as %TIMER_DATA(2).
  TIMER2_DATA    : pcuint16 = pointer($04000108);
  //!	Same as %TIMER_DATA(3).
  TIMER3_DATA    : pcuint16 = pointer($0400010C);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function TIMER_DATA(n: cuint): pcuint16; inline;
begin
  TIMER_DATA := pcuint16($04000100 + (n shl 2));
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
// Timer control registers
  //!	Same as %TIMER_CR(0).
  TIMER0_CR   : pcuint16 = pointer($04000102);
  //!	Same as %TIMER_CR(1).
  TIMER1_CR   : pcuint16 = pointer($04000106);
  //!	Same as %TIMER_CR(2).
  TIMER2_CR   : pcuint16 = pointer($0400010A);
  //!	Same as %TIMER_CR(3).
  TIMER3_CR   : pcuint16 = pointer($0400010E);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function TIMER_CR(n: cint): pcuint16; inline;
begin
  TIMER_CR := pcuint16($04000102 + (n shl 2));
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  //!	Enables the timer.
  TIMER_ENABLE  = (1 shl 7);

  //!	Causes the timer to request an Interupt on overflow.
  TIMER_IRQ_REQ = (1 shl 6);

  //!	When set will cause the timer to count when the timer below overflows (unavailable for timer 0).
  TIMER_CASCADE = (1 shl 2);
  
  //!	Causes the timer to count at 33.514Mhz.
  TIMER_DIV_1   =  (0);
  //!	Causes the timer to count at (33.514 / 64) Mhz.
  TIMER_DIV_64  = (1);
  //!	Causes the timer to count at (33.514 / 256) Mhz.
  TIMER_DIV_256 = (2);
  //!	Causes the timer to count at (33.514 / 1024)Mhz.
  TIMER_DIV_1024  = (3);
{$endif NDS_INTERFACE}

{$ifdef NDS_INTERFACE}
function TIMER_FREQ(n: cint): cint; inline;
function TIMER_FREQ_64(n: cint): cint; inline;
function TIMER_FREQ_256(n: cint): cint; inline;
function TIMER_FREQ_1024(n: cint): cint; inline; 
function TIMER_DATA(n: cuint): pcuint16; inline;
function TIMER_CR(n: cint): pcuint16; inline;
{$endif NDS_INTERFACE}
