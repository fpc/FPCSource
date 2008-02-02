(*
  $Id: background.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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


// Background control defines

{$ifdef NDS_INTERFACE}
// BGxCNT defines ///
const
  BG_MOSAIC_ENABLE	= $40;
  BG_COLOR_256		  = $80;
  BG_COLOR_16			  = $00;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function CHAR_BASE_BLOCK(n: cint): cint; inline;
begin
  CHAR_BASE_BLOCK := (n * $4000)+ $06000000;
end;

function CHAR_BASE_BLOCK_SUB(n: cint): cint; inline;
begin		
  CHAR_BASE_BLOCK_SUB := (n * $4000)+ $06200000;
end;

function SCREEN_BASE_BLOCK(n: cint): cint; inline;
begin		
  SCREEN_BASE_BLOCK := (n * $800) + $06000000;
end;

function SCREEN_BASE_BLOCK_SUB(n: cint): cint; inline;
begin	
  SCREEN_BASE_BLOCK_SUB := (n * $800) + $06200000;
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  CHAR_SHIFT           = 2;
  SCREEN_SHIFT         = 8;
  TEXTBG_SIZE_256x256  = $0;
  TEXTBG_SIZE_256x512  = $8000;
  TEXTBG_SIZE_512x256  = $4000;
  TEXTBG_SIZE_512x512  = $C000;

  ROTBG_SIZE_128x128   = $0;
  ROTBG_SIZE_256x256   = $4000;
  ROTBG_SIZE_512x512   = $8000;
  ROTBG_SIZE_1024x1024 = $C000;

  WRAPAROUND           = $1;
{$endif NDS_INTERFACE}

{$ifdef NDS_INTERFACE}
function CHAR_BASE_BLOCK(n: cint): cint; inline;
function CHAR_BASE_BLOCK_SUB(n: cint): cint; inline;
function SCREEN_BASE_BLOCK(n: cint): cint; inline;
function SCREEN_BASE_BLOCK_SUB(n: cint): cint; inline;
{$endif NDS_INTERFACE}
