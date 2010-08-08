(*
  $Id$
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
  getByteCallback = function(source: pcuint8): pcuint8;
  getHeaderCallback = function(source: pcuint8; dest: pcuint16; arg: cuint32): pcint;

type
  DecompressType = integer;
const
  LZ77    : DecompressType = 0; 
  LZ77Vram: DecompressType = 1;
  HUFF    : DecompressType = 2;
  RLE     : DecompressType = 3;
  RLEVram : DecompressType = 4;

procedure decompress(const data: pointer; dst: pointer; atype: DecompressType); cdecl; external;
procedure decompressStream(const data: pointer; dst: pointer; atype: DecompressType; readCB: getByteCallback; getHeaderCB: getHeaderCallback); cdecl; external;


{$endif NDS_INTERFACE}
