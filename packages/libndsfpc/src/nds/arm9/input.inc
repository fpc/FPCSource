(*
  $Id: input.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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
  
procedure scanKeys(); cdecl; external;

function keysCurrent(): cuint32; cdecl; external;

function keysHeld(): cuint32; cdecl; external;

function keysDown(): cuint32; cdecl; external;

function keysDownRepeat(): cuint32; cdecl; external;

procedure keysSetRepeat(setDelay: cuint8; setRepeat: cuint8); cdecl; external;

function keysUp(): cuint32; cdecl; external;

function touchReadXY(): touchPosition; cdecl; external;

procedure touchRead(var data: touchPosition); cdecl; external;
{$endif NDS_INTERFACE}
