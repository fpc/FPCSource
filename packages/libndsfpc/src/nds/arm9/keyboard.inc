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
//  KeyChangeCallback = function(key: cint): pointer;
  KeyChangeCallback = procedure(key: cint);
  PKeyChangeCallback = ^KeyChangeCallback;

  KeyboardState = integer;
const  
  Lower   : KeyboardState = 0;
  Upper   : KeyboardState = 1;
  Numeric : KeyboardState = 2;
  Reduced : KeyboardState = 3;

type
  KeyMap = record
    mapDataPressed: pcuint16;
    mapDataReleased: pcuint16;
    keymap: pcint;
    width: cint;
    height: cint;
  end;
  TKeyMap = KeyMap;
  PKeyMap = ^KeyMap;

  Keyboard = record
    background: cint;
    keyboardOnSub: cint;
    offset_x: cint;
    offset_y: cint;
    grid_width: cint;
    grid_height: cint;
    state: KeyboardState;
    shifted: cint;
    visible: cint;
    mappings: array [0..3] of PKeyMap;
    tiles: pcuint16;
    tileLen: cuint32;
    palette: pcuint16;
    paletteLen: cuint32;
    mapBase: cint;
    tileBase: cint;
    tileOffset: cint;
    scrollSpeed: cuint32;
    OnKeyPressed: KeyChangeCallback;
    OnKeyReleased: KeyChangeCallback;
  end;
  TKeyboard = Keyboard;
  PKeyboard = ^Keyboard;


function keyboardGetDefault(): PKeyboard; cdecl; external;

function keyboardInit(keyboard: PKeyboard; layer: cint; atype: BgType; size: BgSize; 
  mapBase, tileBase: cint; mainDisplay: cbool; loadGraphics: cbool): PKeyboard; cdecl; external;
function keyboardDemoInit(): PKeyboard; cdecl; external;
procedure keyboardShow(); cdecl; external;
procedure keyboardHide(); cdecl; external;
function keyboardGetKey(x, y: cint): cint; cdecl; external;
procedure keyboardGetString(buffer: pchar; maxLen: cint); cdecl; external;
function keyboardGetChar(): cint; cdecl; external;
function keyboardUpdate(): cint; cdecl; external;
{$endif NDS_INTERFACE}
