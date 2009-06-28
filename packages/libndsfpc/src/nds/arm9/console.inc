(*
  $Id: console.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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
//const
//  CONSOLE_USE_COLOR255 = 16;

type
  ConsolePrint = function(con: pointer; c: cchar): pcbool;

  ConsoleFont = record
    gfx: pcuint16;
    pal: pcuint16;
    numColors: cuint16;
      
    bpp: cuint8;
    
    asciiOffset: cuint16;
    numChars: cuint16;
    convertSingleColor: cbool;
  end;
  PConsoleFont = ^ConsoleFont;
  TConsoleFont = ConsoleFont;
  
  PrintConsole = record
    font: ConsoleFont;
    
    fontBgMap: pcuint16;
    fontBgGfx: pcuint16;
    
    mapBase: cuint8;
    gfxBase: cuint8;
    
    bgLayer: cuint8;
    bgId: cint;
    
    cursorX: cint;
    cursorY: cint;
    
    prevCursorX: cint;
    prevCursorY: cint;
    
    consoleWidth: cint;
    consoleHeight: cint;

    windowX: cint;
    windowY: cint;
    windowWidth: cint;
    windowHeight: cint;
    
    tabSize: cint;
    
    fontCharOffset: cuint16;
    
    
    fontCurPal: cuint16;
    
    PrintChar: ConsolePrint;
    
    consoleInitialised: cbool;
    loadGraphics: cbool;
  end;
  PPrintConsole = ^PrintConsole;
  TPrintConsole = PrintConsole;
  


type
  DebugDevice = integer;
const
  DebugDevice_NOCASH: DebugDevice = $01;
  DebugDevice_CONSOLE: DebugDevice = $02;
  

{$endif NDS_INTERFACE}

{$ifdef NDS_INTERFACE}

procedure consoleSetFont(console: PPrintConsole; font: PConsoleFont); cdecl; external;
procedure consoleSetWindow(console: PPrintConsole; x, y, width, height: cint); cdecl; external;
function consoleGetDefault(): PPrintConsole; cdecl; external;
procedure consoleSelect(console: PPrintConsole); cdecl; external;
function consoleInit(console: PPrintConsole; layer: cint; atype: BgType; size: BgSize; mapBase, 
  tileBase: cint; mainDisplay: cbool; loadGraphics: cbool): pPrintConsole; cdecl; external;
function consoleDemoInit(): PPrintConsole; cdecl; external;
procedure consoleClear(); cdecl; external;
procedure consoleDebugInit(device: DebugDevice); cdecl; external;

{$endif NDS_INTERFACE}
