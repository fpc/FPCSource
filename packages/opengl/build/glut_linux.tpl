{
  $Id$

  Translation of the Mesa GLUT headers for FreePascal
  Linux Version, Copyright (C) 1999-2000 Sebastian Guenther


  Mesa 3-D graphics library
  Version:  3.0
  Copyright (C) 1995-1998  Brian Paul

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


{$MODE delphi}

unit GLUT;

interface

uses GL;

function InitGLUTFromLibrary(libname: PChar): Boolean;

// determines automatically which library to use:
function InitGLUT: Boolean;


var
  GLUTInitialized: Boolean;

  { Set the following value to True if you want to have a list of all
    unresolved GLUT functions dumped to the console }
  GLUTDumpUnresolvedFunctions: Boolean;




const
  // Display mode bit masks
  GLUT_RGB                      = 0;
  GLUT_RGBA                     = GLUT_RGB;
  GLUT_INDEX                    = 1;
  GLUT_SINGLE                   = 0;
  GLUT_DOUBLE                   = 2;
  GLUT_ACCUM                    = 4;
  GLUT_ALPHA                    = 8;
  GLUT_DEPTH                    = 16;
  GLUT_STENCIL                  = 32;
  GLUT_MULTISAMPLE              = 128;
  GLUT_STEREO                   = 256;
  GLUT_LUMINANCE                = 512;

  // Mouse buttons
  GLUT_LEFT_BUTTON              = 0;
  GLUT_MIDDLE_BUTTON            = 1;
  GLUT_RIGHT_BUTTON             = 2;

  // Mouse button state
  GLUT_DOWN                     = 0;
  GLUT_UP                       = 1;

  // Keys ###

  // Enter / exit state
  GLUT_LEFT                     = 0;
  GLUT_ENTERED                  = 1;

  // Menu usage state
  GLUT_MENU_NOT_IN_USE          = 0;
  GLUT_MENU_IN_USE              = 1;

  // Visibility state
  GLUT_NOT_VISIBLE              = 0;
  GLUT_VISIBLE                  = 1;

  // Window status state
  GLUT_HIDDEN                   = 0;
  GLUT_FULLY_RETAINED           = 1;
  GLUT_PARTIALLY_RETAINED       = 2;
  GLUT_FULLY_COVERED            = 3;

  // Color index component selection values
  GLUT_RED                      = 0;
  GLUT_GREEN                    = 1;
  GLUT_BLUE                     = 2;

  // Layers for use
  GLUT_NORMAL                   = 0;
  GLUT_OVERLAY                  = 1;

  // Bitmap stuff###


  // glutGet parameters
  GLUT_WINDOW_X                 = 100;
  GLUT_WINDOW_Y                 = 101;
  GLUT_WINDOW_WIDTH             = 102;
  GLUT_WINDOW_HEIGHT            = 103;
  GLUT_WINDOW_BUFFER_SIZE       = 104;
  GLUT_WINDOW_STENCIL_SIZE      = 105;
  GLUT_WINDOW_DEPTH_SIZE        = 106;
  GLUT_WINDOW_RED_SIZE          = 107;
  GLUT_WINDOW_GREEN_SIZE        = 108;
  GLUT_WINDOW_BLUE_SIZE         = 109;
  GLUT_WINDOW_ALPHA_SIZE        = 110;
  GLUT_WINDOW_ACCUM_RED_SIZE    = 111;
  GLUT_WINDOW_ACCUM_GREEN_SIZE  = 112;
  GLUT_WINDOW_ACCUM_BLUE_SIZE   = 113;
  GLUT_WINDOW_ACCUM_ALPHA_SIZE  = 114;
  GLUT_WINDOW_DOUBLEBUFFER      = 115;
  GLUT_WINDOW_RGBA              = 116;
  GLUT_WINDOW_PARENT            = 117;
  GLUT_WINDOW_NUM_CHILDREN      = 118;
  GLUT_WINDOW_COLORMAP_SIZE     = 119;
  GLUT_WINDOW_NUM_SAMPLES       = 120;
  GLUT_WINDOW_STEREO            = 121;
  GLUT_WINDOW_CURSOR            = 122;
  GLUT_SCREEN_WIDTH             = 200;
  GLUT_SCREEN_HEIGHT            = 201;
  GLUT_SCREEN_WIDTH_MM          = 202;
  GLUT_SCREEN_HEIGHT_MM         = 203;
  GLUT_MENU_NUM_ITEMS           = 300;
  GLUT_DISPLAY_MODE_POSSIBLE    = 400;
  GLUT_INIT_WINDOW_X            = 500;
  GLUT_INIT_WINDOW_Y            = 501;
  GLUT_INIT_WINDOW_WIDTH        = 502;
  GLUT_INIT_WINDOW_HEIGHT       = 503;
  GLUT_INIT_DISPLAY_MODE        = 504;
  GLUT_ELAPSED_TIME             = 700;
  GLUT_WINDOW_FORMAT_ID         = 123;

  // glutDeviceGet parameters
  GLUT_HAS_KEYBOARD             = 600;
  GLUT_HAS_MOUSE                = 601;
  GLUT_HAS_SPACEBALL            = 602;
  GLUT_HAS_DIAL_AND_BUTTON_BOX  = 603;
  GLUT_HAS_TABLET               = 604;
  GLUT_NUM_MOUSE_BUTTONS        = 605;
  GLUT_NUM_SPACEBALL_BUTTONS    = 606;
  GLUT_NUM_BUTTON_BOX_BUTTONS   = 607;
  GLUT_NUM_DIALS                = 608;
  GLUT_NUM_TABLET_BUTTONS       = 609;
  GLUT_DEVICE_IGNORE_KEY_REPEAT = 610;
  GLUT_DEVICE_KEY_REPEAT        = 611;
  GLUT_HAS_JOYSTICK             = 612;
  GLUT_OWNS_JOYSTICK            = 613;
  GLUT_JOYSTICK_BUTTONS         = 614;
  GLUT_JOYSTICK_AXES            = 615;
  GLUT_JOYSTICK_POLL_RATE       = 616;

  // glutLayerGet parameters
  GLUT_OVERLAY_POSSIBLE         = 800;
  GLUT_LAYER_IN_USE             = 801;
  GLUT_HAS_OVERLAY              = 802;
  GLUT_TRANSPARENT_INDEX        = 803;
  GLUT_NORMAL_DAMAGED           = 804;
  GLUT_OVERLAY_DAMAGED          = 805;

  // glutVideoResizeGet parameters
  GLUT_VIDEO_RESIZE_POSSIBLE    = 900;
  GLUT_VIDEO_RESIZE_IN_USE      = 901;
  GLUT_VIDEO_RESIZE_X_DELTA     = 902;
  GLUT_VIDEO_RESIZE_Y_DELTA     = 903;
  GLUT_VIDEO_RESIZE_WIDTH_DELTA = 904;
  GLUT_VIDEO_RESIZE_HEIGHT_DELTA= 905;
  GLUT_VIDEO_RESIZE_X           = 906;
  GLUT_VIDEO_RESIZE_Y           = 907;
  GLUT_VIDEO_RESIZE_WIDTH       = 908;
  GLUT_VIDEO_RESIZE_HEIGHT      = 909;

  // glutGetModifiers return mask
  GLUT_ACTIVE_SHIFT             = 1;
  GLUT_ACTIVE_CTRL              = 2;
  GLUT_ACTIVE_ALT               = 4;

  // Cursor stuff ###

// GLUT window callback sub-API
type
  TGlutDisplayFunc = procedure; #extdecl;
  TGlutReshapeFunc = procedure(width, height: Integer); #extdecl;

  TGlutTimerFunc = procedure(value: Integer); #extdecl;

// GLUT game mode sub-API
// glutGameModeGet
const
  GLUT_GAME_MODE_ACTIVE         = 0;
  GLUT_GAME_MODE_POSSIBLE       = 1;
  GLUT_GAME_MODE_WIDTH          = 2;
  GLUT_GAME_MODE_HEIGHT         = 3;
  GLUT_GAME_MODE_PIXEL_DEPTH    = 4;
  GLUT_GAME_MODE_REFRESH_RATE   = 5;
  GLUT_GAME_MODE_DISPLAY_CHANGED= 6;


var
%GLUTProcs1


implementation

{$LINKLIB Xmu}

function dlopen(AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

procedure FreeLibrary(handle: Pointer);
begin
  dlclose(handle);
end;

function GetProc(handle: Pointer; name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if not Assigned(Result) and GLUTDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGLUT: Pointer;

function InitGLUTFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGLUT := LoadLibrary(libname);
  if not Assigned(libGLUT) then exit;

%GLUTProcs2

  GLUTInitialized := True;
  Result := True;
end;


function InitGLUT: Boolean;
begin
  Result := InitGLUTFromLibrary('libglut.so') or InitGLUTFromLibrary('libglut.so.3');
end;



finalization
  if Assigned(libGLUT) then
    FreeLibrary(libGLUT);
end.
