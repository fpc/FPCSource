{
  $Id$

  Translation of the Mesa GLUT headers for FreePascal
  Linux Version, Copyright (C) 1999 Sebastian Guenther


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

{$MACRO ON}

{$IFDEF Linux}
  {$DEFINE gldecl := cdecl;}
  {$DEFINE extdecl := cdecl;}
  uses GL;
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}


function InitGLUTFromLibrary(libname: PChar): Boolean;

// determines automatically which library to use:
function InitGLUT: Boolean;


var
  GLUTDumpUnresolvedFunctions,
  GLUTInitialized: Boolean;


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
  TGlutDisplayFunc = procedure; extdecl
  TGlutReshapeFunc = procedure(width, height: Integer); extdecl
  TGlutTimerFunc = procedure(value: Integer); extdecl
  TGlutKeyboardFunc = procedure(key: char;x,y:Integer); extdecl
  TGlutIdleFunc = procedure; extdecl
  TGlutVisibilityFunc = procedure(state:Integer); extdecl

// GLUT game mode sub-API
{$ifdef GLUT_GAME}
// glutGameModeGet
const
  GLUT_GAME_MODE_ACTIVE         = 0;
  GLUT_GAME_MODE_POSSIBLE       = 1;
  GLUT_GAME_MODE_WIDTH          = 2;
  GLUT_GAME_MODE_HEIGHT         = 3;
  GLUT_GAME_MODE_PIXEL_DEPTH    = 4;
  GLUT_GAME_MODE_REFRESH_RATE   = 5;
  GLUT_GAME_MODE_DISPLAY_CHANGED= 6;
{$endif GLUT_GAME}

var
// GLUT initialization sub-API
  glutInit: procedure(argcp: PInteger; argv: PPChar); cdecl;
  glutInitDisplayMode: procedure(mode: LongWord); cdecl;
  glutInitDisplayString: procedure(AString: PChar); cdecl;
  glutInitWindowPosition: procedure(x, y: Integer); cdecl;
  glutInitWindowSize: procedure(width, height: Integer); cdecl;
  glutMainLoop: procedure; cdecl;

// GLUT window sub-API
  glutCreateWindow: function(title: PChar): Integer; cdecl;
  glutCreateSubWindow: function(win, x, y, width, height: Integer): Integer; cdecl;
  glutDestroyWindow: procedure(win: Integer); cdecl;
  glutPostRedisplay: procedure; cdecl;
  glutPostWindowRedisplay: procedure(win: Integer); cdecl;
  glutSwapBuffers: procedure; cdecl;
  glutGetWindow: function: Integer; cdecl;
  glutSetWindow: procedure(win: Integer); cdecl;
  glutSetWindowTitle: procedure(title: PChar); cdecl;
  glutSetIconTitle: procedure(title: PChar); cdecl;
  glutPositionWindow: procedure(x, y: Integer); cdecl;
  glutReshapeWindow: procedure(width, height: Integer); cdecl;
  glutPopWindow: procedure; cdecl;
  glutPushWindow: procedure; cdecl;
  glutIconifyWindow: procedure; cdecl;
  glutShowWindow: procedure; cdecl;
  glutHideWindow: procedure; cdecl;
  glutFullScreen: procedure; cdecl;
  glutSetCursor: procedure(cursor: Integer); cdecl;
  glutWarpPointer: procedure(x, y: Integer); cdecl;

//overlays ###

//menus ###

// GLUT window callback sub-API
  glutDisplayFunc: procedure(func: TGlutDisplayFunc); cdecl;
  glutReshapeFunc: procedure(func: TGlutReshapeFunc); cdecl;

  glutTimerFunc: procedure(millis: LongWord; func: TGlutTimerFunc; value: longint); cdecl;
  glutKeyboardFunc : procedure(func: TGlutKeyboardFunc); cdecl;
  glutIdleFunc : procedure(func: TGlutIdleFunc); cdecl;
  glutVisibilityFunc : procedure(func: TGlutVisibilityFunc); cdecl;

// GLUTAPI void APIENTRY glutDisplayFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutReshapeFunc(void (GLUTCALLBACK * func)(int width, int height));
// GLUTAPI void APIENTRY glutKeyboardFunc(void (GLUTCALLBACK * func)(unsigned char key, int x, int y));
// GLUTAPI void APIENTRY glutMouseFunc(void (GLUTCALLBACK * func)(int button, int state, int x, int y));
// GLUTAPI void APIENTRY glutMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutPassiveMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutEntryFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutVisibilityFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutIdleFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutTimerFunc(unsigned int millis, void (GLUTCALLBACK * func)(int value), int value);
// GLUTAPI void APIENTRY glutMenuStateFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutSpecialFunc(void (GLUTCALLBACK * func)(int key, int x, int y));
// GLUTAPI void APIENTRY glutSpaceballMotionFunc(void (GLUTCALLBACK * func)(int x, int y, int z));
// GLUTAPI void APIENTRY glutSpaceballRotateFunc(void (GLUTCALLBACK * func)(int x, int y, int z));
// GLUTAPI void APIENTRY glutSpaceballButtonFunc(void (GLUTCALLBACK * func)(int button, int state));
// GLUTAPI void APIENTRY glutButtonBoxFunc(void (GLUTCALLBACK * func)(int button, int state));
// GLUTAPI void APIENTRY glutDialsFunc(void (GLUTCALLBACK * func)(int dial, int value));
// GLUTAPI void APIENTRY glutTabletMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutTabletButtonFunc(void (GLUTCALLBACK * func)(int button, int state, int x, int y));
// GLUTAPI void APIENTRY glutMenuStatusFunc(void (GLUTCALLBACK * func)(int status, int x, int y));
// GLUTAPI void APIENTRY glutOverlayDisplayFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutWindowStatusFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutKeyboardUpFunc(void (GLUTCALLBACK * func)(unsigned char key, int x, int y));
// GLUTAPI void APIENTRY glutSpecialUpFunc(void (GLUTCALLBACK * func)(int key, int x, int y));
// GLUTAPI void APIENTRY glutJoystickFunc(void (GLUTCALLBACK * func)(unsigned int buttonMask, int x, int y, int z), int pollInterval)

// GLUT color index sub-API
  glutSetColor: procedure(index: Integer; red, green, blue: Single); cdecl;
  glutGetColor: function(ndx, component: Integer): Single; cdecl;
  glutCopyColormap: procedure(win: Integer); cdecl;

// GLUT state retrieval sub-API
  glutGet: function(AType: GLEnum): Integer; cdecl;
  glutDeviceGet: function(AType: GLEnum): Integer; cdecl;
  glutExtensionSupported: function(name: PChar): Integer; cdecl;
  glutGetModifiers: function: Integer; cdecl;
  glutLayerGet: function(AType: GLEnum): Integer; cdecl;

// fonts ###

// pre-built models ###

// video resize ###

// debugging ###

// device control ###


// GLUT game mode sub-API
{$ifdef GLUT_GAME}
  glutGameModeString: procedure(AString: PChar); cdecl;
  glutEnterGameMode: function: Integer; cdecl;
  glutLeaveGameMode: procedure; cdecl;
  glutGameModeGet: function(mode: GLEnum): Integer; cdecl;
{$endif GLUT_GAME}


implementation

{$LINKLIB Xmu}

function dlopen(AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

function GetProc(handle: Pointer; name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if (Result = nil) and GLUTDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGLUT: Pointer;

function InitGLUTFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGLUT := LoadLibrary(libname);
  if not Assigned(libGLUT) then exit;

// GLUT initialization sub-API
  glutInit := GetProc(libglut, 'glutInit');
  glutInitDisplayMode := GetProc(libglut, 'glutInitDisplayMode');
  glutInitDisplayString := GetProc(libglut, 'glutInitDisplayString');
  glutInitWindowPosition := GetProc(libglut, 'glutInitWindowPosition');
  glutInitWindowSize := GetProc(libglut, 'glutInitWindowSize');
  glutMainLoop := GetProc(libglut, 'glutMainLoop');
// GLUT window sub-API
  glutCreateWindow := GetProc(libglut, 'glutCreateWindow');
  glutCreateSubWindow := GetProc(libglut, 'glutCreateSubWindow');
  glutDestroyWindow := GetProc(libglut, 'glutDestroyWindow');
  glutPostRedisplay := GetProc(libglut, 'glutPostRedisplay');
  glutPostWindowRedisplay := GetProc(libglut, 'glutPostWindowRedisplay');
  glutSwapBuffers := GetProc(libglut, 'glutSwapBuffers');
  glutGetWindow := GetProc(libglut, 'glutGetWindow');
  glutSetWindow := GetProc(libglut, 'glutSetWindow');
  glutSetWindowTitle := GetProc(libglut, 'glutSetWindowTitle');
  glutSetIconTitle := GetProc(libglut, 'glutSetIconTitle');
  glutPositionWindow := GetProc(libglut, 'glutPositionWindow');
  glutReshapeWindow := GetProc(libglut, 'glutReshapeWindow');
  glutPopWindow := GetProc(libglut, 'glutPopWindow');
  glutPushWindow := GetProc(libglut, 'glutPushWindow');
  glutIconifyWindow := GetProc(libglut, 'glutIconifyWindow');
  glutShowWindow := GetProc(libglut, 'glutShowWindow');
  glutHideWindow := GetProc(libglut, 'glutHideWindow');
  glutFullScreen := GetProc(libglut, 'glutFullScreen');
  glutSetCursor := GetProc(libglut, 'glutSetCursor');
  glutWarpPointer := GetProc(libglut, 'glutWarpPointer');
//overlays ###
//menus ###
// GLUT window callback sub-API
  glutDisplayFunc := GetProc(libglut, 'glutDisplayFunc');
  glutReshapeFunc := GetProc(libglut, 'glutReshapeFunc');
  glutTimerFunc := GetProc(libglut, 'glutTimerFunc');
  glutKeyboardFunc := GetProc(libglut, 'glutKeyboardFunc');
  glutIdleFunc := GetProc(libglut, 'glutIdleFunc');
  glutVisibilityFunc := GetProc(libglut, 'glutVisibilityFunc');
// GLUTAPI void APIENTRY glutDisplayFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutReshapeFunc(void (GLUTCALLBACK * func)(int width, int height));
// GLUTAPI void APIENTRY glutKeyboardFunc(void (GLUTCALLBACK * func)(unsigned char key, int x, int y));
// GLUTAPI void APIENTRY glutMouseFunc(void (GLUTCALLBACK * func)(int button, int state, int x, int y));
// GLUTAPI void APIENTRY glutMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutPassiveMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutEntryFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutVisibilityFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutIdleFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutTimerFunc(unsigned int millis, void (GLUTCALLBACK * func)(int value), int value);
// GLUTAPI void APIENTRY glutMenuStateFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutSpecialFunc(void (GLUTCALLBACK * func)(int key, int x, int y));
// GLUTAPI void APIENTRY glutSpaceballMotionFunc(void (GLUTCALLBACK * func)(int x, int y, int z));
// GLUTAPI void APIENTRY glutSpaceballRotateFunc(void (GLUTCALLBACK * func)(int x, int y, int z));
// GLUTAPI void APIENTRY glutSpaceballButtonFunc(void (GLUTCALLBACK * func)(int button, int state));
// GLUTAPI void APIENTRY glutButtonBoxFunc(void (GLUTCALLBACK * func)(int button, int state));
// GLUTAPI void APIENTRY glutDialsFunc(void (GLUTCALLBACK * func)(int dial, int value));
// GLUTAPI void APIENTRY glutTabletMotionFunc(void (GLUTCALLBACK * func)(int x, int y));
// GLUTAPI void APIENTRY glutTabletButtonFunc(void (GLUTCALLBACK * func)(int button, int state, int x, int y));
// GLUTAPI void APIENTRY glutMenuStatusFunc(void (GLUTCALLBACK * func)(int status, int x, int y));
// GLUTAPI void APIENTRY glutOverlayDisplayFunc(void (GLUTCALLBACK * func)(void));
// GLUTAPI void APIENTRY glutWindowStatusFunc(void (GLUTCALLBACK * func)(int state));
// GLUTAPI void APIENTRY glutKeyboardUpFunc(void (GLUTCALLBACK * func)(unsigned char key, int x, int y));
// GLUTAPI void APIENTRY glutSpecialUpFunc(void (GLUTCALLBACK * func)(int key, int x, int y));
// GLUTAPI void APIENTRY glutJoystickFunc(void (GLUTCALLBACK * func)(unsigned int buttonMask, int x, int y, int z), int pollInterval)
// GLUT color index sub-API
  glutSetColor := GetProc(libglut, 'glutSetColor');
  glutGetColor := GetProc(libglut, 'glutGetColor');
  glutCopyColormap := GetProc(libglut, 'glutCopyColormap');
// GLUT state retrieval sub-API
  glutGet := GetProc(libglut, 'glutGet');
  glutDeviceGet := GetProc(libglut, 'glutDeviceGet');
  glutExtensionSupported := GetProc(libglut, 'glutExtensionSupported');
  glutGetModifiers := GetProc(libglut, 'glutGetModifiers');
  glutLayerGet := GetProc(libglut, 'glutLayerGet');
// fonts ###
// pre-built models ###
// video resize ###
// debugging ###
// device control ###
// GLUT game mode sub-API
{$ifdef GLUT_GAME}
  glutGameModeString := GetProc(libglut, 'glutGameModeString');
  glutEnterGameMode := GetProc(libglut, 'glutEnterGameMode');
  glutLeaveGameMode := GetProc(libglut, 'glutLeaveGameMode');
  glutGameModeGet := GetProc(libglut, 'glutGameModeGet');
{$endif GLUT_GAME}

  GLUTInitialized := True;
  Result := True;
end;


function InitGLUT: Boolean;
begin
  Result := InitGLUTFromLibrary('libglut.so') or
            InitGLUTFromLibrary('libglut.so.3');
end;


initialization
  InitGLUT;
finalization
  if Assigned(libGLUT) then dlClose(libGLUT);
end.


{
  $Log$
  Revision 1.1.2.1  2000-11-06 11:35:40  marco
   * Standard freebsd-unix-linux split

  Revision 1.4.2.1  2000/10/01 22:12:28  peter
    * new demo

}
