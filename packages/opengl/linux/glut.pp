{
  $Id$

  Translation of the GLUT 3.7 headers for Free Pascal, Linux version
  Copyright (C) 1999-2000 Sebastian Guenther, sg@freepascal.org


  Copyright (c) Mark J. Kilgard, 1994, 1995, 1996, 1998.

  This program is freely distributable without licensing fees  and is
  provided without guarantee or warrantee expressed or  implied. This
  program is -not- in the public domain.
}


unit GLUT;

{$MODE delphi}

interface

uses GL;

function InitGLUTFromLibrary(const libname: PChar): Boolean;

// determines automatically which library to use:
function InitGLUT: Boolean;


var
  GLUTInitialized: Boolean;

  { Set the following value to True if you want to have a list of all
    unresolved GLUT functions dumped to the console }
  GLUTDumpUnresolvedFunctions: Boolean;


{ GLUT API revision history:
 
  GLUT_API_VERSION is updated to reflect incompatible GLUT
  API changes (interface changes, semantic changes, deletions,
  or additions).
 
  GLUT_API_VERSION=1  First public release of GLUT.  11/29/94

  GLUT_API_VERSION=2  Added support for OpenGL/GLX multisampling,
  extension.  Supports new input devices like tablet, dial and button
  box, and Spaceball.  Easy to query OpenGL extensions.

  GLUT_API_VERSION=3  glutMenuStatus added.

  GLUT_API_VERSION=4  glutInitDisplayString, glutWarpPointer,
  glutBitmapLength, glutStrokeLength, glutWindowStatusFunc, dynamic
  video resize subAPI, glutPostWindowRedisplay, glutKeyboardUpFunc,
  glutSpecialUpFunc, glutIgnoreKeyRepeat, glutSetKeyRepeat,
  glutJoystickFunc, glutForceJoystickFunc (NOT FINALIZED!). }

const
  GLUT_API_VERSION		= 4;

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

  // function keys
  GLUT_KEY_F1			= 1;
  GLUT_KEY_F2			= 2;
  GLUT_KEY_F3			= 3;
  GLUT_KEY_F4			= 4;
  GLUT_KEY_F5			= 5;
  GLUT_KEY_F6			= 6;
  GLUT_KEY_F7			= 7;
  GLUT_KEY_F8			= 8;
  GLUT_KEY_F9			= 9;
  GLUT_KEY_F10			= 10;
  GLUT_KEY_F11			= 11;
  GLUT_KEY_F12			= 12;
  // directional keys
  GLUT_KEY_LEFT			= 100;
  GLUT_KEY_UP			= 101;
  GLUT_KEY_RIGHT		= 102;
  GLUT_KEY_DOWN			= 103;
  GLUT_KEY_PAGE_UP		= 104;
  GLUT_KEY_PAGE_DOWN		= 105;
  GLUT_KEY_HOME			= 106;
  GLUT_KEY_END			= 107;
  GLUT_KEY_INSERT		= 108;

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

  // glutSetCursor parameters
  // Basic arrows
  GLUT_CURSOR_RIGHT_ARROW	= 0;
  GLUT_CURSOR_LEFT_ARROW	= 1;
  // Symbolic cursor shapes
  GLUT_CURSOR_INFO		= 2;
  GLUT_CURSOR_DESTROY		= 3;
  GLUT_CURSOR_HELP		= 4;
  GLUT_CURSOR_CYCLE		= 5;
  GLUT_CURSOR_SPRAY		= 6;
  GLUT_CURSOR_WAIT		= 7;
  GLUT_CURSOR_TEXT		= 8;
  GLUT_CURSOR_CROSSHAIR		= 9;
  // Directional cursors
  GLUT_CURSOR_UP_DOWN		= 10;
  GLUT_CURSOR_LEFT_RIGHT	= 11;
  // Sizing cursors
  GLUT_CURSOR_TOP_SIDE		= 12;
  GLUT_CURSOR_BOTTOM_SIDE	= 13;
  GLUT_CURSOR_LEFT_SIDE		= 14;
  GLUT_CURSOR_RIGHT_SIDE	= 15;
  GLUT_CURSOR_TOP_LEFT_CORNER	= 16;
  GLUT_CURSOR_TOP_RIGHT_CORNER	= 17;
  GLUT_CURSOR_BOTTOM_RIGHT_CORNER = 18;
  GLUT_CURSOR_BOTTOM_LEFT_CORNER = 19;
  // Inherit from parent window
  GLUT_CURSOR_INHERIT		= 100;
  // Blank cursor
  GLUT_CURSOR_NONE		= 101;
  // Fullscreen crosshair (if available)
  GLUT_CURSOR_FULL_CROSSHAIR	= 102;

type

  // GLUT menu sub-API
  TGlutCreateMenuFunc = procedure(arg: Int); cdecl;

  // GLUT window callback sub-API
  TGlutDisplayFunc = procedure; cdecl;
  TGlutReshapeFunc = procedure(width, height: Int); cdecl;
  TGlutKeyboardFunc = procedure(key: Char; x, y: Int); cdecl;
  TGlutMouseFunc = procedure(button, state, x, y: Int); cdecl;
  TGlutMotionFunc = procedure(x, y: Int); cdecl;
  TGlutPassiveMotionFunc = procedure(x, y: Int); cdecl;
  TGlutEntryFunc = procedure(x, y: Int); cdecl;
  TGlutVisibilityFunc = procedure(state: Int); cdecl;
  TGlutIdleFunc = procedure; cdecl;
  TGlutTimerFunc = procedure(value: Int); cdecl;
  TGlutMenuStateFunc = procedure(state: Int); cdecl;
  TGlutSpecialFunc = procedure(key, x, y: Int); cdecl;
  TGlutSpaceballMotionFunc = procedure(x, y, z: Int); cdecl;
  TGlutSpaceballRotateFunc = procedure(x, y, z: Int); cdecl;
  TGlutSpaceballButtonFunc = procedure(button, state: Int); cdecl;
  TGlutButtonBoxFunc = procedure(button, state: Int); cdecl;
  TGlutDialsFunc = procedure(dial, value: Int); cdecl;
  TGlutTabletMotionFunc = procedure(x, y: Int); cdecl;
  TGlutTabletButtonFunc = procedure(button, state, x, y: Int); cdecl;
  TGlutMenuStatusFunc = procedure(status, x, y: Int); cdecl;
  TGlutOverlayDisplayFunc = procedure; cdecl;
  TGlutWindowStatusFunc = procedure(state: Int); cdecl;
  TGlutKeyboardUpFunc = procedure(key: Char; x, y: Int); cdecl;
  TGlutSpecialUpFunc = procedure(key, x, y: Int); cdecl;
  TGlutJoystickFunc = procedure(buttonMask: UnsignedInt; x, y, z: Int); cdecl;

const
// GLUT device control sub-API
  // glutSetKeyRepeat modes.
  GLUT_KEY_REPEAT_OFF		= 0;
  GLUT_KEY_REPEAT_ON		= 1;
  GLUT_KEY_REPEAT_DEFAULT	= 2;

  // Joystick button masks
  GLUT_JOYSTICK_BUTTON_A	= 1;
  GLUT_JOYSTICK_BUTTON_B	= 2;
  GLUT_JOYSTICK_BUTTON_C	= 4;
  GLUT_JOYSTICK_BUTTON_D	= 8;

// GLUT game mode sub-API
  // glutGameModeGet
  GLUT_GAME_MODE_ACTIVE         = 0;
  GLUT_GAME_MODE_POSSIBLE       = 1;
  GLUT_GAME_MODE_WIDTH          = 2;
  GLUT_GAME_MODE_HEIGHT         = 3;
  GLUT_GAME_MODE_PIXEL_DEPTH    = 4;
  GLUT_GAME_MODE_REFRESH_RATE   = 5;
  GLUT_GAME_MODE_DISPLAY_CHANGED= 6;


var

{ The following stuff does not exist in the Win32 version: }
(* commented out because cvars don't work in Delphi mode...
// Stroke font opaque addresses (use constants instead in source code).
var
  glutStrokeRoman, glutStrokeMonoRoman: Pointer; cvar; external;

// Stroke font constants (use these in GLUT program).
const
  GLUT_STROKE_ROMAN = @glutStrokeRoman;
  GLUT_STROKE_MONO_ROMAN = @glutStrokeMonoRoman;

// Bitmap font opaque addresses (use constants instead in source code).
var
  glutBitmap9By15, glutBitmap8By13, glutBitmapTimesRoman10,
    glutBitmapTimesRoman24, glutBitmapHelvetica10, glutBitmapHelvetica12,
    glutBitmapHelvetica18: Pointer; cdecl; external;

// Bitmap font constants (use these in GLUT program).
const
  GLUT_BITMAP_9_BY_15 = @glutBitmap9By15;
  GLUT_BITMAP_8_BY_13 = @glutBitmap8By13;
  GLUT_BITMAP_TIMES_ROMAN_10 = @glutBitmapTimesRoman10;
  GLUT_BITMAP_TIMES_ROMAN_24 = @glutBitmapTimesRoman24;
  GLUT_BITMAP_HELVETICA_10 = @glutBitmapHelvetica10;
  GLUT_BITMAP_HELVETICA_12 = @glutBitmapHelvetica12;
  GLUT_BITMAP_HELVETICA_18 = @glutBitmapHelvetica18;*)

// GLUT initialization sub-API
  glutInit: procedure(argcp: PInt; argv: PPChar); cdecl;
  glutInitDisplayMode: procedure(mode: UnsignedInt); cdecl;
  glutInitDisplayString: procedure(const AString: PChar); cdecl;
  glutInitWindowPosition: procedure(x, y: Int); cdecl;
  glutInitWindowSize: procedure(width, height: Int); cdecl;
  glutMainLoop: procedure; cdecl;

// GLUT window sub-API
  glutCreateWindow: function(const title: PChar): Int; cdecl;
  glutCreateSubWindow: function(win, x, y, width, height: Int): Int; cdecl;
  glutDestroyWindow: procedure(win: Int); cdecl;
  glutPostRedisplay: procedure; cdecl;
  glutPostWindowRedisplay: procedure(win: Int); cdecl;
  glutSwapBuffers: procedure; cdecl;
  glutGetWindow: function: Int; cdecl;
  glutSetWindow: procedure(win: Int); cdecl;
  glutSetWindowTitle: procedure(const title: PChar); cdecl;
  glutSetIconTitle: procedure(title: PChar); cdecl;
  glutPositionWindow: procedure(x, y: Int); cdecl;
  glutReshapeWindow: procedure(width, height: Int); cdecl;
  glutPopWindow: procedure; cdecl;
  glutPushWindow: procedure; cdecl;
  glutIconifyWindow: procedure; cdecl;
  glutShowWindow: procedure; cdecl;
  glutHideWindow: procedure; cdecl;
  glutFullScreen: procedure; cdecl;
  glutSetCursor: procedure(cursor: Int); cdecl;
  glutWarpPointer: procedure(x, y: Int); cdecl;

// GLUT overlay sub-API
  glutEstablishOverlay: procedure; cdecl;
  glutRemoveOverlay: procedure; cdecl;
  glutUseLayer: procedure(layer: GLenum); cdecl;
  glutPostOverlayRedisplay: procedure; cdecl;
  glutPostWindowOverlayRedisplay: procedure(win: Int); cdecl;
  glutShowOverlay: procedure; cdecl;
  glutHideOverlay: procedure; cdecl;

// GLUT menu sub-API
  glutCreateMenu: function(func: TGlutCreateMenuFunc): Int; cdecl;
  glutDestroyMenu: procedure(menu: Int); cdecl;
  glutGetMenu: function: Int; cdecl;
  glutSetMenu: procedure(menu: Int); cdecl;
  glutAddMenuEntry: procedure(const ALabel: PChar; value: Int); cdecl;
  glutAddSubMenu: procedure(const ALabel: PChar; submenu: Int); cdecl;
  glutChangeToMenuEntry: procedure(item: Int; const ALabel: PChar; value: Int); cdecl;
  glutChangeToSubMenu: procedure(item: Int; const ALabel: PChar; submenu: Int); cdecl;
  glutRemoveMenuItem: procedure(item: Int); cdecl;
  glutAttachMenu: procedure(button: Int); cdecl;
  glutDetachMenu: procedure(button: Int); cdecl;

// GLUT window callback sub-API
  glutDisplayFunc: procedure(func: TGlutDisplayFunc); cdecl;
  glutReshapeFunc: procedure(func: TGlutReshapeFunc); cdecl;
  glutKeyboardFunc: procedure(func: TGlutKeyboardFunc); cdecl;
  glutMouseFunc: procedure(func: TGlutMouseFunc); cdecl;
  glutMotionFunc: procedure(func: TGlutMotionFunc); cdecl;
  glutPassiveMotionFunc: procedure(func: TGlutPassiveMotionFunc); cdecl;
  glutEntryFunc: procedure(func: TGlutEntryFunc); cdecl;
  glutIdleFunc: procedure(func: TGlutIdleFunc); cdecl;
  glutTimerFunc: procedure(millis: UnsignedInt; func: TGlutTimerFunc; value: Int); cdecl;
  glutMenuStateFunc: procedure(func: TGlutMenuStateFunc); cdecl;
  glutSpecialFunc: procedure(func: TGlutSpecialFunc); cdecl;
  glutSpaceballMotionFunc: procedure(func: TGlutSpaceballMotionFunc); cdecl;
  glutSpaceballRotateFunc: procedure(func: TGlutSpaceballRotateFunc); cdecl;
  glutSpaceballButtonFunc: procedure(func: TGlutSpaceballButtonFunc); cdecl;
  glutButtonBoxFunc: procedure(func: TGlutButtonBoxFunc); cdecl;
  glutDialsFunc: procedure(func: TGlutDialsFunc); cdecl;
  glutTabletMotionFunc: procedure(func: TGlutTabletMotionFunc); cdecl;
  glutTabletButtonFunc: procedure(func: TGlutTabletButtonFunc); cdecl;
  glutMenuStatusFunc: procedure(func: TGlutMenuStatusFunc); cdecl;
  glutOverlayDisplayFunc: procedure(func: TGlutOverlayDisplayFunc); cdecl;
  glutWindowStatusFunc: procedure(func: TGlutWindowStatusFunc); cdecl;
  glutKeyboardUpFunc: procedure(func: TGlutKeyboardUpFunc); cdecl;
  glutSpecialUpFunc: procedure(func: TGlutSpecialUpFunc); cdecl;
  glutJoystickFunc: procedure(func: TGlutJoystickFunc; pollinterval: Int); cdecl;

// GLUT color index sub-API
  glutSetColor: procedure(index: Int; red, green, blue: GLfloat); cdecl;
  glutGetColor: function(ndx, component: Int): GLfloat; cdecl;
  glutCopyColormap: procedure(win: Int); cdecl;

// GLUT state retrieval sub-API
  glutGet: function(AType: GLEnum): Int; cdecl;
  glutDeviceGet: function(AType: GLEnum): Int; cdecl;
  glutExtensionSupported: function(const name: PChar): Int; cdecl;
  glutGetModifiers: function: Int; cdecl;
  glutLayerGet: function(AType: GLEnum): Int; cdecl;

// GLUT font sub-API
  glutBitmapCharacter: procedure(font: Pointer; character: Int); cdecl;
  glutBitmapWidth: function(font: Pointer; character: Int): Int; cdecl;
  glutStrokeCharacter: procedure(font: Pointer; character: Int); cdecl;
  glutStrokeWidth: function(font: Pointer; character: Int): Int; cdecl;
  glutBitmapLength: function(font: Pointer; const AString: PChar): Int; cdecl;
  glutStrokeLength: function(font: Pointer; const AString: PChar): Int; cdecl;

// GLUT pre-built models sub-API
  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); cdecl;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); cdecl;
  glutWireCone: procedure(base: GLdouble; height: GLdouble; slices, stacks: GLint); cdecl;
  glutSolidCone: procedure(base: GLdouble; height: GLdouble; slices, stacks: GLint); cdecl;
  glutWireCube: procedure(size: GLdouble); cdecl;
  glutSolidCube: procedure(size: GLdouble); cdecl;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); cdecl;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); cdecl;
  glutWireDodecahedron: procedure; cdecl;
  glutSolidDodecahedron: procedure; cdecl;
  glutWireTeapot: procedure(size: GLdouble); cdecl;
  glutSolidTeapot: procedure(size: GLdouble); cdecl;
  glutWireOctahedron: procedure; cdecl;
  glutSolidOctahedron: procedure; cdecl;
  glutWireTetrahedron: procedure; cdecl;
  glutSolidTetrahedron: procedure; cdecl;
  glutWireIcosahedron: procedure; cdecl;
  glutSolidIcosahedron: procedure; cdecl;

// GLUT video resize sub-API
  glutVideoResizeGet: function(param: GLenum): Int; cdecl;
  glutSetupVideoResizing: procedure; cdecl;
  glutStopVideoResizing: procedure; cdecl;
  glutVideoResize: procedure(x, y, width, height: Int); cdecl;
  glutVideoPan: procedure(x, y, width, height: Int); cdecl;

// GLUT debugging sub-API
  glutReportErrors: procedure; cdecl;

// GLUT device control sub-API
  glutIgnoreKeyRepeat: procedure(ignore: Int); cdecl;
  glutSetKeyRepeat: procedure(repeatMode: Int); cdecl;
  glutForceJoystickFunc: procedure; cdecl;

// GLUT game mode sub-API
  glutGameModeString: procedure(const AString: PChar); cdecl;
  glutEnterGameMode: function: Integer; cdecl;
  glutLeaveGameMode: procedure; cdecl;
  glutGameModeGet: function(mode: GLEnum): Int; cdecl;



implementation

{$LINKLIB Xmu}

function dlopen(const AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; const name: PChar): Pointer; external 'dl';

function LoadLibrary(const name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

procedure FreeLibrary(handle: Pointer);
begin
  dlclose(handle);
end;

function GetProc(handle: Pointer; const name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if not Assigned(Result) and GLUTDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGLUT: Pointer;

function InitGLUTFromLibrary(const libname: PChar): Boolean;
begin
  Result := False;
  libGLUT := LoadLibrary(libname);
  if not Assigned(libGLUT) then
    exit;

  glutInit := GetProc(libglut, 'glutInit');
  glutInitDisplayMode := GetProc(libglut, 'glutInitDisplayMode');
  glutInitDisplayString := GetProc(libglut, 'glutInitDisplayString');
  glutInitWindowPosition := GetProc(libglut, 'glutInitWindowPosition');
  glutInitWindowSize := GetProc(libglut, 'glutInitWindowSize');
  glutMainLoop := GetProc(libglut, 'glutMainLoop');
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
  glutEstablishOverlay := GetProc(libglut, 'glutEstablishOverlay');
  glutRemoveOverlay := GetProc(libglut, 'glutRemoveOverlay');
  glutUseLayer := GetProc(libglut, 'glutUseLayer');
  glutPostOverlayRedisplay := GetProc(libglut, 'glutPostOverlayRedisplay');
  glutPostWindowOverlayRedisplay := GetProc(libglut, 'glutPostWindowOverlayRedisplay');
  glutShowOverlay := GetProc(libglut, 'glutShowOverlay');
  glutHideOverlay := GetProc(libglut, 'glutHideOverlay');
  glutCreateMenu := GetProc(libglut, 'glutCreateMenu');
  glutDestroyMenu := GetProc(libglut, 'glutDestroyMenu');
  glutGetMenu := GetProc(libglut, 'glutGetMenu');
  glutSetMenu := GetProc(libglut, 'glutSetMenu');
  glutAddMenuEntry := GetProc(libglut, 'glutAddMenuEntry');
  glutAddSubMenu := GetProc(libglut, 'glutAddSubMenu');
  glutChangeToMenuEntry := GetProc(libglut, 'glutChangeToMenuEntry');
  glutChangeToSubMenu := GetProc(libglut, 'glutChangeToSubMenu');
  glutRemoveMenuItem := GetProc(libglut, 'glutRemoveMenuItem');
  glutAttachMenu := GetProc(libglut, 'glutAttachMenu');
  glutDetachMenu := GetProc(libglut, 'glutDetachMenu');
  glutDisplayFunc := GetProc(libglut, 'glutDisplayFunc');
  glutReshapeFunc := GetProc(libglut, 'glutReshapeFunc');
  glutKeyboardFunc := GetProc(libglut, 'glutKeyboardFunc');
  glutMouseFunc := GetProc(libglut, 'glutMouseFunc');
  glutMotionFunc := GetProc(libglut, 'glutMotionFunc');
  glutPassiveMotionFunc := GetProc(libglut, 'glutPassiveMotionFunc');
  glutEntryFunc := GetProc(libglut, 'glutEntryFunc');
  glutIdleFunc := GetProc(libglut, 'glutIdleFunc');
  glutTimerFunc := GetProc(libglut, 'glutTimerFunc');
  glutMenuStateFunc := GetProc(libglut, 'glutMenuStateFunc');
  glutSpecialFunc := GetProc(libglut, 'glutSpecialFunc');
  glutSpaceballMotionFunc := GetProc(libglut, 'glutSpaceballMotionFunc');
  glutSpaceballRotateFunc := GetProc(libglut, 'glutSpaceballRotateFunc');
  glutSpaceballButtonFunc := GetProc(libglut, 'glutSpaceballButtonFunc');
  glutButtonBoxFunc := GetProc(libglut, 'glutButtonBoxFunc');
  glutDialsFunc := GetProc(libglut, 'glutDialsFunc');
  glutTabletMotionFunc := GetProc(libglut, 'glutTabletMotionFunc');
  glutTabletButtonFunc := GetProc(libglut, 'glutTabletButtonFunc');
  glutMenuStatusFunc := GetProc(libglut, 'glutMenuStatusFunc');
  glutOverlayDisplayFunc := GetProc(libglut, 'glutOverlayDisplayFunc');
  glutWindowStatusFunc := GetProc(libglut, 'glutWindowStatusFunc');
  glutKeyboardUpFunc := GetProc(libglut, 'glutKeyboardUpFunc');
  glutSpecialUpFunc := GetProc(libglut, 'glutSpecialUpFunc');
  glutJoystickFunc := GetProc(libglut, 'glutJoystickFunc');
  glutSetColor := GetProc(libglut, 'glutSetColor');
  glutGetColor := GetProc(libglut, 'glutGetColor');
  glutCopyColormap := GetProc(libglut, 'glutCopyColormap');
  glutGet := GetProc(libglut, 'glutGet');
  glutDeviceGet := GetProc(libglut, 'glutDeviceGet');
  glutExtensionSupported := GetProc(libglut, 'glutExtensionSupported');
  glutGetModifiers := GetProc(libglut, 'glutGetModifiers');
  glutLayerGet := GetProc(libglut, 'glutLayerGet');
  glutBitmapCharacter := GetProc(libglut, 'glutBitmapCharacter');
  glutBitmapWidth := GetProc(libglut, 'glutBitmapWidth');
  glutStrokeCharacter := GetProc(libglut, 'glutStrokeCharacter');
  glutStrokeWidth := GetProc(libglut, 'glutStrokeWidth');
  glutBitmapLength := GetProc(libglut, 'glutBitmapLength');
  glutStrokeLength := GetProc(libglut, 'glutStrokeLength');
  glutWireSphere := GetProc(libglut, 'glutWireSphere');
  glutSolidSphere := GetProc(libglut, 'glutSolidSphere');
  glutWireCone := GetProc(libglut, 'glutWireCone');
  glutSolidCone := GetProc(libglut, 'glutSolidCone');
  glutWireCube := GetProc(libglut, 'glutWireCube');
  glutSolidCube := GetProc(libglut, 'glutSolidCube');
  glutWireTorus := GetProc(libglut, 'glutWireTorus');
  glutSolidTorus := GetProc(libglut, 'glutSolidTorus');
  glutWireDodecahedron := GetProc(libglut, 'glutWireDodecahedron');
  glutSolidDodecahedron := GetProc(libglut, 'glutSolidDodecahedron');
  glutWireTeapot := GetProc(libglut, 'glutWireTeapot');
  glutSolidTeapot := GetProc(libglut, 'glutSolidTeapot');
  glutWireOctahedron := GetProc(libglut, 'glutWireOctahedron');
  glutSolidOctahedron := GetProc(libglut, 'glutSolidOctahedron');
  glutWireTetrahedron := GetProc(libglut, 'glutWireTetrahedron');
  glutSolidTetrahedron := GetProc(libglut, 'glutSolidTetrahedron');
  glutWireIcosahedron := GetProc(libglut, 'glutWireIcosahedron');
  glutSolidIcosahedron := GetProc(libglut, 'glutSolidIcosahedron');
  glutVideoResizeGet := GetProc(libglut, 'glutVideoResizeGet');
  glutSetupVideoResizing := GetProc(libglut, 'glutSetupVideoResizing');
  glutStopVideoResizing := GetProc(libglut, 'glutStopVideoResizing');
  glutVideoResize := GetProc(libglut, 'glutVideoResize');
  glutVideoPan := GetProc(libglut, 'glutVideoPan');
  glutReportErrors := GetProc(libglut, 'glutReportErrors');
  glutIgnoreKeyRepeat := GetProc(libglut, 'glutIgnoreKeyRepeat');
  glutSetKeyRepeat := GetProc(libglut, 'glutSetKeyRepeat');
  glutForceJoystickFunc := GetProc(libglut, 'glutForceJoystickFunc');
  glutGameModeString := GetProc(libglut, 'glutGameModeString');
  glutEnterGameMode := GetProc(libglut, 'glutEnterGameMode');
  glutLeaveGameMode := GetProc(libglut, 'glutLeaveGameMode');
  glutGameModeGet := GetProc(libglut, 'glutGameModeGet');

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


{
  $Log$
  Revision 1.2  2000-07-13 11:33:29  michael
  + removed logs
 
}
