{
  $Id$

  Translation of the GLUT headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther
  Version for static linking in Win32 environment by Alexander Stohr.
  Latest change: 1999-11-13

  Further information:

  GLUT is a powerful toolkit for programming multiplatform OpenGL
  applications. It was designed by Mark J. Kilgard while working for SGI,
  he is now working for nVidia.
}
{ this translation of the c header files is done by Sebastian Guenther 1999 }
{ version for static linking for Win32 platforms done by Alexander Stohr 1999 }

{$MODE delphi}

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

unit GLUT_SL; { version which does statically linking }


interface

uses GL_SL;

{x$DEFINE GLUT_GAME} {enable if you need game mode sub api}

{$MACRO ON}

{$IFDEF Win32}
  {$DEFINE glut_dll := external 'glut32.dll'}
  {$DEFINE glut_callback := cdecl}
  {$DEFINE extdecl := }
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}


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

// GLUT initialization sub-API
procedure glutInit(var argcp: Integer; var argv: PChar); glut_dll;
procedure glutInitDisplayMode(mode: LongWord); glut_dll;
procedure glutInitDisplayString(AString: PChar); glut_dll;
procedure glutInitWindowPosition(x, y: Integer); glut_dll;
procedure glutInitWindowSize(width, height: Integer); glut_dll;
procedure glutMainLoop; glut_dll;

// GLUT window sub-API
function glutCreateWindow(title: PChar): Integer; glut_dll;
function glutCreateSubWindow(win, x, y, width, height: Integer): Integer; glut_dll;
procedure glutDestroyWindow(win: Integer); glut_dll;
procedure glutPostRedisplay; glut_dll;
procedure glutPostWindowRedisplay(win: Integer); glut_dll;
procedure glutSwapBuffers; glut_dll;
function glutGetWindow: Integer; glut_dll;
procedure glutSetWindow(win: Integer); glut_dll;
procedure glutSetWindowTitle(title: PChar); glut_dll;
procedure glutSetIconTitle(title: PChar); glut_dll;
procedure glutPositionWindow(x, y: Integer); glut_dll;
procedure glutReshapeWindow(width, height: Integer); glut_dll;
procedure glutPopWindow; glut_dll;
procedure glutPushWindow; glut_dll;
procedure glutIconifyWindow; glut_dll;
procedure glutShowWindow; glut_dll;
procedure glutHideWindow; glut_dll;
procedure glutFullScreen; glut_dll;
procedure glutSetCursor(cursor: Integer); glut_dll;
procedure glutWarpPointer(x, y: Integer); glut_dll;

//overlays ###

//menus ###

// GLUT window callback sub-API
procedure glutDisplayFunc(func: TGlutDisplayFunc); glut_dll;
procedure glutReshapeFunc(func: TGlutReshapeFunc); glut_dll;

procedure glutTimerFunc(millis: LongWord; func: TGlutTimerFunc; value: longint); glut_dll;
procedure glutKeyboardFunc(func: TGlutKeyboardFunc); glut_dll;
procedure glutIdleFunc(func: TGlutIdleFunc); glut_dll;


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
procedure glutSetColor(index: Integer; red, green, blue: Single); glut_dll;
function glutGetColor(ndx, component: Integer): Single; glut_dll;
procedure glutCopyColormap(win: Integer); glut_dll;

// GLUT state retrieval sub-API
function glutGet(AType: GLEnum): Integer; glut_dll;
function glutDeviceGet(AType: GLEnum): Integer; glut_dll;
function glutExtensionSupported(name: PChar): Integer; glut_dll;
function glutGetModifiers: Integer; glut_dll;
function glutLayerGet(AType: GLEnum): Integer; glut_dll;

// fonts ###

// pre-built models ###

// video resize ###

// debugging ###

// device control ###


// GLUT game mode sub-API
 {$ifdef GLUT_GAME} glut_dll;
procedure glutGameModeString(AString: PChar); glut_dll;
function glutEnterGameMode: Integer; glut_dll;
procedure glutLeaveGameMode; glut_dll;
function glutGameModeGet(mode: GLEnum): Integer; glut_dll;
 {$endif GLUT_GAME} glut_dll;


implementation


{begin{of init}
end.


{
  $Log$
  Revision 1.1  2000-09-03 21:25:46  peter
    * new updated version
    * gtkglarea unit and demo
    * win32 opengl headers
    * morph3d demo

}
