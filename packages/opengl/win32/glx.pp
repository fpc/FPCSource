{
  $Id$

  Translation of the Mesa GLX headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther


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

{$MODE delphi}  // objfpc would not work because of direct proc var assignments

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

unit GLX;

interface

{$MACRO ON}

{$IFDEF Win32}
  {$DEFINE glx_dll := external 'unknown.dll'}
  uses Windows;
  {x$DEFINE HasGLX}  // Activate GLX stuff
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}

{$IFNDEF HasGLX}
  {$MESSAGE GLX not present on this platform.}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

// Note: Requires that the GL library has already been initialized
function InitGLX: Boolean;

var
  GLXDumpUnresolvedFunctions,
  GLXInitialized: Boolean;


// =======================================================
//   GLX consts, types and functions
// =======================================================

// Tokens for glXChooseVisual and glXGetConfig:
const
  GLX_USE_GL                            = 1;
  GLX_BUFFER_SIZE                       = 2;
  GLX_LEVEL                             = 3;
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_STEREO                            = 6;
  GLX_AUX_BUFFERS                       = 7;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_ACCUM_RED_SIZE                    = 14;
  GLX_ACCUM_GREEN_SIZE                  = 15;
  GLX_ACCUM_BLUE_SIZE                   = 16;
  GLX_ACCUM_ALPHA_SIZE                  = 17;

  // GLX_EXT_visual_info extension
  GLX_X_VISUAL_TYPE_EXT                 = $22;
  GLX_TRANSPARENT_TYPE_EXT              = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT       = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT         = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT       = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT        = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT       = $28;


  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                        = 1;
  GLX_BAD_ATTRIBUTE                     = 2;
  GLX_NO_EXTENSION                      = 3;
  GLX_BAD_VISUAL                        = 4;
  GLX_BAD_CONTEXT                       = 5;
  GLX_BAD_VALUE                         = 6;
  GLX_BAD_ENUM                          = 7;

  // GLX 1.1 and later:
  GLX_VENDOR                            = 1;
  GLX_VERSION                           = 2;
  GLX_EXTENSIONS                        = 3;

  // GLX_visual_info extension
  GLX_TRUE_COLOR_EXT                    = $8002;
  GLX_DIRECT_COLOR_EXT                  = $8003;
  GLX_PSEUDO_COLOR_EXT                  = $8004;
  GLX_STATIC_COLOR_EXT                  = $8005;
  GLX_GRAY_SCALE_EXT                    = $8006;
  GLX_STATIC_GRAY_EXT                   = $8007;
  GLX_NONE_EXT                          = $8000;
  GLX_TRANSPARENT_RGB_EXT               = $8008;
  GLX_TRANSPARENT_INDEX_EXT             = $8009;

type
  // From XLib:
  XPixmap = TXID;
  XFont = TXID;
  XColormap = TXID;

  GLXContext = Pointer;
  GLXPixmap = TXID;
  GLXDrawable = TXID;
  GLXContextID = TXID;

var
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; var attribList: Integer): PXVisualInfo; cdecl; glx_dll
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl; glx_dll
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl; glx_dll
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl; glx_dll
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl; glx_dll
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl; glx_dll
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl; glx_dll
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl; glx_dll
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl; glx_dll
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl; glx_dll
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl; glx_dll
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl; glx_dll
  glXGetCurrentContext: function: GLXContext; cdecl; glx_dll
  glXGetCurrentDrawable: function: GLXDrawable; cdecl; glx_dll
  glXWaitGL: procedure; cdecl; glx_dll
  glXWaitX: procedure; cdecl; glx_dll
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl; glx_dll

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl; glx_dll
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl; glx_dll
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl; glx_dll

  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl; glx_dll
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl; glx_dll
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl; glx_dll
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl; glx_dll
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl; glx_dll


// =======================================================
//
// =======================================================

implementation

type
  HInstance = LongWord;

{$IFDEF HasGLX}

var
  libGLX : HInstance;

function GetProc(handle: HInstance; name: PChar): Pointer;
begin
  Result := GetProcAddress(handle, name);
  if (Result = nil) and GLXDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

function InitGLX: Boolean;
begin
  Result := False;
  { Unix GLX is implemented as special subset of the GL interface }
  if libGL = 0 then exit;

  glXQueryVersion := GetProcAddress(libGL, 'glXQueryVersion');
  if @glXQueryVersion = nil then exit;

  glXChooseVisual := GetProc(libGLX, 'glXChooseVisual');
  glXCreateContext := GetProc(libGLX, 'glXCreateContext');
  glXDestroyContext := GetProc(libGLX, 'glXDestroyContext');
  glXMakeCurrent := GetProc(libGLX, 'glXMakeCurrent');
  glXCopyContext := GetProc(libGLX, 'glXCopyContext');
  glXSwapBuffers := GetProc(libGLX, 'glXSwapBuffers');
  glXCreateGLXPixmap := GetProc(libGLX, 'glXCreateGLXPixmap');
  glXDestroyGLXPixmap := GetProc(libGLX, 'glXDestroyGLXPixmap');
  glXQueryExtension := GetProc(libGLX, 'glXQueryExtension');
  glXQueryVersion := GetProc(libGLX, 'glXQueryVersion');
  glXIsDirect := GetProc(libGLX, 'glXIsDirect');
  glXGetConfig := GetProc(libGLX, 'glXGetConfig');
  glXGetCurrentContext := GetProc(libGLX, 'glXGetCurrentContext');
  glXGetCurrentDrawable := GetProc(libGLX, 'glXGetCurrentDrawable');
  glXWaitGL := GetProc(libGLX, 'glXWaitGL');
  glXWaitX := GetProc(libGLX, 'glXWaitX');
  glXUseXFont := GetProc(libGLX, 'glXUseXFont');
  // GLX 1.1 and later
  glXQueryExtensionsString := GetProc(libGLX, 'glXQueryExtensionsString');
  glXQueryServerString := GetProc(libGLX, 'glXQueryServerString');
  glXGetClientString := GetProc(libGLX, 'glXGetClientString');
  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA := GetProc(libGLX, 'glXCreateGLXPixmapMESA');
  glXReleaseBufferMESA := GetProc(libGLX, 'glXReleaseBufferMESA');
  glXCopySubBufferMESA := GetProc(libGLX, 'glXCopySubBufferMESA');
  glXGetVideoSyncSGI := GetProc(libGLX, 'glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetProc(libGLX, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

{$ENDIF  IFDEF HasGLX}


initialization
  InitGLX;
finalization
  if libGLX <> 0 then FreeLibrary(libGLX);
end.


{
  $Log$
  Revision 1.2  2000-09-03 22:17:18  peter
    * merged

  Revision 1.1.2.1  2000/09/03 22:14:41  peter
    * regenerated

  Revision 1.1  2000/07/13 06:34:18  michael
  + Initial import

  Revision 1.1  2000/05/31 00:36:08  alex
  dummy - incomplete code.

}


{
  $Log$
  Revision 1.2  2000-09-03 22:17:18  peter
    * merged

  Revision 1.1.2.1  2000/09/03 22:14:41  peter
    * regenerated

}
