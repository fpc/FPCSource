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

unit GLX_SL;

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

function glXChooseVisual(dpy: PDisplay; screen: Integer; var attribList: Integer): PXVisualInfo; cdecl; glx_dll;
function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl; glx_dll;
procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; glx_dll;
function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl; glx_dll;
procedure glXCopyContext(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl; glx_dll;
procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; glx_dll;
function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl; glx_dll;
procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; glx_dll;
function glXQueryExtension(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl; glx_dll;
function glXQueryVersion(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl; glx_dll;
function glXIsDirect(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl; glx_dll;
function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl; glx_dll;
function glXGetCurrentContext: GLXContext; cdecl; glx_dll;
function glXGetCurrentDrawable: GLXDrawable; cdecl; glx_dll;
procedure glXWaitGL; cdecl; glx_dll;
procedure glXWaitX; cdecl; glx_dll;
procedure glXUseXFont(font: XFont; first, count, list: Integer); cdecl; glx_dll;

  // GLX 1.1 and later
function glXQueryExtensionsString(dpy: PDisplay; screen: Integer): PChar; cdecl; glx_dll;
function glXQueryServerString(dpy: PDisplay; screen, name: Integer): PChar; cdecl; glx_dll;
function glXGetClientString(dpy: PDisplay; name: Integer): PChar; cdecl; glx_dll;

  // Mesa GLX Extensions
function glXCreateGLXPixmapMESA(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl; glx_dll;
function glXReleaseBufferMESA(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl; glx_dll;
procedure glXCopySubBufferMESA(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl; glx_dll;
function glXGetVideoSyncSGI(var counter: LongWord): Integer; cdecl; glx_dll;
function glXWaitVideoSyncSGI(divisor, remainder: Integer; var count: LongWord): Integer; cdecl; glx_dll;


// =======================================================
//
// =======================================================

implementation

end.


{
  $Log$
  Revision 1.1.2.2  2000-10-01 22:12:28  peter
    * new demo

  Revision 1.1  2000/09/03 21:25:45  peter
    * new updated version
    * gtkglarea unit and demo
    * win32 opengl headers
    * morph3d demo

  Revision 1.1  2000/07/13 06:34:18  michael
  + Initial import

  Revision 1.1  2000/05/31 00:36:08  alex
  dummy - incomplete code.

}


{
  $Log$
  Revision 1.1.2.2  2000-10-01 22:12:28  peter
    * new demo

}
