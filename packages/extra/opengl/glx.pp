{

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

{$IFDEF Unix}
  uses
    X, XLib, XUtil;
  {$DEFINE HasGLX}  // Activate GLX stuff
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
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; attribList: PInteger): PXVisualInfo; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl;
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl;
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  glXWaitGL: procedure; cdecl;
  glXWaitX: procedure; cdecl;
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl;

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl;

  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl;
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl;


// =======================================================
//
// =======================================================

implementation

{$LINKLIB m}

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
  if (Result = nil) and GLXDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGLX: Pointer;

function InitGLXFromLibrary(libname:pchar): Boolean;
begin
  Result := False;
  libGLX := LoadLibrary(libname);
  if not Assigned(libGLX) then exit;

  glXChooseVisual := GetProc(libglx, 'glXChooseVisual');
  glXCreateContext := GetProc(libglx, 'glXCreateContext');
  glXDestroyContext := GetProc(libglx, 'glXDestroyContext');
  glXMakeCurrent := GetProc(libglx, 'glXMakeCurrent');
  glXCopyContext := GetProc(libglx, 'glXCopyContext');
  glXSwapBuffers := GetProc(libglx, 'glXSwapBuffers');
  glXCreateGLXPixmap := GetProc(libglx, 'glXCreateGLXPixmap');
  glXDestroyGLXPixmap := GetProc(libglx, 'glXDestroyGLXPixmap');
  glXQueryExtension := GetProc(libglx, 'glXQueryExtension');
  glXQueryVersion := GetProc(libglx, 'glXQueryVersion');
  glXIsDirect := GetProc(libglx, 'glXIsDirect');
  glXGetConfig := GetProc(libglx, 'glXGetConfig');
  glXGetCurrentContext := GetProc(libglx, 'glXGetCurrentContext');
  glXGetCurrentDrawable := GetProc(libglx, 'glXGetCurrentDrawable');
  glXWaitGL := GetProc(libglx, 'glXWaitGL');
  glXWaitX := GetProc(libglx, 'glXWaitX');
  glXUseXFont := GetProc(libglx, 'glXUseXFont');
  // GLX 1.1 and later
  glXQueryExtensionsString := GetProc(libglx, 'glXQueryExtensionsString');
  glXQueryServerString := GetProc(libglx, 'glXQueryServerString');
  glXGetClientString := GetProc(libglx, 'glXGetClientString');
  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA := GetProc(libglx, 'glXCreateGLXPixmapMESA');
  glXReleaseBufferMESA := GetProc(libglx, 'glXReleaseBufferMESA');
  glXCopySubBufferMESA := GetProc(libglx, 'glXCopySubBufferMESA');
  glXGetVideoSyncSGI := GetProc(libglx, 'glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetProc(libglx, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

function InitGLX: Boolean;
begin
  Result := InitGLXFromLibrary('libGL.so') or
            InitGLXFromLibrary('libGL.so.1') or
            InitGLXFromLibrary('libMesaGL.so') or
            InitGLXFromLibrary('libMesaGL.so.3');
end;


initialization
  InitGLX;
finalization
  if Assigned(libGLX) then dlclose(libGLX);
end.
