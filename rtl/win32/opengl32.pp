{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Contains part of the OpenGL DLL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit opengl32;

  interface

    uses
       windows;

    function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

    function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
             _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

    function wglUseFontBitmapsA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

    function wglUseFontOutlinesA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
               _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

    function wglCreateContext(_para1:HDC):HGLRC;

    function wglCreateLayerContext(_para1:HDC; _para2:longint):HGLRC;

    function wglCopyContext(_para1:HGLRC; _para2:HGLRC; _para3:UINT):WINBOOL;

    function wglDeleteContext(_para1:HGLRC):WINBOOL;

    function wglDescribeLayerPlane(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; _para5:LPLAYERPLANEDESCRIPTOR):WINBOOL;

    function wglGetCurrentContext:HGLRC;

    function wglGetCurrentDC:HDC;

(* Const before type ignored *)
    function wglGetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint;

    function wglGetProcAddress(_para1:LPCSTR):PROC;

    function wglMakeCurrent(_para1:HDC; _para2:HGLRC):WINBOOL;

    function wglRealizeLayerPalette(_para1:HDC; _para2:longint; _para3:WINBOOL):WINBOOL;

(* Const before type ignored *)
    function wglSetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint;

    function wglShareLists(_para1:HGLRC; _para2:HGLRC):WINBOOL;

    function wglSwapLayerBuffers(_para1:HDC; _para2:UINT):WINBOOL;

    function wglUseFontBitmapsW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

    function wglUseFontOutlinesW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
               _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

  implementation

    function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsA';

    function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
             _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesA';

    function wglUseFontBitmapsA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsA';

    function wglUseFontOutlinesA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
               _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesA';

    function wglCreateContext(_para1:HDC):HGLRC; external 'opengl32' name 'wglCreateContext';

    function wglCreateLayerContext(_para1:HDC; _para2:longint):HGLRC; external 'opengl32' name 'wglCreateLayerContext';

    function wglCopyContext(_para1:HGLRC; _para2:HGLRC; _para3:UINT):WINBOOL; external 'opengl32' name 'wglCopyContext';

    function wglDeleteContext(_para1:HGLRC):WINBOOL; external 'opengl32' name 'wglDeleteContext';

    function wglDescribeLayerPlane(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; _para5:LPLAYERPLANEDESCRIPTOR):WINBOOL; external 'opengl32' name 'wglDescribeLayerPlane';

    function wglGetCurrentContext:HGLRC; external 'opengl32' name 'wglGetCurrentContext';

    function wglGetCurrentDC:HDC; external 'opengl32' name 'wglGetCurrentDC';

    function wglGetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint; external 'opengl32' name 'wglGetLayerPaletteEntries';

    function wglGetProcAddress(_para1:LPCSTR):PROC; external 'opengl32' name 'wglGetProcAddress';

    function wglMakeCurrent(_para1:HDC; _para2:HGLRC):WINBOOL; external 'opengl32' name 'wglMakeCurrent';

    function wglRealizeLayerPalette(_para1:HDC; _para2:longint; _para3:WINBOOL):WINBOOL; external 'opengl32' name 'wglRealizeLayerPalette';

    function wglSetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint; external 'opengl32' name 'wglSetLayerPaletteEntries';

    function wglShareLists(_para1:HGLRC; _para2:HGLRC):WINBOOL; external 'opengl32' name 'wglShareLists';

    function wglSwapLayerBuffers(_para1:HDC; _para2:UINT):WINBOOL; external 'opengl32' name 'wglSwapLayerBuffers';

    function wglUseFontBitmapsW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsW';

    function wglUseFontOutlinesW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
               _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesW';

end.
{
  $Log$
  Revision 1.4  2002-09-15 17:53:44  peter
    * Remove Float type, use Single instead

  Revision 1.3  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

}
