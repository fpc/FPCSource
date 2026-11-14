{** EGL 1.4 headers, see the Khronos license in gles20.pas.
 **
 ** The same declarations are available through the gles20 unit.
 **}
{$IFNDEF FPC_DOTTEDUNITS}
unit Egl;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc}
{$define EGL}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,System.DynLibs
  {$ifdef Windows}
    ,WinApi.Windows
  {$endif}
  ;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils,dynlibs{$ifdef windows},Windows{$endif};
{$ENDIF FPC_DOTTEDUNITS}

{$PACKRECORDS C}

{$i eglh.inc}

// Load the EGL library lib and its entry points, unloading a previously loaded one.
procedure LoadEGL(lib : PAnsiChar);

// Unload the EGL library and clear all entry points.
procedure FreeEGL;

implementation

{$i egl.inc}


initialization
  EGLLib:=0;
  LoadEGL({$if defined(windows)}'libEGL.dll'{$elseif defined(android)}'libEGL.so'{$else}'libEGL.so.1'{$endif});
finalization
  FreeEGL;
end.
