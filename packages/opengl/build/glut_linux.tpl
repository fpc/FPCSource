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

%GLUTDecls

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

%GLUTProcs1


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
