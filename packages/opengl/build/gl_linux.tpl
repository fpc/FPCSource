{
  $Id$
  Translation of the Mesa GL, GLU and GLX headers for Free Pascal, Linux version
  Copyright (C) 1999-2000 Sebastian Guenther, sg@freepascal.org


  Mesa 3-D graphics library
  Version:  3.1

  Copyright (C) 1999  Brian Paul   All Rights Reserved.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  BRIAN PAUL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit GL;

{$MODE delphi}  // objfpc would not work because of direct proc var assignments

interface

uses X, XLib, XUtil;

// ===================================================================
//   Unit specific extensions
// ===================================================================

function InitGLFromLibrary(const libname: PChar): Boolean;
function InitGLUFromLibrary(const libname: PChar): Boolean;
// Requires that the GL library has already been initialized:
function InitGLX: Boolean;

// determines automatically which libraries to use:
function InitGL: Boolean;
function InitGLU: Boolean;


var
  GLInitialized, GLUInitialized, GLXInitialized: Boolean;

  { Set the following value to True if you want to have a list of all
    unresolved GL/GLU/GLX functions dumped to the console }
  GLDumpUnresolvedFunctions: Boolean;


%GLDecls

%GLProcs1

const
%GLExtDecls

%GLExtProcs1


%GLUDecls
%GLUProcs1


%GLXDecls
%GLXProcs1


// ===================================================================
// ===================================================================

implementation

{$LINKLIB m}

function dlopen(AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(const name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

function GetProc(handle: Pointer; const name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if not Assigned(Result) and GLDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGL, libGLU, libGLX: Pointer;

function InitGLFromLibrary(const libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if not Assigned(libGL) then
    exit;

%GLProcs2
#  // Extensions:
#%GLExtProcs2

  GLInitialized := True;
  Result := True;
end;

function InitGLUFromLibrary(const libname: PChar): Boolean;
begin
  Result := False;
  libGLU := LoadLibrary(libname);
  if not Assigned(libGLU) then
    exit;

%GLUProcs2

  GLUInitialized := True;
  Result := True;
end;

function InitGLX: Boolean;
begin
  Result := False;
  if not Assigned(libGL) then
    exit;

%GLXProcs2

  GLXInitialized := True;
  Result := True;
end;

function InitGL: Boolean;
begin
  Result := InitGLFromLibrary('libGL.so') or InitGLFromLibrary('libGL.so.1') or
    InitGLFromLibrary('libMesaGL.so.3');
end;

function InitGLU: Boolean;
begin
  Result := InitGLUFromLibrary('libGLU.so') or
    InitGLUFromLibrary('libGLU.so.1') or InitGLUFromLibrary('libMesaGLU.so.3');
end;


finalization
  // Free all loaded libraries
  if Assigned(libGLX) then
    dlclose(libGLX);
  if Assigned(libGLU) then
    dlclose(libGLU);
  if Assigned(libGL) then
    dlclose(libGL);
end.
