{
  $Id$
  Translation of the Mesa GL, GLU and GLX headers for Free Pascal
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

{$MODE delphi}  // objfpc would not work because of direct proc var assignments

unit GL;

interface
uses XLib;

// ===================================================================
//   Unit specific extensions
// ===================================================================

function InitGLFromLibrary(libname: PChar): Boolean;
function InitGLUFromLibrary(libname: PChar): Boolean;
// Requires that the GL library has already been initialized:
function InitGLX: Boolean;

// determines automatically which libraries to use:
function InitGL: Boolean;
function InitGLU: Boolean;


var
  GLInitialized, GLUInitialized, GLXInitialized: Boolean;

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

function LoadLibrary(name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

function GetProc(handle: Pointer; name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if Result = nil then WriteLn('Unresolved: ', name);
end;

var
  libGL, libGLU, libGLX: Pointer;

function InitGLFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if not Assigned(libGL) then exit;

%GLProcs2
#  // Extensions:
#%GLExtProcs2

  GLInitialized := True;
  Result := True;
end;

function InitGLUFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGLU := LoadLibrary(libname);
  if not Assigned(libGLU) then exit;

%GLUProcs2

  GLUInitialized := True;
  Result := True;
end;

function InitGLX: Boolean;
begin
  Result := False;
  if not Assigned(libGL) then exit;

%GLXProcs2

  GLXInitialized := True;
  Result := True;
end;

function InitGL: Boolean;
begin
  Result := InitGLFromLibrary('libGL.so') or InitGLFromLibrary('libMesaGL.so');
end;

function InitGLU: Boolean;
begin
  Result := InitGLUFromLibrary('libGLU.so') or InitGLUFromLibrary('libMesaGLU.so');
end;



finalization
  if Assigned(libGL)  then dlclose(libGL);
  if Assigned(libGLU) then dlclose(libGLU);
  if Assigned(libGLX) then dlclose(libGLX);
end.
