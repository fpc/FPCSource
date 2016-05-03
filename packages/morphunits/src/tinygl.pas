{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Karoly Balogh

    TinyGL/OpenGL initialization unit for MorphOS/PowerPC

    Thanks to Michal 'kiero' Wozniak and Mark 'bigfoot' Olsen
    for their help.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE FPC} { fsck Delphi mode }
{$INLINE ON}
unit tinygl;

interface

uses
  exec;

const
  TINYGLNAME : PChar = 'tinygl.library';

var
  TinyGLBase: Pointer = nil;
  tglContext: Pointer = nil;

function InitTinyGLLibrary : boolean;

implementation

function _GLInit: Pointer;
syscall sysvbase TinyGLBase 640;

procedure _GLClose(gcl: pointer);
syscall sysvbase TinyGLBase 646;

const
  { Change VERSION and LIBVERSION to proper values }
  VERSION : string[2] = '50';
  LIBVERSION : longword = 50;

function InitTinyGLLibrary : boolean;
begin
  InitTinyGLLibrary := Assigned(tglContext) and Assigned(TinyGLBase);
end;

initialization
  TinyGLBase := OpenLibrary(TINYGLNAME,LIBVERSION);
  if Assigned(TinyGLBase) then
    tglContext := _GLInit;
finalization
  if Assigned(tglContext) then
    _GLClose(tglContext);
  if Assigned(TinyGLBase) then
    CloseLibrary(PLibrary(TinyGLBase));
end.
