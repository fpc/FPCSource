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
  TinyGLBase: Pointer;
  tglContext: Pointer;

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

var
  tinygl_exit : Pointer;

procedure CloseTinyGLLibrary;
begin
  ExitProc := tinygl_exit;
  if TinyGLBase <> nil then begin
    if tglContext <> nil then begin
      _GLClose(tglContext);
      tglContext := nil;
    end;
    CloseLibrary(PLibrary(TinyGLBase));
    TinyGLBase := nil;
  end;
end;

function InitTinyGLLibrary : boolean;
begin
  TinyGLBase := nil;
  TinyGLBase := OpenLibrary(TINYGLNAME,LIBVERSION);
  if TinyGLBase <> nil then begin
    tinygl_exit := ExitProc;
    ExitProc := @CloseTinyGLLibrary;
    tglContext := _GLInit;
    InitTinyGLLibrary := True;
  end else begin
    InitTinyGLLibrary := False;
  end;
end;

end.
