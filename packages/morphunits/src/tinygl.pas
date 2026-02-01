{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005-2026 by Karoly Balogh

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
{$IFNDEF FPC_DOTTEDUNITS}
unit tinygl;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Amiga.Core.Exec;
{$ELSE FPC_DOTTEDUNITS}
uses
  exec;
{$ENDIF FPC_DOTTEDUNITS}

const
  TINYGLNAME : PAnsiChar = 'tinygl.library';

type
  PGLContext = Pointer;
  PPGLContext = ^PGLContext;

var
  TinyGLBase: Pointer = nil;
  tglContext: PGLContext = nil;

const
  {* with these you can specify type of context (using window, screen or bitmap.
     You have to only specify one of them. It more is used then here is the order
     they are checked: SCREEN, WINDOW, BITMAP *}
  TGL_CONTEXT_SCREEN = TAG_USER + 1;   {* pass here screen pointer the context will use *}
  TGL_CONTEXT_WINDOW = TAG_USER + 2;   {* same for window *}
  TGL_CONTEXT_BITMAP = TAG_USER + 3;   {* same for bitmap *}
  TGL_CONTEXT_STENCIL = TAG_USER + 4;  {* requests stencil. It may fail to create one though. check if it's present later *}
  TGL_CONTEXT_NODEPTH = TAG_USER + 5;  {* don't allocate depth buffer. *}

{* Context types *}
const
  GLA_CTYPE_UNKNOWN = 0;
  GLA_CTYPE_WINDOW = 1;
  GLA_CTYPE_SCREEN = 2;
  GLA_CTYPE_BITMAP = 3;

{* GLA Extensions *}
const
  GLA_EXTERNAL_READBUFFER_BITMAP = 1;


function TGLGetContexts: Pointer;
syscall sysvbase TinyGLBase 634;

function GLInit: PGLContext;
syscall sysvbase TinyGLBase 640;

procedure GLClose(gcl: pointer);
syscall sysvbase TinyGLBase 646;

function GLAInitializeContextWindowed(context: PGLContext; w: Pointer): longint;
syscall sysvbase TinyGLBase 838;

procedure GLADestroyContextWindowed(context: PGLContext);
syscall sysvbase TinyGLBase 844;

procedure GLASwapBuffers(c: PGLContext);
syscall sysvbase TinyGLBase 850;

function GLAInitializeContextScreen(context: PGLContext; s: Pointer): longint;
syscall sysvbase TinyGLBase 1060;

procedure GLADestroyContextScreen(context: PGLContext);
syscall sysvbase TinyGLBase 1066;

function GLAInitializeContextBitMap(context: PGLContext; b: Pointer): longint;
syscall sysvbase TinyGLBase 1072;

procedure GLADestroyContextBitMap(context: PGLContext);
syscall sysvbase TinyGLBase 1078;

procedure GLASetSync(context: PGLContext; enable: longint);
syscall sysvbase TinyGLBase 1132;

function GLAReinitializeContextWindowed(context: PGLContext; w: Pointer): longint;
syscall sysvbase TinyGLBase 1186;

procedure GLADestroyContext(context: PGLContext);
syscall sysvbase TinyGLBase 1234;

function GLAInitializeContext(context: PGLContext; tags: PTagItem): longint;
syscall sysvbase TinyGLBase 1240;

procedure GLASetAttr(context: PGLContext; attr: dword; value: dword);
syscall sysvbase TinyGLBase 1414;

procedure TGLEnableNewExtensions(context: PGLContext; value: DWord);
syscall sysvbase TinyGLBase 4432;

function TGLGetProcAddress(tglcontextpointer: PPGLContext; Name: PChar): Pointer;
syscall sysvbase TinyGLBase 4438;

procedure TGLSetContextVersion(context: PGLContext; contextversion: DWord);
syscall sysvbase TinyGLBase 4498;

function TGLGetContextVersion(GLContext: PGLContext): DWord;
syscall sysvbase TinyGLBase 4504;

function InitTinyGLLibrary : boolean;

implementation

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
    tglContext := GLInit;
finalization
  if Assigned(tglContext) then
    GLClose(tglContext);
  if Assigned(TinyGLBase) then
    CloseLibrary(PLibrary(TinyGLBase));
end.
