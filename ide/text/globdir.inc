{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999 by Berczi Gabor

    Conditional defines logic for the IDE

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ --- Special OS settings --- }
{$ifdef TP}
  {$define SUPPORTVESA}
  {$define TPUNIXLF}
  {$undef WinClipSupported}

  {$C FIXED PRELOAD PERMANENT}
  {
    Without defining this I got almost always SEGMENT NOT PRESENT (exc 11)
    on exiting the IDE, when run on under NT4.0... Strange a bit, not?
    (Actually the fault occours in TDOSScreen.GetVideoMode() at the
     BIOS call, but I just can't figure out why....)
  }
{$endif}

{$ifdef Go32V2}
  {$define SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$define WinClipSupported}
{$endif}

{$ifdef Linux}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$undef WinClipSupported}
{$endif}

{$ifdef Win32}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$define WinClipSupported}
{$endif}

{ --- Exclude debugger support --- }
{.$DEFINE NODEBUG}

{ --- Include VESA support --- }
{$ifdef SUPPORTVESA}
  {$ifndef FV20}
    {$define VESA}
  {$endif}
{$endif}

{$ifdef SUPPORTREDIR}
  {$ifndef debug}
    {$define redircompiler}
  {$endif}
  {$ifdef GDB_V418}
    {define redircompiler}
  {$endif GDB_V418}
{$endif}

{$ifdef GABOR}
  {.$define NOOBJREG}
  {$define NODEBUG}
  {$define DEBUG}
{$endif}

{ include Undo/Redo code from Visa Harvey }
{ let everybody try it out  PM }
{ undo should be a bit improved - it does work only with "normal" keystorkes.
  neither the block, nor any shortcut operations (like Ctrl-T - delete word)
  do work... Gabor
  Partially solved at least PM }
{$define Undo}
{$ifdef DEBUG}
  {$define DebugUndo}
{$endif DEBUG}
