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
{$endif}

{$ifdef Linux}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
{$endif}

{$ifdef Win32}
  {$undef SUPPORTVESA}
  {$undef SUPPORTREDIR}
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
{$endif}

{$ifdef GABOR}
  {.$define NOOBJREG}
  {$define NODEBUG}
{$endif}
