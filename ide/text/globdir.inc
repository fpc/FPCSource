{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999 by Berczi Gabor

    Conditional defines logic for the IDE

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ --- Exclude debugger support --- }
{.$DEFINE NODEBUG}

{ --- Text lines end with LF only --- }
{$ifndef FPC}
  {$define TPUNIXLF}
{$endif}

{ --- Include VESA support --- }
{$ifndef LINUX}
  {$ifndef FV20}
    {$define VESA}
  {$endif}
{$endif}

{$ifdef TP}
  {$C FIXED PRELOAD PERMANENT}
  {
    Without defining this I got almost always SEGMENT NOT PRESENT (exc 11)
    on exiting the IDE, when run on under NT4.0... Strange a bit, not?
    (Actually the fault occours in TDOSScreen.GetVideoMode() at the
     BIOS call, but I just can't figure out why....)
  }
{$endif}

{$ifndef debug}
  {$ifndef linux}
    {$define redircompiler}
  {$endif}
{$endif}

{$ifdef GABOR}
  {$define NOOBJREG}
  {$define NODEBUG}
{$endif}
