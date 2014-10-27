{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999 by Berczi Gabor

    Conditional defines logic for the IDE

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef USE_GDBLIBINC}
  {$include gdblib.inc}
{$endif USE_GDBLIBINC}

{ --- Special OS settings --- }
{$ifdef Go32V2}
  { NOVESA conditional avoids use of
    vesa unit, it also disables USE_GRAPH_SWITCH
    conditional, in order to obtain a go32v2 excutable that
    doesn't require full screen switching
    at startup on Windows OS }
  {$ifndef NOVESA}
    {$define SUPPORTVESA}
  {$endif ndef NOVESA}
  {$define SUPPORTREDIR}
  {$define WinClipSupported}
  {$define HasSignal}
  {$define SignalIsFunction}
  {$define SignalIsCdecl}
  {$define FSCaseInsensitive}
  {$define HasSysMsgUnit}
  {$ifdef DEBUG}
    {$ifndef NOVESA}
      {$define USE_GRAPH_SWITCH}
    {$endif not NOVESA}
  {$endif DEBUG}
{$endif}

{$ifdef Linux}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$undef WinClipSupported}
  {$define HasSignal}
  {$define SignalIsCdecl}
  {$define HasSysMsgUnit}
{$endif}


{$ifdef FreeBSD}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$undef WinClipSupported}
  {$define HasSignal}
  {$define SignalIsCdecl}
  {$define HasSysMsgUnit}
{$endif}

{$ifdef NetBSD}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$undef WinClipSupported}
  {$define HasSignal}
  {$define SignalIsCdecl}
  {$define HasSysMsgUnit}
{$endif}

{$ifdef OpenBSD}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$undef WinClipSupported}
  {$define HasSignal}
  {$define SignalIsCdecl}
  {$define HasSysMsgUnit}
{$endif}

{$ifdef Windows}
  {$undef SUPPORTVESA}
  {$define SUPPORTREDIR}
  {$define WinClipSupported}
  {$ifdef win32}
    {$define HasSignal}
    {$define SignalIsFunction}
    {$define SignalIsCdecl}
  {$endif}
  {$ifdef win64}
    {$define HasSignal}
    {$define SignalIsFunction}
    {$define SignalIsCdecl}
  {$endif}
  {$define FSCaseInsensitive}
  {$define HasSysMsgUnit}
{$endif}

{$ifdef OS2}
  {define SUPPORTREDIR}
  {define WinClipSupported}
  {define HasSignal}
  {$define FSCaseInsensitive}
{$endif}

{$ifdef Amiga}
  {$define FSCaseInsensitive}
{$endif}

{$ifdef MorphOS}
  {$define FSCaseInsensitive}
{$endif}

{$ifdef AROS}
  {$define FSCaseInsensitive}
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

{ ----------- define DOS for DOS targets ---------- }
{$ifdef GO32V2}{$define DOS}{$endif}

{ include Undo/Redo code from Visa Harvey }
{ let everybody try it out  PM }
{ undo should be a bit improved - it does work only with "normal" keystrokes.
  neither the block, nor any shortcut operations (like Ctrl-T - delete word)
  do work... Gabor
  Partially solved at least PM }
{$define Undo}
{$ifdef DEBUG}
  {$define DebugUndo}
  {$define TEST_REGEXP}
  { Use old regexpr unit by Joost }
  {$define USE_OLD_REGEXP}
  { Use this to incorporate a call to
    external compiler.
    Parsing of compiler output is done,
    but there is no browser in that case!! PM }
  { $ define USE_EXTERNAL_COMPILER}
  {$define EXEDEBUG}
{$endif DEBUG}

{$ifndef ver2_0}
  {$define USERESSTRINGS}
{$endif}
{$define USE_FREEVISION}
{$define HASOUTLINE}

{ Use inlining for small functions }
  {$inline on}
  {.$define USEINLINE}

{$define TEST_PARTIAL_SYNTAX}
{ $ undef UNDO}
{ $ undef DEBUGUNDO}

{$define BROWSERCOL}

{$ifdef SUPPORTVESA}
  {$define VESA}
{$endif}

{$ifdef NOWINCLIP}
  {$undef WINCLIPSUPPORTED}
{$endif}

{$ifdef CROSSGDB}
  {$define SUPPORT_REMOTE}
{$endif CROSSGDB}

{$ifdef FPC_ARMEL}
  {$define FPC_ARMEL32}
{$endif FPC_ARMEL}
{$ifdef FPC_ARMEB}
  {$define FPC_ARMEB32}
{$endif FPC_ARMEB}
{$ifdef FPC_OARM}
  {$define FPC_ARMEL32}
{$endif FPC_OARM}
{$ifdef FPC_ARMHF}
  {$define FPC_ARMEL32}
{$endif FPC_ARMHF}
