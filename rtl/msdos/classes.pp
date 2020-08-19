{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{$define Win16Res}

{$if defined(FPC_MM_TINY) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
  { CodePointer = Pointer; nothing to define }
{$elseif defined(FPC_MM_SMALL) or defined(FPC_MM_MEDIUM) or defined(FPC_MM_COMPACT)}
  {$define FPC_CODEPOINTER_DIFFERENT_THAN_POINTER}
{$else}
  {$fatal Unknown i8086 memory model.}
{$endif}
{$hugecode on}

unit Classes;

interface

uses
  typinfo,
  rtlconsts,
  types,
{$if defined(FPC_TESTGENERICS) or defined(FPC_CODEPOINTER_DIFFERENT_THAN_POINTER)}
  fgl,
{$endif}
  sysutils;

{$i classesh.inc}

implementation

type
{$ifdef FPC_CODEPOINTER_DIFFERENT_THAN_POINTER}
  TCodePtrList = specialize TFPGList<CodePointer>;
{$else FPC_CODEPOINTER_DIFFERENT_THAN_POINTER}
  TCodePtrList = TList;
{$endif FPC_CODEPOINTER_DIFFERENT_THAN_POINTER}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
