{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998-2006 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for winx64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{ $define Win16Res}

unit Classes;

interface

uses
  rtlconsts,
  sysutils,
  types,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  typinfo,
  windows;

type
  TWndMethod = procedure(var msg : TMessage) of object;

function MakeObjectInstance(Method: TWndMethod): Pointer;
procedure FreeObjectInstance(ObjectInstance: Pointer);

function AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

{$i classesh.inc}

implementation

uses
  sysconst;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

function MakeObjectInstance(Method: TWndMethod): Pointer;
  begin
    runerror(211);
  end;


procedure FreeObjectInstance(ObjectInstance: Pointer);
  begin
    runerror(211);
  end;


function AllocateHWnd(Method: TWndMethod): HWND;
  begin
    runerror(211);
  end;


procedure DeallocateHWnd(Wnd: HWND);
  begin
    runerror(211);
  end;


initialization
  CommonInit;

finalization
  CommonCleanup;
end.
