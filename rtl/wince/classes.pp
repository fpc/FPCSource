{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for wince

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}
{$modeswitch advancedrecords}
{$IF FPC_FULLVERSION>=30301}
{$modeswitch FUNCTIONREFERENCES}
{$define FPC_HAS_REFERENCE_PROCEDURE}
{$endif}


{ determine the type of the resource/form file }
{$define Win16Res}

{$IFNDEF FPC_DOTTEDUNITS}
unit Classes;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.RtlConsts,
  System.SysUtils,
  System.Types,
  System.SortBase,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
  System.TypInfo,
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
  rtlconsts,
  sysutils,
  types,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  typinfo,
  windows;
{$ENDIF FPC_DOTTEDUNITS}

type
  TWndMethod = procedure(var msg : TMessage) of object;

{$i classesh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysConst;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysconst;
{$ENDIF FPC_DOTTEDUNITS}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;
end.
