{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
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
{$INLINE ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.Types,
  System.TypInfo,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
  System.RtlConsts,
  System.SortBase;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  types,
  typinfo,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  rtlconsts,
  sortbase;
{$ENDIF FPC_DOTTEDUNITS}

{$i classesh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  UnixApi.Base,UnixApi.Unix
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  BaseUnix,unix
  ;
{$ENDIF FPC_DOTTEDUNITS}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;
finalization
  CommonCleanup;

  if ThreadsInited then
     DoneThreads;
end.
