{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2010 by Sven Barth

    Classes unit for NativeNT

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

{$INLINE ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.Types,
  System.TypInfo,
  System.SortBase,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
  System.RtlConsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  types,
  typinfo,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  rtlconsts;
{$ENDIF FPC_DOTTEDUNITS}

{$i classesh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  NTApi.NDK
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  NDK
  ;
{$ENDIF FPC_DOTTEDUNITS}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;
finalization
  CommonCleanup;
end.
