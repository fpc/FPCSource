{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for BeOS

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
  System.SysUtils,
  System.RtlConsts,
  System.Types,  
  System.TypInfo,
  System.SortBase;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  rtlconsts,
  types,  
  typinfo,
  sortbase;
{$ENDIF FPC_DOTTEDUNITS}

{$i classesh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  UnixApi.Base,UnixApi.Unix;
{$ELSE FPC_DOTTEDUNITS}
uses
  baseunix,unix;
{$ENDIF FPC_DOTTEDUNITS}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;

finalization
  CommonCleanup;

{$ifndef ver1_0}
  if ThreadsInited then
     DoneThreads;
{$endif}
end.
