{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 by the Free Pascal development team

    Classes unit for Atari

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$IF FPC_FULLVERSION>=30301}
{$modeswitch FUNCTIONREFERENCES}
{$define FPC_HAS_REFERENCE_PROCEDURE}
{$endif}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  sysutils,
  rtlconsts,
  types,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  typinfo;

{$i classesh.inc}


implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;

finalization
  CommonCleanup;

end.
