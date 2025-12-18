{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

    Classes unit for FreeRTOS target
    
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
{$ifndef CPULLVM}
{$if DEFINED(CPUARM) or DEFINED(CPUAARCH64)}
   {$define FPC_USE_INTRINSICS}
{$endif}
{$if defined(CPUPOWERPC) or defined(CPUPOWERPC64)}
   {$define FPC_USE_INTRINSICS}
{$endif}
{$if defined(CPURISCV32) or defined(CPURISCV64)}
   {$define FPC_USE_INTRINSICS}
{$endif}
{$endif}
{$endif}

{$IFNDEF FPC_DOTTEDUNITS}
unit Classes;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.RtlConsts,
  System.Types,
  System.SortBase,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
{$ifdef FPC_USE_INTRINSICS}
  System.Intrinsics,
{$endif}
  System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  rtlconsts,
  types,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
{$ifdef FPC_USE_INTRINSICS}
  intrinsics,
{$endif}
  typinfo;
{$ENDIF FPC_DOTTEDUNITS}

{ Also set FPC_USE_INTRINSICS for i386 and x86_64,
  but only after _USES clause as there
  is not intinsics unit for those CPUs }
{$IF FPC_FULLVERSION>=30301}
{$ifndef CPULLVM}
{$if defined(CPUI386) or defined(CPUX86_64)}
   {$define FPC_USE_INTRINSICS}
{$endif}
{$endif}
{$endif}

{$i classesh.inc}


implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;

finalization
  CommonCleanup;

end.
