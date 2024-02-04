{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit processunicode;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
Uses System.Classes,
     System.Pipes,
     System.SysUtils,
     System.Math,
     System.Process;
{$ELSE FPC_DOTTEDUNITS}
Uses Classes,
     pipes,
     SysUtils,
     Math,
     Process;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TProcessOption = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TProcessOption;
  TShowWindowOptions = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TShowWindowOptions;
  TStartupOption = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TStartupOption ;
  TProcessPriority = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TProcessPriority;
  TProcessOptions = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TProcessOptions;
  TStartupOptions = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}process.TStartupOptions;
  {$ifdef UNIX}
  TProcessForkEvent = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Process.TProcessForkEvent;
  {$endif UNIX}

  TRunCommandEventCode = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Process.TRunCommandEventCode;
  TOnRunCommandEvent = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Process.TOnRunCommandEvent;
  EProcess = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Process.EProcess;
  
  TProcess = Class;

{$macro on}
{$IF SIZEOF(CHAR)=1}
// For unicode RTL, there is nothing to redefine.
{$define processunicodestring}
{$ENDIF}
{$define TProcessnamemacro:=TProcess}
{$DEFINE SKIPHELPERS}
{$i processbody.inc}

end.
