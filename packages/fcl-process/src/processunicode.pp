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
unit processunicode;

interface

Uses Classes,
     pipes,
     SysUtils,
     Math,
     Process;

Type
  TProcessOption = process.TProcessOption;
  TShowWindowOptions = process.TShowWindowOptions;
  TStartupOption = process.TStartupOption ;
  TProcessPriority = process.TProcessPriority;
  TProcessOptions = process.TProcessOptions;
  TStartupOptions = process.TStartupOptions;
  {$ifdef UNIX}
  TProcessForkEvent = Process.TProcessForkEvent;
  {$endif UNIX}

  TRunCommandEventCode = Process.TRunCommandEventCode;
  TOnRunCommandEvent = Process.TOnRunCommandEvent;
  EProcess = Process.EProcess;

{$macro on}
{$define processunicodestring}
{$define TProcessnamemacro:=TProcess}

{$i processbody.inc}
end.
