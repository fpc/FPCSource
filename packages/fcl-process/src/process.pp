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
unit process;

interface

Uses Classes,
     pipes,
     SysUtils,
     Math;

Type
  TProcessOption = (poRunSuspended,poWaitOnExit,
                    poUsePipes,poStderrToOutPut,
                    poNoConsole,poNewConsole,
                    poDefaultErrorMode,poNewProcessGroup,
                    poDebugProcess,poDebugOnlyThisProcess,
                    poPassInput);

  TShowWindowOptions = (swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow,
                        swoShowDefault,swoShowMaximized,swoShowMinimized,
                        swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal);

  TStartupOption = (suoUseShowWindow,suoUseSize,suoUsePosition,
                    suoUseCountChars,suoUseFillAttribute);

  TProcessPriority = (ppHigh,ppIdle,ppNormal,ppRealTime);

  TProcessOptions = set of TProcessOption;
  TStartupOptions = set of TStartupOption;
  TRunCommandEventCode = (RunCommandIdle,RunCommandReadOutput,RunCommandFinished,RunCommandException);
  TOnRunCommandEvent = procedure(Sender,Context : TObject;Status:TRunCommandEventCode;const Message:string) of object;
  EProcess = Class(Exception);

  {$ifdef UNIX}
  TProcessForkEvent = procedure(Sender : TObject) of object;
  {$endif UNIX}

{$macro on}
{define processunicodestring}
{$define TProcessnamemacro:=TProcess}

{$i processbody.inc}
end.
