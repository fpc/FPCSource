{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Desktop loading/saving routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPDesk;

interface

const
     ResHistory         = 'HISTORY';
     ResClipboard       = 'CLIPBOARD';
     ResWatches         = 'WATCHES';
     ResBreakpoints     = 'BREAKPOINTS';
     ResDesktop         = 'DESKTOP';
     ResSymbols         = 'SYMBOLS';

procedure InitDesktopFile;
function  LoadDesktop: boolean;
function  SaveDesktop: boolean;
procedure DoneDesktopFile;

implementation

uses Dos,
     Objects,App,
     WResource,
     FPConst,FPVars,FPUtils;

procedure InitDesktopFile;
begin
  if DesktopLocation=dlCurrentDir then
    DesktopPath:=FExpand(DesktopName)
  else
    DesktopPath:=FExpand(DirOf(INIPath)+DesktopName);
end;

procedure DoneDesktopFile;
begin
end;

function WriteHistory(F: PResourceFile): boolean;
begin
  WriteHistory:=true;
end;

function WriteClipboard(F: PResourceFile): boolean;
begin
  WriteClipboard:=true;
end;

function WriteWatches(F: PResourceFile): boolean;
begin
  WriteWatches:=true;
end;

function WriteBreakpoints(F: PResourceFile): boolean;
begin
  WriteBreakPoints:=true;
end;

function WriteOpenWindows(F: PResourceFile): boolean;
var S: PMemoryStream;
begin
  {$ifndef DEV}Exit;{$endif}

  New(S, Init(1024*1024,4096));
  Desktop^.Store(S^);
  S^.Seek(0);
  F^.CreateResource(resDesktop,rcBinary,0);
  F^.AddResourceEntryFromStream(resDesktop,langDefault,0,S^,S^.GetSize);
  Dispose(S, Done);
  WriteOpenWindows:=true;
end;

function WriteSymbols(F: PResourceFile): boolean;
begin
  WriteSymbols:=true;
end;

function LoadDesktop: boolean;
begin
  LoadDesktop:=true;
end;

function SaveDesktop: boolean;
var OK: boolean;
    F: PResourceFile;
begin
  New(F, CreateFile(DesktopPath));
  OK:=true;
  if OK and ((DesktopFileFlags and dfHistoryLists)<>0) then
    OK:=WriteHistory(F);
  if OK and ((DesktopFileFlags and dfClipboardContent)<>0) then
    OK:=WriteClipboard(F);
  if OK and ((DesktopFileFlags and dfWatches)<>0) then
    OK:=WriteWatches(F);
  if OK and ((DesktopFileFlags and dfBreakpoints)<>0) then
    OK:=WriteBreakpoints(F);
  if OK and ((DesktopFileFlags and dfOpenWindows)<>0) then
    OK:=WriteOpenWindows(F);
  if OK and ((DesktopFileFlags and dfSymbolInformation)<>0) then
    OK:=WriteSymbols(F);
  Dispose(F, Done);
  SaveDesktop:=OK;
end;

END.
{
  $Log$
  Revision 1.3  1999-04-07 21:55:45  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.2  1999/03/23 16:16:39  peter
    * linux fixes

  Revision 1.1  1999/03/23 15:11:28  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

}

