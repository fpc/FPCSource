{
    $Id$
    This file is part of the Free Pascal run time library
    for Netware.
    Copyright (c) 1999-2003 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
					    
**********************************************************************}

unit nwconio;
interface

{$mode objfpc}

  const
    ClibNlm    = 'clib';
    ThreadsNlm = 'threads';


{$PACKRECORDS C}


  const
     DONT_AUTO_ACTIVATE              = 1;
     DONT_SWITCH_SCREEN              = 2;
     DONT_CHECK_CTRL_CHARS           = $10;
     AUTO_DESTROY_SCREEN             = $20;
     POP_UP_SCREEN                   = $40;
     UNCOUPLED_CURSORS               = $80;
     HAS_A_CLIB_HANDLE               = $100;
     _KEYBOARD_INPUT_ACTIVE          = $00010000;
     _PROCESS_BLOCKED_ON_KEYBOARD    = $00020000;
     _PROCESS_BLOCKED_ON_SCREEN      = $00040000;
     _INPUT_CURSOR_DISABLED          = $00080000;
     _SCREEN_HAS_TITLE_BAR           = $00400000;
     _NON_SWITCHABLE_SCREEN          = $01000000;

  function getch :longint;                                cdecl;external ThreadsNlm name 'getch';
  function getche:longint;                                cdecl;external ThreadsNlm name 'getche';
  function kbhit :longint;                                cdecl;external ThreadsNlm name 'kbhit';
  function putch(c:longint):longint;                      cdecl;external ThreadsNlm name 'putch';
  function ungetch(c:longint):longint;                    cdecl;external ThreadsNlm name 'ungetch';
  function cgets(buf:Pchar):Pchar;                        cdecl;external ThreadsNlm name 'cgets';
  function CheckIfScreenDisplayed(screenHandle:longint; waitFlag:longint):longint;
                                                          cdecl;external ThreadsNlm name 'CheckIfScreenDisplayed';
  procedure clrscr;                                       cdecl;external ThreadsNlm name 'clrscr';

  procedure ConsolePrintf(format:Pchar; args:array of const);
                                                          cdecl;external ClibNlm name 'ConsolePrintf';
  procedure ConsolePrintf(format:Pchar);                  cdecl;external ClibNlm name 'ConsolePrintf';

  procedure CopyToScreenMemory(height,width:word; Rect:PBYTE; begx,begy:word);cdecl;external ThreadsNlm name 'CopyToScreenMemory';
  procedure CopyToScreenMemory(height,width:word; var Rect:byte; begx,begy:word);cdecl;external ThreadsNlm name 'CopyToScreenMemory';

  procedure CopyFromScreenMemory(height, width:word; Rect:PBYTE; begx,begy:word);cdecl;external ThreadsNlm name 'CopyFromScreenMemory';
  procedure CopyFromScreenMemory(height, width:word; var Rect:byte; begx,begy:word);cdecl;external ThreadsNlm name 'CopyFromScreenMemory';

  { function CoupleInputOutputCursors:longint;cdecl;external ClibNlm name 'CoupleInputOutputCursors'; }

  function cputs(buf:Pchar):longint;                      cdecl;external ThreadsNlm name 'cputs';
  function cprintf(fmt:Pchar; args:array of const):longint;
                                                          cdecl;external ClibNlm name 'cprintf';
  function cprintf(fmt:Pchar):longint;                    cdecl;external ClibNlm name 'cprintf';

  function CreateScreen(screenName:Pchar; attr:byte):longint;
                                                          cdecl;external ThreadsNlm name 'CreateScreen';

  function cscanf(fmt:Pchar; args:array of const):longint;cdecl;external ClibNlm name 'cscanf';
  function cscanf(fmt:Pchar):longint;                     cdecl;external ClibNlm name 'cscanf';

  { function DecoupleInputOutputCursors:longint;cdecl;external External_library name 'DecoupleInputOutputCursors'; }

  function DestroyScreen(screenHandle:longint):longint;   cdecl;external ThreadsNlm name 'DestroyScreen';
  function DisplayInputCursor:longint;                    cdecl;external ThreadsNlm name 'DisplayInputCursor';
  function DisplayScreen(screenHandle:longint):longint;   cdecl;external ThreadsNlm name 'DisplayScreen';
  function DropPopUpScreen(screenHandle:longint):longint; cdecl;external ThreadsNlm name 'DropPopUpScreen';
  function GetCurrentScreen:longint;                      cdecl;external ThreadsNlm name 'GetCurrentScreen';
  function GetCursorCouplingMode:byte;                    cdecl;external ThreadsNlm name 'GetCursorCouplingMode';
  function GetCursorShape(startline,endline:byte):word;   cdecl;external ThreadsNlm name 'GetCursorShape';
  function GetCursorSize(var firstline,lastl:byte):word;  cdecl;external ThreadsNlm name 'GetCursorSize';

  function GetPositionOfOutputCursor(var row,columnP:word):longint;
                                                          cdecl;external ThreadsNlm name 'GetPositionOfOutputCursor';

  function __GetScreenID(screenHandle:longint):longint;   cdecl;external ThreadsNlm name '__GetScreenID';
  function GetScreenInfo(handle:longint; name:Pchar; var attr:longint):longint;
                                                          cdecl;external ThreadsNlm name 'GetScreenInfo';

  function GetSizeOfScreen(var height,width:word):longint;cdecl;external ThreadsNlm name 'GetSizeOfScreen';
  procedure gotoxy(x,y:word);                             cdecl;external ThreadsNlm name 'gotoxy';
  function HideInputCursor:longint;                       cdecl;external ThreadsNlm name 'HideInputCursor';
  function IsColorMonitor:longint;                        cdecl;external ThreadsNlm name 'IsColorMonitor';
  function PressAnyKeyToContinue:longint;                 cdecl;external ThreadsNlm name 'PressAnyKeyToContinue';
  function PressEscapeToQuit:longint;                     cdecl;external ThreadsNlm name 'PressEscapeToQuit';
  procedure RingTheBell;                                  cdecl;external ThreadsNlm name 'RingTheBell';

  function ScanScreens(LastScreenID:longint; name:Pchar; var attr:longint):longint;
                                                          cdecl;external ThreadsNlm name 'ScanScreens';

  function ScrollScreenRegionDown(firstLine,lines:longint):longint;cdecl;external ThreadsNlm name 'ScrollScreenRegionDown';
  function ScrollScreenRegionUp(firstLine,lines:longint):longint;cdecl;external ThreadsNlm name 'ScrollScreenRegionUp';
  function SetAutoScreenDestructionMode(newMode:byte):byte;cdecl;external ThreadsNlm name 'SetAutoScreenDestructionMode';
  function SetCtrlCharCheckMode(newMode:byte):byte;        cdecl;external ThreadsNlm name 'SetCtrlCharCheckMode';
  function SetCursorCouplingMode(newMode:byte):byte;       cdecl;external ThreadsNlm name 'SetCursorCouplingMode';
  function SetCursorShape(startline,endline:byte):word;    cdecl;external ThreadsNlm name 'SetCursorShape';
  function SetCurrentScreen(screenHandle:longint):longint; cdecl;external ThreadsNlm name 'SetCurrentScreen';
  function SetInputAtOutputCursorPosition:longint;         cdecl;external ThreadsNlm name 'SetInputAtOutputCursorPosition';
  function SetOutputAtInputCursorPosition:longint;         cdecl;external ThreadsNlm name 'SetOutputAtInputCursorPosition';
  function SetPositionOfInputCursor(row,col:word):longint; cdecl;external ThreadsNlm name 'SetPositionOfInputCursor';

  function SetScreenAreaAttribute(line,column,numLines,numColumns,attr:longint):longint;
                                                           cdecl;external ThreadsNlm name 'SetScreenAreaAttribute';
  {function SetScreenAttributes(mask,attr:longint):longint; cdecl;external ClibNlm name 'SetScreenAttributes';}

  function SetScreenCharacterAttribute(line,column,attr:longint):longint;
                                                           cdecl;external ThreadsNlm name 'SetScreenCharacterAttribute';

  function SetScreenRegionAttribute(firstLine,lines:longint; attr:byte):longint;
                                                           cdecl;external ThreadsNlm name 'SetScreenRegionAttribute';
  function wherex:word;                                    cdecl;external ThreadsNlm name 'wherex';
  function wherey:word;                                    cdecl;external ThreadsNlm name 'wherey';


implementation


end.

{
  $Log$
  Revision 1.1  2003-02-16 17:45:08  armin
  * added nwsnut, nwconio and nwthreads for netware

 
}
