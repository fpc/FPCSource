{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Global variables for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPVars;

interface

uses Objects,Views,App,
     FPConst,FPUtils,FPViews,FPCalc;

type
    TRecentFileEntry = record
      FileName  : string;
      LastPos   : TPoint;
    end;

    TCompPhase = (cpCompiling,cpLinking,cpDone);

const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      HighlightExts    : string[80] = '*.pas;*.pp;*.inc';
      PrimaryFile      : string = '';
      IsEXECompiled    : boolean = false;
      MainFile         : string = '';
      CompilationPhase : TCompPhase = cpDone;
      ProgramInfoWindow: PProgramInfoWindow = nil;
      UserScreenWindow : PScreenWindow = nil;
      HelpFiles        : FPViews.PUnsortedStringCollection = nil;
      ShowStatusOnError: boolean = true;
      StartupDir       : string = '.'+DirSep;
      INIPath          : string = ININame;
      SwitchesPath     : string = SwitchesName;
      CtrlMouseAction  : integer = acTopicSearch;
      AltMouseAction   : integer = acBrowseSymbol;
      StartupOptions   : longint = 0;

      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
{
  $Log$
  Revision 1.2  1998-12-30 13:38:42  peter
    * patches from Gabor

  Revision 1.1  1998/12/28 15:47:54  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.0  1998/12/23 07:34:40  gabor

}
