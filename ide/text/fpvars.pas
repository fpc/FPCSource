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

    TCompPhase = (cpNothing,cpCompiling,cpLinking,cpFailed,cpDone);

const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      HighlightExts    : string[80] = '*.pas;*.pp;*.inc';
      PrimaryFile      : string = '';
      IsEXECompiled    : boolean = false;
      MainFile         : string = '';
      EXEFile          : string = '';
      CompilationPhase : TCompPhase = cpNothing;
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
      LastExitCode     : integer = 0;

      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CIDEAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
{
  $Log$
  Revision 1.3  1999-01-04 11:49:52  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:54  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.0  1998/12/23 07:34:40  gabor

}
