{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Constants used by the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPConst;

interface

uses Views,App,Commands;

const
     VersionStr           = '0.9';

     MaxRecentFileCount   = 5;
     MaxToolCount         = 16;

     ININame              = 'fp.ini';
     SwitchesName         = 'fp.cfg';

     { Strings/Messages }
     strLoadingHelp       = 'Loading help files...';
     strBuildingHelpIndex = 'Building Help Index...';
     strLocatingTopic     = 'Locating topic...';

     { Main menu submenu indexes }
     menuFile             = 0;
     menuTools            = 6;

     { MouseAction constants }
     acNone               = 0;
     acTopicSearch        = 1;
     acGotoCursor         = 2;
     acBreakpoint         = 3;
     acEvaluate           = 4;
     acAddWatch           = 5;
     acBrowseSymbol       = 6;
     acFirstAction        = acTopicSearch;
     acLastAction         = acBrowseSymbol;

     { Startup Option constants }
     soReturnToLastDir    = $00000001;

     { Command constants }
     cmShowClipboard     = 201;
     cmFindProcedure     = 206;
     cmObjects           = 207;
     cmModules           = 208;
     cmGlobals           = 209;
     cmRun               = 210;
     cmParameters        = 211;
     cmCompile           = 212;
     cmMake              = 213;
     cmBuild             = 214;
     cmTarget            = 215;
     cmPrimaryFile       = 216;
     cmClearPrimary      = 217;
     cmInformation       = 218;
     cmWindowList        = 219;
     cmHelpTopicSearch   = 220;
     cmMsgGotoSource     = 221;
     cmMsgTrackSource    = 222;
     cmGotoCursor        = 223;
     cmToggleBreakpoint  = 224;
     cmAddWatch          = 225;
     cmTraceInto         = 226;
     cmStepOver          = 227;
     cmResetDebugger     = 228;
     cmContToCursor      = 229;
     
     cmNotImplemented    = 1000;
     cmNewFromTemplate   = 1001;

     cmSearchWindow      = 1500;
     cmUpdate            = 1600;
     cmSourceWndClosing  = 1601;
     cmLocalMenu         = 1602;
     cmCalculatorPaste   = 1603;
     cmMsgClear          = 1604;
     cmUpdateTools       = 1605;
     cmGrep              = 1606;

     cmAddItem           = 1620;
     cmEditItem          = 1621;
     cmDeleteItem        = 1622;

     cmUserScreen        = 1650;
     cmUserScreenWindow  = 1651;
     cmEvaluate          = 1652;
     cmCalculator        = 1653;

     cmToolsMessages     = 1700;
     cmToolsBase         = 1800;
     cmRecentFileBase    = 1850;

     cmCompiler          = 2000;
     cmMemorySizes       = 2001;
     cmLinker            = 2002;
     cmDebugger          = 2003;
     cmDirectories       = 2004;
     cmTools             = 2005;
     cmPreferences       = 2006;
     cmEditor            = 2007;
     cmMouse             = 2008;
     cmStartup           = 2009;
     cmColors            = 2010;
     cmOpenINI           = 2011;
     cmSaveINI           = 2012;
     cmSaveAsINI         = 2013;
     cmSwitchesMode      = 2014;

     cmHelpContents      = 2100;
     cmHelpIndex         = 2101;
     cmHelpPrevTopic     = 2103;
     cmHelpUsingHelp     = 2104;
     cmHelpFiles         = 2105;
     cmAbout             = 2106;

     cmOpenAtCursor      = 2200;
     cmBrowseAtCursor    = 2201;
     cmEditorOptions     = 2202;

     { Help constants }
     hcSourceWindow      = 8000;
     hcHelpWindow        = 8001;
     hcClipboardWindow   = 8002;
     hcCalcWindow        = 8003;
     hcInfoWindow        = 8004;
     hcBrowserWindow     = 8005;

     hcShift             = 10000;

     hcUsingHelp         = 2;
     hcContents          = 3;
     hcQuit              = hcShift+cmQuit;
     hcRedo              = hcShift+cmRedo;
     hcFind              = hcShift+cmFind;
     hcReplace           = hcShift+cmReplace;
     hcSearchAgain       = hcShift+cmSearchAgain;
     hcGotoLine          = hcShift+cmJumpLine;

     hcUserScreen        = hcShift+cmUserScreen;
     hcUserScreenWindow  = hcShift+cmUserScreenWindow;

     hcToolsMessages     = hcShift+cmToolsMessages;
     hcToolsBase         = hcShift+cmToolsBase;
     hcRecentFileBase    = hcShift+cmRecentFileBase;

     hcCompiler          = hcShift+cmCompiler;
     hcMemorySizes       = hcShift+cmMemorySizes;
     hcLinker            = hcShift+cmLinker;
     hcDebugger          = hcShift+cmDebugger;
     hcDirectories       = hcShift+cmDirectories;
     hcTools             = hcShift+cmTools;
     hcPreferences       = hcShift+cmPreferences;
     hcEditor            = hcShift+cmEditor;
     hcMouse             = hcShift+cmMouse;
     hcStartup           = hcShift+cmStartup;
     hcColors            = hcShift+cmColors;
     hcOpenINI           = hcShift+cmOpenINI;
     hcSaveINI           = hcShift+cmSaveINI;
     hcSaveAsINI         = hcShift+cmSaveAsINI;
     hcCalculator        = hcShift+cmCalculator;
     hcGrep              = hcShift+cmGrep;
     hcSwitchesMode      = hcShift+cmSwitchesMode;
     hcAbout             = hcShift+cmAbout;

     hcSystemMenu        = 9000;
     hcFileMenu          = 9001;
     hcEditMenu          = 9002;
     hcSearchMenu        = 9003;
     hcRunMenu           = 9004;
     hcCompileMenu       = 9005;
     hcDebugMenu         = 9006;
     hcToolsMenu         = 9007;
     hcOptionsMenu       = 9008;
     hcEnvironmentMenu   = 9009;
     hcWindowMenu        = 9010;
     hcHelpMenu          = 9011;

     hcFirstCommand      = hcSystemMenu;
     hcLastCommand       = 65535;

     hcShowClipboard     = hcShift+cmShowClipboard;
     hcFindProcedure     = hcShift+cmFindProcedure;
     hcObjects           = hcShift+cmObjects;
     hcModules           = hcShift+cmModules;
     hcGlobals           = hcShift+cmGlobals;
     hcRun               = hcShift+cmRun;
     hcParameters        = hcShift+cmParameters;
     hcResetDebugger     = hcShift+cmResetDebugger;
     hcContToCursor      = hcShift+cmContToCursor;
     hcCompile           = hcShift+cmCompile;
     hcMake              = hcShift+cmMake;
     hcBuild             = hcShift+cmBuild;
     hcTarget            = hcShift+cmTarget;
     hcPrimaryFile       = hcShift+cmPrimaryFile;
     hcClearPrimary      = hcShift+cmClearPrimary;
     hcInformation       = hcShift+cmInformation;
     hcWindowList        = hcShift+cmWindowList;
     hcNewFromTemplate   = hcShift+cmNewFromTemplate;
     hcHelpTopicSearch   = hcShift+cmHelpTopicSearch;
     hcHelpContents      = hcShift+cmHelpContents;
     hcHelpIndex         = hcShift+cmHelpIndex;
     hcHelpPrevTopic     = hcShift+cmHelpPrevTopic;
     hcHelpUsingHelp     = hcShift+cmHelpUsingHelp;
     hcHelpFiles         = hcShift+cmHelpFiles;
     hcUpdate            = hcShift+cmUpdate;
     hcMsgClear          = hcShift+cmMsgClear;
     hcMsgGotoSource     = hcShift+cmMsgGotoSource;
     hcMsgTrackSource    = hcShift+cmMsgTrackSource;
     hcGotoCursor        = hcShift+cmGotoCursor;
     hcToggleBreakpoint  = hcShift+cmToggleBreakpoint;
     hcEvaluate          = hcShift+cmEvaluate;
     hcAddWatch          = hcShift+cmAddWatch;

     hcOpenAtCursor      = hcShift+cmOpenAtCursor;
     hcBrowseAtCursor    = hcShift+cmBrowseAtCursor;
     hcEditorOptions     = hcShift+cmEditorOptions;

     { History constants }
     hisChDirDialog      = 2000;

     CIDEHelpDialog      =
        #128#129#130#131#132#133#134#135#136#137#138#139#140#141#142#143 +
        #144#145#146#147#148#149#150#151#152#153#154#155#156#157#158#159 +
        #160#161#162#163 +
        #164#165#166;

     CSourceWindow =
        #167#168#169#170#171#172#173#174#175#176#177#178#179#180#181#182 +
        #183#184#185#186#187#188#189#190#191#192#193#194#195#196#197#198 +
        #199#200#201#202#203#204#205#206#207#208#209#210#211#212#213#214 ;

     CBrowserWindow =
        #215#216#217#218#219#220#221#222#223#224#225#226;

     CBrowserListBox =
        #9#9#10#11#12;

     CBrowserTab =
        #6#12;

     CIDEAppColor = CAppColor +
         { CIDEHelpDialog }
{128-143}#$70#$7F#$7A#$13#$13#$70#$70#$7F#$7E#$20#$2B#$2F#$78#$2E#$70#$30 + { 1-16}
{144-159}#$3F#$3E#$1F#$2F#$1A#$20#$72#$31#$31#$30#$2F#$3E#$31#$13#$38#$00 + {17-32}
{160-163}#$30#$3E#$1E#$70 + { CHelpViewer }                                 {33-36}
{164-166}#$30#$3F#$3A +     { CHelpFrame }                                  {37-39}
         { CSourceWindow }
{167-182}#$17#$1F#$1A#$31#$31#$1E#$71#$1F#$00#$00#$00#$00#$00#$00#$00#$00 + { 1-16}
{183-198}#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00 + {17-32}
{199-214}#$1E#$1F#$17#$1F#$1E#$1B#$13#$1A#$1E#$71#$3F#$30#$1C#$13#$1F#$4E + {33-48}
         { CBrowserWindow }
{215-   }#$31#$3F#$3A#$31#$31#$31#$71#$1F#$31#$2F#$3E#$3F ;

implementation

END.
{
  $Log$
  Revision 1.9  1999-02-08 17:40:00  pierre
   + cmContToCursor added

  Revision 1.8  1999/02/04 12:23:43  pierre
    + cmResetDebugger and cmGrep
    * Avoid StatusStack overflow

  Revision 1.7  1999/01/22 10:24:02  peter
    * first debugger things

  Revision 1.6  1999/01/21 11:54:12  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.5  1999/01/12 14:29:33  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.4  1999/01/04 11:49:43  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:43  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.3  1998/12/22 10:39:41  peter
    + options are now written/read
    + find and replace routines

}
