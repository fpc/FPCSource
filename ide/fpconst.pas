{
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

uses Views,App,
     FVConsts,
     WViews,WEditor,WHTMLHlp;

const
     VersionStr           = '1.0.12';

     MaxRecentFileCount   = 9;
     MaxToolCount         = 16;

     ReservedWordMaxLen   = 16;

     CompilerStatusUpdateDelay = 0.1; { in secs }

{$undef USE_SPECIAL_BASENAME}
{$ifdef m68k}
  {$ifdef cpui386}
    {$define USE_SPECIAL_BASENAME}
     FPBaseName = 'fpm68k';
  {$endif cpui386}
{$endif m68k}
{$ifdef powerpc}
  {$ifdef cpui386}
    {$define USE_SPECIAL_BASENAME}
     FPBaseName = 'fpppc';
  {$endif powerpc}
{$endif m68k}
{$ifdef i386}
  {$ifdef cpu68k}
    {$define USE_SPECIAL_BASENAME}
     FPBaseName = 'fpi386';
  {$endif cpu68k}
{$endif i386}
{$ifndef USE_SPECIAL_BASENAME}
     FPBaseName = 'fp';
{$endif not USE_SPECIAL_BASENAME}
     ININame              = FPBaseName+'.ini';
     DirInfoName          = FPBaseName+'.dir';
     SwitchesName         = FPBaseName+'.cfg';
     DesktopName          = FPBaseName+'.dsk';
     BrowserName          = FPBaseName+'.brw';
     BackgroundName       = 'fp.ans';
     ReadmeName           = 'readme.ide';

     ToolCaptureName      = '__tool__.out'; { all '$' signs replaces with '_'s }
     ToolCaptureErr       = '__tool__.err'; { all '$' signs replaces with '_'s }
     FilterCaptureName    = '_filter_.out';
     FPOutFileName        = 'fp___.out';
     FPErrFileName        = 'fp___.err';
     GDBOutFileName       = 'gdb___.out';
     GDBOutPutFileName    = 'gdb___.txt';
     GDBPrompt            = 'gdb>';
     DesktopTempName      = 'fp___.dsk';
     GrepOutName          = 'grep$$.out';
     GrepErrName          = 'grep$$.err';
     CodeCompleteUnitName = '__fp__';

     HTMLIndexExt         = WHTMLHlp.extHTMLIndex;
     HTMLExt              = WHTMLHlp.extHTML;
     TemplateExt          = '.pt';
     NGExt                = '.ng';
     INFExt               = '.inf';
     WinHelpExt           = '.hlp';
     HelpFileExts         = '*.tph;*.htm*;*'+HTMLIndexExt+';*'+NGExt+';*'+WinHelpExt+';*'+INFExt+';*'+ExtChm;

     EnterSign            = #17#196#217;

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
     soHeapMonitor        = $00000002;

     { Desktop Flag constants - what to include in the desktop file }
     dfHistoryLists       = $00000001;
     dfClipboardContent   = $00000002;
     dfWatches            = $00000004;
     dfBreakpoints        = $00000008;
     dfOpenWindows        = $00000010;
     dfSymbolInformation  = $00000020;
     dfCodeCompleteWords  = $00000040;
     dfCodeTemplates      = $00000080;

     { Auto Save flag constants }
     asEditorFiles        = $00000001; { Editor files }
     asEnvironment        = $00000002; { .INI file }
     asDesktop            = $00000004; { .DSK file }

     { Misc. Options flag constants }
     moAutoTrackSource    = $00000001;
     moCloseOnGotoSource  = $00000002;
     moChangeDirOnOpen    = $00000004;

     { Desktop Location constants }
     dlCurrentDir         = $00;
     dlConfigFileDir      = $01;

     { History ids }
     hidRunParameters     = 200;
     hidOpenSourceFile    = 201;
     hidPrimaryFile       = 202;
     hidOpenIniFile       = 203;
     hidSaveIniFile       = hidOpenIniFile;
     hidOpenHelpFile      = 204;
     hidConditionalDefines= 205;
     hidCompilerArgs      = 206;
     hidWatchDialog       = 207;
     hidBreakpointDialogName = 208;
     hidRunDir            = 209;
     hidBreakpointDialogCond = 210;
     hidPrinterDevice      = 211;
     hidEvaluate           = 212;

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
     cmRunDir            = 218;
{     cmWindowList        = 219; defined in command.pas, too! - Gabor }
     cmHelpTopicSearch   = 220;
     cmMsgGotoSource     = 221;
     cmMsgTrackSource    = 222;
     cmGotoCursor        = 223;
     {cmToggleBreakpoint  = 224; never disabled =>2403 }
     cmAddWatch          = 225;
     cmTraceInto         = 226;
     cmStepOver          = 227;
     cmResetDebugger     = 228;
     cmContToCursor      = 229;
     cmOpenGDBWindow     = 230;
     cmToolsMsgNext      = 231;
     cmToolsMsgPrev      = 232;
     cmGrep              = 233;
     cmCompilerMessages  = 234;
     cmSymbol            = 235;
     cmStack             = 236;
     cmBreakpointList    = 237;
     cmWatches           = 238;
     cmUntilReturn       = 239;
     { WARNING these two are also defined in weditor.pas PM }
     { and why aren't these defines then removed? Gabor }
     { commented out, now in wviews.pas FK
     cmCopyWin           = 240;
     cmPasteWin          = 241;
     }
     cmRegisters         = 242;
     cmFPURegisters      = 243;
     cmDoReload          = 244;
     cmVectorRegisters   = 245;


     { in wviews.pas defined
     cmSelectAll         = 246;
     cmUnselect          = 247;
     }

     cmPrint             = 248;

     cmNotImplemented    = 1000;
     cmNewFromTemplate   = 1001;
     cmShowReadme        = 1002;
     cmPrinterSetup      = 1003;

     cmSearchWindow      = 1500;
     cmSourceWndClosing  = 1601;
     cmCalculatorPaste   = 1603;
     cmMsgClear          = 1604;
     cmUpdateTools       = 1605;
{     cmGrep              = 160?;}

     cmAddItem           = 1620;
     cmEditItem          = 1621;
     cmDeleteItem        = 1622;
     cmShowItem          = 1623;
     cmHideItem          = 1624;

     cmUserScreen        = 1650;
     cmUserScreenWindow  = 1651;
     cmEvaluate          = 1652;
     cmCalculator        = 1653;
     cmASCIITable        = 1654;

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
     cmBrowser           = 2015;
     cmDesktopOptions    = 2016;
     cmCodeCompleteOptions=2017;
     cmCodeTemplateOptions=2018;
     cmKeys              = 2019;
     cmAskSaveAll        = 2020;
     cmRemoteDialog      = 2021;
     cmTransferRemote    = 2022;

     cmHelpContents      = 2100;
     cmHelpIndex         = 2101;
     cmHelpPrevTopic     = 2103;
     cmHelpUsingHelp     = 2104;
     cmHelpFiles         = 2105;
     cmAbout             = 2106;
     cmHelpDebug         = 2107;

     cmEditorOptions     = 2202;
     cmBrowserOptions    = 2203;

     cmTrackReference    = 2300;
     cmGotoReference     = 2301;

     cmEditBreakpoint    = 2400;
     cmNewBreakpoint     = 2401;
     cmDeleteBreakpoint  = 2402;
     cmToggleBreakpoint  = 2403;
     cmToggleBreakInList = 2404;


     cmDumpUndo          = 2500;
     cmUndoAll           = 2501;
     cmRedoAll           = 2502;

     cmDebuggerStopped   = 2600;
     cmDisassemble       = 2601;
     cmContinue          = 2602;

     cmSymBrowse         = 2700;
     cmSymGotoSource     = 2701;
     cmSymTrackSource    = 2702;
     cmSymOptions        = 2703;

     { Help constants }
     hcSourceWindow      = 8000;
     hcHelpWindow        = 8001;
     hcClipboardWindow   = 8002;
     hcCalcWindow        = 8003;
     hcInfoWindow        = 8004;
     hcBrowserWindow     = 8005;
     hcMessagesWindow    = 8006;
     hcGDBWindow         = 8007;
     hcBreakpointListWindow = 8008;
     hcASCIITableWindow  = 8009;
     hcCompilerMessagesWindow    = 8010;
     hcDisassemblyWindow = 8011;

     hcShift             = 10000;
     hcNoAltXShift       = 20000;

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
     hcCompilerNoAltX    = hcNoAltXShift+cmCompiler;
     hcMemorySizes       = hcShift+cmMemorySizes;
     hcLinker            = hcShift+cmLinker;
     hcDebugger          = hcShift+cmDebugger;
     hcRemoteDialog      = hcShift+cmRemoteDialog;
     hcTransferRemote    = hcShift+cmTransferRemote;
     hcDirectories       = hcShift+cmDirectories;
     hcTools             = hcShift+cmTools;
     hcPreferences       = hcShift+cmPreferences;
     hcEditor            = hcShift+cmEditor;
     hcMouse             = hcShift+cmMouse;
     hcStartup           = hcShift+cmStartup;
     hcColors            = hcShift+cmColors;
     hcKeys              = hcShift+cmKeys;
     hcOpenINI           = hcShift+cmOpenINI;
     hcSaveINI           = hcShift+cmSaveINI;
     hcSaveAsINI         = hcShift+cmSaveAsINI;
     hcCalculator        = hcShift+cmCalculator;
     hcAsciiTable        = hcShift+cmAsciiTable;
{     hcGrep              = hcShift+cmGrep;}
     hcSwitchesMode      = hcShift+cmSwitchesMode;
     hcBrowser           = hcShift+cmBrowser;
     hcDesktopOptions    = hcShift+cmDesktopOptions;
     hcCodeCompleteOptions=hcShift+cmCodeCompleteOptions;
     hcCodeTemplateOptions=hcShift+cmCodeTemplateOptions;
     hcAbout             = hcShift+cmAbout;
     hcCompilerMessages  = hcShift+cmCompilerMessages;

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
     hcLastNormalCommand = hcNoAltXShift - 1;
     hcFirstNoAltXCommand = hcNoAltXShift;
     hcLastCommand       = 65535;

     hcShowClipboard     = hcShift+cmShowClipboard;
     hcCopyWin           = hcShift+cmCopyWin;
     hcPasteWin          = hcShift+cmPasteWin;
     hcSelectAll         = hcShift+cmSelectAll;
     hcUnselect          = hcShift+cmUnselect;

     hcFindProcedure     = hcShift+cmFindProcedure;
     hcObjects           = hcShift+cmObjects;
     hcModules           = hcShift+cmModules;
     hcGlobals           = hcShift+cmGlobals;
     hcSymbol            = hcShift+cmSymbol;
     hcRun               = hcShift+cmRun;
     hcRunDir            = hcShift+cmRunDir;
     hcParameters        = hcShift+cmParameters;
     hcResetDebugger     = hcShift+cmResetDebugger;
     hcContToCursor      = hcShift+cmContToCursor;
     hcUntilReturn       = hcShift+cmUntilReturn;
     hcOpenGDBWindow     = hcShift+cmOpenGDBWindow;
     hcToolsMsgNext      = hcShift+cmToolsMsgNext;
     hcToolsMsgPrev      = hcShift+cmToolsMsgPrev;
     hcCompile           = hcShift+cmCompile;
     hcMake              = hcShift+cmMake;
     hcBuild             = hcShift+cmBuild;
     hcTarget            = hcShift+cmTarget;
     hcPrimaryFile       = hcShift+cmPrimaryFile;
     hcClearPrimary      = hcShift+cmClearPrimary;
     hcWindowList        = hcShift+cmWindowList;
     hcNewFromTemplate   = hcShift+cmNewFromTemplate;
     hcHelpTopicSearch   = hcShift+cmHelpTopicSearch;
     hcHelpContents      = hcShift+cmHelpContents;
     hcHelpIndex         = hcShift+cmHelpIndex;
     hcHelpPrevTopic     = hcShift+cmHelpPrevTopic;
     hcHelpUsingHelp     = hcShift+cmHelpUsingHelp;
     hcHelpFiles         = hcShift+cmHelpFiles;
     hcHelpDebug         = hcShift+cmHelpDebug;
     hcUpdate            = hcShift+cmUpdate;
     hcMsgClear          = hcShift+cmMsgClear;
     hcMsgGotoSource     = hcShift+cmMsgGotoSource;
     hcMsgTrackSource    = hcShift+cmMsgTrackSource;
     hcSymBrowse         = hcShift+cmSymBrowse;
     hcSymGotoSource     = hcShift+cmSymGotoSource;
     hcSymTrackSource    = hcShift+cmSymTrackSource;
     hcSymOptions        = hcShift+cmSymOptions;
     hcGotoCursor        = hcShift+cmGotoCursor;
     hcNewBreakpoint     = hcShift+cmNewBreakpoint;
     hcEditBreakpoint    = hcShift+cmEditBreakpoint;
     hcDeleteBreakpoint  = hcShift+cmDeleteBreakpoint;
     hcToggleBreakpoint  = hcShift+cmToggleBreakpoint;
     hcEvaluate          = hcShift+cmEvaluate;
     hcAddWatch          = hcShift+cmAddWatch;
     hcWatchesWindow     = hcShift+cmWatches;
     hcGrep              = hcShift+cmGrep;
     hcStackWindow       = hcShift+cmStack;
     hcBreakPointList    = hcShift+cmBreakpointList;
     hcRegistersWindow   = hcShift+cmRegisters;
     hcFPURegisters      = hcShift+cmFPURegisters;
     hcVectorRegisters   = hcShift+cmVectorRegisters;
     hcPrint             = hcShift+cmPrint;
     hcPrinterSetup      = hcShift+cmPrinterSetup;

     hcOpenAtCursor      = hcShift+cmOpenAtCursor;
     hcBrowseAtCursor    = hcShift+cmBrowseAtCursor;
     hcEditorOptions     = hcShift+cmEditorOptions;
     hcBrowserOptions    = hcShift+cmBrowserOptions;
     hcDoReload          = hcShift+cmDoReload;
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

     CBrowserOutline = #9#10#10#11;

     CGDBInputLine   = #9#9#10#11#12;

     CFPClockView = #0#227;

     CFPToolTip     = #228;

     CFPMemo        = #26#26#26#28#26#29#26#26#26#27#26#26#26#26#26#26#26;

     CFPSymbolMemo  = #9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9;

     CHTMLSectionAttrs = #229#230#231#232#233#234;

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
{215-226}#$31#$3F#$3A#$31#$31#$31#$71#$1F#$31#$2F#$3E#$3F +
         { CFPClockView }
{227-227}#$70 +
         { CToolTip }
{228-228}#$20 +
         { CHTMLSectionAttrs }
{229-234}#$ff#$3a#$37#$ff#$ff#$ff;

implementation

END.
