{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{   System independent GRAPHICAL clone of DIALOGS.PAS      }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer   }
{   ldeboer@attglobal.net  - primary e-mail addr           }
{   ldeboer@starwon.com.au - backup e-mail addr            }
{                                                          }
{****************[ THIS CODE IS FREEWARE ]*****************}
{                                                          }
{     This sourcecode is released for the purpose to       }
{   promote the pascal language on all platforms. You may  }
{   redistribute it and/or modify with the following       }
{   DISCLAIMER.                                            }
{                                                          }
{     This SOURCE CODE is distributed "AS IS" WITHOUT      }
{   WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR     }
{   ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.     }
{                                                          }
{*****************[ SUPPORTED PLATFORMS ]******************}
{                                                          }
{ Only Free Pascal Compiler supported                      }
{                                                          }
{**********************************************************}
unit FVConsts;
interface

{
  The FVConsts unit declares constants for all object type IDs used in the
  FreeVision library. They have been moved here for easier management. No
  values for views declared in TV 2.0 have been changed from so that original
  resource files may still be used.
}
const
  { Views Unit }
  idView = 1;
  idFrame = 2;
  idScrollBar = 3;
  idScroller = 4;
  idListViewer = 5;
  idGroup = 6;
  idWindow = 7;

  { Dialogs Unit 10 - ? }
  idDialog = 10;
  idInputLine = 11;
  idButton = 12;
  idCluster = 13;
  idRadioButtons = 14;
  idCheckBoxes = 15;
  idListBox = 16;
  idStaticText = 17;
  idLabel = 18;
  idHistory = 19;
  idParamText = 20;
  idCommandCheckBoxes = 21;
  idCommandRadioButtons = 22;
  idCommandIcon = 23;
  idBrowseButton = 24;
  idEditListBox = 25;
  idModalInputLine = 26;
  idMultiCheckBoxes = 27;
  idListDlg = 28;

  { App Unit }
  idBackground = 30;
  idDesktop = 31;

  { Config Unit }
  idConfig = 32;
  idMouseDlg = 33;
  idVideoDlg = 34;
  idClickTester = 35;

  { Menus Unit }
  idMenuBar = 40;
  idMenuBox = 41;
  idStatusLine = 42;
  idMenuPopup = 43;
  idMenuButton = 44;

  { Objects Unit }
  idCollection = 50;
  idStringCollection = 51;
  idStringList = 52;
  idStrListMaker = 52;
  idStrCollection = 69;

  { Resource Unit }
  idMemStringList = 52;

  { Tabs Unit }
  idTab = 55;

  { StdDlg Unit }
  idFileInputLine = 60;
  idFileCollection = 61;
  idFileList = 62;
  idFileInfoPane = 63;
  idFileDialog = 64;
  idDirCollection = 65;
  idDirListBox = 66;
  idChDirDialog = 67;
  idSortedListBox = 68;
  idEditChDirDialog = 69;

  { Editors Unit   70 - ? }
  idEditor = 70;
  idMemo = 71;
  idFileEditor = 72;
  idIndicator = 73;
  idEditWindow = 74;
  idEditWindowCollection = 75; { this value may need to be changed }
  idEditorEngine = 76;

  { Validate Unit }
  idPXPictureValidator = 80;
  idFilterValidator = 81;
  idRangeValidator = 82;
  idStringLookupValidator = 83;
  idRealValidator = 84;
  idByteValidator = 85;
  idIntegerValidator = 86;
  idSingleValidator = 87;
  idWordValidator = 88;
  idDateValidator = 89;
  idTimeValidator = 90;

  { Outline Unit }
  idOutline = 91;

  { ColorSel Unit }
  idColorSelector = 92;
  idMonoSelector = 93;
  idColorDisplay = 94;
  idColorGroupList = 95;
  idColorItemList = 96;
  idColorDialog = 97;

  { TimedDlg Unit }
  idTimedDialog = 98;
  idTimedDialogText = 99;

  { Statuses Unit }
  idStatus   = 300;
  idStatusDlg = 301;
  idStatusMessageDlg = 302;
  idGauge = 303;
  idArrowGauge = 304;
  idBarGauge = 305;
  idPercentGauge = 306;
  idSpinnerGauge = 307;
  idAppStatus = 308;
  idHeapMinAvail = 309;
  idHeapMemAvail = 310;

  { FVList Unit }

  { ColorTxt Unit }
  idColoredText = 611;

  { InpLong Unit }
  idInputLong = 711;

  { ASCIITab Unit }
  idTable = 10030;
  idReport = 10031;
  idASCIIChart = 10032;

{
 The FVConsts unit contains all command constants used in the FreeVision
 library. They have been extracted from their original units and placed here
 for easier maintainence and modification to remove conflicts, such as Borland
 created with the cmChangeDir constant in the StdDlg and App units.
}

const
  { App Unit }
  cmNew           = 30;
  cmOpen          = 31;
  cmSave          = 32;
  cmSaveAs        = 33;
  cmSaveAll       = 34;
  cmSaveDone      = 39;  {! Needs to match value in app.pas}
  cmChangeDir     = 35;  {!}
  cmDosShell      = 36;  {!}
  cmCloseAll      = 37;
  cmDelete        = 38;
  cmEdit          = 40;
  cmAbout         = 41;
  cmDesktopLoad   = 42;
  cmDesktopStore  = 43;
  cmNewDesktop    = 44;
  cmNewMenuBar    = 45;
  cmNewStatusLine = 46;
  cmNewVideo      = 47;
  cmTransfer      = 48;
  cmResizeApp     = 49;
  cmQuitApp       = 57;

  cmRecordHistory  = 60;
  cmGrabDefault    = 61;
  cmReleaseDefault = 62;

  cmHelpContents  = 256;
  cmHelpIndex     = 257;
  cmHelpTopic     = 258;
  cmHelpPrev      = 259;
  cmHelpUsingHelp = 260;
  cmHelpAbout     = 261;

  cmBrowseDir     = 262;
  cmBrowseFile    = 263;

  { Views Unit }
  cmValid   = 0;
  cmQuit    = 1;
  cmError   = 2;
  cmMenu    = 3;
  cmClose   = 4;
  cmZoom    = 5;
  cmResize  = 6;
  cmNext    = 7;
  cmPrev    = 8;
  cmHelp    = 9;
  cmOK      = 10;
  cmCancel  = 11;
  cmYes     = 12;
  cmNo      = 13;
  cmDefault = 14;
  cmCut     = 20;
  cmCopy    = 21;
  cmPaste   = 22;
  cmUndo    = 23;
  cmClear   = 24;
  cmTile    = 25;
  cmCascade = 26;
  cmHide    = 27;
  cmReceivedFocus     = 50;
  cmReleasedFocus     = 51;
  cmCommandSetChanged = 52;
  cmScrollBarChanged  = 53;
  cmScrollBarClicked  = 54;
  cmSelectWindowNum   = 55;
  cmListItemSelected  = 56;

  { ColorSel Unit }
  cmColorForegroundChanged = 71;
  cmColorBackgroundChanged = 72;
  cmColorSet               = 73;
  cmNewColorItem           = 74;
  cmNewColorIndex          = 75;
  cmSaveColorIndex         = 76;

  { StdDlg Unit   800 - ? }
  cmFileOpen    = 800;   { Returned from TFileDialog when Open pressed }
  cmFileReplace = 801;   { Returned from TFileDialog when Replace pressed }
  cmFileClear   = 802;   { Returned from TFileDialog when Clear pressed }
  cmFileInit    = 803;   { Used by TFileDialog internally }
  cmRevert      = 805;   { Used by TChDirDialog internally }
  cmFileFocused = 806;    { A new file was focused in the TFileList }
  cmFileDoubleClicked = 807;  { A file was selected in the TFileList }

  { Config Unit   130-140, 900-999 }
  cmConfigMouse       = 130; { Mouse command disabled by Init if no mouse }
  cmConfigOpen        = 900;
  cmConfigSave        = 901;
  cmConfigSaveAs      = 902;
  cmConfigMenu        = 903;
  cmConfigColors      = 904;
  cmConfigVideo       = 905;
  cmConfigCO80        = 906;
  cmConfigBW80        = 907;
  cmConfigMono        = 908;
  cmClock             = 909;
  cmClockSetFormat    = 910;

    { Editors Unit }
  cmFind           = 82;
  cmReplace        = 83;
  cmSearchAgain    = 84;
  cmPrint          = 85;
  cmRedo           = 86;
  cmJumpLine       = 87;
  cmWindowList     = 88;
  cmCharLeft       = 500;
  cmCharRight      = 501;
  cmWordLeft       = 502;
  cmWordRight      = 503;
  cmLineStart      = 504;
  cmLineEnd        = 505;
  cmLineUp         = 506;
  cmLineDown       = 507;
  cmPageUp         = 508;
  cmPageDown       = 509;
  cmTextStart      = 510;
  cmTextEnd        = 511;
  cmNewLine        = 512;
  cmBackSpace      = 513;
  cmDelChar        = 514;
  cmDelWord        = 515;
  cmDelStart       = 516;
  cmDelEnd         = 517;
  cmDelLine        = 518;
  cmInsMode        = 519;
  cmStartSelect    = 520;
  cmHideSelect     = 521;
  cmEndSelect      = 522;
  cmIndentMode     = 523;
  cmUpdateTitle    = 524;
  cmReformPara     = 525;
  cmTabKey         = 526;
  cmInsertLine     = 527;
  cmScrollUp       = 528;
  cmScrollDown     = 529;
  cmHomePage       = 530;
  cmEndPage        = 531;
  cmJumpMark0      = 532;
  cmJumpMark1      = 533;
  cmJumpMark2      = 534;
  cmJumpMark3      = 535;
  cmJumpMark4      = 536;
  cmJumpMark5      = 537;
  cmJumpMark6      = 538;
  cmJumpMark7      = 539;
  cmJumpMark8      = 540;
  cmJumpMark9      = 541;
  cmReformDoc      = 542;
  cmSetMark0       = 543;
  cmSetMark1       = 544;
  cmSetMark2       = 545;
  cmSetMark3       = 546;
  cmSetMark4       = 547;
  cmSetMark5       = 548;
  cmSetMark6       = 549;
  cmSetMark7       = 550;
  cmSetMark8       = 551;
  cmSetMark9       = 552;
  cmSelectWord     = 553;
  cmSaveExit       = 554;
  cmCenterText     = 555;
  cmSetTabs        = 556;
  cmRightMargin    = 557;
  cmWordwrap       = 558;
  cmBludgeonStats  = 559;
  cmPrinterSetup   = 560;
  cmClipboard      = 561;
  cmSpellCheck     = 562;
  cmCopyBlock      = 563;
  cmMoveBlock      = 564;
  cmDelSelect      = 565;
  cmIdentBlock     = 566;
  cmUnidentBlock   = 567;
  cmFileHistory    = 600;

  { Statuses Unit }
  cmStatusUpdate = 300;  { note - need to set to valid value }
  cmStatusDone   = 301;
  cmStatusPause  = 302;
  cmStatusResume = 303;

  cmCursorChanged = 700;


{
  The FVConsts unit declares standard help contexts used in FreeVision. By
  placing all help contexts in one unit, duplicate help contexts are more
  easily prevented
}

const

  hcNoContext = 0;
  hcDragging = 1;
  hcOk = 2;
  hcCancel = 3;
  hcEdit   = 4;
  hcDelete = 5;
  hcInsert = 6;

    { App Unit }
  hcNew = 65281;        hcFileNew = hcNew;
  hcOpen = 65282;       hcFileOpen = hcOpen;
  hcSave = 65283;       hcFileSave = hcSave;
  hcSaveAs = 65284;     hcFileSaveAs = hcSaveAs;
  hcSaveAll = 65285;    hcFileSaveAll = hcSaveAll;
  hcChangeDir = 65286;  hcFileChangeDir = hcChangeDir;
  hcDosShell = 65287;   hcFileDOSShell = hcDosShell;
  hcExit = 65288;       hcFileExit = hcExit;
  hcEditMenu = 65289;
  hcHelpMenu = 65291;
  hcHelpContents = 65292;
  hcHelpIndex = 65293;
  hcHelpTopic = 65294;
  hcHelpPrev = 65295;
  hcHelpUsingHelp = 65296;
  hcHelpAbout = 65297;
  hcWindowMenu = 65298;
  hcUndo         = $FF10;
  hcCut          = $FF11;
  hcCopy         = $FF12;
  hcPaste        = $FF13;
  hcClear        = $FF14;
  hcTile         = $FF20;
  hcCascade      = $FF21;
  hcCloseAll     = $FF22;
  hcResize       = $FF23;
  hcZoom         = $FF24;
  hcNext         = $FF25;
  hcPrev         = $FF26;
  hcClose        = $FF27;
  hcHide         = $FF28;
  hcFileMenu     = 65320;
  hcSearchAndReplace =65325;

    { Editors Unit }
  hcFile_Menu            = 2100;
{ hcOpen                 = 2101; }
{ hcNew                  = 2102; }
{ hcSave                 = 2103; }
  hcSaveDone             = 2104;
{ hcSaveAs               = 2105; }
{ hcChangeDir            = 2106; }
{ hcShellToDos           = 2107; }
{ hcExit                 = 2108; }
  hcFile_Menu_Items      = hcExit;

  hcEdit_Menu            = 2200;
{ hcUndo                 = 2201; }
{ hcCopy                 = 2202; }
{ hcCut                  = 2203; }
{ hcPaste                = 2204; }
  hcClipboard            = 2205;
{ hcClear                = 2206; }
  hcSpellCheck           = 2207;
  hcEdit_Menu_Items      = hcSpellCheck;

  hcSearch_Menu          = 2300;
  hcFind                 = 2301;
  hcReplace              = 2302;
  hcAgain                = 2303;
  hcSearch_Menu_Items    = hcAgain;

  hcWindows_Menu         = 2400;
{  hcResize               = 2401; }
{  hcZoom                 = 2402; }
{  hcPrev                 = 2403; }
{  hcNext                 = 2404; }
{  hcClose                = 2405; }
{  hcTile                 = 2406; }
{  hcCascade              = 2407; }
  hcWindows_Menu_Items   = hcCascade;

  hcDesktop_Menu         = 2500;
  hcLoadDesktop          = 2501;
  hcSaveDesktop          = 2502;
  hcToggleVideo          = 2503;
  hcDesktop_Menu_Items   = hcToggleVideo;

  hcMisc_Commands        = 2600;
  hckbShift              = 2601;
  hckbCtrl               = 2602;
  hckbAlt                = 2603;
  hcMisc_Items           = hckbAlt;

  hcEditor_Commands      = 2700;
  hcCursor               = 2701;
  hcDeleting             = 2702;
  hcFormatting           = 2703;
  hcMarking              = 2704;
  hcMoving               = 2705;
  hcSaving               = 2706;
  hcSelecting            = 2707;
  hcTabbing              = 2708;
  hcBackSpace            = 2709;
  hcCenterText           = 2710;
  hcCharLeft             = 2711;
  hcCharRight            = 2712;
  hcDelChar              = 2713;
  hcDelEnd               = 2714;
  hcDelLine              = 2715;
  hcDelStart             = 2716;
  hcDelWord              = 2717;
  hcEndPage              = 2718;
  hcHideSelect           = 2719;
  hcHomePage             = 2720;
  hcIndentMode           = 2721;
  hcInsertLine           = 2722;
  hcInsMode              = 2723;
  hcJumpLine             = 2724;
  hcLineDown             = 2725;
  hcLineEnd              = 2726;
  hcLineStart            = 2727;
  hcLineUp               = 2728;
  hcNewLine              = 2729;
  hcPageDown             = 2730;
  hcPageUp               = 2731;
  hcReformDoc            = 2732;
  hcReformPara           = 2733;
  hcRightMargin          = 2734;
  hcScrollDown           = 2735;
  hcScrollUp             = 2736;
  hcSearchAgain          = 2737;
  hcSelectWord           = 2738;
  hcSetTabs              = 2739;
  hcStartSelect          = 2740;
  hcTabKey               = 2741;
  hcTextEnd              = 2742;
  hcTextStart            = 2743;
  hcWordLeft             = 2744;
  hcWordRight            = 2745;
  hcWordWrap             = 2746;

  hcJMarker_Menu         = 2750;
  hcJumpMark1            = 2751;
  hcJumpMark2            = 2752;
  hcJumpMark3            = 2753;
  hcJumpMark4            = 2754;
  hcJumpMark5            = 2755;
  hcJumpMark6            = 2756;
  hcJumpMark7            = 2757;
  hcJumpMark8            = 2758;
  hcJumpMark9            = 2759;
  hcJumpMark0            = 2760;
  hcJMarker_Menu_Items   = 2761;

  hcSMarker_Menu         = 2770;
  hcSetMark1             = 2771;
  hcSetMark2             = 2772;
  hcSetMark3             = 2773;
  hcSetMark4             = 2774;
  hcSetMark5             = 2775;
  hcSetMark6             = 2776;
  hcSetMark7             = 2777;
  hcSetMark8             = 2778;
  hcSetMark9             = 2779;
  hcSetMark0             = 2780;
  hcSMarker_Menu_Items   = 2781;

  hcEditor_Items         = hcSMarker_Menu_Items;

  { Dialog }
  hcDialogs              = 2800;
  hcDCancel              = 2801;
  hcDNo                  = 2802;
  hcDOk                  = 2803;
  hcDYes                 = 2804;
  hcDAbout               = 2805;
  hcDDirName             = 2806;
  hcDDirTree             = 2807;
  hcDChDir               = 2808;
  hcDRevert              = 2809;
  hcDName                = 2810;
  hcDFiles               = 2811;
  hcDFindText            = 2812;
  hcDLineNumber          = 2813;
  hcDReformDoc           = 2814;
  hcDReplaceTExt         = 2815;
  hcDRightMargin         = 2816;
  hcDTabStops            = 2817;
  hcListDlg              = 2818;

  { Checkbox help }
  hcCCaseSensitive       = 2900;
  hcCWholeWords          = 2901;
  hcCPromptReplace       = 2902;
  hcCReplaceAll          = 2903;
  hcCReformCurrent       = 2904;
  hcCReformEntire        = 2905;

    { Statuses unit }
  hcStatusPause          = 2950;
  hcStatusResume         = 2951;

  { Glossary }
  Glossary               = 3000;
  GCloseIcon             = 3001;
  GDesktop               = 3002;
  GDialogBox             = 3003;
  GHistoryIcon           = 3004;
  GInputLine             = 3005;
  GMemIndicator          = 3006;
  GMenuBar               = 3007;
  GPulldownMenu          = 3008;
  GResizeCorner          = 3009;
  GSelectedText          = 3010;
  GStatusBar             = 3011;
  GTitleBar              = 3012;
  GWindowBorder          = 3013;
  GZoomIcon              = 3014;
  hcGlossary_Items       = GZoomIcon;

    { INI Unit }
  hcDateFormatDlg = 1;
  hcDateParts = 1;
  hcDateOrder = 1;
  hcTimeFormatDlg = 1;
  hcClockFormatDlg = 1;
  hcClockDateParts = 1;
  hcClockTimeFormat = 1;

  hcListViewer = 1;

  { Options Help Contexts }
  hcConfigMenu          = 100;
  hcConfigColors        = hcConfigMenu + 1;
  hcConfigDate          = hcConfigColors + 1;
  hcConfigEnvironment   = hcConfigDate + 1;
  hcConfigMouse         = hcConfigEnvironment + 1;
  hcConfigOpen          = hcConfigMouse + 1;
  hcConfigSave          = hcConfigOpen + 1;
  hcConfigSaveAs        = hcConfigSave + 1;
  hcConfigTime          = hcConfigSaveAs + 1;
  hcConfigVideo         = hcConfigTime + 1;
  hcConfigDesktopDlg    = hcConfigVideo + 1;
  hcConfigMouseDlg      = hcConfigDesktopDlg + 1;
  hcConfigTimeFormatDlg = hcConfigMouseDlg + 1;
  hcConfigTimeSeparator = hcConfigTimeFormatDlg + 1;
  hcConfigTimeComponents = hcConfigTimeSeparator + 1;
  hcConfigTimeStyle = hcConfigTimeComponents + 1;
  hcConfigClock = hcConfigTimeStyle + 1;
  hcBrowseDir = 1;
  hcBrowseFile = 1;


{
  The FVConsts unit contains all history list constants used in the FreeVision
  Library.
}

const
  hiConfig = 1;
  hiDirectories = 2;  { non-specific }
  hiDesktop = 3;
  hiCurrentDirectories = 1;
  hiFiles = 4;

implementation

end.
