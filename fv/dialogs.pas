{ $Id:							   }
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
{     16 and 32 Bit compilers                              }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{        DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     11 Nov 96   First DOS/DPMI platform release.   }
{  1.10     13 Jul 97   Windows platform code added.       }
{  1.20     29 Aug 97   Platform.inc sort added.           }
{  1.30     13 Oct 97   Delphi 2 32 bit code added.        }
{  1.40     05 May 98   Virtual pascal 2.0 code added.     }
{  1.50     27 Oct 99   All objects completed and checked  }
{  1.51     03 Nov 99   FPC windows support added          }
{  1.60     26 Nov 99   Graphics stuff moved to GFVGraph   }
{**********************************************************}

UNIT Dialogs;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC}{ FPC doesn't support these switches }
  {$F-} { Short calls are okay }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$P-} { Normal string variables }
  {$N-} { No 80x87 code generation }
  {$E+} { Emulation is on }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     {$IFNDEF PPC_SPEED}                              { NON SPEED COMPILER }
       {$IFDEF PPC_FPC}                               { FPC WINDOWS COMPILER }
       Windows,                                       { Standard units }
       {$ELSE}                                        { OTHER COMPILERS }
       WinTypes,WinProcs,                             { Standard units }
       {$ENDIF}
     {$ELSE}                                          { SPEEDSOFT COMPILER }
       WinBase, WinDef, WinUser, WinGDI,              { Standard units }
     {$ENDIF}
     {$IFDEF PPC_DELPHI}                              { DELPHI COMPILERS }
     Messages,                                        { Standard unit }
     {$ENDIF}
   {$ENDIF}

   {$IFDEF OS_OS2}                                    { OS2 CODE }
     OS2Def, OS2Base, OS2PMAPI,                       { Standard units }
   {$ENDIF}

   Common, GFVGraph, Objects, Drivers, Views, Validate; { Standard GFV units }

{***************************************************************************}
{                              PUBLIC CONSTANTS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                        COLOUR PALETTE DEFINITIONS                         }
{---------------------------------------------------------------------------}
CONST
   CGrayDialog    = #32#33#34#35#36#37#38#39#40#41#42#43#44#45#46#47 +
                    #48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63;
   CBlueDialog    = #64#65#66#67#68#69#70#71#72#73#74#75#76#77#78#79 +
                    #80#81#82#83#84#85#86#87#88#89#90#91#92#92#94#95;
   CCyanDialog    = #96#97#98#99#100#101#102#103#104#105#106#107#108 +
                    #109#110#111#112#113#114#115#116#117#118#119#120 +
                    #121#122#123#124#125#126#127;
   CStaticText    = #6#7#8#9;
   CLabel         = #7#8#9#9;
   CButton        = #10#11#12#13#14#14#14#15;
   CCluster       = #16#17#18#18#31#6;
   CInputLine     = #19#19#20#21#14;
   CHistory       = #22#23;
   CHistoryWindow = #19#19#21#24#25#19#20;
   CHistoryViewer = #6#6#7#6#6;

   CDialog = CGrayDialog;                             { Default palette }


{$IFNDEF OS_DOS}                                      { WIN/NT/OS2 CODE }
{---------------------------------------------------------------------------}
{                        NEW WIN/NT/OS2 COMMAND CODES                       }
{---------------------------------------------------------------------------}
CONST
   cmTvClusterButton = $2001;                         { Cluster button cmd id }
{$ENDIF}

{---------------------------------------------------------------------------}
{                     TDialog PALETTE COLOUR CONSTANTS                      }
{---------------------------------------------------------------------------}
CONST
   dpBlueDialog = 0;                                  { Blue dialog colour }
   dpCyanDialog = 1;                                  { Cyan dialog colour }
   dpGrayDialog = 2;                                  { Gray dialog colour }

{---------------------------------------------------------------------------}
{                           TButton FLAGS MASKS                             }
{---------------------------------------------------------------------------}
CONST
   bfNormal    = $00;                                 { Normal displayed }
   bfDefault   = $01;                                 { Default command }
   bfLeftJust  = $02;                                 { Left just text }
   bfBroadcast = $04;                                 { Broadcast command }
   bfGrabFocus = $08;                                 { Grab focus }

{---------------------------------------------------------------------------}
{          TMultiCheckBoxes FLAGS - (HiByte = Bits LoByte = Mask)           }
{---------------------------------------------------------------------------}
CONST
   cfOneBit    = $0101;                               { One bit masks }
   cfTwoBits   = $0203;                               { Two bit masks }
   cfFourBits  = $040F;                               { Four bit masks }
   cfEightBits = $08FF;                               { Eight bit masks }

{---------------------------------------------------------------------------}
{                        DIALOG BROADCAST COMMANDS                          }
{---------------------------------------------------------------------------}
CONST
   cmRecordHistory = 60;                              { Record history cmd }

{***************************************************************************}
{                            RECORD DEFINITIONS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                          ITEM RECORD DEFINITION                           }
{---------------------------------------------------------------------------}
TYPE
   PSItem = ^TSItem;
   TSItem = RECORD
     Value: PString;                                  { Item string }
     Next: PSItem;                                    { Next item }
   END;

{***************************************************************************}
{                            OBJECT DEFINITIONS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                      TDialog OBJECT - DIALOG OBJECT                       }
{---------------------------------------------------------------------------}
TYPE
   TDialog = OBJECT (TWindow)
      CONSTRUCTOR Init (Var Bounds: TRect; ATitle: TTitleStr);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION Valid (Command: Word): Boolean; Virtual;
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
   END;
   PDialog = ^TDialog;

{---------------------------------------------------------------------------}
{                   TInputLine OBJECT - INPUT LINE OBJECT                   }
{---------------------------------------------------------------------------}
TYPE
   TInputLine = OBJECT (TView)
         MaxLen: Integer;                             { Max input length }
         CurPos: Integer;                             { Cursor position }
         FirstPos: Integer;                           { First position }
         SelStart: Integer;                           { Selected start }
         SelEnd: Integer;                             { Selected end }
         Data: PString;                               { Input line data }
         Validator: PValidator;                       { Validator of view }
      CONSTRUCTOR Init (Var Bounds: TRect; AMaxLen: Integer);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION DataSize: Word; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION Valid (Command: Word): Boolean; Virtual;
      PROCEDURE Draw; Virtual;
      PROCEDURE DrawCursor; Virtual;
      PROCEDURE DrawbackGround; Virtual;
      PROCEDURE SelectAll (Enable: Boolean);
      PROCEDURE SetValidator (AValid: PValidator);
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PRIVATE
      FUNCTION CanScroll (Delta: Integer): Boolean;
   END;
   PInputLine = ^TInputLine;

{---------------------------------------------------------------------------}
{                  TButton OBJECT - BUTTON ANCESTOR OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   TButton = OBJECT (TView)
         AmDefault: Boolean;                          { If default button }
         Flags    : Byte;                             { Button flags }
         Command  : Word;                             { Button command }
         Title    : PString;                          { Button title }
      CONSTRUCTOR Init (Var Bounds: TRect; ATitle: TTitleStr; ACommand: Word;
        AFlags: Word);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      PROCEDURE Press; Virtual;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE DrawState (Down: Boolean);
      PROCEDURE MakeDefault (Enable: Boolean);
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PRIVATE
      DownFlag: Boolean;
   END;
   PButton = ^TButton;

{---------------------------------------------------------------------------}
{                 TCluster OBJECT - CLUSTER ANCESTOR OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
   {$IFNDEF OS_DOS}                                   { WIN/NT/OS2 CODE }
   TWndArray = Array [0..32000] Of HWnd;              { Window handle array }
   PWndArray = ^TWndArray;                            { Ptr to handle array }
   {$ENDIF}

   TCluster = OBJECT (TView)
         Id        : Integer;                         { New communicate id }
         Sel       : Integer;                         { Selected item }
         Value     : LongInt;                         { Bit value }
         EnableMask: LongInt;                         { Mask enable bits }
         Strings   : TStringCollection;               { String collection }
         {$IFNDEF OS_DOS}                             { WIN/NT/OS2 DATA }
         WndHandles: PWndArray;                       { Window handle array }
         {$ENDIF}
      CONSTRUCTOR Init (Var Bounds: TRect; AStrings: PSItem);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION DataSize: Word; Virtual;
      FUNCTION GetHelpCtx: Word; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION Mark (Item: Integer): Boolean; Virtual;
      FUNCTION MultiMark (Item: Integer): Byte; Virtual;
      FUNCTION ButtonState (Item: Integer): Boolean;
      PROCEDURE DrawFocus;                                           Virtual;
      PROCEDURE Press (Item: Integer); Virtual;
      PROCEDURE MovedTo (Item: Integer); Virtual;
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE DrawMultiBox (Const Icon, Marker: String);
      PROCEDURE DrawBox (Const Icon: String; Marker: Char);
      PROCEDURE SetButtonState (AMask: Longint; Enable: Boolean);
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent);                     Virtual;
      {$IFNDEF OS_DOS}                                { WIN/NT/OS2 CODE }
      FUNCTION GetClassName: String; Virtual;
      FUNCTION SubClassAttr: LongInt; Virtual;
      FUNCTION GetMsgHandler: Pointer; Virtual;
      PROCEDURE CreateWindowNow (CmdShow: Integer);                  Virtual;
      {$ENDIF}
      PRIVATE
      FUNCTION FindSel (P: TPoint): Integer;
      FUNCTION Row (Item: Integer): Integer;
      FUNCTION Column (Item: Integer): Integer;
   END;
   PCluster = ^TCluster;

{---------------------------------------------------------------------------}
{                TRadioButtons OBJECT - RADIO BUTTON OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
   TRadioButtons = OBJECT (TCluster)
      FUNCTION Mark (Item: Integer): Boolean; Virtual;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE Press (Item: Integer); Virtual;
      PROCEDURE MovedTo(Item: Integer); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      {$IFNDEF OS_DOS}                                { WIN/NT CODE }
      FUNCTION SubClassAttr: LongInt; Virtual;
      {$ENDIF}
   END;
   PRadioButtons = ^TRadioButtons;

{---------------------------------------------------------------------------}
{                  TCheckBoxes OBJECT - CHECK BOXES OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   TCheckBoxes = OBJECT (TCluster)
      FUNCTION Mark (Item: Integer): Boolean; Virtual;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE Press (Item: Integer); Virtual;
      {$IFNDEF OS_DOS}                                { WIN/NT CODE }
      FUNCTION SubClassAttr: LongInt; Virtual;
      {$ENDIF}
   END;
   PCheckBoxes = ^TCheckBoxes;

{---------------------------------------------------------------------------}
{               TMultiCheckBoxes OBJECT - CHECK BOXES OBJECT                }
{---------------------------------------------------------------------------}
TYPE
   TMultiCheckBoxes = OBJECT (TCluster)
         SelRange: Byte;                              { Select item range }
         Flags   : Word;                              { Select flags }
         States  : PString;                           { Strings }
      CONSTRUCTOR Init (Var Bounds: TRect; AStrings: PSItem;
        ASelRange: Byte; AFlags: Word; Const AStates: String);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION DataSize: Word; Virtual;
      FUNCTION MultiMark (Item: Integer): Byte; Virtual;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE Press (Item: Integer); Virtual;
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      {$IFNDEF OS_DOS}                                { WIN/NT CODE }
      FUNCTION SubClassAttr: LongInt; Virtual;
      {$ENDIF}
   END;
   PMultiCheckBoxes = ^TMultiCheckBoxes;

{---------------------------------------------------------------------------}
{                     TListBox OBJECT - LIST BOX OBJECT                     }
{---------------------------------------------------------------------------}
TYPE
   TListBox = OBJECT (TListViewer)
         List: PCollection;                           { List of strings }
      CONSTRUCTOR Init (Var Bounds: TRect; ANumCols: Word;
        AScrollBar: PScrollBar);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION DataSize: Word; Virtual;
      FUNCTION GetText (Item: Integer; MaxLen: Integer): String; Virtual;
      PROCEDURE NewList(AList: PCollection); Virtual;
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
   END;
   PListBox = ^TListBox;

{---------------------------------------------------------------------------}
{                TStaticText OBJECT - STATIC TEXT OBJECT                    }
{---------------------------------------------------------------------------}
TYPE
   TStaticText = OBJECT (TView)
         Text: PString;                               { Text string ptr }
      CONSTRUCTOR Init (Var Bounds: TRect; Const AText: String);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      PROCEDURE DrawBackGround;                                      Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE GetText (Var S: String); Virtual;
   END;
   PStaticText = ^TStaticText;

{---------------------------------------------------------------------------}
{              TParamText OBJECT - PARMETER STATIC TEXT OBJECT              }
{---------------------------------------------------------------------------}
TYPE
   TParamText = OBJECT (TStaticText)
         ParamCount: Integer;                         { Parameter count }
         ParamList : Pointer;                         { Parameter list }
      CONSTRUCTOR Init (Var Bounds: TRect; Const AText: String;
        AParamCount: Integer);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION DataSize: Word; Virtual;
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE GetText (Var S: String); Virtual;
   END;
   PParamText = ^TParamText;

{---------------------------------------------------------------------------}
{                        TLabel OBJECT - LABEL OBJECT                       }
{---------------------------------------------------------------------------}
TYPE
   TLabel = OBJECT (TStaticText)
         Light: Boolean;
         Link: PView;                                 { Linked view }
      CONSTRUCTOR Init (Var Bounds: TRect; CONST AText: String; ALink: PView);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      PROCEDURE DrawBackGround; Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
   END;
   PLabel = ^TLabel;

{---------------------------------------------------------------------------}
{             THistoryViewer OBJECT - HISTORY VIEWER OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
   THistoryViewer = OBJECT (TListViewer)
         HistoryId: Word;                             { History id }
      CONSTRUCTOR Init(Var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
        AHistoryId: Word);
      FUNCTION HistoryWidth: Integer;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION GetText (Item: Integer; MaxLen: Integer): String; Virtual;
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
   END;
   PHistoryViewer = ^THistoryViewer;

{---------------------------------------------------------------------------}
{             THistoryWindow OBJECT - HISTORY WINDOW OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
  THistoryWindow = OBJECT (TWindow)
         Viewer: PListViewer;                         { List viewer object }
      CONSTRUCTOR Init (Var Bounds: TRect; HistoryId: Word);
      FUNCTION GetSelection: String; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      PROCEDURE InitViewer (HistoryId: Word); Virtual;
   END;
   PHistoryWindow = ^THistoryWindow;

{---------------------------------------------------------------------------}
{                   THistory OBJECT - HISTORY OBJECT                        }
{---------------------------------------------------------------------------}
TYPE
   THistory = OBJECT (TView)
         HistoryId: Word;
         Link: PInputLine;
      CONSTRUCTOR Init (Var Bounds: TRect; ALink: PInputLine; AHistoryId: Word);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION InitHistoryWindow (Var Bounds: TRect): PHistoryWindow; Virtual;
      PROCEDURE Draw; Virtual;
      PROCEDURE RecordHistory (CONST S: String); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
   END;
   PHistory = ^THistory;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           ITEM STRING ROUTINES                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewSItem-----------------------------------------------------------
Allocates memory for a new TSItem record and sets the text field
and chains to the next TSItem. This allows easy construction of
singly-linked lists of strings, to end a chain the next TSItem
should be nil.
28Apr98 LdB
---------------------------------------------------------------------}
FUNCTION NewSItem (Const Str: String; ANext: PSItem): PSItem;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                   DIALOG OBJECT REGISTRATION PROCEDURE                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-RegisterDialogs----------------------------------------------------
This registers all the view type objects used in this unit.
30Sep99 LdB
---------------------------------------------------------------------}
PROCEDURE RegisterDialogs;

{***************************************************************************}
{                        STREAM REGISTRATION RECORDS                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                        TDialog STREAM REGISTRATION                        }
{---------------------------------------------------------------------------}
CONST
   RDialog: TStreamRec = (
     ObjType: 10;                                     { Register id = 10 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TDialog)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TDialog);
     {$ENDIF}
     Load:  @TDialog.Load;                            { Object load method }
     Store: @TDialog.Store                            { Object store method }
   );

{---------------------------------------------------------------------------}
{                      TInputLine STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RInputLine: TStreamRec = (
     ObjType: 11;                                     { Register id = 11 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TInputLine)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TInputLine);
     {$ENDIF}
     Load:  @TInputLine.Load;                         { Object load method }
     Store: @TInputLine.Store                         { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TButton STREAM REGISTRATION                        }
{---------------------------------------------------------------------------}
CONST
   RButton: TStreamRec = (
     ObjType: 12;                                     { Register id = 12 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TButton)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TButton);
     {$ENDIF}
     Load:  @TButton.Load;                            { Object load method }
     Store: @TButton.Store                            { Object store method }
   );

{---------------------------------------------------------------------------}
{                       TCluster STREAM REGISTRATION                        }
{---------------------------------------------------------------------------}
CONST
   RCluster: TStreamRec = (
     ObjType: 13;                                     { Register id = 13 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TCluster)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TCluster);
     {$ENDIF}
     Load:  @TCluster.Load;                           { Object load method }
     Store: @TCluster.Store                           { Objects store method }
   );

{---------------------------------------------------------------------------}
{                    TRadioButtons STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RRadioButtons: TStreamRec = (
     ObjType: 14;                                     { Register id = 14 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TRadioButtons)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TRadioButtons);
     {$ENDIF}
     Load:  @TRadioButtons.Load;                      { Object load method }
     Store: @TRadioButtons.Store                      { Object store method }
   );

{---------------------------------------------------------------------------}
{                     TCheckBoxes STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RCheckBoxes: TStreamRec = (
     ObjType: 15;                                     { Register id = 15 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TCheckBoxes)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TCheckBoxes);
     {$ENDIF}
     Load:  @TCheckBoxes.Load;                        { Object load method }
     Store: @TCheckBoxes.Store                        { Object store method }
   );

{---------------------------------------------------------------------------}
{                   TMultiCheckBoxes STREAM REGISTRATION                    }
{---------------------------------------------------------------------------}
CONST
   RMultiCheckBoxes: TStreamRec = (
     ObjType: 27;                                     { Register id = 27 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TMultiCheckBoxes)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TMultiCheckBoxes);
     {$ENDIF}
     Load:  @TMultiCheckBoxes.Load;                   { Object load method }
     Store: @TMultiCheckBoxes.Store                   { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TListBox STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RListBox: TStreamRec = (
     ObjType: 16;                                     { Register id = 16 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TListBox)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TListBox);
     {$ENDIF}
     Load:  @TListBox.Load;                           { Object load method }
     Store: @TListBox.Store                           { Object store method }
   );

{---------------------------------------------------------------------------}
{                      TStaticText STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RStaticText: TStreamRec = (
     ObjType: 17;                                     { Register id = 17 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TStaticText)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TStaticText);
     {$ENDIF}
     Load:  @TStaticText.Load;                        { Object load method }
     Store: @TStaticText.Store                        { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TLabel STREAM REGISTRATION                         }
{---------------------------------------------------------------------------}
CONST
   RLabel: TStreamRec = (
     ObjType: 18;                                     { Register id = 18 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TLabel)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TLabel);
     {$ENDIF}
     Load:  @TLabel.Load;                             { Object load method }
     Store: @TLabel.Store                             { Object store method }
   );

{---------------------------------------------------------------------------}
{                        THistory STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RHistory: TStreamRec = (
     ObjType: 19;                                     { Register id = 19 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(THistory)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(THistory);
     {$ENDIF}
     Load:  @THistory.Load;                           { Object load method }
     Store: @THistory.Store                           { Object store method }
   );

{---------------------------------------------------------------------------}
{                      TParamText STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RParamText: TStreamRec = (
     ObjType: 20;                                     { Register id = 20 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TParamText)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TParamText);
     {$ENDIF}
     Load:  @TParamText.Load;                         { Object load method }
     Store: @TParamText.Store                         { Object store method }
   );

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

USES HistList;                                        { Standard GFV unit }

{***************************************************************************}
{                         PRIVATE DEFINED CONSTANTS                         }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                 LEFT AND RIGHT ARROW CHARACTER CONSTANTS                  }
{---------------------------------------------------------------------------}
{$IFDEF OS_DOS} CONST LeftArr = #17; RightArr = #16; {$ENDIF}
{$IFDEF OS_WINDOWS} CONST LeftArr = #$AB; RightArr = #$BB; {$ENDIF}
{$IFDEF OS_OS2} CONST LeftArr = #17; RightArr = #16; {$ENDIF}

{---------------------------------------------------------------------------}
{                               TButton MESSAGES                            }
{---------------------------------------------------------------------------}
CONST
   cmGrabDefault    = 61;                             { Grab default }
   cmReleaseDefault = 62;                             { Release default }

{***************************************************************************}
{                          PRIVATE INTERNAL ROUTINES                        }
{***************************************************************************}

{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
{---------------------------------------------------------------------------}
{  TvClusterMsgHandler -> Platforms WIN/NT - Checked 08Jun98 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TvClusterMsgHandler (Wnd: hWnd; iMessage, wParam: sw_Word;
lParam: LongInt): LongInt; {$IFDEF BIT_32} STDCALL; {$ELSE} EXPORT; {$ENDIF}
VAR Sel: Integer; W: sw_Word; P: PCluster;
BEGIN
   TvClusterMsgHandler := 0;                          { Reset return of zero }
   Case iMessage Of
     WM_KeyDown:;                                     { Ignore keypresses }
     WM_Command: Begin
       If (wParam AND $FFFF = cmTvClusterButton)      { Command message }
       Then Begin
         {$IFDEF BIT_16}                              { 16 BIT CODE }
         PtrRec(P).Seg := GetProp(Wnd, ViewSeg);      { Fetch cluster seg }
         PtrRec(P).Ofs := GetProp(Wnd, ViewOfs);      { Fetch cluster ofs }
         {$ENDIF}
         {$IFDEF BIT_32}                              { 32 BIT CODE }
         LongInt(P) := GetProp(Wnd, ViewPtr);         { Fetch cluster ptr }
         {$ENDIF}
         If (P <> Nil) AND (P^.WndHandles <> Nil)     { Cluster/handles valid }
         Then Begin
           If (P^.State AND sfFocused = 0) Then       { We have not focus }
             P^.FocusFromTop;                         { Focus up to us }
           Sel := 0;                                  { Start on first }
           {$IFDEF BIT_16}                            { 16 BIT CODE }
           W := LoWord(lParam);                       { Use only low part }
           {$ENDIF}
           {$IFDEF BIT_32}                            { 32 BIT CODE }
           W := lParam;                               { Use full param }
           {$ENDIF}
           While (Sel < P^.Strings.Count) AND         { Valid item }
           (W <> P^.WndHandles^[Sel]) Do Inc(Sel);    { Find handle }
           If (Sel < P^.Strings.Count) Then Begin     { Handle was found }
             P^.Press(Sel);                           { Call press }
             P^.Sel := Sel;                           { Set selection }
             If (P^.GetState(sfSelected)=False)       { Check not selected }
             Then P^.Select Else Begin                { Select us then }
               P^.SetDrawMask(vdFocus OR vdInner);    { Redraw inner }
               P^.DrawView;                           { Redraw partial view }
             End;
           End;
         End;
       End Else
         TvClusterMsgHandler := TvViewMsgHandler(Wnd,
           iMessage, wParam, lParam);                 { Call TV view handler }
     End;
     Else TvClusterMsgHandler := TvViewMsgHandler(Wnd,
      iMessage, wParam, lParam);                      { Call TV view handler }
   End;
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
{---------------------------------------------------------------------------}
{  TvClusterMsgHandler -> Platforms OS2 - Checked ??Sep99 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TvClusterMsgHandler (Wnd: hWnd; iMessage, wParam: sw_Word;
lParam: LongInt): LongInt; STDCALL;
VAR Sel: Integer; W: sw_Word; P: PCluster;
BEGIN
   TvClusterMsgHandler := 0;                          { Reset return of zero }
   TvClusterMsgHandler := TvViewMsgHandler(Wnd,
    iMessage, wParam, lParam);                        { Call TV view handler }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  IsBlank -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jun98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION IsBlank (Ch: Char): Boolean;
BEGIN
   IsBlank := (Ch = ' ') OR (Ch = #13) OR (Ch = #10); { Check for characters }
END;

{---------------------------------------------------------------------------}
{  HotKey -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jun98 LdB            }
{---------------------------------------------------------------------------}
FUNCTION HotKey (Const S: String): Char;
VAR I: Word;
BEGIN
   HotKey := #0;                                      { Preset fail }
   If (S <> '') Then Begin                            { Valid string }
     I := Pos('~', S);                                { Search for tilde }
     If (I <> 0) Then HotKey := UpCase(S[I+1]);       { Return hotkey }
   End;
END;

{***************************************************************************}
{                              OBJECT METHODS                               }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TDialog OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TDialog------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TDialog.Init (Var Bounds: TRect; ATitle: TTitleStr);
BEGIN
   Inherited Init(Bounds, ATitle, wnNoNumber);        { Call ancestor }
   Options := Options OR ofVersion20;                 { Version two dialog }
   GrowMode := 0;                                     { Clear grow mode }
   Flags := wfMove + wfClose;                         { Close/moveable flags }
   Palette := dpGrayDialog;                           { Default gray colours }
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
   GOptions := GOptions AND NOT goThickFramed;        { Turn thick frame off }
   ExStyle := ws_Ex_DlgModalFrame;                    { Set extended style }
   {$ENDIF}
END;

{--TDialog------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TDialog.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   If (Options AND ofVersion = ofVersion10) Then Begin
     Palette := dpGrayDialog;                         { Set gray palette }
     Options := Options OR ofVersion20;               { Update version flag }
   End;
END;

{--TDialog------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TDialog.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: Array[dpBlueDialog..dpGrayDialog] Of String =
    (CBlueDialog, CCyanDialog, CGrayDialog);          { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: Array[dpBlueDialog..dpGrayDialog] Of String[Length(CBlueDialog)] =
    (CBlueDialog, CCyanDialog, CGrayDialog);          { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P[Palette];                         { Return palette }
END;

{--TDialog------------------------------------------------------------------}
{  Valid -> Platforms DOS/DPMI/WIN/NT/Os2 - Updated 25Apr98 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TDialog.Valid (Command: Word): Boolean;
BEGIN
   If (Command = cmCancel) Then Valid := True         { Cancel returns true }
     Else Valid := TGroup.Valid(Command);             { Call group ancestor }
END;

{--TDialog------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TDialog.HandleEvent (Var Event: TEvent);
BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speed up exit }
     evKeyDown:                                       { Key down event }
       Case Event.KeyCode Of
         kbEsc: Begin                                 { Escape key press }
             Event.What := evCommand;                 { Command event }
             Event.Command := cmCancel;               { cancel command }
             Event.InfoPtr := Nil;                    { Clear info ptr }
             PutEvent(Event);                         { Put event on queue }
             ClearEvent(Event);                       { Clear the event }
           End;
         kbEnter: Begin                               { Enter key press }
             Event.What := evBroadcast;               { Broadcast event }
             Event.Command := cmDefault;              { Default command }
             Event.InfoPtr := Nil;                    { Clear info ptr }
             PutEvent(Event);                         { Put event on queue }
             ClearEvent(Event);                       { Clear the event }
           End;
       End;
     evCommand:                                       { Command event }
       Case Event.Command Of
         cmOk, cmCancel, cmYes, cmNo:                 { End dialog cmds }
           If (State AND sfModal <> 0) Then Begin     { View is modal }
             EndModal(Event.Command);                 { End modal state }
             ClearEvent(Event);                       { Clear the event }
           End;
       End;
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       TInputLine OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TInputLine---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TInputLine.Init (Var Bounds: TRect; AMaxLen: Integer);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   State := State OR sfCursorVis;                     { Cursor visible }
   Options := Options OR (ofSelectable + ofFirstClick
     + ofVersion20);                                  { Set options }
   If (MaxAvail > AMaxLen + 1) Then Begin             { Check enough memory }
     GetMem(Data, AMaxLen + 1);                       { Allocate memory }
     Data^ := '';                                     { Data = empty string }
   End;
   MaxLen := AMaxLen;                                 { Hold maximum length }
END;

{--TInputLine---------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TInputLine.Load (Var S: TStream);
VAR B: Byte;
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(MaxLen, 2);                                 { Read max length }
   S.Read(CurPos, 2);                                 { Read cursor position }
   S.Read(FirstPos, 2);                               { Read first position }
   S.Read(SelStart, 2);                               { Read selected start }
   S.Read(SelEnd, 2);                                 { Read selected end }
   S.Read(B, 1);                                      { Read string length }
   If (MaxAvail > MaxLen+1) Then Begin                { Check enough memory }
     GetMem(Data, MaxLen + 1);                        { Allocate memory }
     S.Read(Data^[1], Length(Data^));                 { Read string data }
     {$IFDEF PPC_DELPHI3}                             { DELPHI 3+ COMPILER }
     SetLength(Data^, B);                             { Xfer string length }
     {$ELSE}                                          { OTHER COMPILERS }
     Data^[0] := Chr(B);                              { Set string length }
     {$ENDIF}
   End Else S.Seek(S.GetPos + B);                     { Move to position }
   If (Options AND ofVersion >= ofVersion20) Then     { Version 2 or above }
     Validator := PValidator(S.Get);                  { Get any validator }
   Options := Options OR ofVersion20;                 { Set version 2 flag }
END;

{--TInputLine---------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TInputLine.Done;
BEGIN
   If (Data <> Nil) Then FreeMem(Data, MaxLen + 1);    { Release any memory }
   SetValidator(Nil);                                  { Clear any validator }
   Inherited Done;                                     { Call ancestor }
END;

{--TInputLine---------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TInputLine.DataSize: Word;
VAR DSize: Word;
BEGIN
   DSize := 0;                                        { Preset zero datasize }
   If (Validator <> Nil) AND (Data <> Nil) Then
     DSize := Validator^.Transfer(Data^, Nil,
       vtDataSize);                                   { Add validator size }
   If (DSize <> 0) Then DataSize := DSize             { Use validtor size }
     Else DataSize := MaxLen + 1;                     { No validator use size }
END;

{--TInputLine---------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TInputLine.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CInputLine;                         { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CInputLine)] = CInputLine;     { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TInputLine---------------------------------------------------------------}
{  Valid -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TInputLine.Valid (Command: Word): Boolean;

   FUNCTION AppendError (Validator: PValidator): Boolean;
   BEGIN
     AppendError := False;                            { Preset false }
     If (Data <> Nil) Then
       With Validator^ Do
         If (Options AND voOnAppend <> 0) AND         { Check options }
         (CurPos <> Length(Data^)) AND                { Exceeds max length }
         NOT IsValidInput(Data^, True) Then Begin     { Check data valid }
           Error;                                     { Call error }
           AppendError := True;                       { Return true }
         End;
   END;

BEGIN
   Valid := Inherited Valid(Command);                 { Call ancestor }
   If (Validator <> Nil) AND (Data <> Nil) AND        { Validator present }
   (State AND sfDisabled = 0) Then                    { Not disabled }
     If (Command = cmValid) Then                      { Valid command }
       Valid := Validator^.Status = vsOk              { Validator result }
       Else If (Command <> cmCancel) Then             { Not cancel command }
         If AppendError(Validator) OR                 { Append any error }
         NOT Validator^.Valid(Data^) Then Begin       { Check validator }
           Select;                                    { Reselect view }
           Valid := False;                            { Return false }
         End;
END;

{--TInputLine---------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.Draw;
VAR Color: Byte; X, L, R: Integer; S, T: String;
BEGIN
   If (State AND sfFocused = 0) Then Color := 1       { Not focused colour }
     Else Color := 2;                                 { Focused colour }
   If CanScroll(-1) Then WriteStr(0, 0, LeftArr, 4);  { Set left scroll mark }
   If CanScroll(1) Then WriteStr(-(RawSize.X + 1 -
     TextWidth(RightArr)), 0, RightArr, 4);           { Set right scroll mark }
   If (Data <> Nil) Then S := Copy(Data^, FirstPos+1,
    Length(Data^)-FirstPos) Else S := '';             { Fetch data string }
   X := TextWidth(LeftArr);                           { left arrow width }
   While (TextWidth(S) > ((RawSize.X+1)-X-TextWidth(
     RightArr))) Do Delete(S, Length(S), 1);          { Cut to right length }
   If (State AND sfFocused <> 0) Then Begin
     L := SelStart - FirstPos;                        { Selected left end }
     R := SelEnd - FirstPos;                          { Selected right end }
     If (L < 0) Then L := 0;                          { Fix any negative }
     If (R > Length(S)) Then R := Length(S);          { Fix to long case }
     If (L > 0) Then Begin
       T := Copy(S, 1, L);                            { Unhighlight bit }
       WriteStr(-X, 0, T, Color);                     { Write string to screen }
       X := X + TextWidth(T);                         { New x position }
       Delete(S, 1, L);                               { Reduce string }
     End;
     If (L < R) Then Begin
       T := Copy(S, 1, R-L);                          { Highlight bit }
       WriteStr(-X, 0, T, 3);                         { Write string to screen }
       X := X + TextWidth(T);                         { New x position }
       Delete(S, 1, R-L);                             { Reduce string }
     End;
     If (Length(S) > 0) Then
       WriteStr(-X, 0, S, Color);                     { Write string to screen }
   End Else WriteStr(-X, 0, S, Color);                { Write string to screen }
   Cursor.X := CurPos - FirstPos + 1;                 { Update cursor position }
END;

{--TInputLine---------------------------------------------------------------}
{  DrawbackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.DrawBackGround;
BEGIN
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
   If (HWindow <> 0) Then DestroyCaret;               { Destroy any caret }
   {$ENDIF}
   {$IFDEF OS_OS2}                                    { OS2 CODE }
   If (HWindow <> 0) Then WinDestroyCursor(HWindow);  { Destroy any caret }
   {$ENDIF}
   Inherited DrawBackGround;                          { Call ancestor }
END;

{--TInputLine---------------------------------------------------------------}
{  DrawCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.DrawCursor;
VAR I, X: Integer; S: String;
BEGIN
   If (State AND sfFocused <> 0) Then Begin           { Focused window }
     X := TextWidth(LeftArr);                         { Preset x position }
     I := 0;                                          { Preset cursor width }
     If (Data <> Nil) Then Begin                      { Data pointer valid }
       S := Copy(Data^, FirstPos+1, CurPos-FirstPos); { Copy the string }
       X := X + TextWidth(S);                         { Calculate position }
       If (State AND sfCursorIns <> 0) Then           { Check insert mode }
         If ((CurPos+1) <= Length(Data^)) Then
           I := TextWidth(Data^[CurPos+1])            { Insert caret width }
           Else I := FontWidth;                       { At end use fontwidth }
     End;
     {$IFDEF OS_DOS}
     If (State AND sfCursorIns <> 0) Then Begin       { Insert mode }
       If ((CurPos+1) <= Length(Data^)) Then          { Not beyond end }
         WriteStr(-X, 0, Data^[CurPos+1], 5)          { Create block cursor }
         Else ClearArea(X, 0, X+I, FontHeight, Green);{ Line cursor }
     End Else ClearArea(X, 0, X+I, FontHeight, Green);{ Line cursor }
     {$ENDIF}
     {$IFDEF OS_WINDOWS}                              { WIN/NT CODE }
     If (HWindow <> 0) Then Begin
       CreateCaret(HWindow, 0, I, FontHeight);        { Create a craet }
       SetCaretPos(X, 0);                             { Set caret position }
       If (State AND sfCursorVis <> 0) Then
         ShowCaret(HWindow);                          { Show the caret }
     End;
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 CODE }
     If (HWindow <> 0) Then Begin
       WinCreateCursor(HWindow, X, 0, 0, FontHeight,
         CURSOR_FLASH, Nil);                          { Create a craet }
       If (State AND sfCursorVis <> 0) Then
         WinShowCursor(HWindow, True);                { Show the caret }
     End;
     {$ENDIF}
   End;
END;

{--TInputLine---------------------------------------------------------------}
{  SelectAll -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.SelectAll (Enable: Boolean);
BEGIN
   CurPos := 0;                                       { Cursor to start }
   FirstPos := 0;                                     { First pos to start }
   SelStart := 0;                                     { Selected at start }
   If Enable AND (Data <> Nil) Then
     SelEnd := Length(Data^) Else SelEnd := 0;        { Selected which end }
   DrawView;                                          { Now redraw the view }
END;

{--TInputLine---------------------------------------------------------------}
{  SetValidator -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.SetValidator (AValid: PValidator);
BEGIN
   If (Validator <> Nil) Then Validator^.Free;        { Release validator }
   Validator := AValid;                               { Set new validator }
END;

{--TInputLine---------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.SetState (AState: Word; Enable: Boolean);
BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState = sfSelected) OR ((AState = sfActive)
   AND (State and sfSelected <> 0)) Then
     SelectAll(Enable) Else                           { Call select all }
     If (AState = sfFocused) Then DrawView;           { Redraw for focus }
END;

{--TInputLine---------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.GetData (Var Rec);
BEGIN
   If (Data <> Nil) Then Begin                        { Data ptr valid }
     If (Validator = Nil) OR (Validator^.Transfer(Data^,
     @Rec, vtGetData) = 0) Then Begin                 { No validator/data }
       FillChar(Rec, DataSize, #0);                   { Clear the data area }
       Move(Data^, Rec, Length(Data^) + 1);           { Transfer our data }
     End;
   End Else FillChar(Rec, DataSize, #0);              { Clear the data area }
END;

{--TInputLine---------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.SetData (Var Rec);
{$IFDEF PPC_DELPHI3} VAR Buf: Array [0..256] Of Char; {$ENDIF}
BEGIN
   If (Data <> Nil) Then Begin                        { Data ptr valid }
     If (Validator = Nil) OR (Validator^.Transfer(
       Data^, @Rec, vtSetData) = 0) Then              { No validator/data }
       {$IFDEF PPC_DELPHI3}                           { DELPHI3+ COMPILER }
       Move(Rec, Buf, DataSize);                      { Fetch our data }
       Move(Buf[1], Data^[1], Ord(Buf[0]));           { Tranfer string }
       SetLength(Data^, Ord(Buf[0]));                 { Set string length }
       {$ELSE}                                        { OTHER COMPILERS }
       Move(Rec, Data^[0], DataSize);                 { Set our data }
       {$ENDIF}
   End;
   SelectAll(True);                                   { Now select all }
END;

{--TInputLine---------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { Implict TView.Store }
   S.Write(MaxLen, 2);                                { Read max length }
   S.Write(CurPos, 2);                                { Read cursor position }
   S.Write(FirstPos, 2);                              { Read first position }
   S.Write(SelStart, 2);                              { Read selected start }
   S.Write(SelEnd, 2);                                { Read selected end }
   S.WriteStr(Data);                                  { Write the data }
   S.Put(Validator);                                  { Write any validator }
END;

{--TInputLine---------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TInputLine.HandleEvent (Var Event: TEvent);
CONST PadKeys = [$47, $4B, $4D, $4F, $73, $74];
VAR WasAppending: Boolean; ExtendBlock: Boolean; OldData: String;
Delta, Anchor, OldCurPos, OldFirstPos, OldSelStart, OldSelEnd: Integer;

   FUNCTION MouseDelta: Integer;
   BEGIN
     If (Event.Where.X <= RawOrigin.X+TextWidth(LeftArr))
       Then MouseDelta := -1 Else                     { To left of text area }
       If ((Event.Where.X-RawOrigin.X) >= RawSize.X -
       TextWidth(RightArr)) Then MouseDelta := 1      { To right of text area }
         Else MouseDelta := 0;                        { In area return 0 }
   END;

   FUNCTION MousePos: Integer;
   VAR Mp, Tw, Pos: Integer; S: String;
   BEGIN
     Mp := Event.Where.X - RawOrigin.X;               { Mouse position }
     If (Data <> Nil) Then S := Copy(Data^, FirstPos+1,
       Length(Data^)-FirstPos) Else S := '';          { Text area string }
     Tw := TextWidth(LeftArr);                        { Text width }
     Pos := 0;                                        { Zero position }
     While (Mp > Tw) AND (Pos <= Length(S)) Do Begin  { Still text to right }
       Tw := Tw + TextWidth(S[Pos+1]);                { Add next character }
       Inc(Pos);                                      { Next character }
     End;
     If (Pos > 0) Then Dec(Pos);
     MousePos := FirstPos + Pos;                      { Return mouse position }
   END;

   PROCEDURE DeleteSelect;
   BEGIN
     If (SelStart <> SelEnd) Then Begin               { An area selected }
       If (Data <> Nil) Then
         Delete(Data^, SelStart+1, SelEnd-SelStart);  { Delete the text }
       CurPos := SelStart;                            { Set cursor position }
     End;
   END;

   PROCEDURE AdjustSelectBlock;
   BEGIN
     If (CurPos < Anchor) Then Begin                  { Selection backwards }
       SelStart := CurPos;                            { Start of select }
       SelEnd := Anchor;                              { End of select }
     End Else Begin
       SelStart := Anchor;                            { Start of select }
       SelEnd := CurPos;                              { End of select }
     End;
   END;

   PROCEDURE SaveState;
   BEGIN
     If (Validator <> Nil) Then Begin                 { Check for validator }
       If (Data <> Nil) Then OldData := Data^;        { Hold data }
       OldCurPos := CurPos;                           { Hold cursor position }
       OldFirstPos := FirstPos;                       { Hold first position }
       OldSelStart := SelStart;                       { Hold select start }
       OldSelEnd := SelEnd;                           { Hold select end }
       If (Data = Nil) Then WasAppending := True      { Invalid data ptr }
         Else WasAppending := Length(Data^) = CurPos; { Hold appending state }
     End;
   END;

   PROCEDURE RestoreState;
   BEGIN
     If (Validator <> Nil) Then Begin                 { Validator valid }
       If (Data <> Nil) Then Data^ := OldData;        { Restore data }
       CurPos := OldCurPos;                           { Restore cursor pos }
       FirstPos := OldFirstPos;                       { Restore first pos }
       SelStart := OldSelStart;                       { Restore select start }
       SelEnd := OldSelEnd;                           { Restore select end }
     End;
   END;

   FUNCTION CheckValid (NoAutoFill: Boolean): Boolean;
   VAR OldLen: Integer; NewData: String;
   BEGIN
     If (Validator <> Nil) Then Begin                 { Validator valid }
       CheckValid := False;                           { Preset false return }
       If (Data <> Nil) Then OldLen := Length(Data^); { Hold old length }
       If (Validator^.Options AND voOnAppend = 0) OR
       (WasAppending AND (CurPos = OldLen)) Then Begin
         If (Data <> Nil) Then NewData := Data^       { Hold current data }
           Else NewData := '';                        { Set empty string }
         If NOT Validator^.IsValidInput(NewData,
         NoAutoFill) Then RestoreState Else Begin
           If (Length(NewData) > MaxLen) Then         { Exceeds maximum }
             {$IFDEF PPC_DELPHI3}                     { DELPHI 3+ COMPILER }
             SetLength(NewData, MaxLen);              { Set string length }
             {$ELSE}                                  { OTHER COMPILERS }
             NewData[0] := Chr(MaxLen);               { Set string length }
             {$ENDIF}
           If (Data <> Nil) Then Data^ := NewData;    { Set data value }
           If (Data <> Nil) AND (CurPos >= OldLen)    { Cursor beyond end }
           AND (Length(Data^) > OldLen) Then          { Cursor beyond string }
             CurPos := Length(Data^);                 { Set cursor position }
           CheckValid := True;                        { Return true result }
         End;
       End Else Begin
         CheckValid := True;                          { Preset true return }
         If (CurPos = OldLen) AND (Data <> Nil) Then  { Lengths match }
           If NOT Validator^.IsValidInput(Data^,
           False) Then Begin                          { Check validator }
             Validator^.Error;                        { Call error }
             CheckValid := False;                     { Return false result }
           End;
       End;
     End Else CheckValid := True;                     { No validator }
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   If (State AND sfSelected <> 0) Then Begin          { View is selected }
     Case Event.What Of
       evNothing: Exit;                               { Speed up exit }
       evMouseDown: Begin                             { Mouse down event }
         Delta := MouseDelta;                         { Calc scroll value }
         If CanScroll(Delta) Then Begin               { Can scroll }
           Repeat
             If CanScroll(Delta) Then Begin           { Still can scroll }
               Inc(FirstPos, Delta);                  { Move start position }
               DrawView;                              { Redraw the view }
             End;
           Until NOT MouseEvent(Event, evMouseAuto);  { Until no mouse auto }
         End Else If Event.Double Then                { Double click }
           SelectAll(True) Else Begin                 { Select whole text }
             Anchor := MousePos;                      { Start of selection }
             Repeat
               {$IFDEF OS_DOS}                        { DOS/DPMI CODE }
               If (Event.What = evMouseAuto)          { Mouse auto event }
               {$ELSE}                                { WIN/NT/OS2 CODE }
               If (Event.What = evMouseMove)          { Mouse move event }
               {$ENDIF}
               Then Begin
                 Delta := MouseDelta;                 { New position }
                 If CanScroll(Delta) Then             { If can scroll }
                   Inc(FirstPos, Delta);
               End;
               CurPos := MousePos;                    { Set cursor position }
               AdjustSelectBlock;                     { Adjust selected }
               DrawView;                              { Redraw the view }
             Until NOT MouseEvent(Event, evMouseMove
               + evMouseAuto);                        { Until mouse released }
           End;
         ClearEvent(Event);                           { Clear the event }
       End;
       evKeyDown: Begin
         SaveState;                                   { Save state of view }
         Event.KeyCode := CtrlToArrow(Event.KeyCode); { Convert keycode }
         If (Event.ScanCode IN PadKeys) AND
         (GetShiftState AND $03 <> 0) Then Begin      { Mark selection active }
           Event.CharCode := #0;                      { Clear char code }
           If (CurPos = SelEnd) Then                  { Find if at end }
             Anchor := SelStart Else                  { Anchor from start }
             Anchor := SelEnd;                        { Anchor from end }
             ExtendBlock := True;                     { Extended block true }
         End Else ExtendBlock := False;               { No extended block }
         Case Event.KeyCode Of
           kbLeft: If (CurPos > 0) Then Dec(CurPos);  { Move cursor left }
           kbRight: If (Data <> Nil) AND              { Move right cursor }
           (CurPos < Length(Data^)) Then Begin        { Check not at end }
             Inc(CurPos);                             { Move cursor }
             CheckValid(True);                        { Check if valid }
           End;
           kbHome: CurPos := 0;                       { Move to line start }
           kbEnd: Begin                               { Move to line end }
             If (Data = Nil) Then CurPos := 0         { Invalid data ptr }
               Else CurPos := Length(Data^);          { Set cursor position }
             CheckValid(True);                        { Check if valid }
           End;
           kbBack: If (Data <> Nil) AND (CurPos > 0)  { Not at line start }
           Then Begin
             Delete(Data^, CurPos, 1);                { Backspace over char }
             Dec(CurPos);                             { Move cursor back one }
             If (FirstPos > 0) Then Dec(FirstPos);    { Move first position }
             CheckValid(True);                        { Check if valid }
           End;
           kbDel: If (Data <> Nil) Then Begin         { Delete character }
             If (SelStart = SelEnd) Then              { Select all on }
               If (CurPos < Length(Data^)) Then Begin { Cursor not at end }
                 SelStart := CurPos;                  { Set select start }
                 SelEnd := CurPos + 1;                { Set select end }
               End;
             DeleteSelect;                            { Deselect selection }
             CheckValid(True);                        { Check if valid }
           End;
           kbIns: SetState(sfCursorIns, State AND
             sfCursorIns = 0);                        { Flip insert state }
           Else Case Event.CharCode Of
             ' '..#255: If (Data <> Nil) Then Begin   { Character key }
               If (State AND sfCursorIns <> 0) Then
                 Delete(Data^, CurPos + 1, 1) Else    { Overwrite character }
                 DeleteSelect;                        { Deselect selected }
               If CheckValid(True) Then Begin         { Check data valid }
                 If (Length(Data^) < MaxLen) Then     { Must not exceed maxlen }
                 Begin
                   If (FirstPos > CurPos) Then
                     FirstPos := CurPos;              { Advance first position }
                   Inc(CurPos);                       { Increment cursor }
                   Insert(Event.CharCode, Data^,
                     CurPos);                         { Insert the character }
                 End;
                 CheckValid(False);                   { Check data valid }
               End;
             End;
             ^Y: If (Data <> Nil) Then Begin          { Clear all data }
                Data^ := '';                          { Set empty string }
                CurPos := 0;                          { Cursor to start }
             End;
             Else Exit;                               { Unused key }
           End
         End;
         If ExtendBlock Then AdjustSelectBlock        { Extended block }
         Else Begin
           SelStart := CurPos;                        { Set select start }
           SelEnd := CurPos;                          { Set select end }
         End;
         If (FirstPos > CurPos) Then
           FirstPos := CurPos;                        { Advance first pos }
         If (Data <> Nil) Then OldData := Copy(Data^,
           FirstPos+1, CurPos-FirstPos)               { Text area string }
           Else OldData := '';                        { Empty string }
         {$IFDEF OS_DOS}                              { DOS/DPMI CODE }
         Delta := FontWidth;                          { Safety = 1 char }
         {$ELSE}                                      { WIN/NT CODE }
         Delta := 2*FontWidth;                        { Safety = 2 char }
         {$ENDIF}
         While (TextWidth(OldData) > ((RawSize.X+1)-Delta)
         - TextWidth(LeftArr) - TextWidth(RightArr))  { Check text fits }
         Do Begin
           Inc(FirstPos);                             { Advance first pos }
           OldData := Copy(Data^, FirstPos+1,
             CurPos-FirstPos)                         { Text area string }
         End;
         DrawView;                                    { Redraw the view }
         ClearEvent(Event);                           { Clear the event }
       End;
     End;
   End;
END;

{***************************************************************************}
{                     TInputLine OBJECT PRIVATE METHODS                     }
{***************************************************************************}
{--TInputLine---------------------------------------------------------------}
{  CanScroll -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TInputLine.CanScroll (Delta: Integer): Boolean;
VAR S: String;
BEGIN
   If (Delta < 0) Then CanScroll := FirstPos > 0      { Check scroll left }
     Else If (Delta > 0) Then Begin
       If (Data = Nil) Then S := '' Else              { Data ptr invalid }
         S := Copy(Data^, FirstPos+1, Length(Data^)
          - FirstPos);                                { Fetch max string }
       CanScroll := (TextWidth(S)) > (RawSize.X -
         TextWidth(LeftArr) - TextWidth(RightArr));   { Check scroll right }
     End Else CanScroll := False;                     { Zero so no scroll }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TButton OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TButton------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TButton.Init (Var Bounds: TRect; ATitle: TTitleStr;
  ACommand: Word; AFlags: Word);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   EventMask := EventMask OR evBroadcast;             { Handle broadcasts }
   GOptions := GOptions OR goDrawFocus;               { Set new option mask }
   Options := Options OR (ofSelectable + ofFirstClick
     + ofPreProcess + ofPostProcess);                 { Set option flags }
   If NOT CommandEnabled(ACommand) Then
     State := State OR sfDisabled;                    { Check command state }
   Flags := AFlags;                                   { Hold flags }
   If (AFlags AND bfDefault <> 0) Then AmDefault := True
     Else AmDefault := False;                         { Check if default }
   Title := NewStr(ATitle);                           { Hold title string }
   Command := ACommand;                               { Hold button command }
   TabMask := TabMask OR (tmLeft + tmRight +
     tmTab + tmShiftTab + tmUp + tmDown);             { Set tab masks }
END;

{--TButton------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TButton.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   Title := S.ReadStr;                                { Read title }
   S.Read(Command, 2);                                { Read command }
   S.Read(Flags, 1);                                  { Read flags }
   S.Read(AmDefault, 1);                              { Read if default }
   If NOT CommandEnabled(Command) Then                { Check command state }
     State := State OR sfDisabled Else                { Command disabled }
     State := State AND NOT sfDisabled;               { Command enabled }
END;

{--TButton------------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TButton.Done;
BEGIN
   If (Title <> Nil) Then DisposeStr(Title);          { Dispose title }
   Inherited Done;                                    { Call ancestor }
END;

{--TButton------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Apr98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TButton.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CButton;                            { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CButton)] = CButton;           { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Get button palette }
END;

{--TButton------------------------------------------------------------------}
{  Press -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TButton.Press;
VAR E: TEvent;
BEGIN
   Message(Owner, evBroadcast, cmRecordHistory, Nil); { Message for history }
   If (Flags AND bfBroadcast <> 0) Then               { Broadcasting button }
     Message(Owner, evBroadcast, Command, @Self)      { Send message }
     Else Begin
       E.What := evCommand;                           { Command event }
       E.Command := Command;                          { Set command value }
       E.InfoPtr := @Self;                            { Pointer to self }
       PutEvent(E);                                   { Put event on queue }
     End;
END;

{--TButton------------------------------------------------------------------}
{  DrawFocus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TButton.DrawFocus;
VAR B: Byte; I: Integer; Bc: Word; Db: TDrawBuffer;
BEGIN
   If DownFlag Then B := 7 Else B := 0;               { Shadow colour }
   GraphRectangle(0, 0, RawSize.X, RawSize.Y, B);     { Draw backing shadow }
   GraphRectangle(1, 1, RawSize.X-1, RawSize.Y-1, B); { Draw backing shadow }
   If DownFlag Then B := 0 Else B := 15;              { Highlight colour }
   GraphLine(0, RawSize.Y, 0, 0, B);
   GraphLine(1, RawSize.Y-1, 1, 1, B);                { Left highlights }
   GraphLine(0, 0, RawSize.X, 0, B);
   GraphLine(1, 1, RawSize.X-1, 1, B);                { Top highlights }
   If DownFlag Then B := 8 Else B := 7;               { Select backing }
   If (State AND sfFocused <> 0) AND
     (DownFlag = False) Then B := 14;                 { Show as focused }
   GraphRectangle(2, 2, RawSize.X-2, RawSize.Y-2, B); { Draw first border }
   GraphRectangle(3, 3, RawSize.X-3, RawSize.Y-3, B); { Draw next border }
   If (State AND sfDisabled <> 0) Then                { Button disabled }
     Bc := GetColor($0404) Else Begin                 { Disabled colour }
       Bc := GetColor($0501);                         { Set normal colour }
       If (State AND sfActive <> 0) Then              { Button is active }
         If (State AND sfSelected <> 0) Then
           Bc := GetColor($0703) Else                 { Set selected colour }
             If AmDefault Then Bc := GetColor($0602); { Set is default colour }
     End;
   If (Title <> Nil) Then Begin                       { We have a title }
     If (Flags AND bfLeftJust = 0) Then Begin         { Not left set title }
       I := TextWidth(Title^);                        { Fetch title width }
       I := (RawSize.X - I) DIV 2;                    { Centre in button }
     End Else I := FontWidth;                         { Left edge of button }
     MoveCStr(Db, Title^, Bc);                        { Move title to buffer }
     GOptions := GOptions OR goGraphView;             { Graphics co-ords mode }
     WriteLine(I, FontHeight DIV 2, CStrLen(Title^),
       1, Db);                                        { Write the title }
     GOptions := GOptions AND NOT goGraphView;        { Return to normal mode }
   End;
END;

{--TButton------------------------------------------------------------------}
{  DrawState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TButton.DrawState (Down: Boolean);
BEGIN
   DownFlag := Down;                                  { Set down flag }
   SetDrawMask(vdFocus);                              { Set focus mask }
   DrawView;                                          { Redraw the view }
END;

{--TButton------------------------------------------------------------------}
{  MakeDefault -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TButton.MakeDefault (Enable: Boolean);
VAR C: Word;
BEGIN
   If (Flags AND bfDefault=0) Then Begin              { Not default }
     If Enable Then C := cmGrabDefault
       Else C := cmReleaseDefault;                    { Change default }
     Message(Owner, evBroadcast, C, @Self);           { Message to owner }
     AmDefault := Enable;                             { Set default flag }
     DrawView;                                        { Now redraw button }
   End;
END;

{--TButton------------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Oct99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TButton.SetState (AState: Word; Enable: Boolean);
BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState AND (sfSelected + sfActive) <> 0)       { Changing select }
     Then DrawView;                                   { Redraw required }
   If (AState AND sfFocused <> 0) Then
     MakeDefault(Enable);                             { Check for default }
END;

{--TButton------------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TButton.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { Implict TView.Store }
   S.WriteStr(Title);                                 { Store title string }
   S.Write(Command, 2);                               { Store command }
   S.Write(Flags, 1);                                 { Store flags }
   S.Write(AmDefault, 1);                             { Store default flag }
END;

{--TButton------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Sep99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TButton.HandleEvent (Var Event: TEvent);
VAR Down: Boolean; C: Char; ButRect: TRect;
BEGIN
   ButRect.A := RawOrigin;                            { Get origin point }
   ButRect.B.X := RawOrigin.X + RawSize.X;            { Calc right side }
   ButRect.B.Y := RawOrigin.Y + RawSize.Y;            { Calc bottom }
   If (Event.What = evMouseDown) Then Begin           { Mouse down event }
     If NOT MouseInView(Event.Where) Then Begin       { If point not in view }
       ClearEvent(Event);                             { Clear the event }
       Exit;                                          { Speed up exit }
     End;
   End;
   If (Flags AND bfGrabFocus <> 0) Then               { Check focus grab }
     Inherited HandleEvent(Event);                    { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speed up exit }
     evMouseDown: Begin
       If (State AND sfDisabled = 0) Then Begin       { Button not disabled }
         Down := False;                               { Clear down flag }
         Repeat
           If (Down <> ButRect.Contains(Event.Where)) { State has changed }
           Then Begin
             Down := NOT Down;                        { Invert down flag }
             DrawState(Down);                         { Redraw button }
           End;
         Until NOT MouseEvent(Event, evMouseMove);    { Wait for mouse move }
         If Down Then Begin                           { Button is down }
           Press;                                     { Send out command }
           DrawState(False);                          { Draw button up }
         End;
       End;
       ClearEvent(Event);                             { Event was handled }
     End;
     evKeyDown: Begin
       If (Title <> Nil) Then C := HotKey(Title^)     { Key title hotkey }
         Else C := #0;                                { Invalid title }
       If (Event.KeyCode = GetAltCode(C)) OR          { Alt char }
       (Owner^.Phase = phPostProcess) AND (C <> #0)
       AND (Upcase(Event.CharCode) = C) OR            { Matches hotkey }
       (State AND sfFocused <> 0) AND                 { View focused }
       ((Event.CharCode = ' ') OR                     { Space bar }
       (Event.KeyCode=kbEnter)) Then Begin            { Enter key }
         DrawState(True);                             { Draw button down }
         Press;                                       { Send out command }
         ClearEvent(Event);                           { Clear the event }
         DrawState(False);                            { Draw button up }
       End;
     End;
     evBroadcast:
       Case Event.Command of
         cmDefault: If AmDefault AND                  { Default command }
         (State AND sfDisabled = 0) Then Begin        { Button enabled }
             Press;                                   { Send out command }
             ClearEvent(Event);                       { Clear the event }
         End;
         cmGrabDefault, cmReleaseDefault:             { Grab and release cmd }
           If (Flags AND bfDefault <> 0) Then Begin   { Change button state }
             AmDefault := Event.Command = cmReleaseDefault;
             DrawView;                                { Redraw the view }
           End;
         cmCommandSetChanged: Begin                   { Command set changed }
           SetState(sfDisabled, NOT
             CommandEnabled(Command));                { Set button state }
            DrawView;                                 { Redraw the view }
         End;
       End;
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TCluster OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

CONST TvClusterClassName = 'TVCLUSTER';

{--TCluster-----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TCluster.Init (Var Bounds: TRect; AStrings: PSItem);
VAR I: Integer; P: PSItem;
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   GOptions := GOptions OR goDrawFocus;               { Draw focus view }
   Options := Options OR (ofSelectable + ofFirstClick
     + ofPreProcess + ofPostProcess + ofVersion20);   { Set option masks }
   I := 0;                                            { Zero string count }
   P := AStrings;                                     { First item }
   While (P <> Nil) Do Begin
     Inc(I);                                          { Count 1 item }
     P := P^.Next;                                    { Move to next item }
   End;
   Strings.Init(I, 0);                                { Create collection }
   While (AStrings <> Nil) Do Begin
     P := AStrings;                                   { Transfer item ptr }
     Strings.AtInsert(Strings.Count, AStrings^.Value);{ Insert string }
     AStrings := AStrings^.Next;                      { Move to next item }
     Dispose(P);                                      { Dispose prior item }
   End;
   EnableMask := $FFFFFFFF;                           { Enable bit masks }
END;

{--TCluster-----------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TCluster.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Value, 4);                                  { Read value }
   S.Read(Sel, 2);                                    { Read select item }
   If ((Options AND ofVersion) >= ofVersion20)        { Version 2 TV view }
   Then S.Read(EnableMask, 4) Else Begin              { Read enable masks }
     EnableMask := $FFFFFFFF;                         { Enable all masks }
     Options := Options OR ofVersion20;               { Set version 2 mask }
   End;
   If (Options AND ofGFVModeView <> 0) Then           { GFV mode view check }
     S.Read(Id, 2);                                   { Read view id }
   Strings.Load(S);                                   { Load string data }
   SetButtonState(0, True);                           { Set button state }
END;

{--TCluster-----------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TCluster.Done;
VAR I: Integer;
BEGIN
   {$IFNDEF OS_DOS}                                   { WIN/NT/OS2 CODE }
   If (WndHandles <> Nil) Then Begin                  { Handles valid }
     For I := 1 To Strings.Count Do                   { For each entry }
       {$IFDEF OS_WINDOWS}                            { WIN/NT CODE }
       DestroyWindow(WndHandles^[I-1]);               { Destroy button views }
       {$ELSE}                                        { OS2 CODE }
       WinDestroyWindow(WndHandles^[I-1]);            { Destroy button views }
       {$ENDIF}
     FreeMem(WndHandles, Strings.Count*SizeOf(HWnd)); { Release memory }
   End;
   {$ENDIF}
   Strings.Done;                                      { Dispose of strings }
   Inherited Done;                                    { Call ancestor }
END;

{--TCluster-----------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TCluster.DataSize: Word;
BEGIN
   DataSize := SizeOf(Word);                          { Exchanges a word }
END;

{--TCluster-----------------------------------------------------------------}
{  GetHelpCtx -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TCluster.GetHelpCtx: Word;
BEGIN
   If (HelpCtx = hcNoContext) Then                    { View has no help }
     GetHelpCtx := hcNoContext Else                   { No help context }
     GetHelpCtx := HelpCtx + Sel;                     { Help of selected }
END;

{--TCluster-----------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TCluster.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CCluster;                           { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CCluster)] = CCluster;         { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Cluster palette }
END;

{--TCluster-----------------------------------------------------------------}
{  Mark -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TCluster.Mark (Item: Integer): Boolean;
BEGIN
   Mark := False;                                     { Default false }
END;

{--TCluster-----------------------------------------------------------------}
{  MultiMark -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TCluster.MultiMark (Item: Integer): Byte;
BEGIN
   MultiMark := Byte(Mark(Item) = True);              { Return multi mark }
END;

{--TCluster-----------------------------------------------------------------}
{  ButtonState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB       }
{---------------------------------------------------------------------------}
FUNCTION TCluster.ButtonState (Item: Integer): Boolean;
BEGIN
   If (Item > 31) Then ButtonState := False Else      { Impossible item }
     ButtonState := ((1 SHL Item) AND EnableMask)<>0; { Return true/false }
END;

{--TCluster-----------------------------------------------------------------}
{  DrawFocus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Jul99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.DrawFocus;
BEGIN
   {$IFNDEF OS_DOS}                                   { WIN/NT/OS2 CODE }
   If (WndHandles <> Nil) Then                        { Valid window handles }
     If (State AND sfFocused <> 0) Then Begin         { View is focused }
       If (Sel >= 0) AND (Sel < Strings.Count) Then
         {$IFDEF OS_WINDOWS}                          { WIN/NT CODE }
         SetFocus(WndHandles^[Sel])                   { Focus selected view }
           Else SetFocus(AppWindow);                  { Focus owner }
         {$ELSE}                                      { OS2 CODE }
         WinSetFocus(HWND_DESKTOP, WndHandles^[Sel])  { Focus selected view }
           Else WinSetFocus(HWND_DESKTOP, HWindow);   { Focus owner }
         {$ENDIF}
     End Else
       {$IFDEF OS_WINDOWS}                            { WIN/NT CODE }
       SetFocus(AppWindow);                           { Focus owner }
       {$ELSE}                                        { OS2 CODE }
       WinSetFocus(HWND_DESKTOP, AppWindow);          { Focus owner }
       {$ENDIF}
   {$ENDIF}
END;

{--TCluster-----------------------------------------------------------------}
{  Press -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.Press (Item: Integer);
VAR P: PView;
BEGIN
   P := TopView;
   If (Id <> 0) AND (P <> Nil) Then NewMessage(P,
     evCommand, cmIdCommunicate, Id, Value, @Self);   { Send new message }
END;

{--TCluster-----------------------------------------------------------------}
{  MovedTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.MovedTo (Item: Integer);
BEGIN                                                 { Abstract method }
END;

{--TCluster-----------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.SetState (AState: Word; Enable: Boolean);
BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState AND sfFocused <> 0) Then Begin
     SetDrawMask(vdFocus OR vdInner);                 { Set redraw masks }
     DrawView;                                        { Redraw masked areas }
   End;
END;

{--TCluster-----------------------------------------------------------------}
{  DrawMultiBox -> Platforms DOS/DPMI/WIN/NT - Updated 05Jun98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.DrawMultiBox (Const Icon, Marker: String);
VAR I, J, K, Cur, Col: Integer; CNorm, CSel, CDis, Color: Word; B: TDrawBuffer;
    Tb, SCOff: Byte;
{$IFNDEF OS_DOS} S: String; P: PString; Q: PChar; {$ENDIF}
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   CNorm := GetColor($0301);                          { Normal colour }
   CSel := GetColor($0402);                           { Selected colour }
   CDis := GetColor($0505);                           { Disabled colour }
   If (Options AND ofFramed <>0) OR                   { Normal frame }
   (GOptions AND goThickFramed <>0) Then              { Thick frame }
     K := 1 Else  K := 0;                             { Select offset }
   For I := 0 To Size.Y-K-K-1 Do Begin                { For each line }
     MoveChar(B, ' ', Byte(CNorm), Size.X-K-K);       { Fill buffer }
     For J := 0 To (Strings.Count - 1) DIV Size.Y + 1
     Do Begin
       Cur := J*Size.Y + I;                           { Current line }
       If (Cur < Strings.Count) Then Begin
         Col := Column(Cur);                          { Calc column }
         If (Col + CStrLen(PString(Strings.At(Cur))^)+
         5 < Sizeof(TDrawBuffer) DIV SizeOf(Word))
         AND (Col < Size.X-K-K) Then Begin            { Text fits in column }
           If NOT ButtonState(Cur) Then
             Color := CDis Else If (Cur = Sel) AND    { Disabled colour }
             (State and sfFocused <> 0) Then
               Color := CSel Else                     { Selected colour }
               Color := CNorm;                        { Normal colour }
           MoveChar(B[Col], ' ', Byte(Color),
             Size.X-K-K-Col);                         { Set this colour }
           MoveStr(B[Col], Icon, Byte(Color));        { Transfer icon string }
           WordRec(B[Col+2]).Lo := Byte(Marker[
             MultiMark(Cur) + 1]);                    { Transfer marker }
           MoveCStr(B[Col+5], PString(Strings.At(
             Cur))^, Color);                          { Transfer item string }
           If ShowMarkers AND (State AND sfFocused <> 0)
           AND (Cur = Sel) Then Begin                 { Current is selected }
             WordRec(B[Col]).Lo := Byte(SpecialChars[0]);
              WordRec(B[Column(Cur+Size.Y)-1]).Lo
                := Byte(SpecialChars[1]);             { Set special character }
           End;
         End;
       End;
     End;
     WriteBuf(K, K+I, Size.X-K-K, 1, B);              { Write buffer }
   End;
   {$ELSE}                                            { WIN/NT/OS2 CODE }
   If (WndHandles <> Nil) Then Begin                  { Valid window handles }
     For I := 1 To Strings.Count Do Begin             { For each window }
       {$IFDEF OS_WINDOWS}                            { WIN/NT CODE }
       Tb := GetWindowText(WndHandles^[I-1], @S[1],
         255);                                        { Get window text }
       {$ELSE}                                        { OS2 CODE }
       Tb := WinQueryWindowText(WndHandles^[I-1], 255,
         @S[1]);                                      { Get window text }
       {$ENDIF}
       {$IFDEF PPC_DELPHI3}                           { DELPHI3+ COMPILER }
       SetLength(S, Tb);                              { Set string length }
       {$ELSE}                                        { OTHER COMPILERS }
       S[0] := Chr(Tb);                               { Set string length }
       {$ENDIF}
       P := Strings.At(I-1);                          { Cluster strings }
       If (P <> Nil) AND (P^ <> S) Then Begin         { Something changed }
         S := P^ + #0;                                { Transfer string }
         {$IFDEF OS_WINDOWS}                          { WIN/NT CODE }
         SetWindowText(WndHandles^[I-1], @S[1]);      { Set new window text }
         {$ELSE}                                      { OS2 CODE }
         WinSetWindowText(WndHandles^[I-1], @S[1]);   { Set new window text }
         {$ENDIF}
       End;
       If Mark(I-1) Then                              { If item marked }
         {$IFDEF OS_WINDOWS}                          { WIN/NT CODE }
         SendMessage(WndHandles^[I-1], bm_SetCheck,
           1, 0) Else                                 { Check the box }
         SendMessage(WndHandles^[I-1], bm_SetCheck,
           0, 0);                                     { Uncheck the box }
         {$ELSE}                                      { OS2 CODE }
         WinSendMsg(WndHandles^[I-1], bm_SetCheck,
           1, 0) Else                                 { Check the box }
         WinSendMsg(WndHandles^[I-1], bm_SetCheck,
           0, 0);                                     { Uncheck the box }
         {$ENDIF}
     End;
   End;
   {$ENDIF}
END;

{--TCluster-----------------------------------------------------------------}
{  DrawBox -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.DrawBox (Const Icon: String; Marker: Char);
BEGIN
   DrawMultiBox(Icon, ' '+Marker);                    { Call draw routine }
END;

{--TCluster-----------------------------------------------------------------}
{  SetButtonState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.SetButtonState (AMask: Longint; Enable: Boolean);
VAR I: Integer; M: Longint;
BEGIN
   If Enable Then EnableMask := EnableMask OR AMask   { Set enable bit mask }
     Else EnableMask := EnableMask AND NOT AMask;     { Disable bit mask }
   If (Strings.Count <= 32) Then Begin                { Valid string number }
     M := 1;                                          { Preset bit masks }
     For I := 1 To Strings.Count Do Begin             { For each item string }
       If ((M AND EnableMask) <> 0) Then Begin        { Bit enabled }
         Options := Options OR ofSelectable;          { Set selectable option }
         Exit;                                        { Now exit }
       End;
       M := M SHL 1;                                  { Create newbit mask }
     End;
     Options := Options AND NOT ofSelectable;         { Make not selectable }
   End;
END;

{--TCluster-----------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.GetData (Var Rec);
BEGIN
   Word(Rec) := Value;                                { Return current value }
END;

{--TCluster-----------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.SetData (Var Rec);
BEGIN
   Value := Word(Rec);                                { Set current value }
   SetDrawMask(vdFocus OR vdInner);                   { Set redraw mask }
   DrawView;                                          { Redraw masked areas }
END;

{--TCluster-----------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { TView.Store called }
   If ((Options AND ofVersion) >= ofVersion20)        { Version 2 TV view }
   Then Begin
     S.Write(Value, SizeOf(LongInt));                 { Write value }
     S.Write(Sel, SizeOf(Sel));                       { Write select item }
     S.Write(EnableMask, SizeOf(EnableMask));         { Write enable masks }
   End Else Begin
     S.Write(Value, SizeOf(Word));                    { Write value }
     S.Write(Sel, SizeOf(Sel));                       { Write select item }
   End;
   If (Options AND ofGFVModeView <> 0) Then           { GFV mode view check }
     S.Write(Id, SizeOf(Id));                         { Write new id value }
   Strings.Store(S);                                  { Store strings }
END;

{--TCluster-----------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Jun98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.HandleEvent (Var Event: TEvent);
VAR C: Char; I, J, S, Vh: Integer; Key: Word; Mouse: TPoint; Ts: PString;

   PROCEDURE MoveSel;
   BEGIN
     If (I <= Strings.Count) Then Begin
       Sel := S;                                      { Set selected item }
       MovedTo(Sel);                                  { Move to selected }
       SetDrawMask(vdInner OR vdFocus);               { Set draw masks }
       DrawView;                                      { Now draw changes }
     End;
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   If ((Options AND ofSelectable) = 0) Then Exit;     { Check selectable }
   If (Event.What = evMouseDown) Then Begin           { MOUSE EVENT }
     MakeLocal(Event.Where, Mouse);                   { Make point local }
     I := FindSel(Mouse);                             { Find selected item }
     If (I <> -1) Then                                { Check in view }
       If ButtonState(I) Then Sel := I;               { If enabled select }
     SetDrawMask(vdFocus OR vdInner);                 { Set draw mask }
     DrawView;                                        { Now draw changes }
     Repeat
       MakeLocal(Event.Where, Mouse);                 { Make point local }
     Until NOT MouseEvent(Event, evMouseMove);        { Wait for mouse up }
     MakeLocal(Event.Where, Mouse);                   { Make point local }
     If (FindSel(Mouse) = Sel) AND ButtonState(Sel)   { If valid/selected }
     Then Begin
       Press(Sel);                                    { Call pressed }
       SetDrawMask(vdFocus OR vdInner);               { Set draw mask }
       DrawView;                                      { Now draw changes }
     End;
     ClearEvent(Event);                               { Event was handled }
   End Else If (Event.What = evKeyDown) Then Begin    { KEY EVENT }
     If (Options AND ofFramed <> 0) OR                { Normal frame }
     (GOptions AND goThickFramed <> 0) Then           { Thick frame }
       J := 1 Else J := 0;                            { Adjust value }
     Vh := Size.Y - J - J;                            { View height }
     S := Sel;                                        { Hold current item }
     Key := CtrlToArrow(Event.KeyCode);               { Convert keystroke }
     Case Key Of
       kbUp, kbDown, kbRight, kbLeft:
       If (State AND sfFocused <> 0) Then Begin       { Focused key event }
         I := 0;                                      { Zero process count }
         Repeat
           Inc(I);                                    { Inc process count }
           Case Key Of
             kbUp: Dec(S);                            { Next item up }
             kbDown: Inc(S);                          { Next item down }
             kbRight: Begin                           { Next column across }
               Inc(S, Vh);                            { Move to next column }
               If (S >= Strings.Count) Then           { No next column check }
                 S := (S+1) MOD Vh;                   { Move to last column }
             End;
             kbLeft: Begin                            { Prior column across }
               Dec(S, Vh);                            { Move to prior column }
               If (S < 0) Then  S := ((Strings.Count +
                 Vh - 1) DIV Vh) * Vh + S - 1;        { No prior column check }
             End;
           End;
           If (S >= Strings.Count) Then S := 0;       { Roll up to top }
           If (S < 0) Then S := Strings.Count - 1;    { Roll down to bottom }
         Until ButtonState(S) OR (I > Strings.Count); { Repeat until select }
         MoveSel;                                     { Move to selected }
         ClearEvent(Event);                           { Event was handled }
       End;
       Else Begin                                     { Not an arrow key }
         For I := 0 To Strings.Count-1 Do Begin       { Scan each item }
           Ts := Strings.At(I);                       { Fetch string pointer }
           If (Ts <> Nil) Then C := HotKey(Ts^)       { Check for hotkey }
             Else C := #0;                            { No valid string }
           If (GetAltCode(C) = Event.KeyCode) OR      { Hot key for item }
           (((Owner^.Phase = phPostProcess) OR        { Owner in post process }
           (State AND sfFocused <> 0)) AND (C <> #0)  { Non zero hotkey }
           AND (UpCase(Event.CharCode) = C))          { Matches current key }
           Then Begin
             If ButtonState(I) Then Begin             { Check mask enabled }
               If Focus Then Begin                    { Check view focus }
                 Sel := I;                            { Set selected }
                 MovedTo(Sel);                        { Move to selected }
                 Press(Sel);                          { Call pressed }
                 SetDrawMask(vdFocus OR vdInner);     { Set draw mask }
                 DrawView;                            { Now draw changes }
               End;
               ClearEvent(Event);                     { Event was handled }
             End;
             Exit;                                    { Now exit }
           End;
         End;
         {$IFDEF OS_DOS}                              { DOS/DPMI CODE }
         If (Event.CharCode = ' ') AND                { Spacebar key }
         (State AND sfFocused <> 0) AND               { Check focused view }
         ButtonState(Sel) Then Begin                  { Check item enabled }
           Press(Sel);                                { Call pressed }
           SetDrawMask(vdFocus OR vdInner);           { Set draw mask }
           DrawView;                                  { Now draw changes }
           ClearEvent(Event);                         { Event was handled }
         End;
         {$ENDIF}
       End;
     End;
   End;
END;

{$IFNDEF OS_DOS}                                      { WIN/NT/OS2 CODE }
{***************************************************************************}
{                  TCLuster OBJECT WIN/NT/OS2 ONLY METHODS                  }
{***************************************************************************}

{--TCluster-----------------------------------------------------------------}
{  GetClassName -> Platforms WIN/NT/OS2 - Updated 03Jun98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TCluster.GetClassName: String;
BEGIN
   GetClassName := TvClusterClassName;                { Cluster class name }
END;

{--TCluster-----------------------------------------------------------------}
{  SubClassAttr -> Platforms WIN/NT/OS2 - Updated 02Jun98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TCluster.SubClassAttr: LongInt;
VAR Li: LongInt;
BEGIN
   If (State AND sfVisible = 0) Then Li := 0          { View not visible }
     Else Li := ws_Visible;                           { View is visible }
   If (State AND sfDisabled <> 0) Then                { Check disabled flag }
     Li := Li OR ws_Disabled;                         { Set disabled flag }
   Li := Li OR ws_ClipChildren OR ws_ClipSiblings;    { Must have these }
   SubClassAttr := Li;                                { Return attributes }
END;

{--TCluster-----------------------------------------------------------------}
{  GetMsgHandler -> Platforms WIN/NT/OS2 - Updated 02Jun98 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TCluster.GetMsgHandler: Pointer;
BEGIN
   GetMsgHandler := @TvClusterMsgHandler;             { Cluster msg handler }
END;

{--TCluster-----------------------------------------------------------------}
{  CreateWindowNow -> Platforms WIN/NT - Updated 28May98 LdB                }
{---------------------------------------------------------------------------}
PROCEDURE TCluster.CreateWindowNow (CmdShow: Integer);
VAR I, J, L: Integer; Li: LongInt; Ct: String; Ts: PString; P: PChar; Wnd: HWnd;
BEGIN
   If (HWindow = 0) Then Begin                        { Window not created }
     Inherited CreateWindowNow (CmdShow);             { Call ancestor }
     If (HWindow <> 0) Then Begin                     { Window now created }
       GetMem(WndHandles, Strings.Count*SizeOf(HWnd));{ Allocate memory }
       For I := 1 To Strings.Count Do Begin
         L := (I-1) * FontHeight;                     { Height of each line }
         Ts := Strings.At(I-1);                       { Fetch string pointer }
         If (Ts <> Nil) Then Ct := Ts^ Else Ct := ''; { Get string text }
         Ct := Ct + #0;                               { Make asciiz }
         J := Pos('~', Ct);                           { Check for tilde }
         If (J <> 0) Then Ct[J] := '&';               { Sub 1st tilde }
         Repeat
           J := Pos('~', Ct);                         { Check for tilde }
           If (J <> 0) Then System.Delete(Ct, J, 1);  { Remove the tilde }
         Until (J = 0);                               { Remove all tildes }
         If (Ct <> #0) Then Begin                     { Check for empty }
           GetMem(P, Length(Ct));                     { Allocate memory }
           Move(Ct[1], P^, Length(Ct));               { Move string data }
         End Else P := Nil;                           { Return nil ptr }
         {$IFDEF OS_WINDOWS}
         If (Options AND ofFramed <> 0) OR            { Normal frame }
         (GOptions AND goThickFramed <> 0) Then       { Thick frame }
           Wnd := CreateWindowEx(0, 'BUTTON', P,
             SubClassAttr OR ws_Child OR ws_Visible, FontWidth,
             L+FontHeight, RawSize.X-2*FontWidth+1,
             FontHeight, HWindow, cmTvClusterButton,
             HInstance, Nil) Else                     { Create window }
           Wnd := CreateWindowEx(0, 'BUTTON', P,
             SubClassAttr OR ws_Child OR ws_Visible, 0, L,
             RawSize.X+1, FontHeight, HWindow,
             cmTvClusterButton, HInstance, Nil);      { Create window }
         If (Wnd <> 0) Then Begin                     { Window created ok }
           {$IFDEF PPC_FPC}
           Windows.SendMessage(Wnd, WM_SetFont,
             DefGFVFont, 1);                          { Set font style }
           {$ELSE}
           WinProcs.SendMessage(Wnd, WM_SetFont,
             DefGFVFont, 1);                          { Set font style }
           {$ENDIF}
           Li := LongInt(@Self);                      { Address of  self }
           {$IFDEF BIT_16}                            { 16 BIT CODE }
           SetProp(Wnd, ViewSeg,
             Li AND $FFFF0000 SHR 16);                { Set seg property }
           SetProp(Wnd, ViewOfs,
             Li AND $0000FFFF);                       { Set ofs propertry }
           {$ENDIF}
           {$IFDEF BIT_32}                            { 32 BIT CODE }
           SetProp(Wnd, ViewPtr, Li);                 { Set view property }
           {$ENDIF}
           If (CmdShow <> 0) Then
             ShowWindow(Wnd, cmdShow);                { Execute show cmd }
           UpdateWindow(Wnd);                         { Update the window }
           BringWindowToTop(Wnd);                     { Bring window to top }
         End;
         WndHandles^[I-1] := Wnd;                     { Hold the handle }
         If  Mark(I-1) Then                           { If item marked }
           SendMessage(WndHandles^[I-1], bm_SetCheck,
             1, 0) Else                               { Check the item }
           SendMessage(WndHandles^[I-1], bm_SetCheck,
             0, 0);                                   { Uncheck the item }
         {$ENDIF}
       End;
     End;
   End;
END;
{$ENDIF}

{***************************************************************************}
{                      TCluster OBJECT PRIVATE METHODS                      }
{***************************************************************************}

{--TCluster-----------------------------------------------------------------}
{  FindSel -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TCluster.FindSel (P: TPoint): Integer;
VAR I, J, S, Vh: Integer; R: TRect;
BEGIN
   GetExtent(R);                                      { Get view extents }
   If R.Contains(P) Then Begin                        { Point in view }
     If (Options AND ofFramed <> 0) OR                { Normal frame }
     (GOptions AND goThickFramed <> 0) Then           { Thick frame }
       J := 1 Else J := 0;                            { Adjust value }
     Vh := Size.Y - J - J;                            { View height }
     I := 0;                                          { Preset zero value }
     While (P.X >= Column(I+Vh)) Do Inc(I, Vh);       { Inc view size }
     S := I + P.Y - J;                                { Line to select }
     If ((S >= 0) AND (S < Strings.Count))            { Valid selection }
       Then FindSel := S Else FindSel := -1;          { Return selected item }
   End Else FindSel := -1;                            { Point outside view }
END;

{--TCluster-----------------------------------------------------------------}
{  Row -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TCluster.Row (Item: Integer): Integer;
BEGIN
   If (Options AND ofFramed <> 0) OR                  { Normal frame }
  (GOptions AND goThickFramed <> 0) Then              { Thick frame }
    Row := Item MOD (Size.Y - 2) Else                 { Allow for frames }
    Row := Item MOD Size.Y;                           { Normal mod value }
END;

{--TCluster-----------------------------------------------------------------}
{  Column -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 03Jun98 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TCluster.Column (Item: Integer): Integer;
VAR I, J, Col, Width, L, Vh: Integer; Ts: PString;
BEGIN
   If (Options AND ofFramed <> 0) OR                  { Normal frame }
   (GOptions AND goThickFramed <> 0) Then             { Thick frame }
     J := 1 Else J := 0;                              { Adjust value }
   Vh := Size.Y - J - J;                              { Vertical size }
   If (Item >= Vh) Then Begin                         { Valid selection }
     Width := 0;                                      { Zero width }
     Col := -6;                                       { Start column at -6 }
     For I := 0 To Item Do Begin                      { For each item }
       If (I MOD Vh = 0) Then Begin                   { Start next column }
         Inc(Col, Width + 6);                         { Add column width }
         Width := 0;                                  { Zero width }
       End;
       If (I < Strings.Count) Then Begin              { Valid string }
         Ts := Strings.At(I);                         { Transfer string }
         If (Ts <> Nil) Then L := CStrLen(Ts^)        { Length of string }
           Else L := 0;                               { No string }
       End;
       If (L > Width) Then Width := L;                { Hold longest string }
     End;
     Column := Col;                                   { Return column }
   End Else Column := 0;                              { Outside select area }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TRadioButtons OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TRadioButtons------------------------------------------------------------}
{  Mark -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TRadioButtons.Mark (Item: Integer): Boolean;
BEGIN
   Mark := Item = Value;                              { True if item = value }
END;

{--TRadioButtons------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TRadioButtons.DrawFocus;
CONST Button = ' ( ) ';
BEGIN
   Inherited DrawFocus;
   DrawMultiBox(Button, #32#7);                       { Redraw the text }
END;

{--TRadioButtons------------------------------------------------------------}
{  Press -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TRadioButtons.Press (Item: Integer);
BEGIN
   Value := Item;                                     { Set value field }
   Inherited Press(Item);                             { Call ancestor }
END;

{--TRadioButtons------------------------------------------------------------}
{  MovedTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TRadioButtons.MovedTo (Item: Integer);
BEGIN
   Value := Item;                                     { Set value to item }
   If (Id <> 0) Then NewMessage(Owner, evCommand,
     cmIdCommunicate, Id, Value, @Self);              { Send new message }
END;

{--TRadioButtons------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TRadioButtons.SetData (Var Rec);
BEGIN
   Sel := Integer(Rec);                               { Set selection }
   Inherited SetData(Rec);                            { Call ancestor }
END;

{$IFNDEF OS_DOS}                                      { WIN/NT CODE }
{***************************************************************************}
{             TRadioButtons OBJECT WIN/NT/OS2 ONLY METHODS                  }
{***************************************************************************}

{--TRadioButtons------------------------------------------------------------}
{  SubClassAttr -> Platforms WIN/NT/OS2 - Updated 20May98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TRadioButtons.SubClassAttr: LongInt;
BEGIN
   SubClassAttr := Inherited SubClassAttr OR
     bs_RadioButton;                                  { Radio button }
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TCheckBoxes OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TCheckBoxes--------------------------------------------------------------}
{  Mark -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TCheckBoxes.Mark(Item: Integer): Boolean;
BEGIN
   If (Value AND (1 SHL Item) <> 0) Then              { Check if item ticked }
     Mark := True Else Mark := False;                 { Return result }
END;

{--TCheckBoxes--------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04May98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxes.DrawFocus;
CONST Button = ' [ ] ';
BEGIN
   Inherited DrawFocus;
   DrawMultiBox(Button, ' X');                        { Redraw the text }
END;

{--TCheckBoxes--------------------------------------------------------------}
{  Press -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxes.Press (Item: Integer);
BEGIN
   Value := Value XOR (1 SHL Item);                   { Flip the item mask }
   Inherited Press(Item);                             { Call ancestor }
END;

{$IFNDEF OS_DOS}                                      { WIN/NT/OS2 CODE }
{***************************************************************************}
{               TCheckBoxes OBJECT WIN/NT/OS2 ONLY METHODS                  }
{***************************************************************************}

{--TCheckBoxes--------------------------------------------------------------}
{  SubClassAttr -> Platforms WIN/NT/OS2 - Updated 20May98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TCheckBoxes.SubClassAttr: LongInt;
BEGIN
   SubClassAttr := Inherited SubClassAttr OR
     bs_CheckBox;                                     { Check box buttons }
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                      TMultiCheckBoxes OBJECT METHODS                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMultiCheckBoxes---------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Jun98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMultiCheckBoxes.Init (Var Bounds: TRect; AStrings: PSItem;
ASelRange: Byte; AFlags: Word; Const AStates: String);
BEGIN
   Inherited Init(Bounds, AStrings);                  { Call ancestor }
   SelRange := ASelRange;                             { Hold select range }
   Flags := AFlags;                                   { Hold flags }
   States := NewStr(AStates);                         { Hold string }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMultiCheckBoxes.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(SelRange, SizeOf(SelRange));                { Read select range }
   S.Read(Flags, SizeOf(Flags));                      { Read flags }
   States := S.ReadStr;                               { Read strings }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TMultiCheckBoxes.Done;
BEGIN
   If (States <> Nil) Then DisposeStr(States);        { Dispose strings }
   Inherited Done;                                    { Call ancestor }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TMultiCheckBoxes.DataSize: Word;
BEGIN
   DataSize := SizeOf(LongInt);                       { Size to exchange }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  MultiMark -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TMultiCheckBoxes.MultiMark (Item: Integer): Byte;
BEGIN
   MultiMark := (Value SHR (Word(Item) *
    WordRec(Flags).Hi)) AND WordRec(Flags).Lo;        { Return mark state }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TMultiCheckBoxes.DrawFocus;
CONST Button = ' [ ] ';
BEGIN
   Inherited DrawFocus;
   DrawMultiBox(Button, States^);                     { Draw the items }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  Press -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TMultiCheckBoxes.Press (Item: Integer);
VAR CurState: ShortInt;
BEGIN
   CurState := (Value SHR (Word(Item) *
     WordRec(Flags).Hi)) AND WordRec(Flags).Lo;       { Hold current state }
   Dec(CurState);                                     { One down }
   If (CurState >= SelRange) OR (CurState < 0) Then
     CurState := SelRange - 1;                        { Roll if needed }
   Value := (Value AND NOT (LongInt(WordRec(Flags).Lo)
     SHL (Word(Item) * WordRec(Flags).Hi))) OR
    (LongInt(CurState) SHL (Word(Item) *
    WordRec(Flags).Hi));                              { Calculate value }
   Inherited Press(Item);                             { Call ancestor }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TMultiCheckBoxes.GetData (Var Rec);
BEGIN
   Longint(Rec) := Value;                             { Return value }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TMultiCheckBoxes.SetData (Var Rec);
BEGIN
   Value := Longint(Rec);                             { Set value }
   SetDrawMask(vdFocus OR vdInner);                   { Set redraw mask }
   DrawView;                                          { Redraw masked areas }
END;

{--TMultiCheckBoxes---------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TMultiCheckBoxes.Store (Var S: TStream);
BEGIN
   TCluster.Store(S);                                 { TCluster store called }
   S.Write(SelRange, SizeOf(SelRange));               { Write select range }
   S.Write(Flags, SizeOf(Flags));                     { Write select flags }
   S.WriteStr(States);                                { Write strings }
END;

{$IFNDEF OS_DOS}                                      { WIN/NT/OS2 CODE }
{***************************************************************************}
{             TMultiCheckBoxes OBJECT WIN/NT/OS2 ONLY METHODS               }
{***************************************************************************}

{--TMultiCheckBoxes---------------------------------------------------------}
{  SubClassAttr -> Platforms WIN/NT/OS2 - Updated 06Jun98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TMultiCheckBoxes.SubClassAttr: LongInt;
BEGIN
   SubClassAttr := Inherited SubClassAttr OR
     bs_CheckBox;                                     { Check box buttons }
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TListBox OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

TYPE
   TListBoxRec = PACKED RECORD
     List: PCollection;                               { List collection ptr }
     Selection: Word;                                 { Selected item }
   END;

{--TListBox-----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TListBox.Init (Var Bounds: TRect; ANumCols: Word;
  AScrollBar: PScrollBar);
BEGIN
   Inherited Init(Bounds, ANumCols, Nil, AScrollBar); { Call ancestor }
   SetRange(0);                                       { Set range to zero }
END;

{--TListBox-----------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TListBox.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   List := PCollection(S.Get);                        { Fetch collection }
END;

{--TListBox-----------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/Os2 - Updated 06Jun98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TListBox.DataSize: Word;
BEGIN
   DataSize := SizeOf(TListBoxRec);                   { Xchg data size }
END;

{--TListBox-----------------------------------------------------------------}
{  GetText -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TListBox.GetText (Item: Integer; MaxLen: Integer): String;
VAR P: PString;
BEGIN
   GetText := '';                                     { Preset return }
   If (List <> Nil) Then Begin                        { A list exists }
     P := PString(List^.At(Item));                    { Get string ptr }
     If (P <> Nil) Then GetText := P^;                { Return string }
   End;
END;

{--TListBox-----------------------------------------------------------------}
{  NewList -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TListBox.NewList (AList: PCollection);
{$IFDEF OS_WINDOWS} VAR I: Integer; S: String; P: PString; {$ENDIF}
BEGIN
   If (List <> Nil) Then Dispose(List, Done);         { Dispose old list }
   List := AList;                                     { Hold new list }
   If (AList <> Nil) Then SetRange(AList^.Count)      { Set new item range }
     Else SetRange(0);                                { Set zero range }
   If (Range > 0) Then FocusItem(0);                  { Focus first item }
   DrawView;                                          { Redraw all view }
END;

{--TListBox-----------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TListBox.GetData (Var Rec);
BEGIN
   TListBoxRec(Rec).List := List;                     { Return current list }
   TListBoxRec(Rec).Selection := Focused;             { Return focused item }
END;

{--TListBox-----------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TListBox.SetData (Var Rec);
BEGIN
   NewList(TListBoxRec(Rec).List);                    { Hold new list }
   FocusItem(TListBoxRec(Rec).Selection);             { Focus selected item }
   DrawView;                                          { Redraw all view }
END;

{--TListBox-----------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TListBox.Store (Var S: TStream);
BEGIN
   TListViewer.Store(S);                              { TListViewer store }
   S.Put(List);                                       { Store list to stream }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TStaticText OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStaticText--------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStaticText.Init (Var Bounds: TRect; Const AText: String);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Text := NewStr(AText);                             { Create string ptr }
END;

{--TStaticText--------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStaticText.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   Text := S.ReadStr;                                 { Read text string }
END;

{--TStaticText--------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TStaticText.Done;
BEGIN
   If (Text <> Nil) Then DisposeStr(Text);            { Dispose string }
   Inherited Done;                                    { Call ancestor }
END;

{--TStaticText--------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TStaticText.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CStaticText;                        { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CStaticText)] = CStaticText;   { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TStaticText--------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TStaticText.DrawBackGround;
VAR Just: Byte; I, J, P, Y, L: Integer; S, T: String;
BEGIN
   Inherited DrawBackGround;                          { Call ancestor }
   GetText(S);                                        { Fetch text to write }
   P := 1;                                            { X start position }
   Y := 0;                                            { Y start position }
   L := Length(S);                                    { Length of text }
   While (Y < Size.Y) AND (P <= L) Do Begin
     Just := 0;                                       { Default left justify }
     If (S[P] = #2) Then Begin                        { Right justify char }
       Just := 2;                                     { Set right justify }
       Inc(P);                                        { Next character }
     End;
     If (S[P] = #3) Then Begin                        { Centre justify char }
       Just := 1;                                     { Set centre justify }
       Inc(P);                                        { Next character }
     End;
     I := P;                                          { Start position }
     While (P <= L) AND (S[P] <> #13) Do Inc(P);      { Scan for end }
     T := Copy(S, I, P-I);                            { String to write }
     Case Just Of
       0: J := 0;                                     { Left justify }
       1: J := (RawSize.X - TextWidth(T)) DIV 2;      { Centre justify }
       2: J := RawSize.X - TextWidth(T);              { Right justify }
     End;
     While (J < 0) Do Begin                           { Text to long }
       J := J + TextWidth(T[1]);                      { Add width to J }
       Delete(T, 1, 1);                               { Delete the char }
     End;
     WriteStr(-J, -(Y*FontHeight), T, 1);             { Write the text }
     While (P <= L) AND ((S[P] = #13) OR (S[P] = #10))
       Do Inc(P);                                     { Remove CR/LF }
     Inc(Y);                                          { Next line }
   End;
END;

{--TStaticText--------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStaticText.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { Call TView store }
   S.WriteStr(Text);                                  { Write text string }
END;

{--TStaticText--------------------------------------------------------------}
{  GetText -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TStaticText.GetText (Var S: String);
BEGIN
   If (Text <> Nil) Then S := Text^                   { Copy text string }
     Else S := '';                                    { Return empty string }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TParamText OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TParamText---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TParamText.Init (Var Bounds: TRect; Const AText: String;
  AParamCount: Integer);
BEGIN
   Inherited Init(Bounds, AText);                     { Call ancestor }
   ParamCount := AParamCount;                         { Hold param count }
END;

{--TParamText---------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TParamText.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(ParamCount, 2);                             { Read parameter count }
END;

{--TParamText---------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TParamText.DataSize: Word;
BEGIN
   DataSize := ParamCount * SizeOf(Pointer);          { Return data size }
END;

{--TParamText---------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TParamText.GetData (Var Rec);
BEGIN
   Pointer(Rec) := @ParamList;                        { Return parm ptr }
END;

{--TParamText---------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Jun98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TParamText.SetData (Var Rec);
BEGIN
   ParamList := @Rec;                                 { Fetch parameter list }
   DrawView;                                          { Redraw all the view }
END;

{--TParamText---------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TParamText.Store (Var S: TStream);
BEGIN
   TStaticText.Store(S);                              { Statictext store }
   S.Write(ParamCount, 2);                            { Store param count }
END;

{--TParamText---------------------------------------------------------------}
{  GetText -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TParamText.GetText (Var S: String);
BEGIN
   If (Text = Nil) Then S := '' Else                  { Return empty string }
     FormatStr(S, Text^, ParamList^);                 { Return text string }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TLabel OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TLabel-------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TLabel.Init (Var Bounds: TRect; CONST AText: String; ALink: PView);
BEGIN
   Inherited Init(Bounds, AText);                     { Call ancestor }
   Link := ALink;                                     { Hold link }
   Options := Options OR (ofPreProcess+ofPostProcess);{ Set pre/post process }
   EventMask := EventMask OR evBroadcast;             { Sees broadcast events }
END;

{--TLabel-------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TLabel.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   GetPeerViewPtr(S, Link);                           { Load link view }
END;

{--TLabel-------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TLabel.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CLabel;                             { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CLabel)] = CLabel;             { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TLabel-------------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TLabel.DrawBackGround;
VAR SCOff: Byte; Color: Word; B: TDrawBuffer;
BEGIN
   TView.DrawBackGround;                              { Explict call to TView }
   If Light Then Begin                                { Light colour select }
     Color := GetColor($0402);                        { Choose light colour }
     SCOff := 0;                                      { Zero offset }
   End Else Begin
     Color := GetColor($0301);                        { Darker colour }
     SCOff := 4;                                      { Set offset }
   End;
   MoveChar(B[0], ' ', Byte(Color), Size.X);          { Clear the buffer }
   If (Text <> Nil) Then MoveCStr(B[1], Text^, Color);{ Transfer label text }
   If ShowMarkers Then WordRec(B[0]).Lo := Byte(
     SpecialChars[SCOff]);                            { Show marker if req }
   WriteLine(0, 0, Size.X, 1, B);                     { Write the text }
END;

{--TLabel-------------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TLabel.Store (Var S: TStream);
BEGIN
   TStaticText.Store(S);                              { TStaticText.Store }
   PutPeerViewPtr(S, Link);                           { Store link view }
END;

{--TLabel-------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TLabel.HandleEvent (Var Event: TEvent);
VAR C: Char;

   PROCEDURE FocusLink;
   BEGIN
     If (Link <> Nil) AND (Link^.Options AND
      ofSelectable <> 0) Then Link^.Focus;            { Focus link view }
     ClearEvent(Event);                               { Clear the event }
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speed up exit }
     evMouseDown: FocusLink;                          { Focus link view }
     evKeyDown: Begin
       C := HotKey(Text^);                            { Check for hotkey }
       If (GetAltCode(C) = Event.KeyCode) OR          { Alt plus char }
       ((C <> #0) AND (Owner^.Phase = phPostProcess)  { Post process phase }
       AND (UpCase(Event.CharCode) = C)) Then         { Upper case match }
         FocusLink;                                   { Focus link view }
     End;
     evBroadcast: If ((Event.Command = cmReceivedFocus)
       OR (Event.Command = cmReleasedFocus)) AND      { Focus state change }
       (Link <> Nil) Then Begin
         Light := Link^.State AND sfFocused <> 0;     { Change light state }
         DrawView;                                    { Now redraw change }
       End;
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       THistoryViewer OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--THistoryViewer-----------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR THistoryViewer.Init (Var Bounds: TRect; AHScrollBar,
AVScrollBar: PScrollBar; AHistoryId: Word);
BEGIN
   Inherited Init(Bounds, 1, AHScrollBar,
     AVScrollBar);                                    { Call ancestor }
   HistoryId := AHistoryId;                           { Hold history id }
   SetRange(HistoryCount(AHistoryId));                { Set history range }
   If (Range > 1) Then FocusItem(1);                  { Set to item 1 }
   If (HScrollBar <> Nil) Then
     HScrollBar^.SetRange(1, HistoryWidth-Size.X + 3);{ Set scrollbar range }
END;

{--THistoryViewer-----------------------------------------------------------}
{  HistoryWidth -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB      }
{---------------------------------------------------------------------------}
FUNCTION THistoryViewer.HistoryWidth: Integer;
VAR Width, T, Count, I: Integer;
BEGIN
   Width := 0;                                        { Zero width variable }
   Count := HistoryCount(HistoryId);                  { Hold count value }
   For I := 0 To Count-1 Do Begin                     { For each item }
     T := Length(HistoryStr(HistoryId, I));           { Get width of item }
     If (T > Width) Then Width := T;                  { Set width to max }
   End;
   HistoryWidth := Width;                             { Return max item width }
END;

{--THistoryViewer-----------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION THistoryViewer.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CHistoryViewer;                     { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CHistoryViewer)] = CHistoryViewer;{ Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--THistoryViewer-----------------------------------------------------------}
{  GetText -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB           }
{---------------------------------------------------------------------------}
FUNCTION THistoryViewer.GetText (Item: Integer; MaxLen: Integer): String;
BEGIN
   GetText := HistoryStr(HistoryId, Item);            { Return history string }
END;

{--THistoryViewer-----------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE THistoryViewer.HandleEvent (Var Event: TEvent);
BEGIN
   If ((Event.What = evMouseDown) AND (Event.Double)) { Double click mouse }
   OR ((Event.What = evKeyDown) AND
   (Event.KeyCode = kbEnter)) Then Begin              { Enter key press }
     EndModal(cmOk);                                  { End with cmOk }
     ClearEvent(Event);                               { Event was handled }
   End Else If ((Event.What = evKeyDown) AND
   (Event.KeyCode = kbEsc)) OR                        { Esc key press }
   ((Event.What = evCommand) AND
   (Event.Command = cmCancel)) Then Begin             { Cancel command }
     EndModal(cmCancel);                              { End with cmCancel }
     ClearEvent(Event);                               { Event was handled }
   End Else Inherited HandleEvent(Event);             { Call ancestor }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       THistoryWindow OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--THistoryWindow-----------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR THistoryWindow.Init (Var Bounds: TRect; HistoryId: Word);
BEGIN
   Inherited Init(Bounds, '', wnNoNumber);            { Call ancestor }
   Flags := wfClose;                                  { Close flag only }
   InitViewer(HistoryId);                             { Create list view }
END;

{--THistoryWindow-----------------------------------------------------------}
{  GetSelection -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB      }
{---------------------------------------------------------------------------}
FUNCTION THistoryWindow.GetSelection: String;
BEGIN
   If (Viewer = Nil) Then GetSelection := '' Else     { Return empty string }
     GetSelection := Viewer^.GetText(Viewer^.Focused,
       255);                                          { Get focused string }
END;

{--THistoryWindow-----------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION THistoryWindow.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CHistoryWindow;                     { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CHistoryWindow)] = CHistoryWindow;{ Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return the palette }
END;

{--THistoryWindow-----------------------------------------------------------}
{  InitViewer -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE THistoryWindow.InitViewer(HistoryId: Word);
VAR R: TRect;
BEGIN
   GetExtent(R);                                      { Get extents }
   R.Grow(-1,-1);                                     { Grow inside }
   Viewer := New(PHistoryViewer, Init(R,
     StandardScrollBar(sbHorizontal + sbHandleKeyboard),
     StandardScrollBar(sbVertical + sbHandleKeyboard),
     HistoryId));                                     { Create the viewer }
   If (Viewer <> Nil) Then Insert(Viewer);            { Insert viewer }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          THistory OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--THistory-----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR THistory.Init (Var Bounds: TRect; ALink: PInputLine;
AHistoryId: Word);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Options := Options OR ofPostProcess;               { Set post process }
   EventMask := EventMask OR evBroadcast;             { See broadcast events }
   Link := ALink;                                     { Hold link view }
   HistoryId := AHistoryId;                           { Hold history id }
END;

{--THistory-----------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR THistory.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   GetPeerViewPtr(S, Link);                           { Load link view }
   S.Read(HistoryId, 2);                              { Read history id }
END;

{--THistory-----------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION THistory.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CHistory;                           { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CHistory)] = CHistory;         { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return the palette }
END;

{--THistory-----------------------------------------------------------------}
{  InitHistoryWindow -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB }
{---------------------------------------------------------------------------}
FUNCTION THistory.InitHistoryWindow (Var Bounds: TRect): PHistoryWindow;
VAR P: PHistoryWindow;
BEGIN
   P := New(PHistoryWindow, Init(Bounds, HistoryId)); { Create history window }
   If (Link <> Nil) Then
     P^.HelpCtx := Link^.HelpCtx;                     { Set help context }
   InitHistoryWindow := P;                            { Return history window }
END;

PROCEDURE THistory.Draw;
VAR B: TDrawBuffer;
BEGIN
   MoveCStr(B, #222'~'#25'~'#221, GetColor($0102));   { Set buffer data }
   WriteLine(0, 0, Size.X, Size.Y, B);                { Write buffer }
END;

{--THistory-----------------------------------------------------------------}
{  RecordHistory -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE THistory.RecordHistory (CONST S: String);
BEGIN
   HistoryAdd(HistoryId, S);                          { Add to history }
END;

{--THistory-----------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE THistory.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { TView.Store called }
   PutPeerViewPtr(S, Link);                           { Store link view }
   S.Write(HistoryId, 2);                             { Store history id }
END;

{--THistory-----------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE THistory.HandleEvent (Var Event: TEvent);
VAR C: Word; Rslt: String; R, P: TRect; HistoryWindow: PHistoryWindow;
BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   If (Link = Nil) Then Exit;                         { No link view exits }
   If (Event.What = evMouseDown) OR                   { Mouse down event }
   ((Event.What = evKeyDown) AND
    (CtrlToArrow(Event.KeyCode) = kbDown) AND         { Down arrow key }
    (Link^.State AND sfFocused <> 0)) Then Begin      { Link view selected }
      If NOT Link^.Focus Then Begin
       ClearEvent(Event);                             { Event was handled }
       Exit;                                          { Now exit }
      End;
     RecordHistory(Link^.Data^);                      { Record current data }
     Link^.GetBounds(R);                              { Get view bounds }
     Dec(R.A.X);                                      { One char in from us }
     Inc(R.B.X);                                      { One char short of us }
     Inc(R.B.Y, 7);                                   { Seven lines down }
     Dec(R.A.Y,1);                                    { One line below us }
     Owner^.GetExtent(P);                             { Get owner extents }
     R.Intersect(P);                                  { Intersect views }
     Dec(R.B.Y,1);                                    { Shorten length by one }
     HistoryWindow := InitHistoryWindow(R);           { Create history window }
     If (HistoryWindow <> Nil) Then Begin             { Window crested okay }
       C := Owner^.ExecView(HistoryWindow);           { Execute this window }
       If (C = cmOk) Then Begin                       { Result was okay }
         Rslt := HistoryWindow^.GetSelection;         { Get history selection }
         If Length(Rslt) > Link^.MaxLen Then
           {$IFDEF PPC_DELPHI3}                       { DELPHI 3+ COMPILER }
            SetLength(Rslt, Link^.MaxLen);            { Hold new length }
           {$ELSE}
            Rslt[0] := Char(Link^.MaxLen);            { Hold new length }
           {$ENDIF}
         Link^.Data^ := Rslt;                         { Hold new selection }
         Link^.SelectAll(True);                       { Select all string }
         Link^.DrawView;                              { Redraw link view }
       End;
       Dispose(HistoryWindow, Done);                  { Dispose of window }
     End;
     ClearEvent(Event);                               { Event was handled }
   End Else If (Event.What = evBroadcast) Then        { Broadcast event }
     If ((Event.Command = cmReleasedFocus) AND
     (Event.InfoPtr = Link)) OR
     (Event.Command = cmRecordHistory) Then           { Record command }
       RecordHistory(Link^.Data^);                    { Record the history }
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           ITEM STRING ROUTINES                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewSItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Apr98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION NewSItem (Const Str: String; ANext: PSItem): PSItem;
VAR Item: PSItem;
BEGIN
   New(Item);                                         { Allocate item }
   Item^.Value := NewStr(Str);                        { Hold item string }
   Item^.Next := ANext;                               { Chain the ptr }
   NewSItem := Item;                                  { Return item }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    DIALOG OBJECT REGISTRATION ROUTINES                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  RegisterDialogs -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Sep99 LdB   }
{---------------------------------------------------------------------------}
PROCEDURE RegisterDialogs;
BEGIN
   RegisterType(RDialog);                             { Register dialog }
   RegisterType(RInputLine);                          { Register inputline }
   RegisterType(RButton);                             { Register button }
   RegisterType(RCluster);                            { Register cluster }
   RegisterType(RRadioButtons);                       { Register radiobutton }
   RegisterType(RCheckBoxes);                         { Register check boxes }
   RegisterType(RMultiCheckBoxes);                    { Register multi boxes }
   RegisterType(RListBox);                            { Register list box }
   RegisterType(RStaticText);                         { Register static text }
   RegisterType(RLabel);                              { Register label }
   RegisterType(RHistory);                            { Register history }
   RegisterType(RParamText);                          { Register parm text }
END;

END.
{
 $Log$
 Revision 1.2  2000-08-24 12:00:20  marco
  * CVS log and ID tags


}