{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{   System independent GRAPHICAL clone of VIEWS.PAS        }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer   }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@starwon.com.au - backup e-mail address         }
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
{  1.00     10 Nov 96   First multi platform release       }
{  1.10     29 Aug 97   Platform.inc sort added.           }
{  1.20     12 Sep 97   FPK pascal 0.92 conversion added.  }
{  1.30     10 Jun 98   Virtual pascal 2.0 code added.     }
{  1.40     10 Jul 99   Sybil 2.0 code added               }
{  1.41     03 Nov 99   FPC Windows support added.         }
{  1.50     26 Nov 99   Graphics stuff moved to GFVGraph   }
{**********************************************************}

UNIT Views;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
         Windows,                                     { Standard unit }
   {$ENDIF}

   {$IFDEF OS_OS2}                                    { OS2 CODE }
     OS2Def, OS2Base, OS2PMAPI,                       { Standard units }
   {$ENDIF}

{$IFDEF GRAPH_API}
   graph,
{$ENDIF GRAPH_API}
   GFVGraph,                                          { GFV standard unit }
   Objects, FVCommon, Drivers;                          { GFV standard units }

{***************************************************************************}
{                              PUBLIC CONSTANTS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                              TView STATE MASKS                            }
{---------------------------------------------------------------------------}
CONST
   sfVisible   = $0001;                               { View visible mask }
   sfCursorVis = $0002;                               { Cursor visible }
   sfCursorIns = $0004;                               { Cursor insert mode }
   sfShadow    = $0008;                               { View has shadow }
   sfActive    = $0010;                               { View is active }
   sfSelected  = $0020;                               { View is selected }
   sfFocused   = $0040;                               { View is focused }
   sfDragging  = $0080;                               { View is dragging }
   sfDisabled  = $0100;                               { View is disabled }
   sfModal     = $0200;                               { View is modal }
   sfDefault   = $0400;                               { View is default }
   sfExposed   = $0800;                               { View is exposed }
   sfIconised  = $1000;                               { View is iconised }

{---------------------------------------------------------------------------}
{                             TView OPTION MASKS                            }
{---------------------------------------------------------------------------}
CONST
   ofSelectable  = $0001;                             { View selectable }
   ofTopSelect   = $0002;                             { Top selectable }
   ofFirstClick  = $0004;                             { First click react }
   ofFramed      = $0008;                             { View is framed }
   ofPreProcess  = $0010;                             { Pre processes }
   ofPostProcess = $0020;                             { Post processes }
   ofBuffered    = $0040;                             { View is buffered }
   ofTileable    = $0080;                             { View is tileable }
   ofCenterX     = $0100;                             { View centred on x }
   ofCenterY     = $0200;                             { View centred on y }
   ofCentered    = $0300;                             { View x,y centred }
   ofValidate    = $0400;                             { View validates }
   ofVersion     = $3000;                             { View TV version }
   ofVersion10   = $0000;                             { TV version 1 view }
   ofVersion20   = $1000;                             { TV version 2 view }
   ofGFVModeView = $4000;                             { View is in GFV mode }

{---------------------------------------------------------------------------}
{                            TView GROW MODE MASKS                          }
{---------------------------------------------------------------------------}
CONST
   gfGrowLoX = $01;                                   { Left side grow }
   gfGrowLoY = $02;                                   { Top side grow  }
   gfGrowHiX = $04;                                   { Right side grow }
   gfGrowHiY = $08;                                   { Bottom side grow }
   gfGrowAll = $0F;                                   { Grow on all sides }
   gfGrowRel = $10;                                   { Grow relative }

{---------------------------------------------------------------------------}
{                           TView DRAG MODE MASKS                           }
{---------------------------------------------------------------------------}
CONST
   dmDragMove = $01;                                  { Move view }
   dmDragGrow = $02;                                  { Grow view }
   dmLimitLoX = $10;                                  { Limit left side }
   dmLimitLoY = $20;                                  { Limit top side }
   dmLimitHiX = $40;                                  { Limit right side }
   dmLimitHiY = $80;                                  { Limit bottom side }
   dmLimitAll = $F0;                                  { Limit all sides }

{---------------------------------------------------------------------------}
{                      >> NEW << TView OPTION MASKS                         }
{---------------------------------------------------------------------------}
CONST
   goThickFramed = $0001;                             { Thick framed mask }
   goDrawFocus   = $0002;                             { Draw focus mask }
   goTitled      = $0004;                             { Draw titled mask }
   goTabSelect   = $0008;                             { Tab selectable }
   goEveryKey    = $0020;                             { Report every key }
   goEndModal    = $0040;                             { End modal }
   goNoShadow    = $0080;                             { Do not write shadows }
   goGraphView   = $1000;                             { Raw graphic view }

   goGraphical   = $2000;                             { Graphical view }
   goNativeClass = $4000;                             { Native class window }
   goNoDrawView  = $8000;                             { View does not draw }

{---------------------------------------------------------------------------}
{                       >> NEW << TAB OPTION MASKS                          }
{---------------------------------------------------------------------------}
CONST
   tmTab      = $01;                                  { Tab move mask }
   tmShiftTab = $02;                                  { Shift+tab move mask }
   tmEnter    = $04;                                  { Enter move mask }
   tmLeft     = $08;                                  { Left arrow move mask }
   tmRight    = $10;                                  { Right arrow move mask }
   tmUp       = $20;                                  { Up arrow move mask }
   tmDown     = $40;                                  { Down arrow move mask }

{---------------------------------------------------------------------------}
{                        >> NEW << VIEW DRAW MASKS                          }
{---------------------------------------------------------------------------}
CONST
   vdBackGnd = $01;                                   { Draw backgound }
   vdInner   = $02;                                   { Draw inner detail }
   vdCursor  = $04;                                   { Draw cursor }
   vdBorder  = $08;                                   { Draw view border }
   vdFocus   = $10;                                   { Draw focus state }
   vdNoChild = $20;                                   { Draw no children }
   vdShadow  = $40;
   vdAll     = vdBackGnd + vdInner + vdCursor + vdBorder + vdFocus + vdShadow;

{---------------------------------------------------------------------------}
{                            TView HELP CONTEXTS                            }
{---------------------------------------------------------------------------}
CONST
   hcNoContext = 0;                                   { No view context }
   hcDragging  = 1;                                   { No drag context }

{---------------------------------------------------------------------------}
{                             TWindow FLAG MASKS                            }
{---------------------------------------------------------------------------}
CONST
   wfMove  = $01;                                     { Window can move }
   wfGrow  = $02;                                     { Window can grow }
   wfClose = $04;                                     { Window can close }
   wfZoom  = $08;                                     { Window can zoom }

{---------------------------------------------------------------------------}
{                              TWindow PALETTES                             }
{---------------------------------------------------------------------------}
CONST
   wpBlueWindow = 0;                                  { Blue palette }
   wpCyanWindow = 1;                                  { Cyan palette }
   wpGrayWindow = 2;                                  { Gray palette }

{---------------------------------------------------------------------------}
{                              COLOUR PALETTES                              }
{---------------------------------------------------------------------------}
CONST
   CFrame      = #1#1#2#2#3;                          { Frame palette }
   CScrollBar  = #4#5#5;                              { Scrollbar palette }
   CScroller   = #6#7;                                { Scroller palette }
   CListViewer = #26#26#27#28#29;                     { Listviewer palette }

   CBlueWindow = #8#9#10#11#12#13#14#15;              { Blue window palette }
   CCyanWindow = #16#17#18#19#20#21#22#23;            { Cyan window palette }
   CGrayWindow = #24#25#26#27#28#29#30#31;            { Grey window palette }

{---------------------------------------------------------------------------}
{                           TScrollBar PART CODES                           }
{---------------------------------------------------------------------------}
CONST
   sbLeftArrow  = 0;                                  { Left arrow part }
   sbRightArrow = 1;                                  { Right arrow part }
   sbPageLeft   = 2;                                  { Page left part }
   sbPageRight  = 3;                                  { Page right part }
   sbUpArrow    = 4;                                  { Up arrow part }
   sbDownArrow  = 5;                                  { Down arrow part }
   sbPageUp     = 6;                                  { Page up part }
   sbPageDown   = 7;                                  { Page down part }
   sbIndicator  = 8;                                  { Indicator part }

{---------------------------------------------------------------------------}
{              TScrollBar OPTIONS FOR TWindow.StandardScrollBar             }
{---------------------------------------------------------------------------}
CONST
   sbHorizontal     = $0000;                          { Horz scrollbar }
   sbVertical       = $0001;                          { Vert scrollbar }
   sbHandleKeyboard = $0002;                          { Handle keyboard }

{---------------------------------------------------------------------------}
{                            STANDARD COMMAND CODES                         }
{---------------------------------------------------------------------------}
CONST
   cmValid   = 0;                                     { Valid command }
   cmQuit    = 1;                                     { Quit command }
   cmError   = 2;                                     { Error command }
   cmMenu    = 3;                                     { Menu command }
   cmClose   = 4;                                     { Close command }
   cmZoom    = 5;                                     { Zoom command }
   cmResize  = 6;                                     { Resize command }
   cmNext    = 7;                                     { Next view command }
   cmPrev    = 8;                                     { Prev view command }
   cmHelp    = 9;                                     { Help command }
   cmOK      = 10;                                    { Okay command }
   cmCancel  = 11;                                    { Cancel command }
   cmYes     = 12;                                    { Yes command }
   cmNo      = 13;                                    { No command }
   cmDefault = 14;                                    { Default command }
   cmCut     = 20;                                    { Clipboard cut cmd }
   cmCopy    = 21;                                    { Clipboard copy cmd }
   cmPaste   = 22;                                    { Clipboard paste cmd }
   cmUndo    = 23;                                    { Clipboard undo cmd }
   cmClear   = 24;                                    { Clipboard clear cmd }
   cmTile    = 25;                                    { Tile subviews cmd }
   cmCascade = 26;                                    { Cascade subviews cmd }
   cmReceivedFocus     = 50;                          { Received focus }
   cmReleasedFocus     = 51;                          { Released focus }
   cmCommandSetChanged = 52;                          { Commands changed }
   cmScrollBarChanged  = 53;                          { Scrollbar changed }
   cmScrollBarClicked  = 54;                          { Scrollbar clicked on }
   cmSelectWindowNum   = 55;                          { Select window }
   cmListItemSelected  = 56;                          { Listview item select }

   cmNotify = 27;
   cmIdCommunicate     = 28;                          { Communicate via id }
   cmIdSelect          = 29;                          { Select via id }

{---------------------------------------------------------------------------}
{                          TWindow NUMBER CONSTANTS                         }
{---------------------------------------------------------------------------}
CONST
   wnNoNumber = 0;                                    { Window has no num }
   MaxViewWidth = 132;                                { Max view width }


{***************************************************************************}
{                          PUBLIC TYPE DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                            TWindow Title string                           }
{---------------------------------------------------------------------------}
TYPE
   TTitleStr = String[80];                            { Window title string }

{---------------------------------------------------------------------------}
{                            COMMAND SET RECORD                             }
{---------------------------------------------------------------------------}
TYPE
   TCommandSet = SET OF Byte;                         { Command set record }
   PCommandSet = ^TCommandSet;                        { Ptr to command set }

{---------------------------------------------------------------------------}
{                              PALETTE RECORD                               }
{---------------------------------------------------------------------------}
TYPE
   TPalette = String;                                 { Palette record }
   PPalette = ^TPalette;                              { Pointer to palette }

{---------------------------------------------------------------------------}
{                            TDrawBuffer RECORD                             }
{---------------------------------------------------------------------------}
TYPE
   TDrawBuffer = Array [0..MaxViewWidth - 1] Of Word; { Draw buffer record }
   PDrawBuffer = ^TDrawBuffer;                        { Ptr to draw buffer }

{---------------------------------------------------------------------------}
{                           TVideoBuffer RECORD                             }
{---------------------------------------------------------------------------}
TYPE
   TVideoBuf = ARRAY [0..3999] of Word;               { Video buffer }
   PVideoBuf = ^TVideoBuf;                            { Pointer to buffer }

{---------------------------------------------------------------------------}
{                            TComplexArea RECORD                            }
{---------------------------------------------------------------------------}
TYPE
   PComplexArea = ^TComplexArea;                      { Complex area }
   TComplexArea = PACKED RECORD
      X1, Y1  : Sw_Integer;                              { Top left corner }
      X2, Y2  : Sw_Integer;                              { Lower right corner }
      NextArea: PComplexArea;                         { Next area pointer }
   END;

{***************************************************************************}
{                        PUBLIC OBJECT DEFINITIONS                          }
{***************************************************************************}

TYPE
   PGroup = ^TGroup;                                  { Pointer to group }

{---------------------------------------------------------------------------}
{                    TView OBJECT - ANCESTOR VIEW OBJECT                    }
{---------------------------------------------------------------------------}
   PView = ^TView;
   TView = OBJECT (TObject)
         GrowMode : Byte;                             { View grow mode }
         DragMode : Byte;                             { View drag mode }
         DrawMask : Byte;                             { Draw masks }
         TabMask  : Byte;                             { Tab move masks }
         ColourOfs: Sw_Integer;                          { View palette offset }
         HelpCtx  : Word;                             { View help context }
         State    : Word;                             { View state masks }
         Options  : Word;                             { View options masks }
         EventMask: Word;                             { View event masks }
         GOptions : Word;                             { Graphics options }
         Origin   : TPoint;                           { View origin }
         Size     : TPoint;                           { View size }
         Cursor   : TPoint;                           { Cursor position }
         RawOrigin: TPoint;                           { View raw origin }
         RawSize  : TPoint;                           { View raw size }
         Next     : PView;                            { Next peerview }
         Owner    : PGroup;                           { Owner group }
         HoldLimit: PComplexArea;                     { Hold limit values }

         RevCol    : Boolean;
         BackgroundChar : Char;

      CONSTRUCTOR Init (Var Bounds: TRect);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION Prev: PView;
      FUNCTION Execute: Word; Virtual;
      FUNCTION Focus: Boolean;
      FUNCTION DataSize: Sw_Word; Virtual;
      FUNCTION TopView: PView;
      FUNCTION PrevView: PView;
      FUNCTION NextView: PView;
      FUNCTION GetHelpCtx: Word; Virtual;
      FUNCTION EventAvail: Boolean;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION GetColor (Color: Word): Word;
      FUNCTION Valid (Command: Word): Boolean; Virtual;
      FUNCTION GetState (AState: Word): Boolean;
      FUNCTION TextWidth (Txt: String): Sw_Integer;
      FUNCTION MouseInView (Point: TPoint): Boolean;
      FUNCTION CommandEnabled (Command: Word): Boolean;
      FUNCTION OverLapsArea (X1, Y1, X2, Y2: Sw_Integer): Boolean;
      FUNCTION MouseEvent (Var Event: TEvent; Mask: Word): Boolean;
      PROCEDURE Hide;
      PROCEDURE Show;
      PROCEDURE Draw; Virtual;
      PROCEDURE ResetCursor; Virtual;
      PROCEDURE Select;
      PROCEDURE Awaken; Virtual;
      PROCEDURE DrawView;
      PROCEDURE MakeFirst;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE DrawCursor; Virtual;
      PROCEDURE DrawBorder; Virtual;
      PROCEDURE DrawShadow; Virtual;
      PROCEDURE HideCursor;
      PROCEDURE ShowCursor;
      PROCEDURE BlockCursor;
      PROCEDURE NormalCursor;
      PROCEDURE FocusFromTop; Virtual;
      PROCEDURE SetViewLimits;
      PROCEDURE DrawBackGround; Virtual;
      PROCEDURE ReleaseViewLimits;
      PROCEDURE MoveTo (X, Y: Sw_Integer);
      PROCEDURE GrowTo (X, Y: Sw_Integer);
      PROCEDURE SetDrawMask (Mask: Byte);
      PROCEDURE EndModal (Command: Word); Virtual;
      PROCEDURE SetCursor (X, Y: Sw_Integer);
      PROCEDURE PutInFrontOf (Target: PView);
      PROCEDURE DisplaceBy (Dx, Dy: Sw_Integer); Virtual;
      PROCEDURE SetCommands (Commands: TCommandSet);
      PROCEDURE ReDrawArea (X1, Y1, X2, Y2: Sw_Integer); Virtual;
      PROCEDURE EnableCommands (Commands: TCommandSet);
      PROCEDURE DisableCommands (Commands: TCommandSet);
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE SetCmdState (Commands: TCommandSet; Enable: Boolean);
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE Locate (Var Bounds: TRect);
      PROCEDURE KeyEvent (Var Event: TEvent);
      PROCEDURE GetEvent (Var Event: TEvent); Virtual;
      PROCEDURE PutEvent (Var Event: TEvent); Virtual;
      PROCEDURE GetExtent (Var Extent: TRect);
      PROCEDURE GetBounds (Var Bounds: TRect);
      PROCEDURE SetBounds (Var Bounds: TRect);
      PROCEDURE GetClipRect (Var Clip: TRect);
      PROCEDURE ClearEvent (Var Event: TEvent);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE ChangeBounds (Var Bounds: TRect); Virtual;
      PROCEDURE SizeLimits (Var Min, Max: TPoint); Virtual;
      PROCEDURE GetCommands (Var Commands: TCommandSet);
      PROCEDURE GetPeerViewPtr (Var S: TStream; Var P);
      PROCEDURE PutPeerViewPtr (Var S: TStream; P: PView);
      PROCEDURE CalcBounds (Var Bounds: TRect; Delta: TPoint); Virtual;

      FUNCTION Exposed: Boolean;   { This needs help!!!!! }
      PROCEDURE GraphLine (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
      PROCEDURE GraphRectangle (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
      PROCEDURE ClearArea (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
      PROCEDURE GraphArc (Xc, Yc: Sw_Integer; Sa, Ea: Real; XRad, YRad: Sw_Integer;
        Colour: Byte);
      PROCEDURE FilletArc (Xc, Yc: Sw_Integer; Sa, Ea: Real; XRad, YRad, Ht: Sw_Integer;
        Colour: Byte);
      PROCEDURE BicolorRectangle (X1, Y1, X2, Y2: Sw_Integer; Light, Dark: Byte;
        Down: Boolean);
      PROCEDURE WriteBuf (X, Y, W, H: Sw_Integer; Var Buf);
      PROCEDURE WriteLine (X, Y, W, H: Sw_Integer; Var Buf);
      PROCEDURE MakeLocal (Source: TPoint; Var Dest: TPoint);
      PROCEDURE MakeGlobal (Source: TPoint; Var Dest: TPoint);
      PROCEDURE WriteStr (X, Y: Sw_Integer; Str: String; Color: Byte);
      PROCEDURE WriteCStr (X, Y: Sw_Integer; Str: String; Color1, Color2 : Byte);
      PROCEDURE WriteChar (X, Y: Sw_Integer; C: Char; Color: Byte;
        Count: Sw_Integer);
      PROCEDURE DragView (Event: TEvent; Mode: Byte; Var Limits: TRect;
        MinSize, MaxSize: TPoint);
      PROCEDURE WriteAbs(X, Y, L :Sw_Integer;var Buf);
      PROCEDURE WriteShadow(X1, Y1, X2, Y2 : Sw_Integer);

      FUNCTION FontWidth: Sw_Integer;
      FUNCTION Fontheight: Sw_Integer;

   END;

   SelectMode = (NormalSelect, EnterSelect, LeaveSelect);

{---------------------------------------------------------------------------}
{                  TGroup OBJECT - GROUP OBJECT ANCESTOR                    }
{---------------------------------------------------------------------------}
   TGroup = OBJECT (TView)
         Phase   : (phFocused, phPreProcess, phPostProcess);
         EndState: Word;                              { Modal result }
         Current : PView;                             { Selected subview }
         Last    : PView;                             { 1st view inserted }
         Buffer  : PVideoBuf;                         { Speed up buffer }
      CONSTRUCTOR Init (Var Bounds: TRect);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION First: PView;
      FUNCTION Execute: Word; Virtual;
      FUNCTION GetHelpCtx: Word; Virtual;
      FUNCTION DataSize: Sw_Word; Virtual;
      FUNCTION ExecView (P: PView): Word; Virtual;
      FUNCTION FirstThat (P: Pointer): PView;
      FUNCTION Valid (Command: Word): Boolean; Virtual;
      FUNCTION FocusNext (Forwards: Boolean): Boolean;
      PROCEDURE Draw; Virtual;
      PROCEDURE DrawBackGround; Virtual;
      PROCEDURE Lock;
      PROCEDURE UnLock;
      PROCEDURE Awaken; Virtual;
      PROCEDURE ReDraw;
      PROCEDURE ReDrawArea (X1, Y1, X2, Y2: Sw_Integer); Virtual;
      PROCEDURE ReDrawVisibleArea (X1, Y1, X2, Y2: Sw_Integer;Cur : PView);
      PROCEDURE SelectDefaultView;
      PROCEDURE Insert (P: PView);
      PROCEDURE Delete (P: PView);
      PROCEDURE ForEach (P: Pointer);
      { ForEach can't be virtual because it generates SIGSEGV }
      PROCEDURE EndModal (Command: Word); Virtual;
      PROCEDURE DisplaceBy (Dx, Dy: Sw_Integer); Virtual;
      PROCEDURE SelectNext (Forwards: Boolean);
      PROCEDURE InsertBefore (P, Target: PView);
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE GetData (Var Rec); Virtual;
      PROCEDURE SetData (Var Rec); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE EventError (Var Event: TEvent); Virtual;
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE ChangeBounds (Var Bounds: TRect); Virtual;
      PROCEDURE GetSubViewPtr (Var S: TStream; Var P);
      PROCEDURE PutSubViewPtr (Var S: TStream; P: PView);

      PRIVATE
         LockFlag: Byte;
         Clip    : TRect;
      FUNCTION IndexOf (P: PView): Sw_Integer;
      FUNCTION FindNext (Forwards: Boolean): PView;
      FUNCTION FirstMatch (AState: Word; AOptions: Word): PView;
      PROCEDURE ResetCurrent;
      PROCEDURE RemoveView (P: PView);
      PROCEDURE InsertView (P, Target: PView);
      PROCEDURE SetCurrent (P: PView; Mode: SelectMode);
   END;

{---------------------------------------------------------------------------}
{                    TFrame OBJECT - FRAME VIEW OBJECT                      }
{---------------------------------------------------------------------------}
TYPE
   TFrame = OBJECT (TView)
      CONSTRUCTOR Init (Var Bounds: TRect);
      FUNCTION GetPalette: PPalette; Virtual;
   END;
   PFrame = ^TFrame;

{---------------------------------------------------------------------------}
{                   TScrollBar OBJECT - SCROLL BAR OBJECT                   }
{---------------------------------------------------------------------------}
TYPE
   TScrollChars = Array [0..4] of Char;

   TScrollBar = OBJECT (TView)
         Value : Sw_Integer;                             { Scrollbar value }
         Min   : Sw_Integer;                             { Scrollbar minimum }
         Max   : Sw_Integer;                             { Scrollbar maximum }
         PgStep: Sw_Integer;                             { One page step }
         ArStep: Sw_Integer;                             { One range step }
         Id    : Sw_Integer;                             { Scrollbar ID }
      CONSTRUCTOR Init (Var Bounds: TRect);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION ScrollStep (Part: Sw_Integer): Sw_Integer; Virtual;
      PROCEDURE Draw; Virtual;
      PROCEDURE ScrollDraw;                                          Virtual;
      PROCEDURE DrawBackGround;                                      Virtual;
      PROCEDURE SetValue (AValue: Sw_Integer);
      PROCEDURE SetRange (AMin, AMax: Sw_Integer);
      PROCEDURE SetStep (APgStep, AArStep: Sw_Integer);
      PROCEDURE SetParams (AValue, AMin, AMax, APgStep, AArStep: Sw_Integer);
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PRIVATE
         Chars: TScrollChars;                         { Scrollbar chars }
      FUNCTION GetPos: Sw_Integer;
      FUNCTION GetSize: Sw_Integer;
      PROCEDURE DrawPos (Pos: Sw_Integer);
      PROCEDURE ClearPos (Pos: Sw_Integer);
   END;
   PScrollBar = ^TScrollBar;

{---------------------------------------------------------------------------}
{                 TScroller OBJECT - SCROLLING VIEW ANCESTOR                }
{---------------------------------------------------------------------------}
TYPE
   TScroller = OBJECT (TView)
         Delta     : TPoint;
         Limit     : TPoint;
         HScrollBar: PScrollBar;                      { Horz scroll bar }
         VScrollBar: PScrollBar;                      { Vert scroll bar }
      CONSTRUCTOR Init (Var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      PROCEDURE ScrollDraw;                                          Virtual;
      PROCEDURE SetLimit (X, Y: Sw_Integer);
      PROCEDURE ScrollTo (X, Y: Sw_Integer);
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE ChangeBounds (Var Bounds: TRect); Virtual;
        PRIVATE
         DrawFlag: Boolean;
         DrawLock: Byte;
      PROCEDURE CheckDraw;
   END;
   PScroller = ^TScroller;

{---------------------------------------------------------------------------}
{                  TListViewer OBJECT - LIST VIEWER OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   TListViewer = OBJECT (TView)
         NumCols   : Sw_Integer;                         { Number of columns }
         TopItem   : Sw_Integer;                         { Top most item }
         Focused   : Sw_Integer;                         { Focused item }
         Range     : Sw_Integer;                         { Range of listview }
         HScrollBar: PScrollBar;                      { Horz scrollbar }
         VScrollBar: PScrollBar;                      { Vert scrollbar }
      CONSTRUCTOR Init (Var Bounds: TRect; ANumCols: Sw_Word; AHScrollBar,
        AVScrollBar: PScrollBar);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION IsSelected (Item: Sw_Integer): Boolean; Virtual;
      FUNCTION GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String; Virtual;
      PROCEDURE DrawFocus; Virtual;
      PROCEDURE DrawLoseFocus; Virtual;
      PROCEDURE DrawBackGround; Virtual;
      PROCEDURE FocusItem (Item: Sw_Integer); Virtual;
      PROCEDURE SetTopItem (Item: Sw_Integer);
      PROCEDURE SetRange (ARange: Sw_Integer);
      PROCEDURE SelectItem (Item: Sw_Integer); Virtual;
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE ChangeBounds (Var Bounds: TRect); Virtual;
      PROCEDURE FocusItemNum (Item: Sw_Integer); Virtual;
   END;
   PListViewer = ^TListViewer;

{---------------------------------------------------------------------------}
{                  TWindow OBJECT - WINDOW OBJECT ANCESTOR                  }
{---------------------------------------------------------------------------}
TYPE
   TWindow = OBJECT (TGroup)
         Flags   : Byte;                              { Window flags }
         Number  : Sw_Integer;                           { Window number }
         Palette : Sw_Integer;                           { Window palette }
         ZoomRect: TRect;                             { Zoom rectangle }
         Frame   : PFrame;                            { Frame view object }
         Title   : PString;                           { Title string }
      CONSTRUCTOR Init (Var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION GetTitle (MaxSize: Sw_Integer): TTitleStr; Virtual;
      FUNCTION StandardScrollBar (AOptions: Word): PScrollBar;
      PROCEDURE Zoom; Virtual;
      PROCEDURE Close; Virtual;
      PROCEDURE InitFrame; Virtual;
      PROCEDURE DrawBorder;                                          Virtual;
      PROCEDURE SetState (AState: Word; Enable: Boolean); Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE SizeLimits (Var Min, Max: TPoint); Virtual;
   END;
   PWindow = ^TWindow;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         WINDOW MESSAGE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-Message------------------------------------------------------------
Message sets up an event record and calls Receiver^.HandleEvent to
handle the event. Message returns nil if Receiver is nil, or if
the event is not handled successfully.
12Sep97 LdB
---------------------------------------------------------------------}
FUNCTION Message (Receiver: PView; What, Command: Word;
  InfoPtr: Pointer): Pointer;

{-NewMessage---------------------------------------------------------
NewMessage sets up an event record including the new fields and calls
Receiver^.HandleEvent to handle the event. Message returns nil if
Receiver is nil, or if the event is not handled successfully.
19Sep97 LdB
---------------------------------------------------------------------}
FUNCTION NewMessage (P: PView; What, Command: Word; Id: Sw_Integer; Data: Real;
  InfoPtr: Pointer): Pointer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                     VIEW OBJECT REGISTRATION ROUTINES                     }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{-RegisterViews------------------------------------------------------
This registers all the view type objects used in this unit.
11Aug99 LdB
---------------------------------------------------------------------}
PROCEDURE RegisterViews;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                            NEW VIEW ROUTINES                              }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-CreateIdScrollBar--------------------------------------------------
Creates and scrollbar object of the given size and direction and sets
the scrollbar id number.
22Sep97 LdB
---------------------------------------------------------------------}
FUNCTION CreateIdScrollBar (X, Y, Size, Id: Sw_Integer; Horz: Boolean): PScrollBar;

{***************************************************************************}
{                        INITIALIZED PUBLIC VARIABLES                       }
{***************************************************************************}


{---------------------------------------------------------------------------}
{                 INITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES                 }
{---------------------------------------------------------------------------}
CONST
   UseNativeClasses: Boolean = True;                  { Native class modes }
   CommandSetChanged: Boolean = False;                { Command change flag }
   ShowMarkers: Boolean = False;                      { Show marker state }
   ErrorAttr: Byte = $CF;                             { Error colours }
   PositionalEvents: Word = evMouse;                  { Positional defined }
   FocusedEvents: Word = evKeyboard + evCommand;      { Focus defined }
   MinWinSize: TPoint = (X: 16; Y: 6);                { Minimum window size }
   ShadowSize: TPoint = (X: 2; Y: 1);                 { Shadow sizes }
   ShadowAttr: Byte = $08;                            { Shadow attribute }

{ Characters used for drawing selected and default items in  }
{ monochrome color sets                                      }
   SpecialChars: Array [0..5] Of Char = (#175, #174, #26, #27, ' ', ' ');

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        STREAM REGISTRATION RECORDS                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{                         TView STREAM REGISTRATION                         }
{---------------------------------------------------------------------------}
CONST
   RView: TStreamRec = (
     ObjType: 1;                                      { Register id = 1 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TView)^);                    { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TView);                          { Alt style VMT link }
     {$ENDIF}
     Load:    @TView.Load;                            { Object load method }
     Store:   @TView.Store                            { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TFrame STREAM REGISTRATION                         }
{---------------------------------------------------------------------------}
CONST
   RFrame: TStreamRec = (
     ObjType: 2;                                      { Register id = 2 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TFrame)^);                   { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TFrame);                         { Alt style VMT link }
     {$ENDIF}
     Load:    @TFrame.Load;                           { Frame load method }
     Store:   @TFrame.Store                           { Frame store method }
   );

{---------------------------------------------------------------------------}
{                      TScrollBar STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RScrollBar: TStreamRec = (
     ObjType: 3;                                      { Register id = 3 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TScrollBar)^);               { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TScrollBar);                     { Alt style VMT link }
     {$ENDIF}
     Load:    @TScrollBar.Load;                       { Object load method }
     Store:   @TScrollBar.Store                       { Object store method }
   );

{---------------------------------------------------------------------------}
{                       TScroller STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RScroller: TStreamRec = (
     ObjType: 4;                                      { Register id = 4 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TScroller)^);                { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TScroller);                      { Alt style VMT link }
     {$ENDIF}
     Load:    @TScroller.Load;                        { Object load method }
     Store:   @TScroller.Store                        { Object store method }
   );

{---------------------------------------------------------------------------}
{                      TListViewer STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RListViewer: TStreamRec = (
     ObjType: 5;                                      { Register id = 5 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TListViewer)^);              { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TListViewer);                    { Alt style VMT link }
     {$ENDIF}
     Load:    @TListViewer.Load;                      { Object load method }
     Store:   @TLIstViewer.Store                      { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TGroup STREAM REGISTRATION                         }
{---------------------------------------------------------------------------}
CONST
   RGroup: TStreamRec = (
     ObjType: 6;                                      { Register id = 6 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TGroup)^);                   { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TGroup);                         { Alt style VMT link }
     {$ENDIF}
     Load:    @TGroup.Load;                           { Object load method }
     Store:   @TGroup.Store                           { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TWindow STREAM REGISTRATION                        }
{---------------------------------------------------------------------------}
CONST
   RWindow: TStreamRec = (
     ObjType: 7;                                      { Register id = 7 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TWindow)^);                  { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TWindow);                        { Alt style VMT link }
     {$ENDIF}
     Load:    @TWindow.Load;                          { Object load method }
     Store:   @TWindow.Store                          { Object store method }
   );


{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                             IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
USES
{$IFDEF USE_VIDEO_API}
  Video,
{$ENDIF USE_VIDEO_API}
  CallSpec;

{***************************************************************************}
{                       PRIVATE TYPE DEFINITIONS                            }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                         TFixupList DEFINITION                             }
{---------------------------------------------------------------------------}
TYPE
   TFixupList = ARRAY [1..4096] Of Pointer;           { Fix up ptr array }
   PFixupList = ^TFixupList;                          { Ptr to fix up list }

{***************************************************************************}
{                      PRIVATE INITIALIZED VARIABLES                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{            INITIALIZED DOS/DPMI/WIN/NT/OS2 PRIVATE VARIABLES              }
{---------------------------------------------------------------------------}
CONST
   TheTopView  : PView = Nil;                         { Top focused view }
   LimitsLocked: PView = Nil;                         { View locking limits }
   OwnerGroup  : PGroup = Nil;                        { Used for loading }
   FixupList   : PFixupList = Nil;                    { Used for loading }
   CurCommandSet: TCommandSet = ([0..255] -
     [cmZoom, cmClose, cmResize, cmNext, cmPrev]);    { All active but these }

   vdInSetCursor = $80;                               { AVOID RECURSION IN SetCursor }

{***************************************************************************}
{                          PRIVATE INTERNAL ROUTINES                        }
{***************************************************************************}

    function posidx(const substr,s : string;idx:sw_integer):sw_integer;
      var
        i,j : sw_integer;
        e   : boolean;
      begin
        i:=idx;
        j:=0;
        e:=(length(SubStr)>0);
        while e and (i<=Length(s)-Length(SubStr)) do
         begin
           if (SubStr[1]=s[i]) and (Substr=Copy(s,i,Length(SubStr))) then
            begin
              j:=i;
              e:=false;
            end;
           inc(i);
         end;
        PosIdx:=j;
      end;


{$ifdef UNIX}
const
  MouseUsesVideoBuf = true;
{$else not UNIX}
const
  MouseUsesVideoBuf = false;
{$endif not UNIX}

procedure DrawScreenBuf;
begin
{$ifdef USE_VIDEO_API}
  if (GetLockScreenCount=0) then
   begin
     If MouseUsesVideoBuf then
       begin
         LockScreenUpdate;
         HideMouse;
         ShowMouse;
         UnlockScreenUpdate;
       end
     else
       HideMouse;
     if TextModeGFV then
       UpdateScreen(false)
{$IFDEF GRAPH_API}
     else
       GraphUpdateScreen(false)
{$ENDIF GRAPH_API}
       ;
     If not MouseUsesVideoBuf then
       ShowMouse;
   end;
{$endif USE_VIDEO_API}
end;

{***************************************************************************}
{                              OBJECT METHODS                               }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                            TView OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TView--------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20Jun96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TView.Init (Var Bounds: TRect);
BEGIN
   Inherited Init;                                    { Call ancestor }
   DragMode := dmLimitLoY;                            { Default drag mode }
   HelpCtx := hcNoContext;                            { Clear help context }
   State := sfVisible;                                { Default state }
   EventMask := evMouseDown + evKeyDown + evCommand;  { Default event masks }
   GOptions := goTabSelect;                           { Set new options }
   BackgroundChar := ' ';
   SetBounds(Bounds);                                 { Set view bounds }
END;

{--TView--------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06May98 LdB              }
{---------------------------------------------------------------------------}
{   This load method will read old original TV data from a stream but the   }
{  new options and tabmasks are not set so some NEW functionality is not    }
{  supported but it should work as per original TV code.                    }
{---------------------------------------------------------------------------}
CONSTRUCTOR TView.Load (Var S: TStream);
BEGIN
   Inherited Init;                                    { Call ancestor }
   S.Read(Origin.X, 2);                               { Read origin x value }
   S.Read(Origin.Y, 2);                               { Read origin y value }
   S.Read(Size.X, 2);                                 { Read view x size }
   S.Read(Size.Y, 2);                                 { Read view y size }
   S.Read(Cursor.X, 2);                               { Read cursor x size }
   S.Read(Cursor.Y, 2);                               { Read cursor y size }
   S.Read(GrowMode, 1);                               { Read growmode flags }
   S.Read(DragMode, 1);                               { Read dragmode flags }
   S.Read(HelpCtx, 2);                                { Read help context }
   S.Read(State, 2);                                  { Read state masks }
   S.Read(Options, 2);                                { Read options masks }
   S.Read(Eventmask, 2);                              { Read event masks }
   If (Options AND ofGFVModeView <> 0) Then Begin     { STREAM HAS GFV TVIEW }
     S.Read(GOptions, 2);                             { Read new option masks }
     S.Read(TabMask, 1);                              { Read new tab masks }
     S.Read(RawOrigin.X, 2);                          { Read raw x origin point }
     S.Read(RawOrigin.Y, 2);                          { Read raw y origin point }
     S.Read(RawSize.X, 2);                            { Read raw x size }
     S.Read(RawSize.Y, 2);                            { Read raw y size }
     S.Read(ColourOfs, 2);                            { Read palette offset }
   End Else Begin                                     { STREAM HAS OLD TView }
     RawOrigin.X := Origin.X * FontWidth;             { Set x origin pt }
     RawOrigin.Y := Origin.Y * FontHeight;            { Set y origin pt }
     RawSize.X := (Size.X * FontWidth) - 1;           { Calc raw x size }
     RawSize.Y := (Size.Y * FontHeight) - 1;          { Calc raw y size }
   End;
END;

{--TView--------------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Nov99 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TView.Done;
VAR P: PComplexArea;
BEGIN
   Hide;                                              { Hide the view }
   If (Owner <> Nil) Then Owner^.Delete(@Self);       { Delete from owner }
   While (HoldLimit <> Nil) Do Begin                  { Free limit memory }
     P := HoldLimit^.NextArea;                        { Hold next pointer }
     FreeMem(HoldLimit, SizeOf(TComplexArea));        { Release memory }
     HoldLimit := P;                                  { Shuffle to next }
   End;
END;

{--TView--------------------------------------------------------------------}
{  Prev -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TView.Prev: PView;
VAR NP : PView;
BEGIN
   Prev := @Self;
   NP := Next;
   While (NP <> Nil) AND (NP <> @Self) Do
     Begin
       Prev := NP;                                       { Locate next view }
       NP := NP^.Next;
     End;
END;

{--TView--------------------------------------------------------------------}
{  Execute -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TView.Execute: Word;
BEGIN
   Execute := cmCancel;                               { Return cancel }
END;

{--TView--------------------------------------------------------------------}
{  Focus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05May98 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TView.Focus: Boolean;
VAR Res: Boolean;
BEGIN
   Res := True;                                       { Preset result }
   If (State AND (sfSelected + sfModal)=0) Then Begin { Not modal/selected }
     If (Owner <> Nil) Then Begin                     { View has an owner }
       Res := Owner^.Focus;                           { Return focus state }
       If Res Then                                    { Owner has focus }
         If ((Owner^.Current = Nil) OR                { No current view }
         (Owner^.Current^.Options AND ofValidate = 0) { Non validating view }
         OR (Owner^.Current^.Valid(cmReleasedFocus))) { Okay to drop focus }
           Then Select Else Res := False;             { Then select us }
     End;
   End;
   Focus := Res;                                      { Return focus result }
END;

{--TView--------------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TView.DataSize: Sw_Word;
BEGIN
   DataSize := 0;                                     { Transfer size }
END;

{--TView--------------------------------------------------------------------}
{  TopView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TView.TopView: PView;
VAR P: PView;
BEGIN
   If (TheTopView = Nil) Then Begin                   { Check topmost view }
     P := @Self;                                      { Start with us }
     While (P <> Nil) AND (P^.State AND sfModal = 0)  { Check if modal }
       Do P := P^.Owner;                              { Search each owner }
     TopView := P;                                    { Return result }
   End Else TopView := TheTopView;                    { Return topview }
END;

{--TView--------------------------------------------------------------------}
{  PrevView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TView.PrevView: PView;
BEGIN
   If (@Self = Owner^.First) Then PrevView := Nil     { We are first view }
     Else PrevView := Prev;                           { Return our prior }
END;

{--TView--------------------------------------------------------------------}
{  NextView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TView.NextView: PView;
BEGIN
   If (@Self = Owner^.Last) Then NextView := Nil      { This is last view }
     Else NextView := Next;                           { Return our next }
END;

{--TView--------------------------------------------------------------------}
{  GetHelpCtx -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TView.GetHelpCtx: Word;
BEGIN
   If (State AND sfDragging <> 0) Then                { Dragging state check }
     GetHelpCtx := hcDragging Else                    { Return dragging }
     GetHelpCtx := HelpCtx;                           { Return help context }
END;

{--TView--------------------------------------------------------------------}
{  EventAvail -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TView.EventAvail: Boolean;
VAR Event: TEvent;
BEGIN
   GetEvent(Event);                                   { Get next event }
   If (Event.What <> evNothing) Then PutEvent(Event); { Put it back }
   EventAvail := (Event.What <> evNothing);           { Return result }
END;

{--TView--------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TView.GetPalette: PPalette;
BEGIN
   GetPalette := Nil;                                 { Return nil ptr }
END;

{--TView--------------------------------------------------------------------}
{  GetColor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Jul99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TView.GetColor (Color: Word): Word;
VAR Col: Byte; W: Word; P: PPalette; Q: PView;
BEGIN
   W := 0;                                            { Clear colour Sw_Word }
   If (Hi(Color) > 0) Then Begin                      { High colour req }
     Col := Hi(Color) + ColourOfs;                    { Initial offset }
     Q := @Self;                                      { Pointer to self }
     Repeat
       P := Q^.GetPalette;                            { Get our palette }
       If (P <> Nil) Then Begin                       { Palette is valid }
         If (Col <= Length(P^)) Then
           Col := Ord(P^[Col]) Else                   { Return colour }
           Col := ErrorAttr;                          { Error attribute }
       End;
       Q := Q^.Owner;                                 { Move up to owner }
     Until (Q = Nil);                                 { Until no owner }
     W := Col SHL 8;                                  { Translate colour }
   End;
   If (Lo(Color) > 0) Then Begin
     Col := Lo(Color) + ColourOfs;                    { Initial offset }
     Q := @Self;                                      { Pointer to self }
     Repeat
       P := Q^.GetPalette;                            { Get our palette }
       If (P <> Nil) Then Begin                       { Palette is valid }
         If (Col <= Length(P^)) Then
           Col := Ord(P^[Col]) Else                   { Return colour }
           Col := ErrorAttr;                          { Error attribute }
       End;
       Q := Q^.Owner;                                 { Move up to owner }
     Until (Q = Nil);                                 { Until no owner }
   End Else Col := ErrorAttr;                         { No colour found }
   GetColor := W OR Col;                              { Return color }
END;

{--TView--------------------------------------------------------------------}
{  Valid -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TView.Valid (Command: Word): Boolean;
BEGIN
   Valid := True;                                     { Simply return true }
END;

{--TView--------------------------------------------------------------------}
{  GetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TView.GetState (AState: Word): Boolean;
BEGIN
   GetState := State AND AState = AState;             { Check states equal }
END;

{--TView--------------------------------------------------------------------}
{  TextWidth -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Nov99 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TView.TextWidth (Txt: String): Sw_Integer;
VAR I: Sw_Integer; S: String;
BEGIN
   S := Txt;                                          { Transfer text }
   Repeat
     I := Pos('~', S);                                { Check for tilde }
      If (I <> 0) Then System.Delete(S, I, 1);        { Remove the tilde }
   Until (I = 0);                                     { Remove all tildes }
   TextWidth := Length(S) * SysFontWidth;             { Calc text length }
END;

{--TView--------------------------------------------------------------------}
{  MouseInView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
FUNCTION TView.MouseInView (Point: TPoint): Boolean;
BEGIN
   MouseInView := False;                              { Preset false }
   If (Point.X < RawOrigin.X) Then Exit;              { Point to left }
   If (Point.X > (RawOrigin.X+RawSize.X)) Then Exit;  { Point to right }
   If (Point.Y < RawOrigin.Y) Then Exit;              { Point is above }
   If (Point.Y > (RawOrigin.Y+RawSize.Y)) Then Exit;  { Point is below }
   MouseInView := True;                               { Return true }
END;

{--TView--------------------------------------------------------------------}
{  CommandEnabled -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB    }
{---------------------------------------------------------------------------}
FUNCTION TView.CommandEnabled(Command: Word): Boolean;
BEGIN
   CommandEnabled := (Command > 255) OR
     (Command IN CurCommandSet);                      { Check command }
END;

{--TView--------------------------------------------------------------------}
{  OverLapsArea -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB      }
{---------------------------------------------------------------------------}
FUNCTION TView.OverlapsArea (X1, Y1, X2, Y2: Sw_Integer): Boolean;
BEGIN
   OverLapsArea := False;                             { Preset false }
   If TextModeGFV then Begin
     If (Origin.X > X2) Then Exit;                   { Area to the left }
     If ((Origin.X + Size.X) < X1) Then Exit;     { Area to the right }
     If (Origin.Y > Y2) Then Exit;                   { Area is above }
     If ((Origin.Y + Size.Y) < Y1) Then Exit;     { Area is below }
   End Else Begin
     If (RawOrigin.X > X2) Then Exit;                   { Area to the left }
     If ((RawOrigin.X + RawSize.X) < X1) Then Exit;     { Area to the right }
     If (RawOrigin.Y > Y2) Then Exit;                   { Area is above }
     If ((RawOrigin.Y + RawSize.Y) < Y1) Then Exit;     { Area is below }
   End;
   OverLapsArea := True;                              { Return true }
END;

{--TView--------------------------------------------------------------------}
{  MouseEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TView.MouseEvent (Var Event: TEvent; Mask: Word): Boolean;
BEGIN
   Repeat
     GetEvent(Event);                                 { Get next event }
   Until (Event.What AND (Mask OR evMouseUp) <> 0);   { Wait till valid }
   MouseEvent := Event.What <> evMouseUp;             { Return result }
END;

{--TView--------------------------------------------------------------------}
{  Hide -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TView.Hide;
BEGIN
   If (State AND sfVisible <> 0) Then                 { View is visible }
     SetState(sfVisible, False);                      { Hide the view }
END;

{--TView--------------------------------------------------------------------}
{  Show -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TView.Show;
BEGIN
   If (State AND sfVisible = 0) Then                  { View not visible }
     SetState(sfVisible, True);                       { Show the view }
END;

{--TView--------------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TView.Draw;
BEGIN                                                 { Abstract method }
END;


procedure TView.ResetCursor;
const
  sfV_CV_F:word = sfVisible + sfCursorVis + sfFocused;
var
  p,p2 : PView;
  G : PGroup;
  cur : TPoint;

  function Check0:boolean;
  var
    res : byte;
  begin
    res:=0;
    while res=0 do
     begin
       p:=p^.next;
       if p=p2 then
        begin
          p:=P^.owner;
          res:=1
        end
       else
        if ((p^.state and sfVisible)<>0) and
           (cur.x>=p^.origin.x) and
           (cur.x<p^.size.x+p^.origin.x) and
           (cur.y>=p^.origin.y) and
           (cur.y<p^.size.y+p^.origin.y) then
          res:=2;
     end;
    Check0:=res=2;
  end;

begin
  if (not TextModeGFV) and not UseFixedFont then
   exit;
{$ifdef USE_VIDEO_API}
  if ((state and sfV_CV_F) = sfV_CV_F) then
   begin
     p:=@Self;
     cur:=cursor;
     { in FVISION origin is always relative to screen corner
       for inserted views PM }
     inc(cur.X,p^.origin.X);
     inc(cur.Y,p^.origin.Y);
     while true do
      begin
        if (cur.x<p^.origin.x) or (cur.x>=p^.origin.x+p^.size.x) or
           (cur.y<p^.origin.y) or (cur.y>=p^.origin.y+p^.size.y) then
          break;
        p2:=p;
        G:=p^.owner;
        if G=Nil then { top view }
         begin
           if TextModeGFV then
             Video.SetCursorPos(cur.x,cur.y)
{$IFDEF GRAPH_API}
           else
             GFVGraph.SetCursorPos(cur.x,cur.y)
{$ENDIF GRAPH_API}
             ;
           if (state and sfCursorIns)<>0 then
             begin
               if TextModeGFV then
                 Video.SetCursorType(crBlock)
{$IFDEF GRAPH_API}
               else
                 GFVGraph.SetCursorType(crBlock)
{$ENDIF GRAPH_API}
                 ;
             end
           else
             begin
               if TextModeGFV then
                 Video.SetCursorType(crUnderline)
{$IFDEF GRAPH_API}
               else
                 GFVGraph.SetCursorType(crUnderline)
{$ENDIF GRAPH_API}
                 ;
             end;
           exit;
         end;
        if (G^.state and sfVisible)=0 then
         break;
        p:=G^.Last;
        if Check0 then
         break;
      end; { while }
   end; { if }
  if TextModeGFV then
    Video.SetCursorType(crHidden)
{$IFDEF GRAPH_API}
  else
    GFVGraph.SetCursorType(crHidden)
{$ENDIF GRAPH_API}
    ;
{$endif USE_VIDEO_API}
end;


{--TView--------------------------------------------------------------------}
{  Select -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05May98 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.Select;
BEGIN
   If (Options AND ofSelectable <> 0) Then            { View is selectable }
     If (Options AND ofTopSelect <> 0) Then MakeFirst { Top selectable }
     Else If (Owner <> Nil) Then                      { Valid owner }
       Owner^.SetCurrent(@Self, NormalSelect);        { Make owners current }
END;

{--TView--------------------------------------------------------------------}
{  Awaken -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.Awaken;
BEGIN                                                 { Abstract method }
END;

{--TView--------------------------------------------------------------------}
{  DrawView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06May98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.DrawView;
VAR ViewPort: ViewPortType;                           { Common variables }
    Parent : PGroup;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfExposed <> 0) AND                     { View is exposed }
   (State AND sfIconised = 0) Then Begin              { View not iconised }
     SetViewLimits;                                   { Set view limits }
     GetViewSettings(ViewPort, TextModeGFV);          { Get set viewport }
     If OverlapsArea(ViewPort.X1, ViewPort.Y1,
                     ViewPort.X2, ViewPort.Y2) Then
      Begin             { Must be in area }
         Parent:=Owner;
         While Assigned(Parent) do Begin
           If (Parent^.LockFlag>0) then
             Begin
               If (DrawMask = 0) OR (DrawMask = vdNoChild)  then
                 SetDrawMask(vdAll);
               ReleaseViewLimits;
               exit;
             End;
           Parent:=Parent^.Owner;
         End;
{$ifdef USE_VIDEO_API}
         LockScreenUpdate;                            { don't update the screen yet }
{$endif USE_VIDEO_API}
         HideMouseCursor;                             { Hide mouse cursor }
         If (DrawMask = 0) OR (DrawMask = vdNoChild)  { No special masks set }
            { OR Assigned(LimitsLocked) }
         Then Begin                                   { Treat as a full redraw }
           DrawBackGround;                            { Draw background }
           Draw;                                      { Draw interior }
           If (GOptions AND goDrawFocus <> 0) Then
             DrawFocus;                               { Draw focus }
           If (State AND sfCursorVis <> 0) Then
             DrawCursor;                              { Draw any cursor }
           If (Options AND ofFramed <> 0) OR
           (GOptions AND goThickFramed <> 0)          { View has border }
             Then DrawBorder;                         { Draw border }
{$ifndef NoShadow}
           If ((State AND sfShadow) <> 0) AND
              (GOptions And goNoShadow = 0) Then
             DrawShadow;
{$endif ndef NoShadow}
         End Else Begin                               { Masked draws only  }
           If (DrawMask AND vdBackGnd <> 0) Then      { Chk background mask }
             Begin
               DrawMask := DrawMask and Not vdBackGnd;
               DrawBackGround;                          { Draw background }
             end;
           If (DrawMask AND vdInner <> 0) Then        { Check Inner mask }
             Begin
               DrawMask := DrawMask and Not vdInner;
               Draw;                                    { Draw interior }
             End;
           If (DrawMask AND vdFocus <> 0)
           AND (GOptions AND goDrawFocus <> 0) then
             Begin
               DrawMask := DrawMask and Not vdFocus;
               DrawFocus;                          { Check focus mask }
             End;
           if not TextModeGFV then
            begin
              If (DrawMask AND vdCursor <> 0) Then       { Check cursor mask }
               Begin
                 DrawMask := DrawMask and Not vdCursor;
                 DrawCursor;                              { Draw any cursor }
               End;
            end;
           If (DrawMask AND vdBorder <> 0) Then       { Check border mask }
             Begin
               DrawMask := DrawMask and Not vdBorder;
               DrawBorder;                              { Draw border }
             End;
{$ifndef NoShadow}
           If ((State AND sfShadow) <> 0) AND
              (DrawMask AND vdShadow <> 0) AND
              (GOptions And goNoShadow = 0) AND
              (not assigned(Owner) OR (Owner^.GOptions And goNoShadow = 0)) Then
             Begin
               DrawMask := DrawMask and Not vdShadow;
               DrawShadow;
             End;
{$endif ndef NoShadow}
         End;
     ShowMouseCursor;                             { Show mouse cursor }
{$ifdef USE_VIDEO_API}
     UnlockScreenUpdate;
{$endif USE_VIDEO_API}
     if TextModeGFV or UseFixedFont then
      begin
        DrawScreenBuf;
        If (DrawMask AND vdCursor <> 0) Then       { Check cursor mask }
          Begin
            DrawMask := DrawMask and Not vdCursor;
            DrawCursor;                              { Draw any cursor }
          End;
      end;
     End;
     ReleaseViewLimits;                               { Release the limits }
   End;
   DrawMask := 0;                                     { Clear the draw mask }
END;

{--TView--------------------------------------------------------------------}
{  MakeFirst -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Sep99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.MakeFirst;
BEGIN
   If (Owner <> Nil) Then Begin                       { Must have owner }
     PutInFrontOf(Owner^.First);                      { Float to the top }
   End;
END;

{--TView--------------------------------------------------------------------}
{  DrawFocus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.DrawFocus;
BEGIN                                                 { Abstract method }
END;

{--TView--------------------------------------------------------------------}
{  DrawCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.DrawCursor;
BEGIN                                                 { Abstract method }
  if State and sfFocused <> 0 then
    ResetCursor;
END;

{--TView--------------------------------------------------------------------}
{  DrawBorder -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.DrawBorder;
BEGIN
   If (TextModeGFV = FALSE) Then Begin                { GRAPHICS GFV MODE }
     BiColorRectangle(0, 0, RawSize.X, RawSize.Y,
       White, DarkGray, False);                       { Draw 3d effect }
     If (GOptions AND goThickFramed <> 0) Then Begin  { Thick frame at work }
       GraphRectangle(1, 1, RawSize.X-1, RawSize.Y-1,
         LightGray);                                  { Draw frame part 1 }
       GraphRectangle(2, 2, RawSize.X-2, RawSize.Y-2,
         LightGray);                                  { Fraw frame part 2 }
       BiColorRectangle(3, 3, RawSize.X-3, RawSize.Y-3,
         White, DarkGray, True);                      { Draw highlights }
     End;
   End;
   { TView DrawBorder is empty for TextModeGFV }
END;

PROCEDURE TView.DrawShadow;
VAR X1, Y1, X2, Y2 : Sw_Integer;
BEGIN
{$ifdef DEBUG}
   if WriteDebugInfo then
     Begin
       Writeln(stderr,'TView(',hexstr(longint(@self),8),')');
       Writeln(stderr,'Object Type(',hexstr(plongint(@self)^,8),')');
       Writeln(stderr,'DrawShadow');
     End;
{$endif DEBUG}
  If not TextModeGFV then
    exit;
  If Assigned(Owner) Then Begin
    X1:=RawOrigin.X+RawSize.X+1;
    X2:=X1+ShadowSize.X*SysFontWidth;
    Y1:=RawOrigin.Y+SysFontHeight;
    Y2:=RawOrigin.Y+RawSize.Y+1+ShadowSize.Y*SysFontHeight;
    GOptions := GOptions OR goNoShadow;
    Owner^.GOptions := Owner^.GOptions OR goNoShadow;
    Owner^.RedrawArea(X1,Y1,X2,Y2);
    WriteShadow(X1 div SysFontWidth, Y1 div SysFontHeight,
      X2 div SysFontWidth, Y2 div SysFontHeight);
    X1:=RawOrigin.X+SysFontWidth;
    X2:=RawOrigin.X+RawSize.X+1;
    Y1:=RawOrigin.Y+RawSize.Y+1;
    Y2:=RawOrigin.Y+RawSize.Y+1+ShadowSize.Y*SysFontHeight;
    Owner^.RedrawArea(X1,Y1,X2,Y2);
    WriteShadow(X1 div SysFontWidth, Y1 div SysFontHeight,
      X2 div SysFontWidth, Y2 div SysFontHeight);
    GOptions := GOptions AND not goNoShadow;
    Owner^.GOptions := Owner^.GOptions AND not goNoShadow;
  End;
END;

{--TView--------------------------------------------------------------------}
{  HideCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.HideCursor;
BEGIN
   SetState(sfCursorVis , False);                     { Hide the cursor }
END;

{--TView--------------------------------------------------------------------}
{  ShowCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.ShowCursor;
BEGIN
   SetState(sfCursorVis , True);                      { Show the cursor }
END;

{--TView--------------------------------------------------------------------}
{  BlockCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.BlockCursor;
BEGIN
   SetState(sfCursorIns, True);                       { Set insert mode }
END;

{--TView--------------------------------------------------------------------}
{  NormalCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TView.NormalCursor;
BEGIN
   SetState(sfCursorIns, False);                      { Clear insert mode }
END;

{--TView--------------------------------------------------------------------}
{  FocusFromTop -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11Aug99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TView.FocusFromTop;
BEGIN
   If (Owner <> Nil) AND
     (Owner^.State AND sfSelected = 0)
       Then Owner^.Select;
   If (State AND sfFocused = 0) Then Focus;
   If (State AND sfSelected = 0) Then Select;
END;

{--TView--------------------------------------------------------------------}
{  SetViewLimits -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22Sep99 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetViewLimits;
VAR X1, Y1, X2, Y2: Sw_Integer; P: PGroup; ViewPort: ViewPortType; Ca: PComplexArea;
BEGIN
{$ifndef PPC_FPC}
   If (MaxAvail >= SizeOf(TComplexArea)) Then
{$endif}
    Begin   { Check enough memory }
     GetMem(Ca, SizeOf(TComplexArea));                { Allocate memory }
     GetViewSettings(ViewPort, TextModeGFV or UseFixedFont);          { Fetch view port }
     Ca^.X1 := ViewPort.X1;                           { Hold current X1 }
     Ca^.Y1 := ViewPort.Y1;                           { Hold current Y1 }
     Ca^.X2 := ViewPort.X2;                           { Hold current X2 }
     Ca^.Y2 := ViewPort.Y2;                           { Hold current Y2 }
     Ca^.NextArea := HoldLimit;                       { Pointer to next }
     HoldLimit := Ca;                                 { Move down chain }
     X1 := RawOrigin.X;                               { Xfer x raw origin }
     Y1 := RawOrigin.Y;                               { Xfer y raw origin }
     X2 := X1 + RawSize.X;                            { Calc right value }
     Y2 := Y1 + RawSize.Y;                            { Calc lower value }
     P := Owner;                                      { Start on owner }
     While (P <> Nil) Do Begin                        { While owner valid }
      If (X1 < P^.RawOrigin.X) Then
         X1 := P^.RawOrigin.X;                        { X minimum contain }
       If (Y1 < P^.RawOrigin.Y) Then
         Y1 := P^.RawOrigin.Y;                        { Y minimum contain }
       If (X2 > P^.RawOrigin.X + P^.RawSize.X)
         Then X2 := P^.RawOrigin.X + P^.RawSize.X;    { X maximum contain }
       If (Y2 > P^.RawOrigin.Y + P^.RawSize.Y)
         Then Y2 := P^.RawOrigin.Y + P^.RawSize.Y;    { Y maximum contain }
       P := P^.Owner;                                 { Move to owners owner }
     End;
     If TextModeGFV or UseFixedFont then Begin
       X1 := X1 div SysFontWidth;
       X2 := (X2 +SysFontWidth - 1) div SysFontWidth;
       Y1 := Y1 div SysFontHeight;
       Y2 := (Y2 +SysFontHeight -1)  div SysFontHeight;
     End;

     If (LimitsLocked <> Nil) Then Begin              { Locked = area redraw }
       If (X2 < ViewPort.X1) Then Exit;               { View left of locked }
       If (X1 > ViewPort.X2) Then Exit;               { View right of locked }
       If (Y2 < ViewPort.Y1) Then Exit;               { View above locked }
       If (Y1 > ViewPort.Y2) Then Exit;               { View below locked }
       If (X1 < ViewPort.X1) Then X1 := ViewPort.X1;  { Adjust x1 to locked }
       If (Y1 < ViewPort.Y1) Then Y1 := ViewPort.Y1;  { Adjust y1 to locked }
       If (X2 > ViewPort.X2) Then X2 := ViewPort.X2;  { Adjust x2 to locked }
       If (Y2 > ViewPort.Y2) Then Y2 := ViewPort.Y2;  { Adjust y2 to locked }
     End;

     SetViewPort(X1, Y1, X2, Y2, ClipOn, TextModeGFV or UseFixedFont);{ Set new clip limits }
   End;
END;

{--TView--------------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 21Sep99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TView.DrawBackGround;
VAR Bc: Byte; X1, Y1, X2, Y2: Sw_Integer; ViewPort: ViewPortType;
    X, Y: Sw_Integer;
    Buf : TDrawBuffer;
BEGIN
   If (GOptions AND goNoDrawView = 0) Then Begin      { Non draw views exit }
     If (State AND sfDisabled = 0) Then
       Bc := GetColor(1) AND $F0 SHR 4 Else           { Select back colour }
       Bc := GetColor(4) AND $F0 SHR 4;               { Disabled back colour }
     GetViewSettings(ViewPort, TextModeGFV or UseFixedFont);          { Get view settings }
     If not TextModeGFV and not UseFixedFont Then Begin            { GRAPHICS MODE GFV }
       If (ViewPort.X1 <= RawOrigin.X) Then X1 := 0     { Right to left edge }
         Else X1 := ViewPort.X1-RawOrigin.X;            { Offset from left }
       If (ViewPort.Y1 <= RawOrigin.Y) Then Y1 := 0     { Right to top edge }
         Else Y1 := ViewPort.Y1-RawOrigin.Y;            { Offset from top }
       If (ViewPort.X2 >= RawOrigin.X+RawSize.X) Then
         X2 := RawSize.X Else                           { Right to right edge }
         X2 := ViewPort.X2-RawOrigin.X;                 { Offset from right }
       If (ViewPort.Y2 >= RawOrigin.Y+RawSize.Y) Then
         Y2 := RawSize.Y Else                           { Right to bottom edge }
         Y2 := ViewPort.Y2-RawOrigin.Y;                 { Offset from bottom }
       SetFillStyle(SolidFill, Bc);                 { Set fill colour }
       Bar(0, 0, X2-X1, Y2-Y1);                     { Clear the area }
     End Else Begin                                 { TEXT MODE GFV }
       If (ViewPort.X1 <= Origin.X) Then
         X1 := Origin.X     { Right to left edge }
         Else X1 := ViewPort.X1;            { Offset from left }
       If (ViewPort.Y1 <= Origin.Y) Then
         Y1 := Origin.Y     { Right to top edge }
         Else Y1 := ViewPort.Y1;            { Offset from top }
       If (ViewPort.X2 >= Origin.X+Size.X) Then
         X2 := Origin.X + Size.X Else                           { Right to right edge }
         X2 := ViewPort.X2;                 { Offset from right }
       If (ViewPort.Y2 >= Origin.Y+Size.Y) Then
         Y2 := Origin.Y + Size.Y Else                           { Right to bottom edge }
         Y2 := ViewPort.Y2;                 { Offset from bottom }
         If (State AND sfDisabled = 0) Then
           Bc := GetColor(1) Else           { Select back colour }
           Bc := GetColor(4);               { Disabled back colour }
         For X := X1 To X2 Do Begin
           Buf[X-X1]:=(Bc shl 8) or ord(BackgroundChar){not a directive,was $20};
         End;
         For Y := Y1 To Y2 Do Begin
           WriteAbs(X1,Y, X2-X1, Buf);
         End;
         DrawScreenBuf;
       End;
   End;
END;

{--TView--------------------------------------------------------------------}
{  ReleaseViewLimits -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05May98 LdB }
{---------------------------------------------------------------------------}
PROCEDURE TView.ReleaseViewLimits;
VAR P: PComplexArea;
BEGIN
   P := HoldLimit;                                    { Transfer pointer }
   If (P <> Nil) Then Begin                           { Valid complex area }
     HoldLimit := P^.NextArea;                        { Move to prior area }
     SetViewPort(P^.X1, P^.Y1, P^.X2, P^.Y2, ClipOn,
       TextModeGFV or UseFixedFont);                                  { Restore clip limits }
     FreeMem(P, SizeOf(TComplexArea));                { Release memory }
   End;
END;

{--TView--------------------------------------------------------------------}
{  MoveTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.MoveTo (X, Y: Sw_Integer);
VAR R: TRect;
BEGIN
   R.Assign(X, Y, X + Size.X, Y + Size.Y);            { Assign area }
   Locate(R);                                         { Locate the view }
END;

{--TView--------------------------------------------------------------------}
{  GrowTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.GrowTo (X, Y: Sw_Integer);
VAR R: TRect;
BEGIN
   R.Assign(Origin.X, Origin.Y, Origin.X + X,
     Origin.Y + Y);                                   { Assign area }
   Locate(R);                                         { Locate the view }
END;

{--TView--------------------------------------------------------------------}
{  SetDrawMask -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Sep99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetDrawMask (Mask: Byte);
VAR
    OldMask : byte;
BEGIN
   If (Options AND ofFramed = 0) AND                  { Check for no frame }
     (GOptions AND goThickFramed = 0) AND             { Check no thick frame }
     (GOptions AND goTitled = 0) Then                 { Check for title }
       Mask := Mask AND NOT vdBorder;                 { Clear border draw }
   If (State AND sfCursorVis = 0) Then                { Check for no cursor }
     Mask := Mask AND NOT vdCursor;                   { Clear cursor draw }
   If (GOptions AND goDrawFocus = 0) Then             { Check no focus draw }
     Mask := Mask AND NOT vdFocus;                    { Clear focus draws }
   OldMask:=DrawMask;
   DrawMask := DrawMask OR Mask;                      { Set draw masks }
END;

{--TView--------------------------------------------------------------------}
{  EndModal -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.EndModal (Command: Word);
VAR P: PView;
BEGIN
   P := TopView;                                      { Get top view }
   If (P <> Nil) Then P^.EndModal(Command);           { End modal operation }
END;

{--TView--------------------------------------------------------------------}
{  SetCursor -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetCursor (X, Y: Sw_Integer);
BEGIN
   Cursor.X := X;                                     { New x position }
   Cursor.Y := Y;                                     { New y position }
   If ((DrawMask and vdInSetCursor)=0) and (State AND sfCursorVis <> 0) Then
     Begin         { Cursor visible }
       if TextModeGFV or UseFixedFont then
        ResetCursor
       else
        begin
          SetDrawMask(vdCursor or vdInSetCursor);          { Set draw mask }
          DrawView;                                        { Draw the cursor }
          DrawMask:=DrawMask and not vdInSetCursor;
        end;
     End;
END;

{--TView--------------------------------------------------------------------}
{  PutInFrontOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Sep99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TView.PutInFrontOf (Target: PView);
VAR P, LastView: PView;
BEGIN
   If (Owner <> Nil) AND (Target <> @Self) AND
   (Target <> NextView) AND ((Target = Nil) OR
   (Target^.Owner = Owner)) Then                      { Check validity }
     If (State AND sfVisible = 0) Then Begin          { View not visible }
       Owner^.RemoveView(@Self);                      { Remove from list }
       Owner^.InsertView(@Self, Target);              { Insert into list }
     End Else Begin
       LastView := NextView;                          { Hold next view }
       If (LastView <> Nil) Then Begin                { Lastview is valid }
         P := Target;                                 { P is target }
         While (P <> Nil) AND (P <> LastView)
           Do P := P^.NextView;                       { Find our next view }
         If (P = Nil) Then LastView := Target;        { Lastview is target }
       End;
       State := State AND NOT sfVisible;              { Temp stop drawing }
       If (LastView = Target) Then
         If (Owner <> Nil) Then Owner^.ReDrawArea(
           RawOrigin.X, RawOrigin.Y, RawOrigin.X + RawSize.X,
           RawOrigin.Y + RawSize.Y);       { Redraw old area }
       Owner^.Lock;
       Owner^.RemoveView(@Self);                      { Remove from list }
       Owner^.InsertView(@Self, Target);              { Insert into list }
       State := State OR sfVisible;                   { Allow drawing again }
       If (LastView <> Target) Then
         Begin
           SetDrawMask(vdAll);
           DrawView;         { Draw the view now }
         End;
       If (Options AND ofSelectable <> 0) Then        { View is selectable }
         If (Owner <> Nil) Then Owner^.ResetCurrent;  { Reset current }
       Owner^.Unlock;
     End;
END;

{ ******************************* REMARK ****************************** }
{   The original TV origin data is only adjusted incase the user uses   }
{  the values directly. New views should rely only on RawOrigin values. }
{ ****************************** END REMARK *** Leon de Boer, 15May98 * }

{--TView--------------------------------------------------------------------}
{  DisplaceBy -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.DisplaceBy (Dx, Dy: Sw_Integer);
BEGIN
   RawOrigin.X := RawOrigin.X + Dx;                   { Displace raw x }
   RawOrigin.Y := RawOrigin.Y + Dy;                   { Displace raw y }
   Origin.X := RawOrigin.X DIV FontWidth;             { Calc new x origin }
   Origin.Y := RawOrigin.Y DIV FontHeight;            { Calc new y origin }
END;

{--TView--------------------------------------------------------------------}
{  SetCommands -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetCommands (Commands: TCommandSet);
BEGIN
   CommandSetChanged := CommandSetChanged OR
     (CurCommandSet <> Commands);                     { Set change flag }
   CurCommandSet := Commands;                         { Set command set }
END;

{--TView--------------------------------------------------------------------}
{  ReDrawArea -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.ReDrawArea (X1, Y1, X2, Y2: Sw_Integer);
var
  StoreDrawMask : Byte;
VAR HLimit: PView; ViewPort: ViewPortType;
BEGIN
{$ifdef DEBUG}
   if WriteDebugInfo then
     Begin
       Writeln(stderr,'TView(',hexstr(longint(@self),8),')');
       Writeln(stderr,'Object Type(',hexstr(plongint(@self)^,8),')');
       Writeln(stderr,'ReDrawArea(',X1,',',Y1,',',X2,',',Y2,')');
     End;
{$endif DEBUG}
   GetViewSettings(ViewPort, TextModeGFV);            { Hold view port }
   If TextModeGFV then Begin
     X1 := X1 div SysFontWidth;
     X2 := (X2 +SysFontWidth - 1) div SysFontWidth;
     Y1 := Y1 div SysFontHeight;
     Y2 := (Y2 +SysFontHeight -1)  div SysFontHeight;
   End;
   SetViewPort(X1, Y1, X2, Y2, ClipOn, TextModeGFV);  { Set new clip limits }
   HLimit := LimitsLocked;                            { Hold lock limits }
   LimitsLocked := @Self;                             { We are the lock view }
   StoreDrawMask:=DrawMask;
   DrawMask:=vdAll;
   DrawView;                                          { Redraw the area }
   DrawMask:=StoreDrawMask;
   LimitsLocked := HLimit;                            { Release our lock }
   SetViewPort(ViewPort.X1, ViewPort.Y1,
     ViewPort.X2, ViewPort.Y2, ClipOn, TextModeGFV);  { Reset old limits }
END;

{--TView--------------------------------------------------------------------}
{  EnableCommands -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TView.EnableCommands (Commands: TCommandSet);
BEGIN
   CommandSetChanged := CommandSetChanged OR
     (CurCommandSet * Commands <> Commands);          { Set changed flag }
   CurCommandSet := CurCommandSet + Commands;         { Update command set }
END;

{--TView--------------------------------------------------------------------}
{  DisableCommands -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB   }
{---------------------------------------------------------------------------}
PROCEDURE TView.DisableCommands (Commands: TCommandSet);
BEGIN
   CommandSetChanged := CommandSetChanged OR
     (CurCommandSet * Commands <> []);                { Set changed flag }
   CurCommandSet := CurCommandSet - Commands;         { Update command set }
END;

{--TView--------------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23Sep99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetState (AState: Word; Enable: Boolean);
VAR OldState, Command: Word;
    ShouldDraw : Boolean;
BEGIN
   OldState := State;
   If Enable Then State := State OR AState            { Set state mask }
     Else State := State AND NOT AState;              { Clear state mask }
   ShouldDraw:=false;
   If (AState AND sfVisible <> 0) Then Begin          { Visibilty change }
     If (Owner <> Nil) AND                            { valid owner }
     (Owner^.State AND sfExposed <> 0)                { If owner exposed }
       Then SetState(sfExposed, Enable);              { Expose this view }
     If Enable Then DrawView Else                     { Draw the view }
       If (Owner <> Nil) Then Owner^.ReDrawArea(      { Owner valid }
         RawOrigin.X, RawOrigin.Y,
         RawOrigin.X + RawSize.X + ShadowSize.X*SysFontWidth,
         RawOrigin.Y + RawSize.Y + ShadowSize.Y*SysFontHeight);         { Owner redraws area }
     If (Options AND ofSelectable <> 0) Then          { View is selectable }
       If (Owner <> Nil) Then Owner^.ResetCurrent;    { Reset selected }
   End;
   If (AState AND sfFocused <> 0) Then Begin          { Focus change }
     If (Owner <> Nil) Then Begin                     { Owner valid }
       If Enable Then Command := cmReceivedFocus      { View gaining focus }
         Else Command := cmReleasedFocus;             { View losing focus }
       Message(Owner, evBroadcast, Command, @Self);   { Send out message }
       SetDrawMask(vdBorder);                           { Set border draw mask }
       ShouldDraw:=true;
     End;
     If (GOptions AND goDrawFocus <> 0) AND
        (((AState XOR OldState) AND sfFocused) <> 0) Then Begin    { Draw focus view }
       SetDrawMask(vdFocus);                          { Set focus draw mask }
       ShouldDraw:=true;
     End;
   End;
   If (AState AND (sfCursorVis + sfCursorIns) <> 0) and  { Change cursor state }
      (OldState<>State)
   Then Begin
     if TextModeGFV or UseFixedFont then
      ResetCursor
     else
      begin
        SetDrawMask(vdCursor);       { Set cursor draw mask }
        ShouldDraw:=true;
      end;
   End;
   If ShouldDraw then
       begin
         DrawView;                                      { Redraw the border }
       end;
END;

{--TView--------------------------------------------------------------------}
{  SetCmdState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetCmdState (Commands: TCommandSet; Enable: Boolean);
BEGIN
   If Enable Then EnableCommands(Commands)            { Enable commands }
     Else DisableCommands(Commands);                  { Disable commands }
END;

{--TView--------------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetData (Var Rec);
BEGIN                                                 { Abstract method }
END;

{--TView--------------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetData (Var Rec);
BEGIN                                                 { Abstract method }
END;

{--TView--------------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06May98 LdB             }
{---------------------------------------------------------------------------}
{  You can save data to the stream compatable with the old original TV by   }
{  temporarily turning off the ofGFVModeView making the call to this store  }
{  routine and resetting the ofGFVModeView flag after the call.             }
{---------------------------------------------------------------------------}
PROCEDURE TView.Store (Var S: TStream);
VAR SaveState: Word;
BEGIN
   SaveState := State;                                { Hold current state }
   State := State AND NOT (sfActive OR sfSelected OR
     sfFocused OR sfExposed);                         { Clear flags }
   S.Write(Origin.X, 2);                              { Write view x origin }
   S.Write(Origin.Y, 2);                              { Write view y origin }
   S.Write(Size.X, 2);                                { Write view x size }
   S.Write(Size.Y, 2);                                { Write view y size }
   S.Write(Cursor.X, 2);                              { Write cursor x size }
   S.Write(Cursor.Y, 2);                              { Write cursor y size }
   S.Write(GrowMode, 1);                              { Write growmode flags }
   S.Write(DragMode, 1);                              { Write dragmode flags }
   S.Write(HelpCtx, 2);                               { Write help context }
   S.Write(State, 2);                                 { Write state masks }
   S.Write(Options, 2);                               { Write options masks }
   S.Write(Eventmask, 2);                             { Write event masks }
   If (Options AND ofGFVModeView <> 0) Then Begin     { GFV GRAPHICAL TVIEW }
     S.Write(GOptions, 2);                            { Write new option masks }
     S.Write(TabMask, 1);                             { Write new tab masks }
     S.Write(RawOrigin.X, 2);                         { Write raw origin x point }
     S.Write(RawOrigin.Y, 2);                         { Write raw origin y point }
     S.Write(RawSize.X, 2);                           { Write raw x size }
     S.Write(RawSize.Y, 2);                           { Write raw y size }
     S.Write(ColourOfs, 2);                           { Write Palette offset }
   End;
   State := SaveState;                                { Reset state masks }
END;

{--TView--------------------------------------------------------------------}
{  Locate -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24Sep99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.Locate (Var Bounds: TRect);
VAR
    X1, Y1, X2, Y2: Sw_Integer;
    Min, Max: TPoint; R: TRect;

   FUNCTION Range(Val, Min, Max: Sw_Integer): Sw_Integer;
   BEGIN
     If (Val < Min) Then Range := Min Else            { Value to small }
       If (Val > Max) Then Range := Max Else          { Value to large }
         Range := Val;                                { Value is okay }
   END;

BEGIN
   X1 := RawOrigin.X;                                 { Current x origin }
   Y1 := RawOrigin.Y;                                 { Current y origin }
   X2 := RawOrigin.X + RawSize.X;                     { Current x size }
   Y2 := RawOrigin.Y + RawSize.Y;                     { Current y size }
   SizeLimits(Min, Max);                              { Get size limits }
   Bounds.B.X := Bounds.A.X + Range(Bounds.B.X -
     Bounds.A.X, Min.X, Max.X);                       { X bound limit }
   Bounds.B.Y := Bounds.A.Y + Range(Bounds.B.Y
     - Bounds.A.Y, Min.Y, Max.Y);                     { Y bound limit }
   GetBounds(R);                                      { Current bounds }
   If NOT Bounds.Equals(R) Then Begin                 { Size has changed }
     ChangeBounds(Bounds);                            { Change bounds }
     If (State AND sfVisible <> 0) AND                { View is visible }
     (State AND sfExposed <> 0) AND (Owner <> Nil)    { Check view exposed }
       Then Owner^.ReDrawArea(X1, Y1, X2, Y2);        { Owner redraw }
     DrawView;                                        { Redraw the view }
   End;
END;

{--TView--------------------------------------------------------------------}
{  KeyEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.KeyEvent (Var Event: TEvent);
BEGIN
   Repeat
     GetEvent(Event);                                 { Get next event }
   Until (Event.What = evKeyDown);                    { Wait till keydown }
END;

{--TView--------------------------------------------------------------------}
{  GetEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetEvent (Var Event: TEvent);
BEGIN
  If (Owner <> Nil) Then Owner^.GetEvent(Event);      { Event from owner }
END;

{--TView--------------------------------------------------------------------}
{  PutEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TView.PutEvent (Var Event: TEvent);
BEGIN
   If (Owner <> Nil) Then Owner^.PutEvent(Event);     { Put in owner }
END;

{--TView--------------------------------------------------------------------}
{  GetExtent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetExtent (Var Extent: TRect);
BEGIN
   Extent.A.X := 0;                                   { Zero x field }
   Extent.A.Y := 0;                                   { Zero y field }
   Extent.B.X := Size.X;                              { Return x size }
   Extent.B.Y := Size.Y;                              { Return y size }
END;

{--TView--------------------------------------------------------------------}
{  GetBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetBounds (Var Bounds: TRect);
BEGIN
   Bounds.A := Origin;                                { Get first corner }
   Bounds.B.X := Origin.X + Size.X;                   { Calc corner x value }
   Bounds.B.Y := Origin.Y + Size.Y;                   { Calc corner y value }
   If (Owner <> Nil) Then
     Bounds.Move(-Owner^.Origin.X, -Owner^.Origin.Y); { Sub owner offset }
END;

{--TView--------------------------------------------------------------------}
{  SetBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24Sep99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.SetBounds (Var Bounds: TRect);
VAR D, COrigin: TPoint;
BEGIN
   { Remove shadow first }
   if (State and (sfShadow or sfVisible or sfExposed) =
        (sfShadow or sfVisible or sfExposed)) and
      assigned(Owner) then
     begin
       State:= State and not sfShadow;
       Owner^.ReDrawArea(RawOrigin.X + RawSize.X, RawOrigin.Y,
         RawOrigin.X + RawSize.X + ShadowSize.X*SysFontWidth,
         RawOrigin.Y + RawSize.Y + ShadowSize.Y*SysFontHeight);         { Owner redraws area }
       Owner^.ReDrawArea(RawOrigin.X, RawOrigin.Y + RawSize.Y,
         RawOrigin.X + RawSize.X + ShadowSize.X*SysFontWidth,
         RawOrigin.Y + RawSize.Y + ShadowSize.Y*SysFontHeight);         { Owner redraws area }
       State:= State or sfShadow;
     end;
   If (Bounds.B.X > 0) AND (Bounds.B.Y > 0)           { Normal text co-ords }
   AND (GOptions AND goGraphView = 0) Then Begin      { Normal text view }
     If (Owner <> Nil) Then Begin                     { Owner is valid }
       COrigin.X := Origin.X - Owner^.Origin.X;       { Corrected x origin }
       COrigin.Y := Origin.Y - Owner^.Origin.Y;       { Corrected y origin }
       D.X := Bounds.A.X - COrigin.X;                 { X origin disp }
       D.Y := Bounds.A.Y - COrigin.Y;                 { Y origin disp }
       If ((D.X <> 0) OR (D.Y <> 0)) Then
         DisplaceBy(D.X*FontWidth, D.Y*FontHeight);   { Offset the view }
     End Else Origin := Bounds.A;                     { Hold as origin }
     Size.X := Bounds.B.X-Bounds.A.X;                 { Hold view x size }
     Size.Y := Bounds.B.Y-Bounds.A.Y;                 { Hold view y size }
     RawOrigin.X := Origin.X * FontWidth;             { Raw x origin }
     RawOrigin.Y := Origin.Y * FontHeight;            { Raw y origin }
     RawSize.X := Size.X * FontWidth - 1;             { Set raw x size }
     RawSize.Y := Size.Y * FontHeight - 1;            { Set raw y size }
   End Else Begin                                     { Graphical co-ords }
     If (Owner <> Nil) Then Begin                     { Owner is valid }
       COrigin.X := RawOrigin.X - Owner^.RawOrigin.X; { Corrected x origin }
       COrigin.Y := RawOrigin.Y - Owner^.RawOrigin.Y; { Corrected y origin }
       D.X := Bounds.A.X - COrigin.X;                 { X origin disp }
       D.Y := Bounds.A.Y - COrigin.Y;                 { Y origin disp }
       If ((D.X <> 0) OR (D.Y <> 0)) Then
         DisplaceBy(D.X, D.Y);                        { Offset the view }
     End Else RawOrigin := Bounds.A;                  { Hold as origin }
     RawSize.X := Abs(Bounds.B.X) - Bounds.A.X;       { Set raw x size }
     RawSize.Y := Abs(Bounds.B.Y) - Bounds.A.Y;       { Set raw y size }
     Origin.X := RawOrigin.X DIV FontWidth;           { Rough x position }
     Origin.Y := RawOrigin.Y DIV FontHeight;          { Rough y position }
     Size.X := RawSize.X DIV FontWidth;               { Rough x size }
     Size.Y := RawSize.Y DIV FontHeight;              { Rough y size }
   End;
   Options := Options OR ofGFVModeView;               { Now in GFV mode }
END;

{--TView--------------------------------------------------------------------}
{  GetClipRect -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetClipRect (Var Clip: TRect);
BEGIN
   GetBounds(Clip);                                   { Get current bounds }
   If (Owner <> Nil) Then Clip.Intersect(Owner^.Clip);{ Intersect with owner }
   Clip.Move(-Origin.X, -Origin.Y);                   { Sub owner origin }
END;

{--TView--------------------------------------------------------------------}
{  ClearEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.ClearEvent (Var Event: TEvent);
BEGIN
   Event.What := evNothing;                           { Clear the event }
   Event.InfoPtr := @Self;                            { Set us as handler }
END;

{--TView--------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.HandleEvent (Var Event: TEvent);
BEGIN
   If (Event.What = evMouseDown) Then                 { Mouse down event }
     If (State AND (sfSelected OR sfDisabled) = 0)    { Not selected/disabled }
       AND (Options AND ofSelectable <> 0) Then       { View is selectable }
       If (Focus = False) OR                          { Not view with focus }
         (Options AND ofFirstClick = 0)               { Not 1st click select }
           Then ClearEvent(Event);                    { Handle the event }
   If (Event.What = evKeyDown) AND                    { Key down event }
   (Options OR ofGFVModeView <> 0) Then Begin         { GFV mode view check }
     If (Owner <> Nil) AND (TabMask <> 0) AND         { Owner and tab masks }
     (State AND sfFocused <> 0) Then Begin            { View has focus }
       Case Event.KeyCode Of
         kbTab: If (TabMask AND tmTab <> 0) Then      { Tab key mask set }
           Owner^.FocusNext(False) Else Exit;         { Focus next view }
         kbEnter: If (TabMask AND tmEnter <> 0) Then  { Enter key mask set }
           Owner^.FocusNext(False) Else Exit;         { Focus next view }
         kbShiftTab: If (TabMask AND tmShiftTab <> 0) { Shit tab mask set }
           Then Owner^.FocusNext(True) Else Exit;     { Focus prior view }
         kbLeft: If (TabMask AND tmLeft <> 0) Then    { Left arrow mask set }
           Owner^.FocusNext(True) Else Exit;          { Focus prior view }
         kbRight: If (TabMask AND tmRight <> 0) Then  { Right arrow mask set }
           Owner^.FocusNext(False) Else Exit;         { Focus next view }
         kbUp: If (TabMask AND tmUp <> 0) Then        { Up arrow mask set }
           Owner^.FocusNext(True) Else Exit;          { Focus prior view }
         kbDown: If (TabMask AND tmDown <> 0) Then    { Down arrow mask set }
           Owner^.FocusNext(False) Else Exit;         { Focus next view }
         Else Exit;                                   { Not a tab key }
       End;
       ClearEvent(Event);                             { Clear handled events }
     End;
   End;
END;

{--TView--------------------------------------------------------------------}
{  ChangeBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TView.ChangeBounds (Var Bounds: TRect);
BEGIN
   SetBounds(Bounds);                                 { Set new bounds }
   DrawView;                                          { Draw the view }
END;

{--TView--------------------------------------------------------------------}
{  SizeLimits -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.SizeLimits (Var Min, Max: TPoint);
BEGIN
   Min.X := 0;                                        { Zero x minimum }
   Min.Y := 0;                                        { Zero y minimum }
   If (Owner = Nil) Then Begin
     Max.X := $7FFF;                                  { Max possible x size }
     Max.Y := $7FFF;                                  { Max possible y size }
   End Else Max := Owner^.Size;                       { Max owner size }
END;

{--TView--------------------------------------------------------------------}
{  GetCommands -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetCommands (Var Commands: TCommandSet);
BEGIN
   Commands := CurCommandSet;                         { Return command set }
END;

{--TView--------------------------------------------------------------------}
{  GetPeerViewPtr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TView.GetPeerViewPtr (Var S: TStream; Var P);
VAR Index: Sw_Integer;
BEGIN
   Index := 0;                                        { Zero index value }
   S.Read(Index, 2);                                  { Read view index }
   If (Index = 0) OR (OwnerGroup = Nil) Then          { Check for peer views }
     Pointer(P) := Nil Else Begin                     { Return nil }
       Pointer(P) := FixupList^[Index];               { New view ptr }
       FixupList^[Index] := @P;                       { Patch this pointer }
     End;
END;

{--TView--------------------------------------------------------------------}
{  PutPeerViewPtr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TView.PutPeerViewPtr (Var S: TStream; P: PView);
VAR Index: Sw_Integer;
BEGIN
   If (P = Nil) OR (OwnerGroup = Nil) Then Index := 0 { Return zero index }
     Else Index := OwnerGroup^.IndexOf(P);            { Return view index }
   S.Write(Index, 2);                                 { Write the index }
END;

{--TView--------------------------------------------------------------------}
{  CalcBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TView.CalcBounds (Var Bounds: Objects.TRect; Delta: TPoint);
VAR S, D: Sw_Integer; Min, Max: TPoint;

   FUNCTION Range (Val, Min, Max: Sw_Integer): Sw_Integer;
   BEGIN
     If (Val < Min) Then Range := Min Else            { Value below min }
     If (Val > Max) Then Range := Max Else            { Value above max }
       Range := Val;                                  { Accept value }
   END;

   PROCEDURE GrowI (Var I: Sw_Integer);
   BEGIN
     If (GrowMode AND gfGrowRel = 0) Then Inc(I, D)
       Else I := (I * S + (S - D) SHR 1) DIV (S - D); { Calc grow value }
   END;

BEGIN
   GetBounds(Bounds);                                 { Get bounds }
   If (GrowMode = 0) Then Exit;                       { No grow flags exits }
   S := Owner^.Size.X;                                { Set initial size }
   D := Delta.X;                                      { Set initial delta }
   If (GrowMode AND gfGrowLoX <> 0) Then
     GrowI(Bounds.A.X);                                { Grow left side }
   If (GrowMode AND gfGrowHiX <> 0) Then
     GrowI(Bounds.B.X);                                { Grow right side }
   If (Bounds.B.X - Bounds.A.X > MaxViewWidth) Then
     Bounds.B.X := Bounds.A.X + MaxViewWidth;         { Check values }
   S := Owner^.Size.Y; D := Delta.Y;                  { set initial values }
   If (GrowMode AND gfGrowLoY <> 0) Then
     GrowI(Bounds.A.Y);                                { Grow top side }
   If (GrowMode AND gfGrowHiY <> 0) Then
     GrowI(Bounds.B.Y);                                { grow lower side }
   SizeLimits(Min, Max);                              { Check sizes }
   Bounds.B.X := Bounds.A.X + Range(Bounds.B.X -
     Bounds.A.X, Min.X, Max.X);                       { Set right side }
   Bounds.B.Y := Bounds.A.Y + Range(Bounds.B.Y -
     Bounds.A.Y, Min.Y, Max.Y);                       { Set lower side }
END;

{***************************************************************************}
{                       TView OBJECT PRIVATE METHODS                        }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TGroup OBJECT METHODS                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TGroup-------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TGroup.Init (Var Bounds: TRect);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Options := Options OR (ofSelectable + ofBuffered); { Set options }
   GOptions := GOptions OR goNoDrawView;              { Non drawing view }
   GetExtent(Clip);                                   { Get clip extents }
   EventMask := $FFFF;                                { See all events }
   GOptions := GOptions OR goTabSelect;               { Set graphic options }
END;

{--TGroup-------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TGroup.Load (Var S: TStream);
VAR I, Count: Sw_Word; P, Q: ^Pointer; V: PView; OwnerSave: PGroup;
    FixupSave: PFixupList;
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   GetExtent(Clip);                                   { Get view extents }
   OwnerSave := OwnerGroup;                           { Save current group }
   OwnerGroup := @Self;                               { We are current group }
   FixupSave := FixupList;                            { Save current list }
   Count := 0;                                        { Zero count value }
   S.Read(Count, 2);                                  { Read entry count }
   If (MaxAvail >= Count*SizeOf(Pointer)) Then Begin  { Memory available }
     GetMem(FixupList, Count*SizeOf(Pointer));        { List size needed }
     FillChar(FixUpList^, Count*SizeOf(Pointer), #0); { Zero all entries }
     For I := 1 To Count Do Begin
       V := PView(S.Get);                             { Get view off stream }
       If (V <> Nil) Then InsertView(V, Nil);         { Insert valid views }
     End;
     V := Last;                                       { Start on last view }
     For I := 1 To Count Do Begin
       V := V^.Next;                                  { Fetch next view }
       P := FixupList^[I];                            { Transfer pointer }
       While (P <> Nil) Do Begin                      { If valid view }
         Q := P;                                      { Copy pointer }
         P := P^;                                     { Fetch pointer }
         Q^ := V;                                     { Transfer view ptr }
       End;
     End;
     FreeMem(FixupList, Count*SizeOf(Pointer));       { Release fixup list }
   End;
   OwnerGroup := OwnerSave;                           { Reload current group }
   FixupList := FixupSave;                            { Reload current list }
   GetSubViewPtr(S, V);                               { Load any subviews }
   SetCurrent(V, NormalSelect);                       { Select current view }
   If (OwnerGroup = Nil) Then Awaken;                 { If topview activate }
END;

{--TGroup-------------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TGroup.Done;
VAR P, T: PView;
BEGIN
   Hide;                                              { Hide the view }
   P := Last;                                         { Start on last }
   If (P <> Nil) Then Begin                           { Subviews exist }
     Repeat
       P^.Hide;                                       { Hide each view }
       P := P^.Prev;                                  { Prior view }
     Until (P = Last);                                { Loop complete }
     Repeat
       T := P^.Prev;                                  { Hold prior pointer }
       Dispose(P, Done);                              { Dispose subview }
       P := T;                                        { Transfer pointer }
     Until (Last = Nil);                              { Loop complete }
   End;
   Inherited Done;                                    { Call ancestor }
END;

{--TGroup-------------------------------------------------------------------}
{  First -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TGroup.First: PView;
BEGIN
   If (Last = Nil) Then First := Nil                  { No first view }
     Else First := Last^.Next;                        { Return first view }
END;

{--TGroup-------------------------------------------------------------------}
{  Execute -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TGroup.Execute: Word;
VAR Event: TEvent;
BEGIN
   Repeat
     EndState := 0;                                   { Clear end state }
     Repeat
       GetEvent(Event);                               { Get next event }
       HandleEvent(Event);                            { Handle the event }
       If (Event.What <> evNothing) Then
         EventError(Event);                           { Event not handled }
     Until (EndState <> 0);                           { Until command set }
   Until Valid(EndState);                             { Repeat until valid }
   Execute := EndState;                               { Return result }
   EndState := 0;                                     { Clear end state }
END;

{--TGroup-------------------------------------------------------------------}
{  GetHelpCtx -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TGroup.GetHelpCtx: Word;
VAR H: Word;
BEGIN
   H := hcNoContext;                                  { Preset no context }
   If (Current <> Nil) Then H := Current^.GetHelpCtx; { Current context }
   If (H=hcNoContext) Then H := Inherited GetHelpCtx; { Call ancestor }
   GetHelpCtx := H;                                   { Return result }
END;

{--TGroup-------------------------------------------------------------------}
{  DataSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Jul98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TGroup.DataSize: Sw_Word;
VAR Total: Word; P: PView;
BEGIN
   Total := 0;                                        { Zero totals count }
   P := Last;                                         { Start on last view }
   If (P <> Nil) Then Begin                           { Subviews exist }
     Repeat
       P := P^.Next;                                  { Move to next view }
       Total := Total + P^.DataSize;                  { Add view size }
     Until (P = Last);                                { Until last view }
   End;
   DataSize := Total;                                 { Return data size }
END;

{--TGroup-------------------------------------------------------------------}
{  ExecView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Jul99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TGroup.ExecView (P: PView): Word;
VAR SaveOptions: Word; SaveTopView, SaveCurrent: PView; SaveOwner: PGroup;
    SaveCommands: TCommandSet;
BEGIN
   If (P<>Nil) Then Begin
     SaveOptions := P^.Options;                       { Hold options }
     SaveOwner := P^.Owner;                           { Hold owner }
     SaveTopView := TheTopView;                       { Save topmost view }
     SaveCurrent := Current;                          { Save current view }
     GetCommands(SaveCommands);                       { Save commands }
     TheTopView := P;                                 { Set top view }
     P^.Options := P^.Options AND NOT ofSelectable;   { Not selectable }
     P^.SetState(sfModal, True);                      { Make modal }
     SetCurrent(P, EnterSelect);                      { Select next }
     If (SaveOwner = Nil) Then Insert(P);             { Insert view }
     ExecView := P^.Execute;                          { Execute view }
     If (SaveOwner = Nil) Then Delete(P);             { Remove view }
     SetCurrent(SaveCurrent, LeaveSelect);            { Unselect current }
     P^.SetState(sfModal, False);                     { Clear modal state }
     P^.Options := SaveOptions;                       { Restore options }
     TheTopView := SaveTopView;                       { Restore topview }
     SetCommands(SaveCommands);                       { Restore commands }
   End Else ExecView := cmCancel;                     { Return cancel }
END;

{ ********************************* REMARK ******************************** }
{    This call really is very COMPILER SPECIFIC and really can't be done    }
{    effectively any other way but assembler code as SELF & FRAMES need     }
{    to be put down in exact order and OPTIMIZERS make a mess of it.        }
{ ******************************** END REMARK *** Leon de Boer, 17Jul99 *** }

{--TGroup-------------------------------------------------------------------}
{  FirstThat -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Jul99 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TGroup.FirstThat (P: Pointer): PView;
VAR
  Tp : PView;
BEGIN
  If (Last<>Nil) Then
   Begin
     Tp := Last;                                      { Set temporary ptr }
     Repeat
       Tp := Tp^.Next;                                { Get next view }
       IF Byte(Longint(CallPointerMethodLocal(P,PreviousFramePointer,@self,Tp)))<>0 THEN
        Begin       { Test each view }
          FirstThat := Tp;                             { View returned true }
          Exit;                                        { Now exit }
        End;
     Until (Tp=Last);                                 { Until last }
     FirstThat := Nil;                                { None passed test }
   End
  Else
   FirstThat := Nil;                         { Return nil }
END;

{--TGroup-------------------------------------------------------------------}
{  Valid -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TGroup.Valid (Command: Word): Boolean;

   FUNCTION IsInvalid (P: PView): Boolean; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     IsInvalid := NOT P^.Valid(Command);              { Check if valid }
   END;

BEGIN
   Valid := True;                                     { Preset valid }
   If (Command = cmReleasedFocus) Then Begin          { Release focus cmd }
     If (Current <> Nil) AND                          { Current view exists }
       (Current^.Options AND ofValidate <> 0) Then    { Validating view }
         Valid := Current^.Valid(Command);            { Validate command }
   End Else Valid := FirstThat(@IsInvalid) = Nil;     { Check first valid }
END;

{--TGroup-------------------------------------------------------------------}
{  FocusNext -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TGroup.FocusNext (Forwards: Boolean): Boolean;
VAR P: PView;
BEGIN
   P := FindNext(Forwards);                           { Find next view }
   FocusNext := True;                                 { Preset true }
   If (P <> Nil) Then FocusNext := P^.Focus;          { Check next focus }
END;

{--TGroup-------------------------------------------------------------------}
{  ReDraw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.ReDraw;
VAR P: PView;
BEGIN
   If (DrawMask AND vdNoChild = 0) Then Begin         { No draw child clear }
     P := Last;                                       { Start on Last }
     While (P <> Nil) Do Begin
       P^.DrawView;                                   { Redraw each subview }
       P := P^.PrevView;                              { Move to prior view }
     End;
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  ReDraw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.ReDrawArea (X1, Y1, X2, Y2: Sw_Integer);
VAR P: PView;
BEGIN
   { redraw this }
   // inherited RedrawArea(X1,Y1,X2,Y2);
   { This should do the whole job now }
   If (DrawMask AND vdNoChild = 0) and
      (State AND (sfExposed or sfVisible) = (sfExposed or sfVisible)) and
      (X1<RawOrigin.X+RawSize.X) and
      (Y1<RawOrigin.Y+RawSize.Y) and
      (X2>=RawOrigin.X) and                      { No need to parse childs for Shadows }
      (Y2>=RawOrigin.Y) Then                     { No draw child clear }
   ReDrawVisibleArea(X1, Y1, X2, Y2,First); { Redraw each subview }
(*   { redraw group members }
   If (DrawMask AND vdNoChild = 0) and
      (X1<RawOrigin.X+RawSize.X) and                  { No need to parse childs for Shadows }
      (Y1<RawOrigin.Y+RawSize.Y) Then Begin           { No draw child clear }
     P := Last;                                       { Start on Last }
     While (P <> Nil) Do Begin
       P^.ReDrawVisibleArea(X1, Y1, X2, Y2,First,P); { Redraw each subview }
       P := P^.PrevView;                              { Move to prior view }
     End;
   End; *)
END;

PROCEDURE TGroup.ReDrawVisibleArea (X1, Y1, X2, Y2: Sw_Integer;Cur : PView);
var
  StoreDrawMask : Byte;
VAR CurN: PView; ViewPort: ViewPortType;
   x3,x4,y3,y4 : sw_integer;
BEGIN
   while(assigned(Cur) and ((Cur^.State and sfvisible)=0)) and (Cur<>Last) do
       Cur:=Cur^.Next;

   if not assigned(Cur) then
     Begin
       TView.ReDrawArea(x1,y1,x2,y2);
       exit;
     End;

   x3:=Cur^.RawOrigin.x;
   x4:=x3+Cur^.RawSize.x;
   y3:=Cur^.RawOrigin.Y;
   y4:=y3+Cur^.RawSize.Y;
   { depending on relative positions of x1,x2,x3,x4
     we should only draw subrectangles }
   if cur=last then
     CurN:=nil
   else
     curN:=Cur^.Next;
   { number of possible cases :
      x3<x1, x1<=x3<x2, x3>=x2 : 3 possibilities
      total : 3^4: 81... }
   if ((x3>=x2) or (x4<=x1) or (y3>=y2) or (y4<=y1)) or
      (assigned(Cur) and ((Cur^.State and sfvisible)=0)) then
     ReDrawVisibleArea(x1,y1,x2,y2,curn)
   else
     begin
       if x3>x1 then
         begin
           ReDrawVisibleArea(x1,y1,x3,y2,curn);
           x1:=x3;
         end;
       if x4<x2 then
         begin
           ReDrawVisibleArea(x4,y1,x2,y2,curn);
           x2:=x4;
         end;
       if y3>y1 then
         Begin
           ReDrawVisibleArea(x1,y1,x2,y3,curn);
           y1:=y3;
         End;
       if y4<y2 then
         Begin
           ReDrawVisibleArea(x1,y4,x2,y2,curn);
           y2:=y4;
         End;
       if (x1<=x2) and (y1<=y2) then
         Cur^.ReDrawArea(x1,y1,x2,y2);
     end;
END;


{--TGroup-------------------------------------------------------------------}
{  Awaken -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Awaken;

   PROCEDURE DoAwaken (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     If (P <> Nil) Then P^.Awaken;                    { Awaken view }
   END;

BEGIN
   ForEach(@DoAwaken);                                { Awaken each view }
END;

{--TGroup-------------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Draw;
BEGIN
   If Buffer=Nil then
     ReDraw
   else
     WriteBuf(0,0,Size.X,Size.Y,Buffer);
END;

{--TGroup-------------------------------------------------------------------}
{  DrawBackground                                                           }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.DrawBackground;
var
   P : PView;
BEGIN
   Inherited DrawBackground;
   P:=Last;
     While (P <> Nil) Do Begin
       If P^.Exposed then
         P^.SetDrawMask(vdAll);                       { Redraw each exposed subview }
       P := P^.PrevView;                              { Move to prior view }
     End;
END;

{--TGroup-------------------------------------------------------------------}
{  SelectDefaultView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22Oct99 LdB }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.SelectDefaultView;
VAR P: PView;
BEGIN
   P := Last;                                         { Start at last }
   While (P <> Nil) Do Begin
     If P^.GetState(sfDefault) Then Begin             { Search 1st default }
       P^.Select;                                     { Select default view }
       P := Nil;                                      { Force kick out }
     End Else P := P^.PrevView;                       { Prior subview }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  Insert -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Sep99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Insert (P: PView);
BEGIN
   If (P <> Nil) and assigned(P^.Owner) Then          { View is valid and already inserted }
     If (Options AND ofGFVModeView <> 0) Then         { GFV mode view check }
       P^.DisplaceBy(-P^.Owner^.RawOrigin.X,
       -P^.Owner^.RawOrigin.Y) Else                    { We are in GFV mode }
       P^.DisplaceBy(-P^.Owner^.Origin.X*FontWidth,
        -P^.Owner^.Origin.Y*FontHeight);               { Displace old view }
   If (P <> Nil) Then                                 { View is valid }
     If (Options AND ofGFVModeView <> 0) Then         { GFV mode view check }
       P^.DisplaceBy(RawOrigin.X, RawOrigin.Y) Else   { We are in GFV mode }
       P^.DisplaceBy(Origin.X*FontWidth,
         Origin.Y*FontHeight);                        { Displace old view }
   InsertBefore(P, First);                            { Insert the view }
END;

{--TGroup-------------------------------------------------------------------}
{  Delete -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Delete (P: PView);
VAR SaveState: Word;
BEGIN
   SaveState := P^.State;                             { Save state }
   P^.Hide;                                           { Hide the view }
   RemoveView(P);                                     { Remove the view }
   P^.Owner := Nil;                                   { Clear owner ptr }
   P^.Next := Nil;                                    { Clear next ptr }
   { We need to recalculate correct position }
   If (P <> Nil) Then                                 { View is valid }
     If (Options AND ofGFVModeView <> 0) Then         { GFV mode view check }
       P^.DisplaceBy(-RawOrigin.X, -RawOrigin.Y) Else   { We are in GFV mode }
       P^.DisplaceBy(-Origin.X*FontWidth,
         -Origin.Y*FontHeight);                        { Displace old view }
   If (SaveState AND sfVisible <> 0) Then P^.Show;    { Show view }
END;

{ ********************************* REMARK ******************************** }
{    This call really is very COMPILER SPECIFIC and really can't be done    }
{    effectively any other way but assembler code as SELF & FRAMES need     }
{    to be put down in exact order and OPTIMIZERS make a mess of it.        }
{ ******************************** END REMARK *** Leon de Boer, 17Jul99 *** }

{--TGroup-------------------------------------------------------------------}
{  ForEach -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Jul99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.ForEach (P: Pointer);
VAR
  Tp,Hp,L0 : PView;
{ Vars Hp and L0 are necessary to hold original pointers in case   }
{ when some view closes himself as a result of broadcast message ! }
BEGIN
  If (Last<>Nil) Then
   Begin
     Tp:=Last;
     Hp:=Tp^.Next;
     L0:=Last;              { Set temporary ptr }
     Repeat
       Tp:=Hp;
       if tp=nil then
        exit;
       Hp:=Tp^.Next;                        { Get next view }
       CallPointerMethodLocal(P,PreviousFramePointer,@self,Tp);
     Until (Tp=L0);                                   { Until last }
   End;
END;



{--TGroup-------------------------------------------------------------------}
{  EndModal -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.EndModal (Command: Word);
BEGIN
   If (State AND sfModal <> 0) Then                   { This view is modal }
     EndState := Command Else                         { Set endstate }
     Inherited EndModal(Command);                     { Call ancestor }
END;

{--TGroup-------------------------------------------------------------------}
{  DisplaceBy -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.DisplaceBy (Dx, Dy: Sw_Integer);
VAR P: PView;
BEGIN
   P := First;                                        { Get first view }
   While (P <> Nil) Do Begin
     P^.DisplaceBy(Dx, Dy);                           { Displace subviews }
     P := P^.NextView;                                { Next view }
   End;
   Inherited DisplaceBy(Dx, Dy);                      { Call ancestor }
END;

{--TGroup-------------------------------------------------------------------}
{  SelectNext -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.SelectNext (Forwards: Boolean);
VAR P: PView;
BEGIN
   P := FindNext(Forwards);                           { Find next view }
   If (P <> Nil) Then P^.Select;                      { Select view }
END;

{--TGroup-------------------------------------------------------------------}
{  InsertBefore -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Sep99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.InsertBefore (P, Target: PView);
VAR SaveState : Word;
    I: Sw_integer;
BEGIN
   If (P <> Nil) AND (P^.Owner = Nil) AND             { View valid }
   ((Target = Nil) OR (Target^.Owner = @Self))        { Target valid }
   Then Begin
     If (P^.Options AND ofCenterX <> 0) Then Begin    { Centre on x axis }
       If (Options AND ofGFVModeView <> 0) Then       { GFV mode view check }
         I := RawSize.X Else I := Size.X * FontWidth; { Calc owner x size }
       If (P^.Options AND ofGFVModeView <> 0)         { GFV mode view check }
       Then Begin
         I := (I - P^.RawSize.X) DIV 2;               { Calc view offset }
         I := I - P^.RawOrigin.X;                     { Subtract x origin }
       End Else Begin
         I := (I - (P^.Size.X * FontWidth)) DIV 2;    { Calc view offset }
         I := I - (P^.Origin.X * FontWidth);          { Subtract x origin }
       End;
       { make sure that I is a multiple of FontWidth }
       if TextModeGFV or UseFixedFont then
         I:= (I div FontWidth) * FontWidth;
       P^.DisplaceBy(I, 0);                           { Displace the view }
     End;
     If (P^.Options AND ofCenterY <> 0) Then Begin    { Centre on y axis }
       If (Options AND ofGFVModeView <> 0) Then       { GFV mode view check }
         I := RawSize.Y Else I := Size.Y * FontHeight;{ Calc owner y size }
       If (P^.Options AND ofGFVModeView <> 0)         { GFV mode view check }
       Then Begin
         I := (I - P^.RawSize.Y) DIV 2;               { Calc view offset }
         I := I - P^.RawOrigin.Y;                     { Subtract y origin }
       End Else Begin
         I := (I - (P^.Size.Y * FontHeight)) DIV 2;   { Calc view offset }
         I := I - (P^.Origin.Y * FontHeight);         { Subtract y origin }
       End;
       { make sure that I is a multiple of FontHeight }
       if TextModeGFV or UseFixedFont then
         I:= (I div FontHeight) * FontHeight;
       P^.DisplaceBy(0, I);                           { Displace the view }
     End;
     SaveState := P^.State;                           { Save view state }
     P^.Hide;                                         { Make sure hidden }
     InsertView(P, Target);                           { Insert into list }
     If (SaveState AND sfVisible <> 0) Then P^.Show;  { Show the view }
     If (State AND sfActive <> 0) Then                { Was active before }
       P^.SetState(sfActive , True);                  { Make active again }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.SetState (AState: Word; Enable: Boolean);

    PROCEDURE DoSetState (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
    BEGIN
      If (P <> Nil) Then P^.SetState(AState, Enable); { Set subview state }
    END;

    PROCEDURE DoExpose (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
    BEGIN
      If (P <> Nil) Then Begin
        If (P^.State AND sfVisible <> 0) Then         { Check view visible }
          P^.SetState(sfExposed, Enable);             { Set exposed flag }
      End;
    END;

BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   Case AState Of
     sfActive, sfDragging: Begin
         Lock;                                        { Lock the view }
         ForEach(@DoSetState);                        { Set each subview }
         UnLock;                                      { Unlock the view }
       End;
     sfFocused: Begin
         If (Current <> Nil) Then
           Current^.SetState(sfFocused, Enable);          { Focus current view }
         {If TextModeGFV then
           SetDrawMask(vdBackGnd OR vdFocus OR vdInner OR vdBorder);  Set redraw masks }
       End;
     sfExposed: Begin
         ForEach(@DoExpose);                          { Expose each subview }
       End;
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  GetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Mar98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.GetData (Var Rec);
VAR Total: Sw_Word; P: PView;
BEGIN
   Total := 0;                                        { Clear total }
   P := Last;                                         { Start at last }
   While (P <> Nil) Do Begin                          { Subviews exist }
     P^.GetData(TByteArray(Rec)[Total]);              { Get data }
     Inc(Total, P^.DataSize);                         { Increase total }
     P := P^.PrevView;                                { Previous view }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  SetData -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 29Mar98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.SetData (Var Rec);
VAR Total: Sw_Word; P: PView;
BEGIN
   Total := 0;                                        { Clear total }
   P := Last;                                         { Start at last }
   While (P <> Nil) Do Begin                          { Subviews exist }
     P^.SetData(TByteArray(Rec)[Total]);              { Get data }
     Inc(Total, P^.DataSize);                         { Increase total }
     P := P^.PrevView;                                { Previous view }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Mar98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Store (Var S: TStream);
VAR Count: Sw_Integer; OwnerSave: PGroup;

   PROCEDURE DoPut (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     S.Put(P);                                        { Put view on stream }
   END;

BEGIN
   TView.Store(S);                                    { Call view store }
   OwnerSave := OwnerGroup;                           { Save ownergroup }
   OwnerGroup := @Self;                               { Set as owner group }
   Count := IndexOf(Last);                            { Subview count }
   S.Write(Count, 2);                                 { Write the count }
   ForEach(@DoPut);                                   { Put each in stream }
   PutSubViewPtr(S, Current);                         { Current on stream }
   OwnerGroup := OwnerSave;                           { Restore ownergroup }
END;

{--TGroup-------------------------------------------------------------------}
{  EventError -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.EventError (Var Event: TEvent);
BEGIN
   If (Owner <> Nil) Then Owner^.EventError(Event);   { Event error }
END;

{--TGroup-------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.HandleEvent (Var Event: TEvent);

   FUNCTION ContainsMouse (P: PView): Boolean; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     ContainsMouse := (P^.State AND sfVisible <> 0)   { Is view visible }
       AND P^.MouseInView(Event.Where);               { Is point in view }
   END;

   PROCEDURE DoHandleEvent (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     If (P = Nil) OR ((P^.State AND sfDisabled <> 0) AND
       (Event.What AND(PositionalEvents OR FocusedEvents) <>0 ))
        Then Exit;                                     { Invalid/disabled }
     Case Phase Of
       phPreProcess: If (P^.Options AND ofPreProcess = 0)
         Then Exit;                                   { Not pre processing }
       phPostProcess: If (P^.Options AND ofPostProcess = 0)
         Then Exit;                                   { Not post processing }
     End;
     If (Event.What AND P^.EventMask <> 0) Then       { View handles event }
       P^.HandleEvent(Event);                         { Pass to view }
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   If (Event.What = evNothing) Then Exit;             { No valid event exit }
   If (Event.What AND FocusedEvents <> 0) Then Begin  { Focused event }
     Phase := phPreProcess;                           { Set pre process }
     ForEach(@DoHandleEvent);                         { Pass to each view }
     Phase := phFocused;                              { Set focused }
     DoHandleEvent(Current);                          { Pass to current }
     Phase := phPostProcess;                          { Set post process }
     ForEach(@DoHandleEvent);                         { Pass to each }
   End Else Begin
     Phase := phFocused;                              { Set focused }
     If (Event.What AND PositionalEvents <> 0) Then   { Positional event }
       DoHandleEvent(FirstThat(@ContainsMouse))       { Pass to first }
       Else ForEach(@DoHandleEvent);                  { Pass to all }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  ChangeBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.ChangeBounds (Var Bounds: TRect);
VAR D: TPoint;

   PROCEDURE DoCalcChange (P: PView); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   VAR R: TRect;
   BEGIN
     P^.CalcBounds(R, D);                             { Calc view bounds }
     P^.ChangeBounds(R);                              { Change view bounds }
   END;

BEGIN
   D.X := Bounds.B.X - Bounds.A.X - Size.X;           { Delta x value }
   D.Y := Bounds.B.Y - Bounds.A.Y - Size.Y;           { Delta y value }
   If ((D.X=0) AND (D.Y=0)) Then Begin
     SetBounds(Bounds);                               { Set new bounds }
     DrawView;                                        { Draw the view }
   End Else Begin
     SetBounds(Bounds);                               { Set new bounds }
     GetExtent(Clip);                                 { Get new clip extents }
     Lock;                                            { Lock drawing }
     ForEach(@DoCalcChange);                          { Change each view }
     UnLock;                                          { Unlock drawing }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  GetSubViewPtr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20May98 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.GetSubViewPtr (Var S: TStream; Var P);
VAR Index, I: Sw_Word; Q: PView;
BEGIN
   Index := 0;                                        { Zero index value }
   S.Read(Index, 2);                                  { Read view index }
   If (Index > 0) Then Begin                          { Valid index }
     Q := Last;                                       { Start on last }
     For I := 1 To Index Do Q := Q^.Next;             { Loop for count }
     Pointer(P) := Q;                                 { Return the view }
   End Else Pointer(P) := Nil;                        { Return nil }
END;

{--TGroup-------------------------------------------------------------------}
{  PutSubViewPtr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20May98 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.PutSubViewPtr (Var S: TStream; P: PView);
VAR Index: Sw_Word;
BEGIN
   If (P = Nil) Then Index := 0 Else                  { Nil view, Index = 0 }
     Index := IndexOf(P);                             { Calc view index }
   S.Write(Index, 2);                                 { Write the index }
END;


{***************************************************************************}
{                       TGroup OBJECT PRIVATE METHODS                       }
{***************************************************************************}

{--TGroup-------------------------------------------------------------------}
{  IndexOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TGroup.IndexOf (P: PView): Sw_Integer;
VAR I: Sw_Integer; Q: PView;
BEGIN
   Q := Last;                                         { Start on last view }
   If (Q <> Nil) Then Begin                           { Subviews exist }
     I := 1;                                          { Preset value }
     While (Q <> P) AND (Q^.Next <> Last) Do Begin
       Q := Q^.Next;                                  { Load next view }
       Inc(I);                                        { Increment count }
     End;
     If (Q <> P) Then IndexOf := 0 Else IndexOf := I; { Return index }
   End Else IndexOf := 0;                             { Return zero }
END;

{--TGroup-------------------------------------------------------------------}
{  FindNext -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23Sep99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TGroup.FindNext (Forwards: Boolean): PView;
VAR P: PView;
BEGIN
   FindNext := Nil;                                   { Preset nil return }
   If (Current <> Nil) Then Begin                     { Has current view }
     P := Current;                                    { Start on current }
     Repeat
       If Forwards Then P := P^.Next                  { Get next view }
         Else P := P^.Prev;                           { Get prev view }
     Until ((P^.State AND (sfVisible+sfDisabled) = sfVisible)
     AND ((P^.Options AND ofSelectable <> 0) AND      { Selectable }
     (P^.GOptions AND goTabSelect <> 0))) OR          { Tab selectable }
     (P = Current);                                   { Not singular select }
     If (P <> Current) Then FindNext := P;            { Return result }
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  FirstMatch -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TGroup.FirstMatch (AState: Word; AOptions: Word): PView;

   FUNCTION Matches (P: PView): Boolean; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
   BEGIN
     Matches := (P^.State AND AState = AState) AND
       (P^.Options AND AOptions = AOptions);          { Return match state }
   END;

BEGIN
   FirstMatch := FirstThat(@Matches);                 { Return first match }
END;

{--TGroup-------------------------------------------------------------------}
{  ResetCurrent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.ResetCurrent;
BEGIN
   SetCurrent(FirstMatch(sfVisible, ofSelectable),
     NormalSelect);                                   { Reset current view }
END;

{--TGroup-------------------------------------------------------------------}
{  RemoveView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.RemoveView (P: PView);
VAR Q: PView;
BEGIN
   If (P <> Nil) AND (Last <> Nil) Then Begin         { Check view is valid }
     Q := Last;                                       { Start on last view }
     While (Q^.Next <> P) AND (Q^.Next <> Last) Do
       Q := Q^.Next;                                  { Find prior view }
     If (Q^.Next = P) Then Begin                      { View found }
       If (Q^.Next <> Q) Then Begin                   { Not only view }
         Q^.Next := P^.Next;                          { Rechain views }
         If (P = Last) Then Last := P^.Next;          { Fix if last removed }
       End Else Last := Nil;                          { Only view }
     End;
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  InsertView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.InsertView (P, Target: PView);
BEGIN
   If (P <> Nil) Then Begin                           { Check view is valid }
     P^.Owner := @Self;                               { Views owner is us }
     If (Target <> Nil) Then Begin                    { Valid target }
       Target := Target^.Prev;                        { 1st part of chain }
       P^.Next := Target^.Next;                       { 2nd part of chain }
       Target^.Next := P;                             { Chain completed }
     End Else Begin
       If (Last <> Nil) Then Begin                    { Not first view }
         P^.Next := Last^.Next;                       { 1st part of chain }
         Last^.Next := P;                             { Completed chain }
       End Else P^.Next := P;                         { 1st chain to self }
       Last := P;                                     { P is now last }
     End;
   End;
END;

{--TGroup-------------------------------------------------------------------}
{  SetCurrent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23Sep99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.SetCurrent (P: PView; Mode: SelectMode);

   PROCEDURE SelectView (P: PView; Enable: Boolean);
   BEGIN
     If (P <> Nil) Then                               { View is valid }
       P^.SetState(sfSelected, Enable);               { Select the view }
   END;

   PROCEDURE FocusView (P: PView; Enable: Boolean);
   BEGIN
     If (State AND sfFocused <> 0) AND (P <> Nil)     { Check not focused }
       Then P^.SetState(sfFocused, Enable);           { Focus the view }
   END;

BEGIN
   If (Current<>P) Then Begin                         { Not already current }
     Lock;                                            { Stop drawing }
     FocusView(Current, False);                       { Defocus current }
     If (Mode <> EnterSelect) Then
       SelectView(Current, False);                    { Deselect current }
     If (Mode<>LeaveSelect) Then SelectView(P, True); { Select view P }
     FocusView(P, True);                              { Focus view P }
     Current := P;                                    { Set as current view }
     UnLock;                                          { Redraw now }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TFrame OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TFrame-------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TFrame.Init (Var Bounds: TRect);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   GrowMode := gfGrowHiX + gfGrowHiY;                 { Set grow modes }
   EventMask := EventMask OR evBroadcast;             { See broadcasts }
END;

{--TFrame-------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TFrame.GetPalette: PPalette;
CONST P: String[Length(CFrame)] = CFrame;             { Always normal string }
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TScrollBar OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}


{--TScrollBar---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TScrollBar.Init (Var Bounds: TRect);
const
  VChars: array[boolean] of TScrollChars =
     (('^','V', #177, #254, #178),(#30, #31, #177, #254, #178));
  HChars: array[boolean] of TScrollChars =
     (('<','>', #177, #254, #178),(#17, #16, #177, #254, #178));
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   PgStep := 1;                                       { Page step size = 1 }
   ArStep := 1;                                       { Arrow step sizes = 1 }
   If (Size.X = 1) Then Begin                         { Vertical scrollbar }
     GrowMode := gfGrowLoX + gfGrowHiX + gfGrowHiY;   { Grow vertically }
     Chars := VChars[LowAscii];                       { Vertical chars }
   End Else Begin                                     { Horizontal scrollbar }
     GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;   { Grow horizontal }
     Chars := HChars[LowAscii];                       { Horizontal chars }
   End;
END;

{--TScrollBar---------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB              }
{---------------------------------------------------------------------------}
{   This load method will read old original TV data from a stream with the  }
{   scrollbar id set to zero.                                               }
{---------------------------------------------------------------------------}
CONSTRUCTOR TScrollBar.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Value, 2);                                  { Read current value }
   S.Read(Min , 2);                                   { Read min value }
   S.Read(Max, 2);                                    { Read max value }
   S.Read(PgStep, 2);                                 { Read page step size }
   S.Read(ArStep, 2);                                 { Read arrow step size }
   S.Read(Chars, SizeOf(Chars));                      { Read scroll chars }
   If (Options AND ofGFVModeView <> 0) Then           { GFV mode view check }
     S.Read(Id, 2);                                   { Read id }
END;

{--TScrollBar---------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TScrollBar.GetPalette: PPalette;
CONST P: String[Length(CScrollBar)] = CScrollBar;     { Always normal string }
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TScrollBar---------------------------------------------------------------}
{  ScrollStep -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TScrollBar.ScrollStep (Part: Sw_Integer): Sw_Integer;
VAR Step: Sw_Integer;
BEGIN
   If (Part AND $0002 = 0) Then Step := ArStep        { Range step size }
     Else Step := PgStep;                             { Page step size }
   If (Part AND $0001 = 0) Then ScrollStep := -Step   { Upwards move }
     Else ScrollStep := Step;                         { Downwards move }
END;

{--TScrollBar---------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.Draw;
BEGIN
   If (GOptions AND goNativeClass = 0) Then
     DrawPos(GetPos);                                 { Draw position }
END;

{--TScrollBar---------------------------------------------------------------}
{  ScrollDraw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.ScrollDraw;
VAR P: PView;
BEGIN
   If (Id <> 0) Then Begin
     P := TopView;                                    { Get topmost view }
     NewMessage(P, evCommand, cmIdCommunicate, Id,
       Value, @Self);                                 { New Id style message }
   End;
   NewMessage(Owner, evBroadcast, cmScrollBarChanged,
     Id, Value, @Self);                               { Old TV style message }
END;

{--TScrollBar---------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.DrawBackGround;
VAR Bc: Byte;
    I : Longint;
    B : TDrawBuffer;
BEGIN
   If (GOptions AND goNativeClass = 0) Then Begin     { Non natives draw }
     Inherited DrawBackGround;                        { Call ancestor }
     Bc := GetColor(1) AND $F0 SHR 4;                 { Background colour }
     If TextModeGFV {or UseFixedFont} then
       Begin
         WriteChar(0,0,Chars[0],Bc,1);
         If (Size.X = 1) Then Begin                         { Vertical scrollbar }
           For i:=1 to Size.Y-2 do
             WriteChar(0,i,Chars[2],Bc,1);
           WriteChar(0,Size.Y-1,Chars[1],Bc,1);
         End Else Begin
           WriteChar(1,0,Chars[2],Bc,Size.X-2);
           WriteChar(Size.X-1,0,Chars[1],Bc,1);
         End;
       End
     else
       Begin
         ClearArea(0, 0, FontWidth-1, FontHeight-1, Bc);  { Clear top/left area }
         BiColorRectangle(0, 0, FontWidth-1, FontHeight-1,
           15, 0, False);                                 { Draw 3d effect }
         ClearArea(RawSize.X-FontWidth+1, RawSize.Y-
           FontHeight+1, RawSize.X, RawSize.Y, Bc);       { Clr right/lower area }
         BiColorRectangle(RawSize.X-FontWidth+1,
           RawSize.Y-FontHeight+1,RawSize.X, RawSize.Y,
           15, 0, False);                                 { Draw 3d effect }
       End;
   End;
END;

{--TScrollBar---------------------------------------------------------------}
{  SetValue -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.SetValue (AValue: Sw_Integer);
BEGIN
   SetParams(AValue, Min, Max, PgStep, ArStep);       { Set value }
END;

{--TScrollBar---------------------------------------------------------------}
{  SetRange -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.SetRange (AMin, AMax: Sw_Integer);
BEGIN
   SetParams(Value, AMin, AMax, PgStep, ArStep);      { Set range }
END;

{--TScrollBar---------------------------------------------------------------}
{  SetStep -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.SetStep (APgStep, AArStep: Sw_Integer);
BEGIN
   SetParams(Value, Min, Max, APgStep, AArStep);      { Set step sizes }
END;

{--TScrollBar---------------------------------------------------------------}
{  SetParams -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 21Jul99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.SetParams (AValue, AMin, AMax, APgStep, AArStep: Sw_Integer);
BEGIN
   If (AMax < AMin) Then AMax := AMin;                { Max below min fix up }
   If (AValue < AMin) Then AValue := AMin;            { Value below min fix }
   If (AValue > AMax) Then AValue := AMax;            { Value above max fix }
   If (Value <> AValue) OR (Min <> AMin) OR
   (Max <> AMax) Then Begin                           { Something changed }
     If (Min <> AMin) OR (Max <> AMax) Then Begin     { Range has changed }
       If (GOptions AND goNativeClass = 0) Then
         ClearPos(GetPos);                            { Clear old position }
       Min := AMin;                                   { Set new minimum }
       Max := AMax;                                   { Set new maximum }
       { This was removed as found not needed but if you
       change limits but value unchanged scrollbar is not redrawm..LdB }
       {If (Value = AValue) AND (State and sfVisible <> 0)
         Then ScrollDraw;}                             { Send message out }
     End Else Begin
       If (GOptions AND goNativeClass = 0) Then       { Not in native mode }
         ClearPos(GetPos);                            { Clear old position }
     End;
     If (Value <> AValue) Then Begin                  { Position moved }
       Value := AValue;                               { Set new value }
       If (GOptions AND goNativeClass = 0) Then Begin { Not in native mode }
         SetDrawMask(vdInner);                        { Set draw masks }
         DrawView;                                    { Redraw changed }
       End;
       {If (State AND sfVisible <> 0) Then }
       { We need to inform the owner if the value changes
         even if not visible !! }
       ScrollDraw;                                    { Send update message }
     End;
   End;
   PgStep := APgStep;                                 { Hold page step }
   ArStep := AArStep;                                 { Hold arrow step }
END;

{--TScrollBar---------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB             }
{---------------------------------------------------------------------------}
{  You can save data to the stream compatable with the old original TV by   }
{  temporarily turning off the ofGrafVersion making the call to this store  }
{  routine and resetting the ofGrafVersion flag after the call.             }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { TView.Store called }
   S.Write(Value, 2);                                 { Write current value }
   S.Write(Min, 2);                                   { Write min value }
   S.Write(Max, 2);                                   { Write max value }
   S.Write(PgStep, 2);                                { Write page step size }
   S.Write(ArStep, 2);                                { Write arrow step size }
   S.Write(Chars, SizeOf(Chars));                     { Write scroll chars }
   If (Options AND ofGFVModeView <> 0) Then           { GFV mode view check }
     S.Write(Id, 2);                                  { Write scrollbar id }
END;

{--TScrollBar---------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.HandleEvent (Var Event: TEvent);
VAR Tracking: Boolean; I, P, S, ClickPart, Iv: Sw_Integer;
    Mouse: TPoint; Extent: TRect;

   FUNCTION GetPartCode: Sw_Integer;
   VAR Mark, Part, J: Sw_Integer;
   BEGIN
     Part := -1;                                      { Preset failure }
     If Extent.Contains(Mouse) Then Begin             { Contains mouse }
       If (Size.X = 1) Then Begin                     { Vertical scrollbar }
         Mark := Mouse.Y - FontHeight;                { Calc position }
         J := FontHeight;                             { Font height }
       End Else Begin                                 { Horizontal bar }
         Mark := Mouse.X - FontWidth;                 { Calc position }
         J := FontWidth;                              { Font width }
       End;
       If (Mark >= P) AND (Mark < P+J) Then           { Within thumbnail }
         Part := sbIndicator;                         { Indicator part }
       If (Part <> sbIndicator) Then Begin            { Not indicator part }
         If (Mark < 1) Then Part := sbLeftArrow Else  { Left arrow part }
         If (Mark < P) Then Part := sbPageLeft Else   { Page left part }
         If (Mark < S) Then Part := sbPageRight Else  { Page right part }
           Part := sbRightArrow;                      { Right arrow part }
         If (Size.X = 1) Then Inc(Part, 4);           { Correct for vertical }
       End;
     End;
     GetPartCode := Part;                             { Return part code }
   END;

   PROCEDURE Clicked;
   BEGIN
     NewMessage(Owner, evBroadcast, cmScrollBarClicked,
       Id, Value, @Self);                             { Old TV style message }
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speed up exit }
     evCommand: Begin                                 { Command event }
       If (Event.Command = cmIdCommunicate) AND       { Id communication }
       (Event.Id = Id) AND (Event.InfoPtr <> @Self)   { Targeted to us }
       Then Begin
         SetValue(Round(Event.Data));                 { Set scrollbar value }
         ClearEvent(Event);                           { Event was handled }
       End;
     End;
     evKeyDown:
       If (State AND sfVisible <> 0) Then Begin       { Scrollbar visible }
         ClickPart := sbIndicator;                    { Preset result }
         If (Size.Y = 1) Then                         { Horizontal bar }
           Case CtrlToArrow(Event.KeyCode) Of
             kbLeft: ClickPart := sbLeftArrow;        { Left one item }
             kbRight: ClickPart := sbRightArrow;      { Right one item }
             kbCtrlLeft: ClickPart := sbPageLeft;     { One page left }
             kbCtrlRight: ClickPart := sbPageRight;   { One page right }
             kbHome: I := Min;                        { Move to start }
             kbEnd: I := Max;                         { Move to end }
             Else Exit;                               { Not a valid key }
           End
         Else                                         { Vertical bar }
           Case CtrlToArrow(Event.KeyCode) Of
             kbUp: ClickPart := sbUpArrow;            { One item up }
             kbDown: ClickPart := sbDownArrow;        { On item down }
             kbPgUp: ClickPart := sbPageUp;           { One page up }
             kbPgDn: ClickPart := sbPageDown;         { One page down }
             kbCtrlPgUp: I := Min;                    { Move to top }
             kbCtrlPgDn: I := Max;                    { Move to bottom }
             Else Exit;                               { Not a valid key }
           End;
         Clicked;                                     { Send out message }
         If (ClickPart <> sbIndicator) Then
           I := Value + ScrollStep(ClickPart);        { Calculate position }
         SetValue(I);                                 { Set new item }
         ClearEvent(Event);                           { Event now handled }
     End;
     evMouseDown: Begin                               { Mouse press event }
         Clicked;                                     { Scrollbar clicked }
         Mouse.X := Event.Where.X - RawOrigin.X;      { Localize x value }
         Mouse.Y := Event.Where.Y - RawOrigin.Y;      { Localize y value }
         Extent.A.X := 0;                             { Zero x extent value }
         Extent.A.Y := 0;                             { Zero y extent value }
         Extent.B.X := RawSize.X;                     { Set extent x value }
         Extent.B.Y := RawSize.Y;                     { set extent y value }
         P := GetPos;                                 { Current position }
         S := GetSize;                                { Initial size }
         ClickPart := GetPartCode;                    { Get part code }
         If (ClickPart <> sbIndicator) Then Begin     { Not thumb nail }
           Repeat
             Mouse.X := Event.Where.X-RawOrigin.X;    { Localize x value }
             Mouse.Y := Event.Where.Y-RawOrigin.Y;    { Localize y value }
             If GetPartCode = ClickPart Then
               SetValue(Value+ScrollStep(ClickPart)); { Same part repeat }
           Until NOT MouseEvent(Event, evMouseAuto);  { Until auto done }
           Clicked;                                   { Scrollbar clicked }
         End Else Begin                               { Thumb nail move }
           Iv := Value;                               { Initial value }
           Repeat
             Mouse.X := Event.Where.X - RawOrigin.X;  { Localize x value }
             Mouse.Y := Event.Where.Y - RawOrigin.Y;  { Localize y value }
             Tracking := Extent.Contains(Mouse);      { Check contains }
             If Tracking Then Begin                   { Tracking mouse }
               If (Size.X=1) Then
                 I := Mouse.Y-FontHeight Else         { Calc vert position }
                 I := Mouse.X-FontWidth;              { Calc horz position }
               If (I < 0) Then I := 0;                { Check underflow }
               If (I > S) Then I := S;                { Check overflow }
             End Else I := GetPos;                    { Get position }
             If (I <> P) Then Begin
               SetValue(LongInt((LongInt(I)*(Max-Min))
                 +(S SHR 1)) DIV S + Min);            { Set new value }
               P := I;                                { Hold new position }
             End;
           Until NOT MouseEvent(Event, evMouseMove);  { Until not moving }
           If Tracking AND (S > 0) Then               { Tracking mouse }
             SetValue(LongInt((LongInt(P)*(Max-Min))+
               (S SHR 1)) DIV S + Min);               { Set new value }
           If (Iv <> Value) Then Clicked;             { Scroll has moved }
         End;
         ClearEvent(Event);                           { Clear the event }
     End;
   End;
END;

{***************************************************************************}
{                 TScrollBar OBJECT PRIVATE METHODS                         }
{***************************************************************************}

{--TScrollBar---------------------------------------------------------------}
{  GetPos -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May98 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TScrollBar.GetPos: Sw_Integer;
VAR R: Sw_Integer;
BEGIN
   R := Max - Min;                                    { Get full range }
   If (R = 0) Then GetPos := 0 Else                   { Return zero }
     GetPos := LongInt((LongInt(Value-Min) * GetSize)
       + (R SHR 1)) DIV R;                            { Calc position }
END;

{--TScrollBar---------------------------------------------------------------}
{  GetSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TScrollBar.GetSize: Sw_Integer;
VAR S: Sw_Integer;
BEGIN
   If TextModeGFV then Begin
     If Size.X = 1 Then
       S:= (Size.Y-3)*FontHeight
     else
       S:= (Size.X-3)*FontWidth;
   end else If (Size.X = 1) Then S := RawSize.Y-3*FontHeight+1 { Vertical bar }
     Else S := RawSize.X-3*FontWidth+1;               { Horizontal bar }
   If (S < 1) Then S := 1;                            { Fix minimum size }
   GetSize := S;                                      { Return size }
END;

{--TScrollBar---------------------------------------------------------------}
{  DrawPos -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27OctMay99 LdB        }
{---------------------------------------------------------------------------}
{   This could be called from a message handling event so it must check the }
{  view is visible, exposed and not obstructed before drawing the thumbnail }
{  square area.                                                             }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.DrawPos (Pos: Sw_Integer);
VAR i, X1, Y1, X2, Y2: Sw_Integer; ViewPort: ViewPortType;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfExposed <> 0) AND                     { View is exposed }
   (Max <> Min) Then Begin                            { View has some size }
     SetViewLimits;                                   { Set view limits }
     GetViewSettings(ViewPort, TextModeGFV);          { Get set viewport }
     If OverlapsArea(ViewPort.X1, ViewPort.Y1,
     ViewPort.X2, ViewPort.Y2) Then Begin             { Must be in area }
       HideMouseCursor;                               { Hide the mouse }
       X1 := 0;                                       { Initial x position }
       Y1 := 0;                                       { Initial y position }
       If TextModeGFV then Begin
         If (Size.X = 1) Then Begin                         { Vertical scrollbar }
           Pos:=Pos div FontHeight;
           WriteChar(0,0,Chars[0],2,1);
           For i:=1 to Size.Y-2 do
             WriteChar(0,i,Chars[2],2,1);
           WriteChar(0,Size.Y-1,Chars[1],2,1);
         End Else Begin
           Pos:=Pos div FontWidth;
           WriteChar(0,0,Chars[0],2,1);
           WriteChar(1,0,Chars[2],2,Size.X-2);
           WriteChar(Size.X-1,0,Chars[1],2,1);
         End;
         If (Size.X=1) Then Y1 := Pos+1                 { Vertical bar }
           Else X1 := Pos+1;                            { Horizontal bar }
           WriteChar(X1,Y1,Chars[3],2,1);
       End Else Begin
         If (Size.X=1) Then Y1 := Pos + FontHeight      { Vertical bar }
           Else X1 := Pos + FontWidth;                  { Horizontal bar }
         X2 := X1 + FontWidth - 1;                      { Right side point }
         Y2 := Y1 + FontHeight - 1;                     { Lower side point }
         ClearArea(X1, Y1, X2, Y2, GetColor(2) AND $0F);{ Thumbnail back }
         BiColorRectangle(X1, Y1, X2, Y2, 15, 8, False);{ Draw highlight }
         Y1 := (Y2 + Y1) DIV 2;                         { Middle of thumb }
         Y2 := Y1+1;                                    { One line down }
         Inc(X1, 1);                                    { One in off left }
         Dec(X2, 1);                                    { One in off right }
         BiColorRectangle(X1, Y1, X2, Y2, 15, 8, True); { Draw line marker }
       End;
       ShowMouseCursor;                               { Show the mouse }
     End;
     ReleaseViewLimits;                               { Release the limits }
   End;
END;

{--TScrollBar---------------------------------------------------------------}
{  ClearPos -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB          }
{---------------------------------------------------------------------------}
{   This could be called from a message handling event so it must check the }
{  view is visible, exposed and not obstructed before clearing the old      }
{  thumbnail area.                                                          }
{---------------------------------------------------------------------------}
PROCEDURE TScrollBar.ClearPos (Pos: Sw_Integer);
VAR X, Y: Sw_Integer; ViewPort: ViewPortType;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfExposed <> 0) Then Begin              { View is exposed }
     SetViewLimits;                                   { Set view limits }
     GetViewSettings(ViewPort, TextModeGFV);          { Get set viewport }
     If OverlapsArea(ViewPort.X1, ViewPort.Y1,
     ViewPort.X2, ViewPort.Y2) Then Begin             { Must be in area }
       HideMouseCursor;                               { Hide the mouse }
       X := 0;                                        { Initial x position }
       Y := 0;                                        { Initial y position }
       If (Size.X=1) Then Y := Pos + FontHeight       { Vertical bar }
         Else X := Pos + FontWidth;                   { Horizontal bar }
       ClearArea(X, Y, X+FontWidth-1, Y+FontHeight-1,
         GetColor(1) AND $F0 SHR 4);                  { Clear the area }
       ShowMouseCursor;                               { Show the mouse }
     End;
     ReleaseViewLimits;                               { Release the limits }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TScroller OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TScroller----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TScroller.Init (Var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Options := Options OR ofSelectable;                { View is selectable }
   EventMask := EventMask OR evBroadcast;             { See broadcasts }
   HScrollBar := AHScrollBar;                         { Hold horz scrollbar }
   VScrollBar := AVScrollBar;                         { Hold vert scrollbar }
END;

{--TScroller----------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB              }
{---------------------------------------------------------------------------}
{   This load method will read old original TV data from a stream as well   }
{   as the new graphical scroller views.                                    }
{---------------------------------------------------------------------------}
CONSTRUCTOR TScroller.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   GetPeerViewPtr(S, HScrollBar);                     { Load horz scrollbar }
   GetPeerViewPtr(S, VScrollBar);                     { Load vert scrollbar }
   S.Read(Delta.X, 2);                                { Read delta x value }
   S.Read(Delta.Y, 2);                                { Read delta y value }
   S.Read(Limit.X, 2);                                { Read limit x value }
   S.Read(Limit.Y, 2);                                { Read limit y value }
END;

{--TScroller----------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TScroller.GetPalette: PPalette;
CONST P: String[Length(CScroller)] = CScroller;       { Always normal string }
BEGIN
   GetPalette := @P;                                  { Scroller palette }
END;

{--TScroller----------------------------------------------------------------}
{  ScrollTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TScroller.ScrollTo (X, Y: Sw_Integer);
BEGIN
   Inc(DrawLock);                                     { Set draw lock }
   If (HScrollBar<>Nil) Then HScrollBar^.SetValue(X); { Set horz scrollbar }
   If (VScrollBar<>Nil) Then VScrollBar^.SetValue(Y); { Set vert scrollbar }
   Dec(DrawLock);                                     { Release draw lock }
   CheckDraw;                                         { Check need to draw }
END;

{--TScroller----------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TScroller.SetState (AState: Word; Enable: Boolean);

   PROCEDURE ShowSBar (SBar: PScrollBar);
   BEGIN
     If (SBar <> Nil) Then                            { Scroll bar valid }
       If GetState(sfActive + sfSelected) Then        { Check state masks }
         SBar^.Show Else SBar^.Hide;                  { Draw appropriately }
   END;

BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState AND (sfActive + sfSelected) <> 0)       { Active/select change }
   Then Begin
     ShowSBar(HScrollBar);                            { Redraw horz scrollbar }
     ShowSBar(VScrollBar);                            { Redraw vert scrollbar }
   End;
END;

{--TScroller----------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB             }
{---------------------------------------------------------------------------}
{  The scroller is saved to the stream compatable with the old TV object.   }
{---------------------------------------------------------------------------}
PROCEDURE TScroller.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { Call TView explicitly }
   PutPeerViewPtr(S, HScrollBar);                     { Store horz bar }
   PutPeerViewPtr(S, VScrollBar);                     { Store vert bar }
   S.Write(Delta.X, 2);                               { Write delta x value }
   S.Write(Delta.Y, 2);                               { Write delta y value }
   S.Write(Limit.X, 2);                               { Write limit x value }
   S.Write(Limit.Y, 2);                               { Write limit y value }
END;

{--TScroller----------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TScroller.HandleEvent (Var Event: TEvent);
BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   If (Event.What = evBroadcast) AND
     (Event.Command = cmScrollBarChanged) AND         { Scroll bar change }
     ((Event.InfoPtr = HScrollBar) OR                 { Our scrollbar? }
      (Event.InfoPtr = VScrollBar)) Then ScrollDraw;  { Redraw scroller }
END;

{--TScroller----------------------------------------------------------------}
{  ChangeBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TScroller.ChangeBounds (Var Bounds: TRect);
BEGIN
   SetBounds(Bounds);                                 { Set new bounds }
   Inc(DrawLock);                                     { Set draw lock }
   SetLimit(Limit.X, Limit.Y);                        { Adjust limits }
   Dec(DrawLock);                                     { Release draw lock }
   DrawFlag := False;                                 { Clear draw flag }
   DrawView;                                          { Redraw now }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TListViewer OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

CONST TvListViewerName = 'LISTBOX';                   { Native name }

{--TListViewer--------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TListViewer.Init (Var Bounds: TRect; ANumCols: Sw_Word; AHScrollBar,
  AVScrollBar: PScrollBar);
VAR ArStep, PgStep: Sw_Integer;
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Options := Options OR (ofFirstClick+ofSelectable); { Set options }
   EventMask := EventMask OR evBroadcast;             { Set event mask }
   NumCols := ANumCols;                               { Hold column number }
   If (AVScrollBar <> Nil) Then Begin                 { Chk vert scrollbar }
     If (NumCols = 1) Then Begin                      { Only one column }
       PgStep := Size.Y -1;                           { Set page size }
       ArStep := 1;                                   { Set step size }
     End Else Begin                                   { Multiple columns }
       PgStep := Size.Y * NumCols;                    { Set page size }
       ArStep := Size.Y;                              { Set step size }
     End;
     AVScrollBar^.SetStep(PgStep, ArStep);            { Set scroll values }
   End;
   If (AHScrollBar <> Nil) Then
     AHScrollBar^.SetStep(Size.X DIV NumCols, 1);     { Set step size }
   HScrollBar := AHScrollBar;                         { Horz scrollbar held }
   VScrollBar := AVScrollBar;                         { Vert scrollbar held }
   GOptions := GOptions OR goDrawFocus;               { Draw focus changes }
END;

{--TListViewer--------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TListViewer.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   GetPeerViewPtr(S, HScrollBar);                     { Get horz scrollbar }
   GetPeerViewPtr(S, VScrollBar);                     { Get vert scrollbar }
   S.Read(NumCols, 2);                                { Read column number }
   S.Read(TopItem, 2);                                { Read top most item }
   S.Read(Focused, 2);                                { Read focused item }
   S.Read(Range, 2);                                  { Read listview range }
END;

{--TListViewer--------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TListViewer.GetPalette: PPalette;
CONST P: String[Length(CListViewer)] = CListViewer;   { Always normal string }
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TListViewer--------------------------------------------------------------}
{  IsSelected -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TListViewer.IsSelected (Item: Sw_Integer): Boolean;
BEGIN
   If (Item = Focused) Then IsSelected := True Else
     IsSelected := False;                             { Selected item }
END;

{--TListViewer--------------------------------------------------------------}
{  GetText -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TListViewer.GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String;
BEGIN                                                 { Abstract method }
   GetText := '';                                     { Return empty }
END;

{--TListViewer--------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.DrawBackGround;
VAR  I, J, ColWidth, Item, Indent, CurCol: Sw_Integer; Color: Word;
    Text: String; B: TDrawBuffer;

BEGIN
   ColWidth := Size.X DIV NumCols + 1;                { Calc column width }
   If (HScrollBar = Nil) Then Indent := 0 Else        { Set indent to zero }
     Indent := HScrollBar^.Value;                     { Fetch any indent }
   Inherited DrawBackGround;                          { Call ancestor }
   Color := GetColor(2);                              { Normal colour }
   For I := 0 To Size.Y - 1 Do Begin                  { For each line }
     For J := 0 To NumCols-1 Do Begin                 { For each column }
       Item := J*Size.Y + I + TopItem;                { Process this item }
       CurCol := J*ColWidth;                          { Current column }
       MoveChar(B[CurCol], ' ', Color, ColWidth);     { Clear buffer }
       If (Item < Range) Then Begin                   { Within text range }
         Text := GetText(Item, ColWidth + Indent);    { Fetch text }
         Text := Copy(Text, Indent, ColWidth);        { Select right bit }
         MoveStr(B[CurCol+1], Text, Color);           { Transfer to buffer }
         If ShowMarkers Then Begin
           WordRec(B[CurCol]).Lo := Byte(
             SpecialChars[4]);                        { Set marker character }
           WordRec(B[CurCol+ColWidth-2]).Lo := Byte(
             SpecialChars[5]);                        { Set marker character }
         End;
       End;
       MoveChar(B[CurCol+ColWidth-1], #179,
         GetColor(5), 1);                             { Put centre line marker }
     End;
     WriteLine(0, I, Size.X-1, 1, B);                 { Write line to screen }
   End;
END;

{--TListViewer--------------------------------------------------------------}
{  DrawFocus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.DrawFocus;
VAR DrawIt: Boolean; SCOff: Byte; I, J, Item, CurCol, ColWidth: Sw_Integer;
    Color: Word;

  Indent: Sw_Integer;
  B: TDrawBuffer;
  Text: String;
BEGIN
   ColWidth := Size.X DIV NumCols + 1;                { Calc column width }
   If (HScrollBar = Nil) Then Indent := 0 Else        { Set indent to zero }
     Indent := HScrollBar^.Value;                     { Fetch any indent }
   For I := 0 To Size.Y - 1 Do Begin                  { For each line }
     For J := 0 To NumCols-1 Do Begin                 { For each column }
       Item := J*Size.Y + I + TopItem;                { Process this item }
       CurCol := J*ColWidth;                          { Current column }
       DrawIt := False;                               { Preset false }
       If (State AND (sfSelected + sfActive) =
       (sfSelected + sfActive)) AND (Focused = Item)  { Focused item }
       AND (Range > 0) Then Begin
         DrawIt := True;                              { Draw this item }
         Color := GetColor(3);                        { Focused colour }
         SetCursor(CurCol+1,I);                       { Set the cursor }
         SCOff := 0;                                  { Zero colour offset }
       End Else If (Item < Range) AND IsSelected(Item){ Selected item }
       Then Begin
         DrawIt := True;                              { Draw this item }
         If (State AND sfActive <> 0) Then
           Color := GetColor(4) Else                  { Selected colour }
           Color := GetColor(2);                      { Remove focus }
         SCOff := 2;                                  { Colour offset=2 }
       End;
       If DrawIt Then Begin                           { We are drawing item }
         ClearArea(CurCol*FontWidth, I*FontHeight, (CurCol+ColWidth-1)*FontWidth,
           (I+1)*FontHeight-1, Color AND $F0 SHR 4);  { Draw the bar }
         MoveChar(B[CurCol], ' ', Color, ColWidth);
         if Item < Range then begin
           Text := GetText(Item, ColWidth + Indent);
           Text := Copy(Text,Indent,ColWidth);
           MoveStr(B[CurCol+1], Text, Color);
           if ShowMarkers then begin
             WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
             WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
           end;
         end;
         { MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);}
         WriteLine(CurCol, I, Min(Size.X-1-CurCol,ColWidth-2), 1, B[CurCol]);
       End;
     End;
   End;
END;

{--TListViewer--------------------------------------------------------------}
{  DrawLoseFocus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.DrawLoseFocus;
VAR DrawIt: Boolean; SCOff: Byte; I, J, Item, CurCol, ColWidth: Sw_Integer;
    Color: Word;

  Indent: Sw_Integer;
  B: TDrawBuffer;
  Text: String;
BEGIN
   ColWidth := Size.X DIV NumCols + 1;                { Calc column width }
   If (HScrollBar = Nil) Then Indent := 0 Else        { Set indent to zero }
     Indent := HScrollBar^.Value;                     { Fetch any indent }
   For I := 0 To Size.Y - 1 Do Begin                  { For each line }
     For J := 0 To NumCols-1 Do Begin                 { For each column }
       Item := J*Size.Y + I + TopItem;                { Process this item }
       CurCol := J*ColWidth;                          { Current column }
       DrawIt := False;                               { Preset false }
       If (State AND (sfSelected + sfActive) =
       (sfSelected + sfActive)) AND (Focused = Item)  { Focused item }
       AND (Range > 0) Then Begin
         DrawIt := True;                              { Draw this item }
         Color := GetColor(2);                        { Focused colour }
         SetCursor(CurCol+1,I);                       { Set the cursor }
         SCOff := 2;                                  { Zero colour offset }
       End Else If (Item < Range) AND IsSelected(Item){ Selected item }
       Then Begin
         DrawIt := True;                              { Draw this item }
         Color := GetColor(2);                        { Remove focus }
         SCOff := 2;                                  { Colour offset=2 }
       End;
       If DrawIt Then Begin                           { We are drawing item }
         ClearArea(CurCol*FontWidth, I*FontHeight, (CurCol+ColWidth-1)*FontWidth,
           (I+1)*FontHeight-1, Color AND $F0 SHR 4);  { Draw the bar }
         MoveChar(B[CurCol], ' ', Color, ColWidth);
         if Item < Range then begin
           Text := GetText(Item, ColWidth + Indent);
           Text := Copy(Text,Indent,ColWidth);
           MoveStr(B[CurCol+1], Text, Color);
           if ShowMarkers then begin
             WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
             WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
           end;
         end;
         { MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);}
         WriteLine(CurCol, I, Min(Size.X-1-CurCol,ColWidth-2), 1, B[CurCol]);
       End;
     End;
   End;
END;


{--TListViewer--------------------------------------------------------------}
{  FocusItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.FocusItem (Item: Sw_Integer);
BEGIN
   If Focused<>Item then
     DrawLoseFocus;
   Focused := Item;                                   { Set focus to item }
   If (VScrollBar <> Nil) Then
     VScrollBar^.SetValue(Item);                      { Scrollbar to value }
   If (Item < TopItem) Then                           { Item above top item }
     If (NumCols = 1) Then TopItem := Item            { Set top item }
       Else TopItem := Item - Item MOD Size.Y         { Set top item }
   Else If (Item >= TopItem + (Size.Y*NumCols)) Then  { Item below bottom }
     If (NumCols = 1) Then TopItem := Item-Size.Y+1   { Set new top item }
     Else TopItem := Item - Item MOD Size.Y -
       (Size.Y*(NumCols-1));                          { Set new top item }
END;

{--TListViewer--------------------------------------------------------------}
{  SetTopItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Aug99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.SetTopItem (Item: Sw_Integer);
BEGIN
   TopItem := Item;                                   { Set the top item }
END;

{--TListViewer--------------------------------------------------------------}
{  SetRange -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.SetRange (ARange: Sw_Integer);
BEGIN
   Range := ARange;                                   { Set new range }
   If (VScrollBar <> Nil) Then Begin                  { Vertical scrollbar }
     If (Focused > ARange) Then Focused := 0;         { Clear focused }
     VScrollBar^.SetParams(Focused, 0, ARange - 1,
       VScrollBar^.PgStep, VScrollBar^.ArStep);       { Set parameters }
   End;
END;

{--TListViewer--------------------------------------------------------------}
{  SelectItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.SelectItem (Item: Sw_Integer);
BEGIN
   Message(Owner, evBroadcast, cmListItemSelected,
     @Self);                                          { Send message }
END;

{--TListViewer--------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.SetState (AState: Word; Enable: Boolean);

   PROCEDURE ShowSBar(SBar: PScrollBar);
   BEGIN
     If (SBar <> Nil) Then                            { Valid scrollbar }
       If GetState(sfActive) AND GetState(sfVisible)  { Check states }
         Then SBar^.Show Else SBar^.Hide;             { Show or hide }
   END;

   PROCEDURE LoseFocus;
   VAR Cs: Sw_Integer;
   BEGIN
     If (GOptions AND goNativeClass = 0) Then Begin   { Not in native mode }
       Cs := State;                                   { Hold current state }
       State := State AND NOT sfActive;               { Must remove focus }
       SetDrawmask(vdFocus);                          { Set focus mask }
       DrawView;                                      { Remove focus box }
       State := Cs;                                   { Reset state masks }
     End;
   END;

BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState AND sfFocused <> 0) Then                { Focus change }
     If NOT Enable Then LoseFocus;                    { Redraw drop focus }
   If (AState AND (sfSelected + sfActive + sfVisible) <> 0)
   Then Begin                                         { Check states }
     SetDrawMask(vdFocus);
     DrawView;                                        { Draw the view }
     ShowSBar(HScrollBar);                            { Show horz scrollbar }
     ShowSBar(VScrollBar);                            { Show vert scrollbar }
   End;
END;

{--TListViewer--------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.Store (Var S: TStream);
BEGIN
   TView.Store(S);                                    { Call TView explicitly }
   PutPeerViewPtr(S, HScrollBar);                     { Put horz scrollbar }
   PutPeerViewPtr(S, VScrollBar);                     { Put vert scrollbar }
   S.Write(NumCols, 2);                               { Write column number }
   S.Write(TopItem, 2);                               { Write top most item }
   S.Write(Focused, 2);                               { Write focused item }
   S.Write(Range, 2);                                 { Write listview range }
END;

{--TListViewer--------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.HandleEvent (Var Event: TEvent);
CONST MouseAutosToSkip = 4;
VAR Oi, Ni: Sw_Integer; Ct, Cw: Word; Mouse: TPoint;

   PROCEDURE MoveFocus (Req: Sw_Integer);
   VAR Ti, Cs: Sw_Integer;
   BEGIN
     If (GOptions AND goNativeClass = 0) Then Begin   { Not in native mode }
       Ti := TopItem;                                 { Hold top item }
       Cs := State;                                   { Hold current state }
       State := State AND NOT sfActive;               { Must remove focus }
       SetDrawmask(vdFocus);                          { Set focus mask }
       DrawView;                                      { Remove focus box }
       State := Cs;                                   { Reset state masks }
     End;
     FocusItemNum(Req);                               { Focus req item }
     If (GOptions AND goNativeClass = 0) Then Begin   { Not in native mode }
       If (Ti <> TopItem) Then DrawView Else Begin    { Redraw all view }
         SetDrawmask(vdFocus);                        { Set focus mask }
         DrawView;                                    { Redraw focus box }
       End;
     End;
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speed up exit }
     evKeyDown: Begin                                 { Key down event }
       If (Event.CharCode = ' ') AND (Focused < Range){ Spacebar select }
       Then Begin
         SelectItem(Focused);                         { Select focused item }
         Ni := Focused;                               { Hold new item }
       End Else Case CtrlToArrow(Event.KeyCode) Of
         kbUp: Ni := Focused - 1;                     { One item up }
         kbDown: Ni := Focused + 1;                   { One item down }
         kbRight: If (NumCols > 1) Then
           Ni := Focused + Size.Y Else Exit;          { One column right }
         kbLeft: If (NumCols > 1) Then
           Ni := Focused - Size.Y Else Exit;          { One column left }
         kbPgDn: Ni := Focused + Size.Y * NumCols;    { One page down }
         kbPgUp: Ni := Focused - Size.Y * NumCols;    { One page up }
         kbHome: Ni := TopItem;                       { Move to top }
         kbEnd: Ni := TopItem + (Size.Y*NumCols)-1;   { Move to bottom }
         kbCtrlPgDn: Ni := Range - 1;                 { Move to last item }
         kbCtrlPgUp: Ni := 0;                         { Move to first item }
         Else Exit;
       End;
       MoveFocus(Ni);                                 { Move the focus }
       ClearEvent(Event);                             { Event was handled }
     End;
     evBroadcast: Begin                               { Broadcast event }
       If (Options AND ofSelectable <> 0) Then        { View is selectable }
         If (Event.Command = cmScrollBarClicked) AND  { Scrollbar click }
         ((Event.InfoPtr = HScrollBar) OR
         (Event.InfoPtr = VScrollBar)) Then Select    { Scrollbar selects us }
         Else If (Event.Command = cmScrollBarChanged) { Scrollbar changed }
         Then Begin
           If (VScrollBar = Event.InfoPtr) Then Begin
             MoveFocus(VScrollBar^.Value);            { Focus us to item }
           End Else If (HScrollBar = Event.InfoPtr)
             Then DrawView;                           { Redraw the view }
         End;
     End;
     evMouseDown: Begin                               { Mouse down event }
       Cw := Size.X DIV NumCols + 1;                  { Column width }
       Oi := Focused;                                 { Hold focused item }
       MakeLocal(Event.Where, Mouse);                 { Localize mouse }
       If MouseInView(Event.Where) Then Ni := Mouse.Y
         + (Size.Y*(Mouse.X DIV Cw))+TopItem          { Calc item to focus }
         Else Ni := Oi;                               { Focus old item }
       Ct := 0;                                       { Clear count value }
       Repeat
         If (Ni <> Oi) Then Begin                     { Item is different }
           MoveFocus(Ni);                             { Move the focus }
           Oi := Focused;                             { Hold as focused item }
         End;
         MakeLocal(Event.Where, Mouse);               { Localize mouse }
         If NOT MouseInView(Event.Where) Then Begin
           If (Event.What = evMouseAuto) Then Inc(Ct);{ Inc auto count }
           If (Ct = MouseAutosToSkip) Then Begin
             Ct := 0;                                 { Reset count }
             If (NumCols = 1) Then Begin              { Only one column }
               If (Mouse.Y < 0) Then Ni := Focused-1; { Move up one item  }
               If (Mouse.Y >= Size.Y) Then
                 Ni := Focused+1;                     { Move down one item }
             End Else Begin                           { Multiple columns }
               If (Mouse.X < 0) Then                  { Mouse x below zero }
                 Ni := Focused-Size.Y;                { Move down 1 column }
               If (Mouse.X >= Size.X) Then            { Mouse x above width }
                 Ni := Focused+Size.Y;                { Move up 1 column }
               If (Mouse.Y < 0) Then                  { Mouse y below zero }
                 Ni := Focused-Focused MOD Size.Y;    { Move up one item }
               If (Mouse.Y > Size.Y) Then             { Mouse y above height }
                 Ni := Focused-Focused MOD
                   Size.Y+Size.Y-1;                   { Move down one item }
             End;
           End;
         End Else Ni := Mouse.Y + (Size.Y*(Mouse.X
           DIV Cw))+TopItem;                          { New item to focus }
       Until NOT MouseEvent(Event, evMouseMove +
         evMouseAuto);                                { Mouse stopped }
       If (Oi <> Ni) Then MoveFocus(Ni);              { Focus moved again }
       If (Event.Double AND (Range > Focused)) Then
         SelectItem(Focused);                         { Select the item }
       ClearEvent(Event);                             { Event was handled }
     End;
   End;
END;

{--TListViewer--------------------------------------------------------------}
{  ChangeBounds -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.ChangeBounds (Var Bounds: TRect);
BEGIN
   Inherited ChangeBounds(Bounds);                    { Call ancestor }
   If (HScrollBar <> Nil) Then                        { Valid horz scrollbar }
     HScrollBar^.SetStep(Size.X DIV NumCols,
       HScrollBar^.ArStep);                           { Update horz bar }
   If (VScrollBar <> Nil) Then                        { Valid vert scrollbar }
     VScrollBar^.SetStep(Size.Y * NumCols,
       VScrollBar^.ArStep);                           { Update vert bar }
END;

{***************************************************************************}
{                     TListViewer OBJECT PRIVATE METHODS                    }
{***************************************************************************}

{--TListViewer--------------------------------------------------------------}
{  FocusItemNum -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Jul99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TListViewer.FocusItemNum (Item: Sw_Integer);
BEGIN
   If (Item < 0) Then Item := 0 Else                  { Restrain underflow }
     If (Item >= Range) AND (Range > 0) Then
       Item := Range-1;                               { Restrain overflow }
   If (Range <> 0) Then FocusItem(Item);              { Set focus value }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TWindow OBJECT METHODS                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TWindow------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TWindow.Init (Var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   State := State OR sfShadow;                        { View is shadowed }
   Options := Options OR (ofSelectable+ofTopSelect);  { Select options set }
   GrowMode := gfGrowAll + gfGrowRel;                 { Set growmodes }
   Flags := wfMove + wfGrow + wfClose + wfZoom;       { Set flags }
   Title := NewStr(ATitle);                           { Hold title }
   Number := ANumber;                                 { Hold number }
   Palette := wpBlueWindow;                           { Default palette }
   GOptions := GOptions OR goThickFramed;             { Thick frame }
   GOptions := GOptions OR goTitled;                  { Title window }
   GOptions := GOptions AND NOT goNoDrawView;         { View does draw self }
   InitFrame;                                         { Initialize frame }
   If (Frame <> Nil) Then Insert(Frame);              { Insert any frame }
   GetBounds(ZoomRect);                               { Default zoom rect }
END;

{--TWindow------------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB              }
{---------------------------------------------------------------------------}
{   This load method will read old original TV data from a stream however   }
{   although a frame view is read for compatability it is disposed of.      }
{---------------------------------------------------------------------------}
CONSTRUCTOR TWindow.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Flags, 1);                                  { Read window flags }
   S.Read(Number, 2);                                 { Read window number }
   S.Read(Palette, 2);                                { Read window palette }
   S.Read(ZoomRect.A.X, 2);                           { Read zoom area x1 }
   S.Read(ZoomRect.A.Y, 2);                           { Read zoom area y1 }
   S.Read(ZoomRect.B.X, 2);                           { Read zoom area x2 }
   S.Read(ZoomRect.B.Y, 2);                           { Read zoom area y2 }
   GetSubViewPtr(S, Frame);                           { Now read frame object }
   If (Frame <> Nil) Then Begin
     Dispose(Frame, Done);                            { Kill we don't use it }
     Frame := Nil;                                    { Clear the pointer }
   End;
   Title := S.ReadStr;                                { Read title }
END;

{--TWindow------------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TWindow.Done;
BEGIN
   Inherited Done;                                    { Call ancestor }
   If (Title <> Nil) Then DisposeStr(Title);          { Dispose title }
END;

{--TWindow------------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TWindow.GetPalette: PPalette;
CONST  P: ARRAY [wpBlueWindow..wpGrayWindow] Of String[Length(CBlueWindow)] =
  (CBlueWindow, CCyanWindow, CGrayWindow);            { Always normal string }
BEGIN
   GetPalette := @P[Palette];                         { Return palette }
END;

{--TWindow------------------------------------------------------------------}
{  GetTitle -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB          }
{  Modified 31may2002 PM  (No number included anymore)                      }
{---------------------------------------------------------------------------}
FUNCTION TWindow.GetTitle (MaxSize: Sw_Integer): TTitleStr;
VAR S: String;
BEGIN
   If (Title <> Nil) Then S:=Title^
   Else S := '';
   if Length(S)>MaxSize then
     GetTitle:=Copy(S,1,MaxSize)
   else
     GetTitle:=S;
END;

{--TWindow------------------------------------------------------------------}
{  StandardScrollBar -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB }
{---------------------------------------------------------------------------}
FUNCTION TWindow.StandardScrollBar (AOptions: Word): PScrollBar;
VAR R: TRect; S: PScrollBar;
BEGIN
   GetExtent(R);                                      { View extents }
   If (AOptions AND sbVertical = 0) Then
     R.Assign(R.A.X+2, R.B.Y-1, R.B.X-2, R.B.Y)       { Horizontal scrollbar }
     Else R.Assign(R.B.X-1, R.A.Y+1, R.B.X, R.B.Y-1); { Vertical scrollbar }
   S := New(PScrollBar, Init(R));                     { Create scrollbar }
   Insert(S);                                         { Insert scrollbar }
   If (AOptions AND sbHandleKeyboard <> 0) Then
     S^.Options := S^.Options or ofPostProcess;       { Post process }
   StandardScrollBar := S;                            { Return scrollbar }
END;

{--TWindow------------------------------------------------------------------}
{  Zoom -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23Sep97 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.Zoom;
VAR R: TRect; Max, Min: TPoint;
BEGIN
   SizeLimits(Min, Max);                              { Return size limits }
   If ((Size.X <> Max.X) OR (Size.Y <> Max.Y))        { Larger size possible }
   Then Begin
     GetBounds(ZoomRect);                             { Get zoom bounds }
     R.A.X := 0;                                      { Zero x origin }
     R.A.Y := 0;                                      { Zero y origin }
     R.B := Max;                                      { Bounds to max size }
     Locate(R);                                       { Locate the view }
   End Else Locate(ZoomRect);                         { Move to zoom rect }
END;

{--TWindow------------------------------------------------------------------}
{  Close -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23Sep97 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.Close;
BEGIN
   If Valid(cmClose) Then Free;                       { Dispose of self }
END;

{--TWindow------------------------------------------------------------------}
{  InitFrame -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.InitFrame;
BEGIN                                                 { Compatability only }
END;

{--TWindow------------------------------------------------------------------}
{  SetState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Mar98 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.SetState (AState: Word; Enable: Boolean);
VAR WindowCommands: TCommandSet;
BEGIN
   Inherited SetState(AState, Enable);                { Call ancestor }
   If (AState = sfSelected) Then
     SetState(sfActive, Enable);                      { Set active state }
   If (AState = sfSelected) OR ((AState = sfExposed)
   AND (State AND sfSelected <> 0)) Then Begin        { View is selected }
     WindowCommands := [cmNext, cmPrev];              { Set window commands }
     If (Flags AND (wfGrow + wfMove) <> 0) Then
       WindowCommands := WindowCommands + [cmResize]; { Add resize command }
     If (Flags AND wfClose <> 0) Then
       WindowCommands := WindowCommands + [cmClose];  { Add close command }
     If (Flags AND wfZoom <> 0) Then
       WindowCommands := WindowCommands + [cmZoom];   { Add zoom command }
     If Enable Then EnableCommands(WindowCommands)    { Enable commands }
       Else DisableCommands(WindowCommands);          { Disable commands }
   End;
END;

{--TWindow------------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Mar98 LdB             }
{---------------------------------------------------------------------------}
{  You can save data to the stream compatable with the old original TV by   }
{  temporarily turning off the ofGrafVersion making the call to this store  }
{  routine and resetting the ofGrafVersion flag after the call.             }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.Store (Var S: TStream);
BEGIN
   TGroup.Store(S);                                   { Call group store }
   S.Write(Flags, 1);                                 { Write window flags }
   S.Write(Number, 2);                                { Write window number }
   S.Write(Palette, 2);                               { Write window palette }
   S.Write(ZoomRect.A.X, 2);                          { Write zoom area x1 }
   S.Write(ZoomRect.A.Y, 2);                          { Write zoom area y1 }
   S.Write(ZoomRect.B.X, 2);                          { Write zoom area x2 }
   S.Write(ZoomRect.B.Y, 2);                          { Write zoom area y2 }
   PutSubViewPtr(S, Frame);                           { Write any frame }
   S.WriteStr(Title);                                 { Write title string }
END;

{--TWindow------------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11Aug99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.HandleEvent (Var Event: TEvent);
VAR I, J: Sw_Integer;
    Min, Max: TPoint; Limits: TRect;

   PROCEDURE DragWindow (Mode: Byte);
   VAR Limits: TRect; Min, Max: TPoint;
   BEGIN
     Owner^.GetExtent(Limits);                        { Get owner extents }
     SizeLimits(Min, Max);                            { Restrict size }
     DragView(Event, DragMode OR Mode, Limits, Min,
       Max);                                          { Drag the view }
     ClearEvent(Event);                               { Clear the event }
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evNothing: Exit;                                 { Speeds up exit }
     evCommand:                                       { COMMAND EVENT }
       Case Event.Command Of                          { Command type case }
         cmResize:                                    { RESIZE COMMAND }
           If (Flags AND (wfMove + wfGrow) <> 0)      { Window can resize }
           AND (Owner <> Nil) Then Begin              { Valid owner }
             Owner^.GetExtent(Limits);                { Owners extents }
             SizeLimits(Min, Max);                    { Check size limits }
             DragView(Event, DragMode OR (Flags AND
               (wfMove + wfGrow)), Limits, Min, Max); { Drag the view }
             ClearEvent(Event);                       { Clear the event }
           End;
         cmClose:                                     { CLOSE COMMAND }
           If (Flags AND wfClose <> 0) AND            { Close flag set }
           ((Event.InfoPtr = Nil) OR                  { None specific close }
           (Event.InfoPtr = @Self)) Then Begin        { Close to us }
             ClearEvent(Event);                       { Clear the event }
             If (State AND sfModal = 0) Then Close    { Non modal so close }
             Else Begin                               { Modal window }
               Event.What := evCommand;               { Command event }
               Event.Command := cmCancel;             { Cancel command }
               PutEvent(Event);                       { Place on queue }
               ClearEvent(Event);                     { Clear the event }
             End;
           End;
         cmZoom:                                      { ZOOM COMMAND }
           If (Flags AND wfZoom <> 0) AND             { Zoom flag set }
           ((Event.InfoPtr = Nil) OR                  { No specific zoom }
           (Event.InfoPtr = @Self)) Then Begin
             Zoom;                                    { Zoom our window }
             ClearEvent(Event);                       { Clear the event }
           End;
       End;
     evBroadcast:                                     { BROADCAST EVENT }
       If (Event.Command = cmSelectWindowNum) AND
       (Event.InfoInt = Number) AND                   { Select our number }
       (Options AND ofSelectable <> 0) Then Begin     { Is view selectable }
         Select;                                      { Select our view }
         ClearEvent(Event);                           { Clear the event }
       End;
     evKeyDown: Begin                                 { KEYDOWN EVENT }
       Case Event.KeyCode Of
         kbTab: Begin                                 { TAB KEY }
           FocusNext(False);                          { Select next view }
           ClearEvent(Event);                         { Clear the event }
         End;
         kbShiftTab: Begin                            { SHIFT TAB KEY }
           FocusNext(True);                           { Select prior view }
           ClearEvent(Event);                         { Clear the event }
         End;
       End;
     End;
     evMouseDown:                                     { MOUSE DOWN EVENT }
       If (GOptions AND goTitled <> 0) Then Begin     { Must have title area }
         If TextModeGFV then
           I:=0 Else
           If (GOptions AND goThickFramed <> 0) Then
             I := 5 Else                                { Thick frame adjust }
             If (Options AND ofFramed <> 0) Then I := 1 { Frame adjust }
               Else I := 0;                             { No frame size }
         If (Event.Where.Y >= (RawOrigin.Y + I)) AND
         (Event.Where.Y < RawOrigin.Y+FontHeight+I)
         Then Begin                                   { Within top line }
           If (Current <> Nil) AND
           (Current^.Options AND ofSelectable <> 0)
             Then Current^.FocusFromTop Else
             FocusFromTop;
           If (Flags AND wfClose <> 0) Then Begin     { Has close icon }
             J := I + 2*FontWidth;                      { Set X value }
             If (Event.Where.X >= RawOrigin.X+J) AND
             (Event.Where.X < RawOrigin.X+J+3*FontWidth)
             Then Begin                               { In close area }
               Event.What := evCommand;               { Command event }
               Event.Command := cmClose;              { Close command }
               Event.InfoPtr := Nil;                  { Clear pointer }
               PutEvent(Event);                       { Put event on queue }
               ClearEvent(Event);                     { Clear the event }
               Exit;                                  { Now exit }
             End;
           End;
           If (Flags AND wfZoom <> 0) Then Begin     { Has Zoom icon }
             J := (SIZE.X-5)*FontWidth;                      { Set X value }
             If (Event.Where.X >= RawOrigin.X+J) AND
             (Event.Where.X < RawOrigin.X+J+3*FontWidth)
             Then Begin                               { In close area }
               Event.What := evCommand;               { Command event }
               Event.Command := cmZoom;               { Close command }
               Event.InfoPtr := Nil;                  { Clear pointer }
               PutEvent(Event);                       { Put event on queue }
               ClearEvent(Event);                     { Clear the event }
               Exit;                                  { Now exit }
             End;
           End;
           If (Owner <> Nil) AND (Flags AND wfMove <> 0)
             Then DragWindow(dmDragMove);             { Drag the window }
         End Else If (Event.Where.X >= RawOrigin.X + RawSize.X-2*FontWidth) AND
         (Event.Where.Y >= RawOrigin.Y + RawSize.Y - FontHeight)
         Then If (Flags AND wfGrow <> 0) Then         { Check grow flags }
           DragWindow(dmDragGrow);                    { Change window size }
       End;
   End;                                               { Event.What case end }
END;

{--TWindow------------------------------------------------------------------}
{  SizeLimits -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15Sep97 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TWindow.SizeLimits (Var Min, Max: TPoint);
BEGIN
   Inherited SizeLimits(Min, Max);                    { View size limits }
   Min.X := MinWinSize.X;                             { Set min x size }
   Min.Y := MinWinSize.Y;                             { Set min y size }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       UNCOMPLETED OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}



{--TView--------------------------------------------------------------------}
{  Exposed -> Platforms DOS/DPMI/WIN/OS2 - Checked 17Sep97 LdB              }
{---------------------------------------------------------------------------}
{ This needs big help!!!!!   }
FUNCTION TView.Exposed: Boolean;
VAR ViewPort: ViewPortType;
BEGIN
   GetViewSettings(ViewPort, TextModeGFV);            { Fetch viewport }
   If (State AND sfVisible<>0) AND                    { View visible }
     (State AND sfExposed<>0) AND                     { View exposed }
     OverlapsArea(ViewPort.X1, ViewPort.Y1,
     ViewPort.X2, ViewPort.Y2) Then Exposed := True   { Must be exposed }
       Else Exposed := False;                         { Is hidden }
END;

{--TView--------------------------------------------------------------------}
{  GraphLine -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22Sep99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TView.GraphLine (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
VAR ViewPort: ViewPortType;
    x,y,i,j : sw_integer;
BEGIN
   GetViewSettings(ViewPort, TextModeGFV);            { Get viewport settings }
   If (TextModeGFV <> TRUE) Then Begin
     SetColor(Colour);                                { Set line colour }
     Line(RawOrigin.X + X1 - ViewPort.X1,
       RawOrigin.Y + Y1 - ViewPort.Y1, RawOrigin.X + X2
       - ViewPort.X1, RawOrigin.Y + Y2-ViewPort.Y1);  { Draw the line }
       { mark the corresponding lines as without chars }
{$IFDEF GRAPH_API}
     If UseFixedFont then
       begin
         if abs(x1-x2)<abs(y1-y2) then
           begin
             for j:=y1 to y2 do
               begin
                 y:=(RawOrigin.y + j);
                 x:=(RawOrigin.X + X1 + ((x2-x1)*(j-y1)) div (y2-y1));
                 if (x>=0) and (x<=Graph.GetMaxX) and (y>=0) and (y<=Graph.GetMaxY) then
                   SetExtraInfo(x div SysFontWidth,y div SysFontHeight,
                     x mod SysFontWidth,y mod SysFontHeight, Colour);
               end;
           end
         else
           begin
             for j:=x1 to x2 do
               begin
                 x:=(RawOrigin.x + j);
                 y:=(RawOrigin.y + y1 + ((j-x1)*(y2-y1)) div (x2-x1));
                 if (x>=0) and (x<=Graph.GetMaxX) and (y>=0) and (y<=Graph.GetMaxY) then
                   SetExtraInfo(x div SysFontWidth,y div SysFontHeight,
                     x mod SysFontWidth,y mod SysFontHeight, Colour);
               end;
           end;
       end;
{$ENDIF GRAPH_API}
   End Else Begin                                     { LEON???? }
   End;
END;

PROCEDURE TView.GraphRectangle (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
VAR ViewPort: ViewPortType;
BEGIN
   If (TextModeGFV <> TRUE) Then Begin                { GRAPHICS MODE GFV }
     If UseFixedFont then
       begin
         Graphline(x1,y1,x1,y2,colour);
         Graphline(x1,y2,x2,y2,colour);
         Graphline(x2,y2,x2,y1,colour);
         Graphline(x2,y1,x1,y1,colour);
       end
     else
       begin
         SetColor(Colour);                                { Set line colour }
         GetViewSettings(ViewPort, TextModeGFV);
         Rectangle(RawOrigin.X + X1 - ViewPort.X1, RawOrigin.Y + Y1
           - ViewPort.Y1, RawOrigin.X + X2 - ViewPort.X1,
           RawOrigin.Y+Y2-ViewPort.Y1);                   { Draw a rectangle }
       end;
   End Else Begin                                     { LEON???? }
   End;
END;

{--TView--------------------------------------------------------------------}
{  ClearArea -> Platforms DOS/DPMI/WIN/OS2 - Checked 19Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.ClearArea (X1, Y1, X2, Y2: Sw_Integer; Colour: Byte);
VAR
    X, Y: Sw_Integer; ViewPort: ViewPortType;
    Buf : TDrawBuffer;
BEGIN
   GetViewSettings(ViewPort, TextModeGFV);            { Get viewport }
   If not TextModeGFV {and not UseFixedFont} Then Begin                { GRAPHICAL GFV MODE }
     SetFillStyle(SolidFill, Colour);                 { Set colour up }
     Bar(RawOrigin.X+X1-ViewPort.X1, RawOrigin.Y+Y1-
       ViewPort.Y1, RawOrigin.X+X2-ViewPort.X1,
       RawOrigin.Y+Y2-ViewPort.Y1);                   { Clear the area }
     { Force redraw if something new is written after }
     if UseFixedFont and assigned(OldVideoBuf) then
       begin
         X1 := (RawOrigin.X+X1) DIV SysFontWidth;
         Y1 := (RawOrigin.Y+Y1) DIV SysFontHeight;
         X2 := (RawOrigin.X+X2-1) DIV SysFontWidth;
         Y2 := (RawOrigin.Y+Y2-1) DIV SysFontHeight;
         For Y := Y1 To Y2 Do
           For X := X1 To X2 Do
             begin
               VideoBuf^[Y*TextScreenWidth+X]:=0;
               OldVideoBuf^[Y*TextScreenWidth+X]:=0;
             end;
       end;
   End Else Begin                                     { TEXT MODE GFV }
     X1 := (RawOrigin.X+X1) DIV SysFontWidth;
     Y1 := (RawOrigin.Y+Y1) DIV SysFontHeight;
     X2 := (RawOrigin.X+X2-1) DIV SysFontWidth;
     Y2 := (RawOrigin.Y+Y2-1) DIV SysFontHeight;
     For X := X1 To X2 Do Begin
       Buf[X-X1]:=(Colour shl 12) or $20;
     End;
     For Y := Y1 To Y2 Do
       WriteAbs(X1,Y, X2-X1, Buf);
     DrawScreenBuf;
   End;
END;


PROCEDURE TView.GraphArc (Xc, Yc: Sw_Integer; Sa, Ea: Real; XRad, YRad: Sw_Integer;
Colour: Byte);
CONST RadConv  = 57.2957795130823229;                 { Degrees per radian }
VAR X1, Y1, X2, Y2, X3, Y3: Sw_Integer; {$IFDEF OS_WINDOWS} ODc: hDc; {$ENDIF}
BEGIN
   {$IFDEF NOT_IMPLEMENTED}
   {$IFDEF OS_WINDOWS}
   Xc := Xc - FrameSize;
   Yc := Yc - CaptSize;
   {$ENDIF}
   While (Ea < -360) Do Ea := Ea + 360;               { Max of a full circle }
   While (Ea > 360) Do Ea := Ea - 360;                { Max of a full circle }
   Sa := Sa/RadConv;                                  { Convert to radians }
   Ea := Ea/RadConv;                                  { Convert to radians }
   X1 := Xc + Round(Sin(Sa)*XRad);                    { Calc 1st x value }
   Y1 := Yc - Round(Cos(Sa)*YRad);                    { Calc 1st y value }
   X2 := Xc + Round(Sin(Sa+Ea)*XRad);                 { Calc 2nd x value }
   Y2 := Yc - Round(Cos(Sa+Ea)*YRad);                 { Calc 2nd y value }
   X3 := X2;                                          { Use X2 value }
   Y3 := Y2;                                          { Use Y2 value }
   If (Abs(Ea) > Pi) Then Begin
     X3 := Xc + Round(Sin(Sa+Pi)*XRad);               { Calc 3rd x value }
     Y3 := Yc - Round(Cos(Sa+Pi)*YRad);               { Calc 3rd y value }
   End;
   {$IFDEF OS_WINDOWS}
   If (HWindow <> 0) Then Begin                       { Valid window }
     ODc := Dc;                                       { Hold device context }
     If (Dc = 0) Then Dc := GetDC(HWindow);           { Create a context }
     SelectObject(Dc, ColPen[Colour]);                { Pen colour }
     If (Abs(X1-X3) > 1) OR (Abs(Y1-Y3) > 1)          { Must exceed 2x2 arc }
     Then Begin
       If (Ea < 0) Then
         Arc(Dc, Xc-XRad, Yc-YRad, Xc+XRad, Yc+YRad,
           X1, Y1, X2, Y2) Else                       { Draw c/clkwise arc }
         Arc(Dc, Xc-XRad, Yc-YRad, Xc+XRad, Yc+YRad,
           X2, Y2, X1, Y1);                           { Draw clockwise arc }
     End;
     If (ODc = 0) Then ReleaseDC(HWindow, Dc);        { Release context }
     Dc := ODc;                                       { Reset held context }
   End;
   {$ENDIF}
   {$ENDIF NOT_IMPLEMENTED}
END;

PROCEDURE TView.FilletArc (Xc, Yc: Sw_Integer; Sa, Ea: Real; XRad, YRad, Ht: Sw_Integer;
Colour: Byte);
CONST RadConv  = 57.2957795130823229;                 { Degrees per radian }
{$IFDEF OS_WINDOWS} VAR X1, Y1, X2, Y2, X3, Y3, X4, Y4: Sw_Integer; ODc: hDc; {$ENDIF}
BEGIN
   {$IFDEF NOT_IMPLEMENTED}
   {$IFDEF OS_WINDOWS}
   If (HWindow <> 0) Then Begin                       { Valid window }
     Xc := Xc - FrameSize;
     Yc := Yc - CaptSize;
     ODc := Dc;                                       { Hold device context }
     If (Dc = 0) Then Dc := GetDC(HWindow);           { Create a context }
     Ea := (Ea-Sa);
     While (Ea<-360) Do Ea := Ea+360;                 { One lap only }
     While (Ea>360) Do Ea := Ea-360;                  { One lap only }
     X1 := Round(Sin(Sa/RadConv)*XRad);
     Y1 := -Round(Cos(Sa/RadConv)*YRad);              { Calc 1st values }
     X2 := Round(Sin((Sa+Ea)/RadConv)*XRad);
     Y2 := -Round(Cos((Sa+Ea)/RadConv)*YRad);         { Calc 2nd values }
     X3 := Round(Sin(Sa/RadConv)*(XRad+Ht));
     Y3 := -Round(Cos(Sa/RadConv)*(YRad+Ht));         { Calc 3rd values }
     X4 := Round(Sin((Sa+Ea)/RadConv)*(XRad+Ht));
     Y4 := -Round(Cos((Sa+Ea)/RadConv)*(YRad+Ht));    { Calc 4th values }
     SelectObject(Dc, ColPen[Colour]);                { Pen colour }
     {$IFDEF WIN32}
     MoveToEx(Dc, Xc+X1, Yc+Y1, Nil);                 { Move to first point }
     {$ELSE}
     WinProcs.MoveTo(Dc, Xc+X1, Yc+Y1);               { Move to first point }
     {$ENDIF}
     LineTo(Dc, Xc+X3, Yc+Y3);
     {$IFDEF WIN32}
     MoveToEx(Dc, Xc+X2, Yc+Y2, Nil);
     {$ELSE}
     WinProcs.MoveTo(Dc, Xc+X2, Yc+Y2);
     {$ENDIF}
     LineTo(Dc, Xc+X4, Yc+Y4);
     If (Ea < 0) Then
       Arc(Dc, Xc-XRad-Ht, Yc-YRad-Ht, Xc+XRad+Ht, Yc+YRad+Ht,
         Xc+X1, Yc+Y1, Xc+X2, Yc+Y2) Else
       Arc(Dc, Xc-XRad-Ht, Yc-YRad-Ht, Xc+XRad+Ht, Yc+YRad+Ht,
         Xc+X2, Yc+Y2, Xc+X1, Yc+Y1);                 { Draw arc }
     If (Ea < 0) Then
       Arc(Dc, Xc-XRad, Yc-YRad, Xc+XRad, Yc+YRad,
         Xc+X3, Yc+Y3, Xc+X4, Yc+Y4) Else
       Arc(Dc, Xc-XRad, Yc-YRad, Xc+XRad, Yc+YRad,
         Xc+X4, Yc+Y4, Xc+X3, Yc+Y3);                 { Draw arc }
     If (ODc = 0) Then ReleaseDC(HWindow, Dc);        { Release context }
     Dc := ODc;                                       { Reset held context }
   End;
   {$ENDIF}
   {$ENDIF NOT_IMPLEMENTED}
END;

{--TView--------------------------------------------------------------------}
{  BiColorRectangle -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06May98 LdB  }
{---------------------------------------------------------------------------}
PROCEDURE TView.BicolorRectangle (X1, Y1, X2, Y2: Sw_Integer; Light, Dark: Byte;
Down: Boolean);
VAR UpperLeft, RightDown: Byte;
BEGIN
   If Down Then Begin
     UpperLeft := Dark;                               { Dark upper left }
     RightDown := Light;                              { Light down }
   End Else Begin
     UpperLeft := Light;                              { Light upper left }
     RightDown := Dark;                               { Dark down }
   End;
   GraphLine(X1, Y1, X1, Y2, UpperLeft);              { Draw left side }
   GraphLine(X1, Y1, X2, Y1, UpperLeft);              { Draw top line }
   GraphLine(X1, Y2, X2, Y2, RightDown);              { Draw bottom line }
   GraphLine(X2, Y1, X2, Y2, RightDown);              { Draw right line }
END;

PROCEDURE TView.WriteBuf (X, Y, W, H: Sw_Integer; Var Buf);
VAR I, J, K, L, CW: Sw_Integer; P: PDrawBuffer;
    Tix, Tiy: Sw_Integer; ViewPort: ViewPortType;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfIconised = 0) AND                     { View is not icon}
   (State AND sfExposed <> 0) AND (W > 0) AND (H > 0) { View is exposed }
     then begin
       P := @TDrawBuffer(Buf);                          { Set draw buffer ptr }
       L := 0;                                          { Set buffer position }
     If (X >= 0) AND (Y >= 0) AND ((GOptions and (goGraphical or goGraphView))=0) Then Begin
       X := RawOrigin.X+X*FontWidth;                    { X position }
       Y := RawOrigin.Y+Y*FontHeight;                   { Y position }
     End Else Begin
       X := RawOrigin.X + Abs(X);
       Y := RawOrigin.Y + Abs(Y);
     End;
     If TextModeGFV or UseFixedFont then Begin
       X := X DIV SysFontWidth;
       Y := Y DIV SysFontHeight;
     End;
     GetViewSettings(ViewPort, TextModeGFV);          { Get current viewport }
     If not TextModeGFV and not UseFixedFont then Begin
       X := X - ViewPort.X1;                          { Calc x position }
       Y := Y - ViewPort.Y1;                          { Calc y position }
     End;
     For J := 1 To H Do Begin                         { For each line }
       If (TextModeGFV) or UseFixedFont Then Begin                    { TEXT MODE GFV }
         WriteAbs(X,Y,W,P^[L]);
         Inc(Y);
         Inc(L,W);
       End Else Begin
         K := X;                                        { Reset x position }
         For I := 0 To (W-1) Do Begin                   { For each character }
           Cw := TextWidth(Chr(Lo(P^[L])));             { Width of this char }
           SetFillStyle(SolidFill, Hi(P^[L]) AND
             $F0 SHR 4);                                { Set back colour }
           SetColor(Hi(P^[L]) AND $0F);                 { Set text colour }
           Bar(K, Y, K+Cw, Y+FontHeight-1);             { Clear text backing }
           OutTextXY(K, Y+2, Chr(Lo(P^[L])));           { Write text char }
           Inc(K,Cw);
           Inc(L);                                      { Next character }
         End;
         Y := Y + SysFontHeight;                        { Next line down }
       end;
       DrawScreenBuf;
     End;
   end;
END;

PROCEDURE TView.WriteLine (X, Y, W, H: Sw_Integer; Var Buf);
VAR I, J, K, Cw: Sw_Integer; P: PDrawBuffer;
    Tix, Tiy: Sw_Integer; ViewPort: ViewPortType;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfIconised = 0) AND                     { View is not icon}
   (State AND sfExposed <> 0) AND (W > 0) AND (H > 0) { View is exposed }
     then begin
       P := @TDrawBuffer(Buf);                          { Set draw buffer ptr }
     If (X >= 0) AND (Y >= 0) AND ((GOptions and (goGraphical or goGraphView))=0) Then Begin
       X := RawOrigin.X+X*FontWidth;                    { X position }
       Y := RawOrigin.Y+Y*FontHeight;                   { Y position }
     End Else Begin
       X := RawOrigin.X + Abs(X);
       Y := RawOrigin.Y + Abs(Y);
     End;
     If TextModeGFV or UseFixedFont then Begin
       X := X DIV SysFontWidth;
       Y := Y DIV SysFontHeight;
     End;
     GetViewSettings(ViewPort, TextModeGFV);          { Get current viewport }
     If not TextModeGFV and not UseFixedFont then Begin
       X := X - ViewPort.X1;                          { Calc x position }
       Y := Y - ViewPort.Y1;                          { Calc y position }
     End;
     For J := 1 To H Do Begin                         { For each line }
       If (TextModeGFV) or UseFixedFont Then Begin                    { TEXT MODE GFV }
         WriteAbs(X,Y,W,P^);
         Inc(Y);
       End Else Begin
         K := X;                                        { Reset x position }
         For I := 0 To (W-1) Do Begin                   { For each character }
           Cw := TextWidth(Chr(Lo(P^[I])));             { Width of this char }
           SetFillStyle(SolidFill, Hi(P^[I]) AND
             $F0 SHR 4);                                { Set back colour }
           SetColor(Hi(P^[I]) AND $0F);                 { Set text colour }
           Bar(K, Y, K+Cw, Y+FontHeight-1);             { Clear text backing }
{$IFDEF GRAPH_API}
           SetTextJustify(LeftText,TopText);
{$ENDIF GRAPH_API}
           OutTextXY(K, Y+2, Chr(Lo(P^[I])));           { Write text char }
           Inc(K,Cw);
         End;
         Y := Y + SysFontHeight;                       { Next line down }
       End;
     end;
     If TextModeGFV or UseFixedFont then
       DrawScreenBuf;
   End;
END;

{--TView--------------------------------------------------------------------}
{  MakeLocal -> Platforms DOS/DPMI/WIN/OS2 - Checked 12Sep97 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TView.MakeLocal (Source: TPoint; Var Dest: TPoint);
BEGIN
   If (Options AND ofGFVModeView <> 0) Then Begin     { GFV MODE TVIEW }
     Dest.X := (Source.X-RawOrigin.X) DIV FontWidth;  { Local x value }
     Dest.Y := (Source.Y-RawOrigin.Y) DIV FontHeight; { Local y value }
   End Else Begin                                     { OLD MODE TVIEW }
     Dest.X := Source.X - Origin.X;                   { Local x value }
     Dest.Y := Source.Y - Origin.Y;                   { Local y value }
   End;
END;

{--TView--------------------------------------------------------------------}
{  MakeGlobal -> Platforms DOS/DPMI/WIN/OS2 - Checked 12Sep97 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TView.MakeGlobal (Source: TPoint; Var Dest: TPoint);
BEGIN
   If (Options AND ofGFVModeView <> 0) Then Begin     { GFV MODE TVIEW }
     Dest.X := Source.X*FontWidth + RawOrigin.X;      { Global x value }
     Dest.Y := Source.Y*FontHeight + RawOrigin.Y;     { Global y value }
   End Else Begin                                     { OLD MODE TVIEW }
     Dest.X := Source.X + Origin.X;                   { Global x value }
     Dest.Y := Source.Y + Origin.Y;                   { Global y value }
   End;
END;

PROCEDURE TView.WriteCStr (X, Y: Sw_Integer; Str: String; Color1, Color2 : Byte);
VAR I, J, Fc, Bc, B: Byte; X1, Y1, X2, Y2: Sw_Integer;
    Xw, Yw, TiBuf, Tix, Tiy, Ti: Sw_Integer; ViewPort: ViewPortType;
    Buf : TDrawBuffer;
    FoundSwap : boolean;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfExposed <> 0) AND                     { View is exposed }
   (State AND sfIconised = 0) AND                     { View not iconized }
   (Length(Str) > 0) Then Begin                       { String is valid }

     j:=1;
     repeat
       FoundSwap:=false;
       i:=PosIdx('~',Str,j);
       if i>0 then
        FoundSwap:=true
       else
        i:=Length(Str)+1;

        Fc := GetColor(Color1);                          { Get view color }
        Bc := Fc AND $F0 SHR 4;                          { Calc back colour }
        Fc := Fc AND $0F;                                { Calc text colour }

        If RevCol Then Begin
          B := Bc;
          Bc := Fc;
          Fc := B;
        End;

        If (X >= 0) AND (Y >= 0) AND ((GOptions and goGraphView)=0) Then Begin
          Xw := RawOrigin.X+X*FontWidth;                    { X position }
          Yw := RawOrigin.Y+Y*FontHeight;                   { Y position }
        End Else Begin
          Xw := RawOrigin.X + Abs(X);
          Yw := RawOrigin.Y + Abs(Y);
        End;
        GetViewSettings(ViewPort, TextModeGFV);

       If not TextModeGFV and not UseFixedFont Then Begin              { GRAPHICAL MODE GFV }
         SetFillStyle(SolidFill, Bc);                   { Set fill style }
         Bar(Xw-ViewPort.X1, Yw-ViewPort.Y1,
           Xw-ViewPort.X1+Length(Str)*FontWidth,
           Yw-ViewPort.Y1+FontHeight-1);
         SetColor(Fc);
         OutTextXY(Xw-ViewPort.X1, Yw-ViewPort.Y1+2, Copy(Str,j,i-j));{ Write text char }
       End Else Begin                                   { TEXT MODE GFV }
         Tix := Xw DIV SysFontWidth;
         Tiy := Yw DIV SysFontHeight;
         TiBuf := 0;
         For Ti := j To i-1 Do Begin
           Buf[TiBuf]:=((Fc or (Bc shl 4)) shl 8) or Ord(Str[Ti]);
           inc(TiBuf);
         end;
         WriteAbs(Tix,TiY,i-j,Buf);
       End;

      { increase position on screen }
      If (X >= 0) AND (Y >= 0) AND ((GOptions and goGraphView)=0) Then
        inc(X,(i-j))
      else if X>0 then
        inc(X,(i-j)*FontWidth)
      else
        dec(X,(i-j)*FontWidth);
      { Swap colors }
      if FoundSwap then
       begin
         { Swap color1 and color2 }
         B := Color1;
         Color1 := Color2;
         Color2 := B;
         { increase position in string }
         j:=i+1;
         { we're at the last char }
         if (j>length(Str)) then
          break;
       end;

     until not FoundSwap;
     If TextModeGFV then
       DrawScreenBuf;
   End;
END;

PROCEDURE TView.WriteStr (X, Y: Sw_Integer; Str: String; Color: Byte);
VAR Fc, Bc, B: Byte; X1, Y1, X2, Y2: Sw_Integer;
    Tix, Tiy, Ti: Sw_Integer; ViewPort: ViewPortType;
    Buf : TDrawBuffer;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View is visible }
   (State AND sfExposed <> 0) AND                     { View is exposed }
   (State AND sfIconised = 0) AND                     { View not iconized }
   (Length(Str) > 0) Then Begin                       { String is valid }

     Fc := GetColor(Color);                           { Get view color }
     Bc := Fc AND $F0 SHR 4;                          { Calc back colour }
     Fc := Fc AND $0F;                                { Calc text colour }

     If RevCol Then Begin
       B := Bc;
       Bc := Fc;
       Fc := B;
     End;

     If (X >= 0) AND (Y >= 0) AND ((GOptions and goGraphView)=0) Then Begin
       X := RawOrigin.X+X*FontWidth;                    { X position }
       Y := RawOrigin.Y+Y*FontHeight;                   { Y position }
     End Else Begin
       X := RawOrigin.X + Abs(X);
       Y := RawOrigin.Y + Abs(Y);
     End;
     GetViewSettings(ViewPort, TextModeGFV);
     If not TextModeGFV and not UseFixedFont Then Begin              { GRAPHICAL MODE GFV }
       SetFillStyle(SolidFill, Bc);                   { Set fill style }
       Bar(X-ViewPort.X1, Y-ViewPort.Y1,
         X-ViewPort.X1+Length(Str)*FontWidth,
         Y-ViewPort.Y1+FontHeight-1);
       SetColor(Fc);
       OutTextXY(X-ViewPort.X1, Y-ViewPort.Y1+2, Str);{ Write text char }
     End Else Begin                                   { TEXT MODE GFV }
       Tix := X DIV SysFontWidth;
       Tiy := Y DIV SysFontHeight;
       For Ti := 1 To length(Str) Do Begin
         Buf[Ti-1]:=(GetColor(Color) shl 8) or Ord(Str[Ti]);
       end;
       WriteAbs(Tix,TiY,Length(Str),Buf);
     End;
     If TextModeGFV or UseFixedFont then
       DrawScreenBuf;
   End;
END;

PROCEDURE TView.WriteChar (X, Y: Sw_Integer; C: Char; Color: Byte;
  Count: Sw_Integer);
VAR Fc, Bc, B: Byte; I, Ti, Tix, Tiy: Sw_Integer; Col: Word; S: String; ViewPort: ViewPortType;
    Buf : TDrawBuffer;
BEGIN
   If (State AND sfVisible <> 0) AND                  { View visible }
   (State AND sfExposed <> 0) Then Begin              { View exposed }
     GetViewSettings(ViewPort, TextModeGFV);
     Col := GetColor(Color);                          { Get view color }
     Fc := Col AND $0F;                               { Foreground colour }
     Bc := Col AND $F0 SHR 4;                         { Background colour }

     If RevCol Then Begin
       B := Bc;
       Bc := Fc;
       Fc := B;
     End;

     If (X >= 0) AND (Y >= 0) AND ((GOptions and goGraphView)=0) Then Begin
       X := RawOrigin.X+X*FontWidth;                    { X position }
       Y := RawOrigin.Y+Y*FontHeight;                   { Y position }
     End Else Begin
       X := RawOrigin.X + Abs(X);
       Y := RawOrigin.Y + Abs(Y);
     End;
     FillChar(S[1], 255, C);                          { Fill the string }
     While (Count>0) Do Begin
       If (Count>Size.X) Then I := Size.X Else I := Count;  { Size to make }
       S[0] := Chr(I);                                { Set string length }
       If not TextModeGFV and not UseFixedFont Then Begin            { GRAPHICAL MODE GFV }
         SetFillStyle(SolidFill, Bc);                 { Set fill style }
         Bar(X-ViewPort.X1, Y-ViewPort.Y1,
           X-ViewPort.X1+I*FontWidth,
           Y-ViewPort.Y1+FontHeight-1);
         SetColor(Fc);
         OutTextXY(X-ViewPort.X1, Y-ViewPort.Y1+2, S);  { Write text char }
       End Else Begin                                 { TEXT MODE GFV }
         Tix := X DIV SysFontWidth;
         Tiy := Y DIV SysFontHeight;
         For Ti := 1 To I Do Begin
           Buf[Ti-1]:=(GetColor(Color) shl 8) or Ord(S[Ti]);
         End;
         WriteAbs(TiX,TiY,Length(S),Buf);
       End;
       Count := Count - I;                            { Subtract count }
       If TextModeGFV or UseFixedFont then
         X := X + I                                    { Move x position }
       else
         X := X + I*FontWidth;                          { Move x position }
     End;
     If TextModeGFV or UseFixedFont then
       DrawScreenBuf;
   End;
END;

PROCEDURE TView.WriteAbs(X, Y, L : Sw_Integer; Var Buf);
VAR
  P: PGroup;
  PrevP,PP : PView;
  CurOrigin : TPoint;
  I,XI : longint;
  ViewPort : ViewPortType;
  Skip : boolean;
BEGIN
{$ifdef DEBUG}
   if WriteDebugInfo then
     Begin
       Writeln(stderr,'TView(',hexstr(longint(@self),8),')');
       Writeln(stderr,'Object Type(',hexstr(plongint(@self)^,8),')');
       Writeln(stderr,'WriteAbs(',X,',',Y,',',L,',',hexstr(longint(@Buf),8),')');
     End;
{$endif DEBUG}
  { Direct wrong method }
  GetViewSettings(ViewPort, TextModeGFV or UseFixedFont);          { Get set viewport }
  { Pedestrian character method }
  { Must be in area }
  If (X+L<ViewPort.X1) OR (Y<ViewPort.Y1) OR
     (X>=ViewPort.X2) OR (Y>=ViewPort.Y2) Then
     Exit;
  For I:=0 to L-1 do Begin
    P:=Owner;
    PrevP :=@Self;
    XI:=X+I;
    { Must be in area }
    If (XI<ViewPort.X1) OR
       (XI>=ViewPort.X2) Then
      Continue;
    Skip:=false;
    While Assigned(P) do Begin
      { If parent not visible or
        position outside parent's limit then skip }
      if not assigned(P^.Buffer) AND
         (((P^.State AND sfVisible) = 0) OR
         (XI<P^.Origin.X) OR (XI>=P^.Origin.X+P^.Size.X) OR
         (Y<P^.Origin.Y) OR (Y>=P^.Origin.Y+P^.Size.Y)) then
        Begin
          Skip:=true;
          Break;
        End;
      { Here we must check if X,Y is exposed for this view }
      PP:=P^.Last;
      { move to first }
      If Assigned(PP) then
        PP:=PP^.Next;
      While Assigned(PP) and (PP<>P^.Last) and (PP<>PrevP) do Begin
        { If position is owned by another view that is before self
         then skip }
        If ((PP^.State AND sfVisible) <> 0) AND
           (XI>=PP^.Origin.X) AND
           (XI<PP^.Origin.X+PP^.Size.X) AND
           (Y>=PP^.Origin.Y) AND
           (Y<PP^.Origin.Y+PP^.Size.Y) then
          Begin
            Skip:=true;
            break;
          End;
        PP:=PP^.Next;
      End;

      If Not Skip and Assigned(P^.Buffer) then Begin
        begin
          P^.Buffer^[(Y-P^.Origin.Y)*P^.size.X+(XI-P^.Origin.X)]:=TDrawBuffer(Buf)[I];
{$IFDEF GRAPH_API}
          If (pointer(P^.Buffer)=pointer(VideoBuf)) and (SpVideoBuf^[Y*TextScreenWidth+XI]=EmptyVideoBufCell) then
            OldVideoBuf^[Y*TextScreenWidth+XI]:=0;
{$ENDIF GRAPH_API}
        end;
      End;
      PrevP:=P;
      If Skip then
        P:=Nil
      else
        P:=P^.Owner;
    End;
  End;
END;

{define DirectWriteShadow}
PROCEDURE TView.WriteShadow(X1, Y1, X2, Y2 : Sw_Integer);
VAR
  P: PGroup;
  PrevP,PP : PView;
  CurOrigin : TPoint;
  I,J : longint;
  Col,OrigCol : byte;
  B : Word;
  ViewPort : ViewPortType;
  Skip : boolean;
BEGIN
  GetViewSettings(ViewPort, TextModeGFV);          { Get set viewport }
  { Pedestrian character method }
  { Must be in area }
  {If (X+L<ViewPort.X1) OR (Y<ViewPort.Y1) OR
     (X>=ViewPort.X2) OR (Y>=ViewPort.Y2) Then
     Exit;}
  If Y2>ScreenHeight then
    Y2:=ScreenHeight;
  If X2>ScreenWidth then
    X2:=ScreenWidth;
  For J:=Y1 to Y2-1 do Begin
    For i:=X1 to X2-1 do Begin
    P:=Owner;
    PrevP :=@Self;
    { Must be in area
    If (XI<ViewPort.X1) OR
       (XI>=ViewPort.X2) Then
      Continue;    }
    Skip:=false;
    While Assigned(P) do Begin
      if not assigned(P^.Buffer) AND
         (((P^.State AND sfVisible) = 0) OR
         (I<P^.Origin.X) OR (I>=P^.Origin.X+P^.Size.X) OR
         (J<P^.Origin.Y) OR (J>=P^.Origin.Y+P^.Size.Y)) then
        Begin
          Skip:=true;
          Break;
        End;
      { Here we must check if X,Y is exposed for this view }
      PP:=P^.Last;
      { move to first }
      If Assigned(PP) then
        PP:=PP^.Next;
      While Assigned(PP) and (PP<>P^.Last) and (PP<>PrevP) do Begin
        If ((PP^.State AND sfVisible) <> 0) AND
           (I>=PP^.Origin.X) AND
           (I<PP^.Origin.X+PP^.Size.X) AND
           (J>=PP^.Origin.Y) AND
           (J<PP^.Origin.Y+PP^.Size.Y) then
          Begin
            Skip:=true;
            Break;
          End;
        PP:=PP^.Next;
      End;

      If not Skip and Assigned(P^.Buffer) then Begin
        B:=P^.Buffer^[(J-P^.Origin.Y)*P^.size.X+(I-P^.Origin.X)];
        OrigCol:=B shr 8;
        if OrigCol and $F >= 8 then
          Col:=OrigCol and $7
        else
          Col:=0;
        P^.Buffer^[(J-P^.Origin.Y)*P^.size.X+(I-P^.Origin.X)]:=  (col shl 8) or (B and $FF);
      End;
      PrevP:=P;
      If Skip then
        P:=Nil
      else
        P:=P^.Owner;
    End;
  End;
  End;
END;

PROCEDURE TView.DragView (Event: TEvent; Mode: Byte; Var Limits: TRect;
  MinSize, MaxSize: TPoint);
VAR PState: Word; Mouse, Q, R, P, S, Op1, Op2: TPoint; SaveBounds: TRect;

   PROCEDURE MoveGrow (P, S: TPoint);
   VAR R: TRect;
   BEGIN
     S.X := Min(Max(S.X, MinSize.X), MaxSize.X);      { Minimum S.X value }
     S.Y := Min(Max(S.Y, MinSize.Y), MaxSize.Y);      { Minimum S.Y value }
     P.X := Min(Max(P.X, Limits.A.X - S.X + 1),
       Limits.B.X - 1);                               { Minimum P.X value }
     P.Y := Min(Max(P.Y, Limits.A.Y - S.Y + 1),
       Limits.B.Y - 1);                               { Mimimum P.Y value }
     If (Mode AND dmLimitLoX <> 0) Then
       P.X := Max(P.X, Limits.A.X);                   { Left side move }
     If (Mode AND dmLimitLoY <> 0) Then
       P.Y := Max(P.Y, Limits.A.Y);                   { Top side move }
     If (Mode AND dmLimitHiX <> 0) Then
       P.X := Min(P.X, Limits.B.X - S.X);             { Right side move }
     If (Mode AND dmLimitHiY <> 0) Then
       P.Y := Min(P.Y, Limits.B.Y - S.Y);             { Bottom side move }
     R.Assign(P.X, P.Y, P.X + S.X, P.Y + S.Y);        { Assign area }
     Locate(R);                                       { Locate view }
   END;

   PROCEDURE Change (DX, DY: Sw_Integer);
   BEGIN
     If (Mode AND dmDragMove <> 0) AND
     (GetShiftState AND $03 = 0) Then Begin
       Inc(P.X, DX); Inc(P.Y, DY);                    { Adjust values }
     End Else If (Mode AND dmDragGrow <> 0) AND
     (GetShiftState AND $03 <> 0) Then Begin
       Inc(S.X, DX); Inc(S.Y, DY);                    { Adjust values }
     End;
   END;

   PROCEDURE Update (X, Y: Sw_Integer);
   BEGIN
     If (Mode AND dmDragMove <> 0) Then Begin
       P.X := X; P.Y := Y;                            { Adjust values }
     End;
   END;

BEGIN
   SetState(sfDragging, True);                        { Set drag state }
   If (Event.What = evMouseDown) Then Begin           { Mouse down event }
     Q.X := Event.Where.X DIV FontWidth - Origin.X;   { Offset mouse x origin }
     Q.Y := Event.Where.Y DIV FontHeight - Origin.Y;  { Offset mouse y origin }
     Op1.X := RawOrigin.X; Op1.Y := RawOrigin.Y;      { Hold origin point }
     Op2.X := RawOrigin.X+RawSize.X;                  { Right side x value }
     Op2.Y := RawOrigin.Y+RawSize.Y;                  { Right side y value }
     PState := State;                                 { Hold current state }
     if not TextModeGFV then
       State := State AND NOT sfVisible;                { Temp not visible }
     HideMouseCursor;                                 { Hide the mouse }
     SetWriteMode(XORPut, TextModeGFV);
     GraphRectangle(0, 0, RawSize.X, RawSize.Y, Red);
     SetWriteMode(NormalPut, TextModeGFV);
     ShowMouseCursor;                                 { Show the mouse }
     Repeat
       Mouse.X := Round(Event.Where.X/FontWidth)-Q.X; { New x origin point }
       Mouse.Y := Round(Event.Where.Y/FontHeight)-Q.Y;{ New y origin point }
       If (Mode AND dmDragMove<>0) Then Begin
         If (Owner<>Nil) Then Begin
           Dec(Mouse.X, Owner^.Origin.X);             { Sub owner x origin }
           Dec(Mouse.Y, Owner^.Origin.Y);             { Sub owner y origin }
         End;
         R := Mouse; Mouse := Size;                   { Exchange values }
       End Else Begin
         R := Origin;                                 { Start at origin }
         If (Owner<>Nil) Then Begin
           Dec(R.X, Owner^.Origin.X);                 { Sub owner x origin }
           Dec(R.Y, Owner^.Origin.Y);                 { Sub owner y origin }
         End;
         Mouse.X := Mouse.X+Q.X-Origin.X;
         Mouse.Y := Mouse.Y+Q.Y-Origin.Y;
       End;
       HideMouseCursor;                               { Hide the mouse }
       if not TextModeGFV then
         begin
           SetWriteMode(XORPut, TextModeGFV);
           GraphRectangle(0, 0, RawSize.X, RawSize.Y, Red);
           SetWriteMode(NormalPut, TextModeGFV);
           MoveGrow(R, Mouse);                            { Resize the view }
           SetWriteMode(XORPut, TextModeGFV);
           GraphRectangle(0, 0, RawSize.X, RawSize.Y, Red);
           SetWriteMode(NormalPut, TextModeGFV);
         end
       else
         begin
           MoveGrow(R, Mouse);                            { Resize the view }
         end;
       ShowMouseCursor;                               { Show the mouse }
     Until NOT MouseEvent(Event, evMouseMove);        { Finished moving }
     State := PState;                                 { Restore view state }
     If (Owner<>Nil) Then
       Owner^.ReDrawArea(Op1.X, Op1.Y, Op2.X, Op2.Y); { Redraw old area }
     SetState(sfDragging, False);                     { Clr dragging flag }
     DrawView;                                        { Now redraw the view }
   End Else Begin
     GetBounds(SaveBounds);                           { Get current bounds }
     Repeat
       P := Origin; S := Size;                        { Set values }
       If Assigned(Owner) then
         Begin
           Dec(P.X,Owner^.Origin.X);
           Dec(P.Y,Owner^.Origin.Y);
         End;
       KeyEvent(Event);                               { Get key event }
       Case Event.KeyCode AND $FF00 Of
         kbLeft: Change(-1, 0);                       { Move left }
         kbRight: Change(1, 0);                       { Move right }
         kbUp: Change(0, -1);                         { Move up }
         kbDown: Change(0, 1);                        { Move down }
         kbCtrlLeft: Change(-8, 0);
         kbCtrlRight: Change(8, 0);
         kbHome: Update(Limits.A.X, P.Y);
         kbEnd: Update(Limits.B.X - S.X, P.Y);
         kbPgUp: Update(P.X, Limits.A.Y);
         kbPgDn: Update(P.X, Limits.B.Y - S.Y);
       End;
       MoveGrow(P, S);                                { Now move the view }
     Until (Event.KeyCode = kbEnter) OR
     (Event.KeyCode = kbEsc);
     If (Event.KeyCode=kbEsc) Then Locate(SaveBounds);{ Restore original }
   End;
   SetState(sfDragging, False);                       { Clr dragging flag }
END;

FUNCTION TView.FontWidth: Sw_Integer;
BEGIN
   FontWidth := SysFontWidth;
END;

FUNCTION TView.FontHeight: Sw_Integer;
BEGIN
   FontHeight := SysFontHeight;
END;



{***************************************************************************}
{                         TScroller OBJECT METHODS                          }
{***************************************************************************}

PROCEDURE TScroller.ScrollDraw;
VAR D: TPoint;
BEGIN
   If (HScrollBar<>Nil) Then D.X := HScrollBar^.Value
     Else D.X := 0;                                   { Horz scroll value }
   If (VScrollBar<>Nil) Then D.Y := VScrollBar^.Value
     Else D.Y := 0;                                   { Vert scroll value }
   If (D.X<>Delta.X) OR (D.Y<>Delta.Y) Then Begin     { View has moved }
     SetCursor(Cursor.X+Delta.X-D.X,
       Cursor.Y+Delta.Y-D.Y);                         { Move the cursor }
     Delta := D;                                      { Set new delta }
     If (DrawLock<>0) Then DrawFlag := True           { Draw will need draw }
       Else DrawView;                                 { Redraw the view }
   End;
END;

PROCEDURE TScroller.SetLimit (X, Y: Sw_Integer);
VAR PState: Word;
BEGIN
   Limit.X := X;                                      { Hold x limit }
   Limit.Y := Y;                                      { Hold y limit }
   Inc(DrawLock);                                     { Set draw lock }
   If (HScrollBar<>Nil) Then Begin
     PState := HScrollBar^.State;                     { Hold bar state }
     HScrollBar^.State := PState AND NOT sfVisible;   { Temp not visible }
     HScrollBar^.SetParams(HScrollBar^.Value, 0,
       X-Size.X, Size.X-1, HScrollBar^.ArStep);       { Set horz scrollbar }
     HScrollBar^.State := PState;                     { Restore bar state }
   End;
   If (VScrollBar<>Nil) Then Begin
     PState := VScrollBar^.State;                     { Hold bar state }
     VScrollBar^.State := PState AND NOT sfVisible;   { Temp not visible }
     VScrollBar^.SetParams(VScrollBar^.Value, 0,
       Y-Size.Y, Size.Y-1, VScrollBar^.ArStep);       { Set vert scrollbar }
     VScrollBar^.State := PState;                     { Restore bar state }
   End;
   Dec(DrawLock);                                     { Release draw lock }
   CheckDraw;                                         { Check need to draw }
END;

{***************************************************************************}
{                      TScroller OBJECT PRIVATE METHODS                     }
{***************************************************************************}
PROCEDURE TScroller.CheckDraw;
BEGIN
   If (DrawLock = 0) AND DrawFlag Then Begin          { Clear & draw needed }
     DrawFlag := False;                               { Clear draw flag }
     DrawView;                                        { Draw now }
   End;
END;



{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TGroup OBJECT METHODS                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}




{--TGroup-------------------------------------------------------------------}
{  Lock -> Platforms DOS/DPMI/WIN/OS2 - Checked 23Sep97 LdB                 }
{---------------------------------------------------------------------------}
{$ifndef  NoLock}
{$define UseLock}
{$endif ndef  NoLock}
PROCEDURE TGroup.Lock;
BEGIN
{$ifdef UseLock}
   {If (Buffer <> Nil) OR (LockFlag <> 0)
     Then} Inc(LockFlag);                              { Increment count }
{$endif UseLock}
END;

{--TGroup-------------------------------------------------------------------}
{  UnLock -> Platforms DOS/DPMI/WIN/OS2 - Checked 23Sep97 LdB               }
{---------------------------------------------------------------------------}
PROCEDURE TGroup.Unlock;
BEGIN
{$ifdef UseLock}
   If (LockFlag <> 0) Then Begin
     Dec(LockFlag);                                   { Decrement count }
     If (LockFlag = 0) Then DrawView;                 { Lock release draw }
   End;
{$endif UseLock}
END;


PROCEDURE TWindow.DrawBorder;
const
  LargeC:array[boolean] of char=('^',#24);
  RestoreC:array[boolean] of char=('|',#18);
  ClickC:array[boolean] of char=('*',#15);
VAR Fc, Bc: Byte; X, Y: Sw_Integer; S: String;
    ViewPort: ViewPortType;
    I : Sw_Integer;
    LeftUpCorner,
    RightUpCorner,
    HorizontalBar,
    VerticalBar,
    LeftLowCorner,
    RightLowCorner,C : Char;
    Color : Byte;
    Focused : Boolean;
    Min, Max: TPoint;
    NP : PView;

BEGIN
   Fc := GetColor(2) AND $0F;                        { Foreground colour }
   Bc := (GetColor(2) AND $70) SHR 4;                { Background colour }
   If TextModeGFV then
     Y:=0 else
     begin
       Y:=0;
       (* If (Options AND ofFramed<>0) Then Y := 1
         Else Y := 0;                                     { Initial value }
       If (GOptions AND goThickFramed<>0) Then Inc(Y, 3); { Adjust position }
       *)
     end;
   ClearArea(0, Y, RawSize.X, Y+FontHeight-1, Bc);      { Clear background }
   If not TextModeGFV then
     Inherited DrawBorder
   Else Begin                                     { TEXT GFV MODE }
     {Focused:=(State AND (sfSelected + sfModal)<>0);
     if Assigned(Owner) then
       Focused := Focused AND (@Self = Owner^.Current); }
     Focused:=(State AND sfActive)<>0;
     If not Focused or (GOptions AND goThickFramed = 0) then
       begin
         LeftUpCorner:='з';
         RightUpCorner:='©';
         HorizontalBar:='д';
         VerticalBar:='Ё';
         LeftLowCorner:='ю';
         RightLowCorner:='ы';
       end
     else
       begin
         LeftUpCorner:='и';
         RightUpCorner:='╩';
         HorizontalBar:='м';
         VerticalBar:='╨';
         LeftLowCorner:='х';
         RightLowCorner:='╪';
       end;
     if (State AND sfDragging)<>0 then
       Color := 5
     else if Focused then
       Color := 2
     else
       Color := 1;
     WriteChar(0,0,LeftUpCorner,Color,1);
     WriteChar(1,0,HorizontalBar,Color,Size.X-2);
     WriteChar(Size.X-1,0,RightUpcorner,Color,1);
     For i:=1 to Size.Y -1 do
       begin
         WriteChar(0,i,VerticalBar,Color,1);
         WriteChar(Size.X-1,i,VerticalBar,Color,1);
       end;
     WriteChar(0,Size.Y-1,LeftLowCorner,Color,1);
     WriteChar(1,Size.Y-1,HorizontalBar,Color,Size.X-2);
     WriteChar(Size.X-1,Size.Y-1,RightLowCorner,Color,1);
   End;
   If not TextModeGFV then
     GOptions := GOptions OR goGraphView;             { Graphics co-ords mode }
   If (Title<>Nil) AND (GOptions AND goTitled<>0)
   Then Begin                                         { View has a title }
     GetViewSettings(ViewPort, TextModeGFV);
     X := (RawSize.X DIV 2);                          { Half way point }
     X := X - ((Length(Title^)+2)*FontWidth) DIV 2;       { Calc start point }
     If (TextModeGFV <> TRUE) Then Begin              { GRAPHICS MODE GFV }
      (* SetColor(Fc);
       OutTextXY(RawOrigin.X+X-ViewPort.X1,
         RawOrigin.Y+Y+1-ViewPort.Y1+2, ' '+Title^+' ');      { Write the title }*)
         WriteStr(X,Y+1,' '+Title^+' ',Color);

     End Else Begin                                   { LEON??? }
       WriteStr(X div SysFontWidth,0,' '+Title^+' ',Color);
     End;
   End;
   If (Number>0) AND (Number<10) Then Begin           { Valid number }
     Str(Number, S);                                  { Make number string }
     If (Flags and wfZoom)<>0 then
       I:=7
     else
       I:=3;
     If (TextModeGFV <> True) Then Begin              { GRAPHICS MODE GFV }
       (* SetColor(GetColor(2) AND $0F);
       OutTextXY(RawOrigin.X+RawSize.X-I*FontWidth-ViewPort.X1,
         RawOrigin.Y+Y+1-ViewPort.Y1+2, S);           { Write number } *)
       WriteCStr(RawSize.X-I*FontWidth,Y+1,S,1,Color);
     End Else Begin                                   { LEON ????? }
       WriteCStr(Size.X-I,0,S,1,Color);
     End;
   End;
   If Focused and (Flags AND wfClose<>0) Then Begin   { Close icon request }
     If (TextModeGFV <> True) Then Begin              { GRAPHICS MODE GFV }
       (*SetColor(Fc);
       OutTextXY(RawOrigin.X+Y+FontWidth-ViewPort.X1,
         RawOrigin.Y+Y+1-ViewPort.Y1+2, '[*]');       { Write close icon } *)
       WriteCStr(2*FontWidth,Y+1,'[~'+ClickC[LowAscii]+'~]', 2, 3);
     End Else Begin                                   { LEON??? }
       WriteCStr(2,0,'[~'+ClickC[LowAscii]+'~]', 2, 3);
     End;
   End;
   If Focused and (Flags AND wfZoom<>0) Then Begin
     if assigned(Owner) and
        (Size.X=Owner^.Size.X) and (Size.Y=Owner^.Size.Y) then
      C:=RestoreC[LowAscii]
     else
      C:=LargeC[LowAscii];
     If (TextModeGFV <> True) Then Begin              { GRAPHICS MODE GFV }
       (* SetColor(GetColor(2) AND $0F);
       OutTextXY(RawOrigin.X+RawSize.X-4*FontWidth-Y-ViewPort.X1,
         RawOrigin.Y+Y+1-ViewPort.Y1+2, '['+C+']'); { Write zoom icon } *)
       WriteCStr(RawSize.X-5*FontWidth,Y+1,'[~'+C+'~]', 2, 3);
       WriteCStr(RawSize.X-2*FontWidth,RawSize.Y-FontHeight+Y+1,'~ды~',2, 3);
     End Else Begin                                   { LEON??? }
       WriteCStr(Size.X-5,0,'[~'+C+'~]', 2, 3);
       WriteCStr(Size.X-2,Size.Y-1,'~ды~',2, 3);
     End;
   End;
   If not TextModeGFV then
     begin
       BiColorRectangle(Y+1, Y+1, RawSize.X-Y-1, Y+FontHeight,
         White, DarkGray, False);                         { Draw 3d effect }
       BiColorRectangle(Y+1, Y+1, RawSize.X-Y-2, Y+FontHeight-1,
         White, DarkGray, False);                         { Draw 3d effect }
       GOptions := GOptions AND NOT goGraphView;        { Return to normal mode }
     end;
   { Ensure that the scrollers are repainted }
   NP:=Last;
   while assigned(NP) do
     begin
       If (NP^.Origin.X<=Origin.X) or
          (NP^.Origin.Y<=Origin.Y) or
          (NP^.Origin.X>=Origin.X+Size.X) or
          (NP^.Origin.Y>=Origin.Y+Size.Y) then
         begin
           NP^.ReDrawArea(RawOrigin.X,RawOrigin.Y,
             RawOrigin.X+FontWidth*Size.X,RawOrigin.Y+FontHeight);
           NP^.ReDrawArea(RawOrigin.X,RawOrigin.Y+FontHeight*(Size.Y-1),
             RawOrigin.X+FontWidth*Size.X,RawOrigin.Y+FontHeight*Size.Y);
           NP^.ReDrawArea(RawOrigin.X,RawOrigin.Y+FontHeight,
             RawOrigin.X+FontWidth,RawOrigin.Y+FontHeight*(Size.Y-1));
           NP^.ReDrawArea(RawOrigin.X+FontWidth*(Size.X-1),RawOrigin.Y+FontHeight,
             RawOrigin.X+FontWidth*Size.X,RawOrigin.Y+FontHeight*(Size.Y-1));
         end;
       NP:=NP^.Prevview;
     end;
END;


{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         WINDOW MESSAGE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  Message -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Sep97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION Message (Receiver: PView; What, Command: Word;
  InfoPtr: Pointer): Pointer;
VAR Event: TEvent;
BEGIN
   Message := Nil;                                    { Preset nil }
   If (Receiver <> Nil) Then Begin                    { Valid receiver }
     Event.What := What;                              { Set what }
     Event.Command := Command;                        { Set command }
     Event.Id := 0;                                   { Zero id field }
     Event.Data := 0;                                 { Zero data field }
     Event.InfoPtr := InfoPtr;                        { Set info ptr }
     Receiver^.HandleEvent(Event);                    { Pass to handler }
     If (Event.What = evNothing) Then
       Message := Event.InfoPtr;                      { Return handler }
   End;
END;

{---------------------------------------------------------------------------}
{  NewMessage -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19Sep97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION NewMessage (P: PView; What, Command: Word; Id: Sw_Integer;
  Data: Real; InfoPtr: Pointer): Pointer;
VAR Event: TEvent;
BEGIN
   NewMessage := Nil;                                 { Preset failure }
   If (P <> Nil) Then Begin
     Event.What := What;                              { Set what }
     Event.Command := Command;                        { Set event command }
     Event.Id := Id;                                  { Set up Id }
     Event.Data := Data;                              { Set up data }
     Event.InfoPtr := InfoPtr;                        { Set up event ptr }
     P^.HandleEvent(Event);                           { Send to view }
     If (Event.What = evNothing) Then
       NewMessage := Event.InfoPtr;                   { Return handler }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                            NEW VIEW ROUTINES                              }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  CreateIdScrollBar -> Platforms DOS/DPMI/WIN/NT/OS2 - Checked 22May97 LdB }
{---------------------------------------------------------------------------}
FUNCTION CreateIdScrollBar (X, Y, Size, Id: Sw_Integer; Horz: Boolean): PScrollBar;
VAR R: TRect; P: PScrollBar;
BEGIN
   If Horz Then R.Assign(X, Y, X+Size, Y+1) Else      { Horizontal bar }
     R.Assign(X, Y, X+1, Y+Size);                     { Vertical bar }
   P := New(PScrollBar, Init(R));                     { Create scrollbar }
   If (P <> Nil) Then Begin
     P^.Id := Id;                                     { Set scrollbar id }
     P^.Options := P^.Options OR ofPostProcess;       { Set post processing }
   End;
   CreateIdScrollBar := P;                            { Return scrollbar }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                      OBJECT REGISTRATION PROCEDURES                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  RegisterViews -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May97 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE RegisterViews;
BEGIN
   RegisterType(RView);                               { Register views }
   RegisterType(RFrame);                              { Register frame }
   RegisterType(RScrollBar);                          { Register scrollbar }
   RegisterType(RScroller);                           { Register scroller }
   RegisterType(RListViewer);                         { Register listview }
   RegisterType(RGroup);                              { Register group }
   RegisterType(RWindow);                             { Register window }
END;

END.

{
 $Log$
 Revision 1.38  2002-09-12 12:03:13  pierre
  * handle unix mouse differently as it uses video buffer

 Revision 1.37  2002/09/09 08:06:33  pierre
  * remove other warnings

 Revision 1.36  2002/09/09 08:04:06  pierre
  * remove all warnings about far

 Revision 1.35  2002/09/07 15:06:38  peter
   * old logs removed and tabs fixed

 Revision 1.34  2002/08/22 13:40:49  pierre
  * several graphic mode improovements

 Revision 1.33  2002/06/10 13:47:38  pierre
  * correct the check for drawing a double line border

 Revision 1.32  2002/06/10 12:39:43  pierre
  * always call ScrooDraw if TscrollBar.value filed is changed

 Revision 1.31  2002/06/06 06:42:21  pierre
  + use gfvgraph cursor functions for UseFixedFont

 Revision 1.30  2002/05/31 13:36:42  pierre
  * avoid SIGSEGV in owner^.close code by removing virtual attribute to ForEach method

 Revision 1.29  2002/05/31 12:40:48  pierre
  * several fixes + graph enhancements

 Revision 1.28  2002/05/30 22:28:33  pierre
  * tried to get a faster RedrawArea method

 Revision 1.27  2002/05/30 14:53:54  pierre
  * try to follow TV better

 Revision 1.26  2002/05/29 19:36:52  pierre
  * fix UseFixedFont related code

 Revision 1.25  2002/05/28 19:15:16  pierre
  * adapt to new GraphUpdateScreen function

 Revision 1.24  2002/05/25 23:30:47  pierre
  * partly fix the scrollbar behavior

 Revision 1.23  2002/05/24 13:16:11  pierre
  * add window number and resize handle

 Revision 1.22  2002/05/23 10:27:12  pierre
  * avoid problems with shadows when moving or resizing a window

 Revision 1.21  2002/05/23 09:06:01  pierre
  * force views to have raworigin multiples of cell width and height

 Revision 1.20  2002/05/21 11:47:36  pierre
  * avoid infinite recursions in graphic mode

 Revision 1.19  2002/05/16 21:23:34  pierre
  * fix some display problems

}
