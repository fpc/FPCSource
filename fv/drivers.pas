{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{    System independent clone of DRIVERS.PAS               }
{                                                          }
{    Interface Copyright (c) 1992 Borland International    }
{                                                          }
{    Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer  }
{    ldeboer@attglobal.net  - primary e-mail addr          }
{    ldeboer@starwon.com.au - backup e-mail addr           }
{                                                          }
{    Original FormatStr kindly donated by Marco Schmidt    }
{                                                          }
{    Mouse callback hook under FPC with kind assistance of }
{    Pierre Muller, Gertjan Schouten & Florian Klaempfl.   }
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
{                 - Speed Pascal 1.0+       (32 Bit)       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     26 Jul 96   First DOS/DPMI platform release    }
{  1.10     18 Nov 97   Windows conversion added.          }
{  1.20     29 Aug 97   Platform.inc sort added.           }
{  1.30     10 Jun 98   Virtual pascal 2.0 code added.     }
{  1.40     13 Jul 98   Added FormatStr by Marco Schmidt.  }
{  1.50     14 Jul 98   Fixed width = 0 in FormatStr.      }
{  1.60     13 Aug 98   Complete rewrite of FormatStr.     }
{  1.70     10 Sep 98   Added mouse int hook for FPC.      }
{  1.80     10 Sep 98   Checks run & commenting added.     }
{  1.90     15 Oct 98   Fixed for FPC version 0.998        }
{  1.91     18 Feb 99   Added PrintStr functions           }
{  1.92     18 Feb 99   FormatStr literal '%' fix added    }
{  1.93     10 Jul 99   Sybil 2.0 code added               }
{  1.94     15 Jul 99   Fixed for FPC 0.9912 release       }
{  1.95     26 Jul 99   Windows..Scales to GFV system font }
{  1.96     30 Jul 99   Fixed Ctrl+F1..F10 in GetKeyEvent  }
{  1.97     07 Sep 99   InitEvent, DoneEvent fixed for OS2 }
{  1.98     09 Sep 99   GetMouseEvent fixed for OS2.       }
{  1.99     03 Nov 99   FPC windows support added.         }
{  2.00     26 Nov 99   Graphics stuff moved to GFVGraph   }
{**********************************************************}

UNIT Drivers;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC} { FPC doesn't support these switches }
  {$F+} { Force far calls - Used because of the ShowMouseProc etc... }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O-} { This unit may >>> NOT <<< be overlaid }
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
         Windows,                                     { Standard unit }
       {$ELSE}                                        { OTHER COMPILERS }
         WinTypes, WinProcs,                          { Standard units }
       {$ENDIF}
       {$IFDEF PPC_DELPHI}                            { DELPHI3+ COMPILER }
       SysUtils, Messages,                            { Standard unit }
       {$ENDIF}
     {$ELSE}                                          { SYBIL2+ COMPILER }
       WinBase, WinDef, WinUser, WinGDI,              { Standard units }
     {$ENDIF}
   {$ENDIF}

   {$IFDEF OS_OS2}                                    { OS2 CODE }
     {$IFDEF PPC_Virtual}                             { VIRTUAL PASCAL UNITS }
       OS2Def, OS2Base, OS2PMAPI,                     { Standard units }
     {$ENDIF}
     {$IFDEF PPC_Speed}                               { SPEED PASCAL UNITS }
       BseDos, Os2Def,                                { Standard units }
     {$ENDIF}
   {$ENDIF}

   Common, GFVGraph, Objects;                         { GFV standard units }

{***************************************************************************}
{                              PUBLIC CONSTANTS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                              EVENT TYPE MASKS                             }
{---------------------------------------------------------------------------}
CONST
   evMouseDown = $0001;                               { Mouse down event }
   evMouseUp   = $0002;                               { Mouse up event }
   evMouseMove = $0004;                               { Mouse move event }
   evMouseAuto = $0008;                               { Mouse auto event }
   evKeyDown   = $0010;                               { Key down event }
   evCommand   = $0100;                               { Command event }
   evBroadcast = $0200;                               { Broadcast event }

{---------------------------------------------------------------------------}
{                             EVENT CODE MASKS                              }
{---------------------------------------------------------------------------}
CONST
   evNothing   = $0000;                               { Empty event }
   evMouse     = $000F;                               { Mouse event }
   evKeyboard  = $0010;                               { Keyboard event }
   evMessage   = $FF00;                               { Message event }

{---------------------------------------------------------------------------}
{                             EXTENDED KEY CODES                            }
{---------------------------------------------------------------------------}
CONST
   kbNoKey       = $0000;  kbAltEsc      = $0100;  kbEsc         = $011B;
   kbAltSpace    = $0200;  kbCtrlIns     = $0400;  kbShiftIns    = $0500;
   kbCtrlDel     = $0600;  kbShiftDel    = $0700;  kbAltBack     = $0800;
   kbAltShiftBack= $0900;  kbBack        = $0E08;  kbCtrlBack    = $0E7F;
   kbShiftTab    = $0F00;  kbTab         = $0F09;  kbAltQ        = $1000;
   kbCtrlQ       = $1011;  kbAltW        = $1100;  kbCtrlW       = $1117;
   kbAltE        = $1200;  kbCtrlE       = $1205;  kbAltR        = $1300;
   kbCtrlR       = $1312;  kbAltT        = $1400;  kbCtrlT       = $1414;
   kbAltY        = $1500;  kbCtrlY       = $1519;  kbAltU        = $1600;
   kbCtrlU       = $1615;  kbAltI        = $1700;  kbCtrlI       = $1709;
   kbAltO        = $1800;  kbCtrlO       = $180F;  kbAltP        = $1900;
   kbCtrlP       = $1910;  kbAltLftBrack = $1A00;  kbAltRgtBrack = $1B00;
   kbCtrlEnter   = $1C0A;  kbEnter       = $1C0D;  kbAltA        = $1E00;
   kbCtrlA       = $1E01;  kbAltS        = $1F00;  kbCtrlS       = $1F13;
   kbAltD        = $2000;  kbCtrlD       = $2004;  kbAltF        = $2100;
   kbCtrlF       = $2106;  kbAltG        = $2200;  kbCtrlG       = $2207;
   kbAltH        = $2300;  kbCtrlH       = $2308;  kbAltJ        = $2400;
   kbCtrlJ       = $240A;  kbAltK        = $2500;  kbCtrlK       = $250B;
   kbAltL        = $2600;  kbCtrlL       = $260C;  kbAltSemiCol  = $2700;
   kbAltQuote    = $2800;  kbAltOpQuote  = $2900;  kbAltBkSlash  = $2B00;
   kbAltZ        = $2C00;  kbCtrlZ       = $2C1A;  kbAltX        = $2D00;
   kbCtrlX       = $2D18;  kbAltC        = $2E00;  kbCtrlC       = $2E03;
   kbAltV        = $2F00;  kbCtrlV       = $2F16;  kbAltB        = $3000;
   kbCtrlB       = $3002;  kbAltN        = $3100;  kbCtrlN       = $310E;
   kbAltM        = $3200;  kbCtrlM       = $320D;  kbAltComma    = $3300;
   kbAltPeriod   = $3400;  kbAltSlash    = $3500;  kbAltGreyAst  = $3700;
   kbSpaceBar    = $3920;  kbF1          = $3B00;  kbF2          = $3C00;
   kbF3          = $3D00;  kbF4          = $3E00;  kbF5          = $3F00;
   kbF6          = $4000;  kbF7          = $4100;  kbF8          = $4200;
   kbF9          = $4300;  kbF10         = $4400;  kbHome        = $4700;
   kbUp          = $4800;  kbPgUp        = $4900;  kbGrayMinus   = $4A2D;
   kbLeft        = $4B00;  kbCenter      = $4C00;  kbRight       = $4D00;
   kbAltGrayPlus = $4E00;  kbGrayPlus    = $4E2B;  kbEnd         = $4F00;
   kbDown        = $5000;  kbPgDn        = $5100;  kbIns         = $5200;
   kbDel         = $5300;  kbShiftF1     = $5400;  kbShiftF2     = $5500;
   kbShiftF3     = $5600;  kbShiftF4     = $5700;  kbShiftF5     = $5800;
   kbShiftF6     = $5900;  kbShiftF7     = $5A00;  kbShiftF8     = $5B00;
   kbShiftF9     = $5C00;  kbShiftF10    = $5D00;  kbCtrlF1      = $5E00;
   kbCtrlF2      = $5F00;  kbCtrlF3      = $6000;  kbCtrlF4      = $6100;
   kbCtrlF5      = $6200;  kbCtrlF6      = $6300;  kbCtrlF7      = $6400;
   kbCtrlF8      = $6500;  kbCtrlF9      = $6600;  kbCtrlF10     = $6700;
   kbAltF1       = $6800;  kbAltF2       = $6900;  kbAltF3       = $6A00;
   kbAltF4       = $6B00;  kbAltF5       = $6C00;  kbAltF6       = $6D00;
   kbAltF7       = $6E00;  kbAltF8       = $6F00;  kbAltF9       = $7000;
   kbAltF10      = $7100;  kbCtrlPrtSc   = $7200;  kbCtrlLeft    = $7300;
   kbCtrlRight   = $7400;  kbCtrlEnd     = $7500;  kbCtrlPgDn    = $7600;
   kbCtrlHome    = $7700;  kbAlt1        = $7800;  kbAlt2        = $7900;
   kbAlt3        = $7A00;  kbAlt4        = $7B00;  kbAlt5        = $7C00;
   kbAlt6        = $7D00;  kbAlt7        = $7E00;  kbAlt8        = $7F00;
   kbAlt9        = $8000;  kbAlt0        = $8100;  kbAltMinus    = $8200;
   kbAltEqual    = $8300;  kbCtrlPgUp    = $8400;  kbF11         = $8500;
   kbF12         = $8600;  kbShiftF11    = $8700;  kbShiftF12    = $8800;
   kbCtrlF11     = $8900;  kbCtrlF12     = $8A00;  kbAltF11      = $8B00;
   kbAltF12      = $8C00;  kbCtrlUp      = $8D00;  kbCtrlMinus   = $8E00;
   kbCtrlCenter  = $8F00;  kbCtrlGreyPlus= $9000;  kbCtrlDown    = $9100;
   kbCtrlTab     = $9400;  kbAltHome     = $9700;  kbAltUp       = $9800;
   kbAltPgUp     = $9900;  kbAltLeft     = $9B00;  kbAltRight    = $9D00;
   kbAltEnd      = $9F00;  kbAltDown     = $A000;  kbAltPgDn     = $A100;
   kbAltIns      = $A200;  kbAltDel      = $A300;  kbAltTab      = $A500;

{ ------------------------------- REMARK ------------------------------ }
{ New keys not initially defined by Borland in their unit interface.    }
{ ------------------------------ END REMARK --- Leon de Boer, 15May96 - }
   kbFullStop    = $342E;  kbComma       = $332C;  kbBackSlash   = $352F;
   kbApostrophe  = $2827;  kbSemiColon   = $273B;  kbEqual       = $0D3D;
   kbGreaterThan = $343E;  kbLessThan    = $333C;  kbQuestion    = $353F;
   kbQuote       = $2822;  kbColon       = $273A;  kbPlus        = $0D2B;
   kbPipe        = $2B7C;  kbSlash       = $2B5C;  kbExclaim     = $0221;
   kbAt          = $0340;  kbNumber      = $0423;  kbPercent     = $0625;
   kbCaret       = $075E;  kbAmpersand   = $0826;  kbAsterix     = $092A;
   kbLeftBracket = $0A28;  kbRightBracket= $0B29;  kbApprox      = $2960;
   kbTilde       = $297E;  kbDollar      = $0524;  kbMinus       = $0C2D;
   kbUnderline   = $0C5F;  kbLeftSqBr    = $1A5B;  kbRightSqBr   = $1B5D;
   kbLeftCurlyBr = $1A7B;  kbRightCurlyBr= $1B7D;

{---------------------------------------------------------------------------}
{                      KEYBOARD STATE AND SHIFT MASKS                       }
{---------------------------------------------------------------------------}
CONST
   kbRightShift  = $0001;                             { Right shift key }
   kbLeftShift   = $0002;                             { Left shift key }
   kbCtrlShift   = $0004;                             { Control key down }
   kbAltShift    = $0008;                             { Alt key down }
   kbScrollState = $0010;                             { Scroll lock on }
   kbNumState    = $0020;                             { Number lock on }
   kbCapsState   = $0040;                             { Caps lock on }
   kbInsState    = $0080;                             { Insert mode on }

   kbBothShifts  = kbRightShift + kbLeftShift;        { Right & Left shifts }

{---------------------------------------------------------------------------}
{                         MOUSE BUTTON STATE MASKS                          }
{---------------------------------------------------------------------------}
CONST
   mbLeftButton   = $01;                              { Left mouse button }
   mbRightButton  = $02;                              { Right mouse button }
   mbMiddleButton = $04;                              { Middle mouse button }

{---------------------------------------------------------------------------}
{                         SCREEN CRT MODE CONSTANTS                         }
{---------------------------------------------------------------------------}
CONST
   smBW80    = $0002;                                 { Black and white }
   smCO80    = $0003;                                 { Colour mode }
   smMono    = $0007;                                 { Monochrome mode }
   smFont8x8 = $0100;                                 { 8x8 font mode }

{***************************************************************************}
{                          PUBLIC TYPE DEFINITIONS                          }
{***************************************************************************}

{ ******************************* REMARK ****************************** }
{    The TEvent definition is completely compatable with all existing   }
{  code but adds two new fields ID and Data into the message record     }
{  which helps with WIN/NT and OS2 message processing.                  }
{ ****************************** END REMARK *** Leon de Boer, 11Sep97 * }

{---------------------------------------------------------------------------}
{                          EVENT RECORD DEFINITION                          }
{---------------------------------------------------------------------------}
TYPE
   TEvent = PACKED RECORD
      What: Word;                                     { Event type }
      Case Word Of
        evNothing: ();                                { ** NO EVENT ** }
        evMouse: (
          Buttons: Byte;                              { Mouse buttons }
          Double: Boolean;                            { Double click state }
          Where: TPoint);                             { Mouse position }
        evKeyDown: (                                  { ** KEY EVENT ** }
          Case Integer Of
            0: (KeyCode: Word);                       { Full key code }
            1: (CharCode: Char;                       { Char code }
                ScanCode: Byte));                     { Scan code }
        evMessage: (                                  { ** MESSAGE EVENT ** }
          Command: Word;                              { Message command }
          Id     : Word;                              { Message id }
          Data   : Real;                              { Message data }
          Case Word Of
            0: (InfoPtr: Pointer);                    { Message pointer }
            1: (InfoLong: Longint);                   { Message longint }
            2: (InfoWord: Word);                      { Message word }
            3: (InfoInt: Integer);                    { Message integer }
            4: (InfoByte: Byte);                      { Message byte }
            5: (InfoChar: Char));                     { Message character }
   END;
   PEvent = ^TEvent;

{---------------------------------------------------------------------------}
{                    ERROR HANDLER FUNCTION DEFINITION                      }
{---------------------------------------------------------------------------}
TYPE
   TSysErrorFunc = FUNCTION (ErrorCode: Integer; Drive: Byte): Integer;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          BUFFER MOVE ROUTINES                             }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-CStrLen------------------------------------------------------------
Returns the length of string S, where S is a control string using tilde
characters ('~') to designate shortcut characters. The tildes are
excluded from the length of the string, as they will not appear on
the screen. For example, given the string '~B~roccoli' as its
parameter, CStrLen returns 8.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION CStrLen (Const S: String): Integer;

{-MoveStr------------------------------------------------------------
Moves a string into a buffer for use with a view's WriteBuf or WriteLine.
Dest must be a TDrawBuffer (or an equivalent array of words). The
characters in Str are moved into the low bytes of corresponding words
in Dest. The high bytes of the words are set to Attr, or remain
unchanged if Attr is zero.
25May96 LdB
---------------------------------------------------------------------}
PROCEDURE MoveStr (Var Dest; Const Str: String; Attr: Byte);

{-MoveCStr-----------------------------------------------------------
The characters in Str are moved into the low bytes of corresponding
words in Dest. The high bytes of the words are set to Lo(Attr) or
Hi(Attr). Tilde characters (~) in the string toggle between the two
attribute bytes passed in the Attr word.
25May96 LdB
---------------------------------------------------------------------}
PROCEDURE MoveCStr (Var Dest; Const Str: String; Attrs: Word);

{-MoveBuf------------------------------------------------------------
Count bytes are moved from Source into the low bytes of corresponding
words in Dest. The high bytes of the words in Dest are set to Attr,
or remain unchanged if Attr is zero.
25May96 LdB
---------------------------------------------------------------------}
PROCEDURE MoveBuf (Var Dest, Source; Attr: Byte; Count: Word);

{-MoveChar------------------------------------------------------------
Moves characters into a buffer for use with a view's WriteBuf or
WriteLine. Dest must be a TDrawBuffer (or an equivalent array of words).
The low bytes of the first Count words of Dest are set to C, or
remain unchanged if Ord(C) is zero. The high bytes of the words are
set to Attr, or remain unchanged if Attr is zero.
25May96 LdB
---------------------------------------------------------------------}
PROCEDURE MoveChar (Var Dest; C: Char; Attr: Byte; Count: Word);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        KEYBOARD SUPPORT ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetAltCode---------------------------------------------------------
Returns the scancode corresponding to Alt+Ch key that is given.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION GetAltCode (Ch: Char): Word;

{-GetCtrlCode--------------------------------------------------------
Returns the scancode corresponding to Alt+Ch key that is given.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION GetCtrlCode (Ch: Char): Word;

{-GetAltChar---------------------------------------------------------
Returns the ascii character for the Alt+Key scancode that was given.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION GetAltChar (KeyCode: Word): Char;

{-GetCtrlChar--------------------------------------------------------
Returns the ascii character for the Ctrl+Key scancode that was given.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION GetCtrlChar (KeyCode: Word): Char;

{-CtrlToArrow--------------------------------------------------------
Converts a WordStar-compatible control key code to the corresponding
cursor key code.
25May96 LdB
---------------------------------------------------------------------}
FUNCTION CtrlToArrow (KeyCode: Word): Word;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        KEYBOARD CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetShiftState------------------------------------------------------
Returns a byte containing the current Shift key state. The return
value contains a combination of the kbXXXX constants for shift states.
08Jul96 LdB
---------------------------------------------------------------------}
FUNCTION GetShiftState: Byte;

{-GetKeyEvent--------------------------------------------------------
Checks whether a keyboard event is available. If a key has been pressed,
Event.What is set to evKeyDown and Event.KeyCode is set to the scan
code of the key. Otherwise, Event.What is set to evNothing.
19May98 LdB
---------------------------------------------------------------------}
PROCEDURE GetKeyEvent (Var Event: TEvent);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          MOUSE CONTROL ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-ShowMouse----------------------------------------------------------
Decrements the hide counter and if zero the mouse is shown on screen.
30Jun98 LdB
---------------------------------------------------------------------}
PROCEDURE ShowMouse;

{-HideMouse----------------------------------------------------------
If mouse hide counter is zero it removes the cursor from the screen.
The hide counter is then incremented by one count.
30Jun98 LdB
---------------------------------------------------------------------}
PROCEDURE HideMouse;

{-GetMouseEvent------------------------------------------------------
Checks whether a mouse event is available. If a mouse event has occurred,
Event.What is set to evMouseDown, evMouseUp, evMouseMove, or evMouseAuto
and the button and double click variables are set appropriately.
06Jan97 LdB
---------------------------------------------------------------------}
PROCEDURE GetMouseEvent (Var Event: TEvent);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                      EVENT HANDLER CONTROL ROUTINES                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-InitEvents---------------------------------------------------------
Initializes the event manager, enabling the mouse handler routine and
under DOS/DPMI shows the mouse on screen. It is called automatically
by TApplication.Init.
02May98 LdB
---------------------------------------------------------------------}
PROCEDURE InitEvents;

{-DoneEvents---------------------------------------------------------
Terminates event manager and disables the mouse and under DOS hides
the mouse. It is called automatically by TApplication.Done.
02May98 LdB
---------------------------------------------------------------------}
PROCEDURE DoneEvents;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           VIDEO CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-InitVideo---------------------------------------------------------
Initializes the video manager, Saves the current screen mode in
StartupMode, and switches to the mode indicated by ScreenMode.
19May98 LdB
---------------------------------------------------------------------}
PROCEDURE InitVideo;

{-DoneVideo---------------------------------------------------------
Terminates the video manager by restoring the initial screen mode
(given by StartupMode), clearing the screen, and restoring the cursor.
Called automatically by TApplication.Done.
03Jan97 LdB
---------------------------------------------------------------------}
PROCEDURE DoneVideo;

{-ClearScreen--------------------------------------------------------
Does nothing provided for compatability purposes only.
04Jan97 LdB
---------------------------------------------------------------------}
PROCEDURE ClearScreen;

{-SetVideoMode-------------------------------------------------------
Does nothing provided for compatability purposes only.
04Jan97 LdB
---------------------------------------------------------------------}
PROCEDURE SetVideoMode (Mode: Word);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           ERROR CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-InitSysError-------------------------------------------------------
Error handling is not yet implemented so this simply sets
SysErrActive=True (ie it lies) and exits.
20May98 LdB
---------------------------------------------------------------------}
PROCEDURE InitSysError;

{-DoneSysError-------------------------------------------------------
Error handling is not yet implemented so this simply sets
SysErrActive=False and exits.
20May98 LdB
---------------------------------------------------------------------}
PROCEDURE DoneSysError;

{-SystemError---------------------------------------------------------
Error handling is not yet implemented so this simply drops through.
20May98 LdB
---------------------------------------------------------------------}
FUNCTION SystemError (ErrorCode: Integer; Drive: Byte): Integer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           STRING FORMAT ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-PrintStr-----------------------------------------------------------
Does nothing provided for compatability purposes only.
30Jun98 LdB
---------------------------------------------------------------------}
PROCEDURE PrintStr (CONST S: String);

{-FormatStr----------------------------------------------------------
A string formatting routine that given a string that includes format
specifiers and a list of parameters in Params, FormatStr produces a
formatted output string in Result.
18Feb99 LdB
---------------------------------------------------------------------}
PROCEDURE FormatStr (Var Result: String; CONST Format: String; Var Params);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                 >> NEW QUEUED EVENT HANDLER ROUTINES <<                   }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-PutEventInQueue-----------------------------------------------------
If there is room in the queue the event is placed in the next vacant
position in the queue manager.
17Mar98 LdB
---------------------------------------------------------------------}
FUNCTION PutEventInQueue (Var Event: TEvent): Boolean;

{-NextQueuedEvent----------------------------------------------------
If there are queued events the next event is loaded into event else
evNothing is returned.
17Mar98 LdB
---------------------------------------------------------------------}
PROCEDURE NextQueuedEvent(Var Event: TEvent);

{***************************************************************************}
{                        INITIALIZED PUBLIC VARIABLES                       }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                     INITIALIZED DOS/DPMI VARIABLES                        }
{---------------------------------------------------------------------------}
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
{ ******************************* REMARK ****************************** }
{    In Hi-Res graphics modes where the usual mouse handler will not    }
{  work these can be set so the user can provide his own hide, show     }
{  and redraw on move routines, otherwise leave them set as nil.        }
{ ****************************** END REMARK *** Leon de Boer, 20Jul98 * }
TYPE DrawProc = PROCEDURE;

CONST
   HideMouseProc: DrawProc = Nil;                     { Hide mouse procedure }
   ShowMouseProc: DrawProc = Nil;                     { Show mouse procedure }
   MouseMoveProc: DrawProc = Nil;                     { Mouse moved procedure }

PROCEDURE HideMouseCursor;
PROCEDURE ShowMouseCursor;
{$ENDIF}

{---------------------------------------------------------------------------}
{                      INITIALIZED WIN/NT VARIABLES                         }
{---------------------------------------------------------------------------}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
CONST
   AppWindow    : HWnd = 0;                           { Application window }
   DefGfvFont   : HFont = 0;                          { Default GFV font }
   DefFontWeight: Integer = fw_Normal;                { Default font weight }
   DefFontStyle : String = 'Times New Roman';         { Default font style }
{$ENDIF}

{---------------------------------------------------------------------------}
{                        INITIALIZED OS2 VARIABLES                          }
{---------------------------------------------------------------------------}
{$IFDEF OS_OS2}                                       { OS2 CODE }
CONST
   AppWindow   : HWnd = 0;                            { Application window }
   Anchor      : HAB = 0;                             { Anchor block }
   MsgQue      : HMq = 0;                             { Message queue }
   DefGFVFont  : LongInt = 0;                         { Default font style }
   DefPointer  : HPointer = 0;                        { Default pointer }
   DefFontStyle: String = 'Times';                    { Default font style }
{$ENDIF}

{---------------------------------------------------------------------------}
{                INITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES                  }
{---------------------------------------------------------------------------}
CONST
   CheckSnow    : Boolean = False;                    { Compatability only }
   MouseEvents  : Boolean = False;                    { Mouse event state }
   MouseReverse : Boolean = False;                    { Mouse reversed }
   HiResScreen  : Boolean = False;                    { Compatability only }
   CtrlBreakHit : Boolean = False;                    { Compatability only }
   SaveCtrlBreak: Boolean = False;                    { Compatability only }
   SysErrActive : Boolean = False;                    { Compatability only }
   FailSysErrors: Boolean = False;                    { Compatability only }
   ButtonCount  : Byte = 0;                           { Mouse button count }
   DoubleDelay  : Word = 8;                           { Double click delay }
   RepeatDelay  : Word = 8;                           { Auto mouse delay }
   SysColorAttr : Word = $4E4F;                       { System colour attr }
   SysMonoAttr  : Word = $7070;                       { System mono attr }
   StartupMode  : Word = $FFFF;                       { Compatability only }
   CursorLines  : Word = $FFFF;                       { Compatability only }
   ScreenBuffer : Pointer = Nil;                      { Compatability only }
   SaveInt09    : Pointer = Nil;                      { Compatability only }
   SysErrorFunc : TSysErrorFunc = SystemError;        { System error ptr }

{---------------------------------------------------------------------------}
{          >>> NEW INITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES <<<            }
{---------------------------------------------------------------------------}
CONST
   DefLineNum     : Integer = 25;                     { Default line number }
   DefFontHeight  : Integer = 0;                      { Default font height }
   SysFontWidth   : Integer = 8;                      { System font width }
   SysFontHeight  : Integer = 16;                     { System font height }

{***************************************************************************}
{                      UNINITIALIZED PUBLIC VARIABLES                       }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                UNINITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES                }
{---------------------------------------------------------------------------}
VAR
   MouseIntFlag: Byte;                                { Mouse in int flag }
   MouseButtons: Byte;                                { Mouse button state }
   ScreenWidth : Byte;                                { Screen text width }
   ScreenHeight: Byte;                                { Screen text height }
   ScreenMode  : Word;                                { Screen mode }
   MouseWhere  : TPoint;                              { Mouse position }

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                               IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
  {$IFDEF PPC_FPC}                                    { FPC DOS COMPILER }
  USES Go32;                                          { Standard unit }
  {$ENDIF}
{$ENDIF}

{***************************************************************************}
{                        PRIVATE INTERNAL CONSTANTS                         }
{***************************************************************************}

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
{---------------------------------------------------------------------------}
{                 DOS/DPMI MOUSE INTERRUPT EVENT QUEUE SIZE                 }
{---------------------------------------------------------------------------}
CONST EventQSize = 16;                                { Default irq bufsize }
{$ENDIF}

{---------------------------------------------------------------------------}
{                DOS/DPMI/WIN/NT/OS2 NEW EVENT QUEUE MAX SIZE               }
{---------------------------------------------------------------------------}
CONST QueueMax = 64;                                  { Max new queue size }

{***************************************************************************}
{                          PRIVATE INTERNAL TYPES                           }
{***************************************************************************}

{$IFDEF OS_WINDOWS}                                   { DOS/DPMI CODE }
{---------------------------------------------------------------------------}
{                   SYBIL2+ WIN/NT COMPILER TYPE FIX UPS                    }
{---------------------------------------------------------------------------}
   {$IFDEF PPC_SPEED}                                 { SYBIL2+ COMPILER }
   TYPE TLogFont = LogFont;                           { Type fix up }
   TYPE TMsg = Msg;                                   { Type fix up }
   TYPE TTextMetric = TextMetric;                     { Type fix up }
   {$ENDIF}
{$ENDIF}

{***************************************************************************}
{                  PRIVATE INTERNAL INITIALIZED VARIABLES                   }
{***************************************************************************}

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
{---------------------------------------------------------------------------}
{                       DOS/DPMI INITIALIZED VARIABLES                      }
{---------------------------------------------------------------------------}
   {$IFDEF GO32V2}                                    { GO32V2 needs these }
   CONST
     RealSeg: Word = 0;                               { Real mode segment }
     RealOfs: Word = 0;                               { Real mode offset }
     MouseCallback: Pointer = Nil;                    { Mouse call back ptr }
   {$ENDIF}

{$ENDIF}

{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
{---------------------------------------------------------------------------}
{           WIN/NT TABLE OF ALT + ASCII CODES FROM VIRTUAL CODES            }
{---------------------------------------------------------------------------}
CONST AltVirtualToAscii: Array [0..127] Of Word =
     ($00, $00, $00, $00, $00, $00, $00, $00,
      kbAltBack, kbAltTab, $00, $00, $00, kbEnter, $00, $00,
{10H} $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, kbEsc, $00, $00, $00, $00,
{20H} kbAltSpace, kbAltPgUp, kbAltPgDn, kbAltEnd, kbAltHome,
        kbAltLeft, kbAltUp, kbAltRight,
      kbAltDown, $00, $00, $00, $00, kbAltIns, kbAltDel, $00,
{30H} kbAlt0, kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt6, kbAlt7,
      kbAlt8, kbAlt9, $00, $00, $00, $00, $00, $00,
{40H} $00, kbAltA, kbAltB, kbAltC, kbAltD, kbAltE, kbAltF, kbAltG,
      kbAltH, kbAltI, kbAltJ, kbAltK, kbAltL, kbAltM, kbAltN, kbAltO,
{50H} kbAltP, kbAltQ, kbAltR, kbAltS, kbAltT, kbAltU, kbAltV, kbAltW,
      kbAltX, kbAltY, kbAltZ, $00, $00, $00, $00, $00,
{60H} $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $372A, $4E2B, $00, $4A2D, $00, $352F,
{70H} kbAltF1, kbAltF2, kbAltF3, kbAltF4, kbAltF5, kbAltF6, kbAltF7, kbAltF8,
      kbAltF9, kbAltF10, $00, $00, $00, $00, $00, $00);

{---------------------------------------------------------------------------}
{            WIN/NT TABLE OF WINDOWS ASCII TO INTERNATIONAL ASCII           }
{---------------------------------------------------------------------------}
CONST WinAsciiToIntAscii: Array [128..255] Of Byte = (
{80H} $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
{90H} $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
{A0H} $00, $AD, $BD, $9C, $CF, $BE, $B3, $F5,
      $00, $B8, $A6, $AE, $AA, $B0, $A9, $00,
{B0H} $F8, $F1, $FD, $00, $EF, $E6, $F4, $00,
      $3C, $3E, $A7, $AF, $AC, $AB, $F3, $A8,
{C0H} $B7, $B5, $B6, $C7, $8E, $8F, $92, $80,
      $D4, $90, $D2, $D3, $DE, $D6, $D7, $D8,
{D0H} $D1, $A5, $E3, $E0, $E2, $E5, $99, $00,
      $9D, $EB, $E9, $EA, $9A, $ED, $E7, $E1,
{E0H} $85, $A0, $83, $C6, $84, $86, $91, $87,
      $8A, $82, $88, $89, $8D, $A1, $8C, $8B,
{F0H} $D0, $A4, $95, $A2, $93, $E4, $94, $F6,
      $9B, $97, $A3, $96, $81, $EC, $E8, $98);
{$ENDIF}

{---------------------------------------------------------------------------}
{          DOS/DPMI/WIN/NT/OS2 ALT KEY SCANCODES FROM KEYS (0-127)          }
{---------------------------------------------------------------------------}
CONST AltCodes: Array [0..127] Of Byte = (
      $00, $00, $00, $00, $00, $00, $00, $00,         { $00 - $07 }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $08 - $0F }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $10 - $17 }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $18 - $1F }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $20 - $27 }
      $00, $00, $00, $00, $00, $82, $00, $00,         { $28 - $2F }
      $81, $78, $79, $7A, $7B, $7C, $7D, $7E,         { $30 - $37 }
      $7F, $80, $00, $00, $00, $83, $00, $00,         { $38 - $3F }
      $00, $1E, $30, $2E, $20, $12, $21, $22,         { $40 - $47 }
      $23, $17, $24, $25, $26, $32, $31, $18,         { $48 - $4F }
      $19, $10, $13, $1F, $14, $16, $2F, $11,         { $50 - $57 }
      $2D, $15, $2C, $00, $00, $00, $00, $00,         { $58 - $5F }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $60 - $67 }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $68 - $6F }
      $00, $00, $00, $00, $00, $00, $00, $00,         { $70 - $77 }
      $00, $00, $00, $00, $00, $00, $00, $00);        { $78 - $7F }

{***************************************************************************}
{                  PRIVATE INTERNAL INITIALIZED VARIABLES                   }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                           NEW CONTROL VARIABLES                           }
{---------------------------------------------------------------------------}
CONST
   HideCount : Integer = 0;                           { Cursor hide count }
   QueueCount: Word = 0;                              { Queued message count }
   QueueHead : Word = 0;                              { Queue head pointer }
   QueueTail : Word = 0;                              { Queue tail pointer }

{***************************************************************************}
{                 PRIVATE INTERNAL UNINITIALIZED VARIABLES                  }
{***************************************************************************}

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
{---------------------------------------------------------------------------}
{                     UNINITIALIZED DOS/DPMI VARIABLES                      }
{---------------------------------------------------------------------------}
VAR
   LastDouble : Boolean;                              { Last double buttons }
   LastButtons: Byte;                                 { Last button state }
   DownButtons: Byte;                                 { Last down buttons }
   EventCount : Word;                                 { Events in queue }
   AutoDelay  : Word;                                 { Delay time count }
   DownTicks  : Word;                                 { Down key tick count }
   AutoTicks  : Word;                                 { Held key tick count }
   LastWhereX : Word;                                 { Last x position }
   LastWhereY : Word;                                 { Last y position }
   DownWhereX : Word;                                 { Last x position }
   DownWhereY : Word;                                 { Last y position }
   EventQHead : Pointer;                              { Head of queue }
   EventQTail : Pointer;                              { Tail of queue }
   EventQueue : Array [0..EventQSize - 1] Of TEvent;  { Event queue }
   EventQLast : RECORD END;                           { Simple end marker }

{---------------------------------------------------------------------------}
{                ABSOLUTE PRIVATE DOS/DPMI ADDRESS VARIABLES                }
{---------------------------------------------------------------------------}
VAR
   {$IFNDEF GO32V1}
   ShiftState: Byte Absolute $40:$17;                 { Shift state mask }
   Ticks: Word Absolute $40:$6C;                      { DOS tick counter }
   {$ENDIF}

   {$IFDEF GO32V2}                                    { GO32V2 registers }
   ActionRegs: TRealRegs;                             { Real mode registers }
   {$ENDIF}

{$ENDIF}

{---------------------------------------------------------------------------}
{                UNINITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES                }
{---------------------------------------------------------------------------}
VAR
   SaveExit: Pointer;                                 { Saved exit pointer }
   Queue   : Array [0..QueueMax-1] Of TEvent;         { New message queue }

{***************************************************************************}
{                         PRIVATE INTERNAL ROUTINES                         }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                  DOS/DPMI ONLY PRIVATE INTERNAL ROUTINES                  }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }

{$IFDEF GO32V2}                                       { GO32V2 CODE }
{---------------------------------------------------------------------------}
{  MouseTrap -> Platforms GO32V2 - FPC COMPILER Updated 10Sep98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE Mouse_Trap; FAR; ASSEMBLER;
ASM
   PUSH %ES;                                          { Save ES register }
   PUSH %DS;                                          { Save DS register }
   PUSHL %EDI;                                        { Save register }
   PUSHL %ESI;                                        { Save register }
   ;{ caution : ds is not the selector for our data !! }
   PUSH %ES;                                          { Push data seg }
   POP %DS;                                           { Load data seg }
   PUSHL %EDI;                                        { Actionregs address }
   MOVL MOUSECALLBACK, %EAX;                          { Fetch callback addr }
   CMPL $0, %EAX;                                     { Check for nil ptr }
   JS .L_NoCallBack;                                  { Ignore if nil }
   POPL %EAX;                                         { %EAX = @actionregs }
   MOVL (%EAX), %EDI;                                 { EDI from actionregs }
   MOVL 4(%EAX), %ESI;                                { ESI from actionregs }
   MOVL 16(%EAX), %EBX;                               { EBX from actionregs }
   MOVL 20(%EAX), %EDX;                               { EDX from actionregs }
   MOVL 24(%EAX), %ECX;                               { ECX from actionregs }
   MOVL 28(%EAX), %EAX;                               { EAX from actionregs }
   CALL *MOUSECALLBACK;                               { Call callback proc }
.L_NoCallBack:
   POPL %ESI;                                         { Recover register }
   POPL %EDI;                                         { Recover register }
   POP %DS;                                           { Restore DS register }
   POP %ES;                                           { Restore ES register }
   MOVL (%ESI), %EAX;
   MOVL %EAX, %ES:42(%EDI);                           { Set as return addr }
   ADDW $4, %ES:46(%EDI);                             { adjust stack }
   IRET;                                              { Interrupt return }
END;
{$ENDIF}

{$IFDEF PPC_FPC}                                      { FPC COMPILER CODE }
{---------------------------------------------------------------------------}
{  Mouse_Action -> Platforms DPMI - FPC COMPILER Updated 10Sep98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE Mouse_Action (Mask : Integer; P : Pointer);
VAR Error: Word; ErrStr: String; {$IFDEF GO32V2} Rg: TRealRegs; {$ENDIF}
BEGIN
   {$IFDEF GO32V1}                                    { GO32V1 CODE }
   ErrStr := 'GO32V1 mouse handler set failed !!';    { Set error string }
   ASM
     MOVL $0xFF, %EAX;                                { GO32v1 special id }
     MOVL P, %ECX;                                    { Fuction to chain }
     MOVL $0x20, %EBX;                                { Event queue size > 0 }
     MOVL $0x12345678, %EDX;                          { Test for version ? }
     INT $0x33;                                       { Call special wrapper }
     CMPW $0xFF0, %AX;                                { AX=$FF0 if success }
     JNZ .L_GO32V1Err;
     MOVW $0, %AX;                                    { Zero register }
     JMP .LGO32V1Ok;                                  { Now jump over }
   .L_GO32V1Err:
     MOVW $0xFFFF, %AX;                               { -1 to register }
   .L_GO32V1Ok:
     MOVW %AX, Error;                                 { Set error result }
   END;
   {$ENDIF}
   {$IFDEF GO32V2}                                    { GO32V2 CODE }
   Error := 0;                                        { Preset no error }
   ErrStr := 'GO32V2 mouse handler set failed !!';    { Set error string }
   If (P <> MouseCallBack) Then Begin                 { Check func different }
     If (RealSeg <> 0) Then Begin                     { Remove old calback }
       Rg.AX := 12;                                   { Function id }
       Rg.CX := 0;                                    { Zero mask register }
       Rg.ES := 0;                                    { Zero proc seg }
       Rg.DX := 0;                                    { Zero proc ofs }
       RealIntr($33, Rg);                             { Stop INT 33 callback }
       ASM
         MOVW $0x304, %AX;                            { Set function id }
         MOVW REALSEG, %CX;                           { Bridged real seg }
         MOVW REALOFS, %DX;                           { Bridged real ofs }
         INT $0x31;                                   { Release bridge }
       END;
     End;
     MouseCallback := P;                              { Set call back addr }
     If (P <> Nil) Then Begin                         { Check non nil proc }
       ASM
         LEAL ACTIONREGS, %EDI;                       { Addr of actionregs }
         LEAL MOUSE_TRAP, %ESI;                       { Procedure address }
         PUSH %DS;                                    { Save DS segment }
         PUSH %ES;                                    { Save ES segment }
         PUSH %DS;
         POP  %ES;                                    { ES now has dataseg }
         PUSH %CS;
         POP  %DS;                                    { DS now has codeseg }
         MOVW $0x303, %AX;                            { Function id }
         INT  $0x31;                                  { Call DPMI bridge }
         POP  %ES;                                    { Restore ES segment }
         POP  %DS;                                    { Restore DS segment }
         MOVW %CX, REALSEG;                           { Transfer real seg }
         MOVW %DX, REALOFS;                           { Transfer real ofs }
         MOVW $0, %AX;                                { Preset zero error }
         JNC .L_call_ok;                              { Branch if ok }
         MOVW $0xFFFF, %AX;                           { Force a -1 error }
       .L_call_ok:
         MOVW %AX, ERROR;                             { Return error state }
       END;
       Rg.CX := Mask;                                 { Set mask register }
     End Else Begin
       Rg.EDI := 0;                                   { Zero proc register }
       Rg.CX := 0;                                    { Zero mask register }
     End;
     If (Error = 0) Then Begin                        { If no error }
       Rg.AX := 12;                                   { Set function id }
       Rg.ES := RealSeg;                              { Real mode segment }
       Rg.DX := RealOfs;                              { Real mode offset }
       RealIntr($33, Rg);                             { Set interrupt 33 }
     End Else Begin
       RealSeg := 0;                                  { Zero real mode seg }
       RealOfs := 0;                                  { Zero real mode ofs }
     End;
   End;
   {$ENDIF}
   If (Error <> 0) Then Begin                         { Error encountered }
     WriteLn(ErrStr);                                 { Write error }
     ReadLn;                                          { Wait for user to see }
   End;
END;

{$ENDIF}

{---------------------------------------------------------------------------}
{  MouseInt -> Platforms DOS/DPMI - Updated 30Jun98 LdB                     }
{---------------------------------------------------------------------------}
PROCEDURE MouseInt; FAR; ASSEMBLER;
{$IFDEF ASM_BP}                                       { BP COMPATABLE ASM }
ASM
   MOV SI, SEG @DATA;                                 { Fetch data segment }
   MOV DS, SI;                                        { Fix data segment }
   MOV SI, CX;                                        { Transfer x position }
   MOV MouseButtons, BL;                              { Update mouse buttons }
   MOV MouseWhere.X, SI;                              { Update x position }
   MOV MouseWhere.Y, DX;                              { Update y position }
   CMP EventCount, EventQSize;                        { Check if queue full }
   JZ @@QueueFull;                                    { Queue is full exit }
   MOV ES, Seg0040;                                   { Fetch DOS segment }
   MOV AX, ES:Ticks;                                  { Fetch dos tick count }
   MOV DI, WORD PTR EventQTail;                       { Address of tail }
   PUSH DS;                                           { Push to stack }
   POP ES;                                            { ES to data segment }
   CLD;                                               { Store forward }
   STOSW;                                             { Store tick count }
   XCHG AX, BX;                                       { Transfer register }
   STOSW;                                             { Store button state }
   XCHG AX, SI;                                       { Transfer register }
   STOSW;                                             { Store x position }
   XCHG AX, DX;                                       { Transfer register }
   STOSW;                                             { Store y position }
   CMP DI, OFFSET EventQLast;                         { Roll if at queue end }
   JNE @@NoRollNeeded;                                { Not at queue end }
   MOV DI, OFFSET EventQueue;                         { Roll back to start }
@@NoRollNeeded:
   MOV WORD PTR EventQTail, DI;                       { Update queue tail }
   INC EventCount;                                    { One message added }
@@QueueFull:
   MOV MouseIntFlag, 1;                               { Set interrupt flag }
   MOV SI, WORD PTR MouseMoveProc;                    { Low address word }
   OR SI, WORD PTR MouseMoveProc+2;                   { "OR" high word }
   JZ @@Exit;                                         { No move call so exit }
   DB $66; PUSH AX;                                   { Store EAX }
   DB $66; PUSH BX;                                   { Store EBX }
   DB $66; PUSH CX;                                   { Store ECX }
   DB $66; PUSH DX;                                   { Store EDX }
   DB $66; PUSH SI;                                   { Store ESI }
   DB $66; PUSH DI;                                   { Store EDI }
   DB $66; PUSH BP;                                   { Store EBP }
   PUSH ES;                                           { Store ES }
   PUSH BP;                                           { Standard BP push }
   MOV BP, SP;                                        { Transfer stack ptr }
   CALL MouseMoveProc;                                { Standard procedure }
   POP BP;                                            { Standard BP recover }
   POP ES;                                            { Recover ES }
   DB $66; POP BP;                                    { Recover EBP }
   DB $66; POP DI;                                    { Recover EDI }
   DB $66; POP SI;                                    { Recover ESI }
   DB $66; POP DX;                                    { Recover EDX }
   DB $66; POP CX;                                    { Recover ECX }
   DB $66; POP BX;                                    { Recover EBX }
   DB $66; POP AX;                                    { Recover EAX }
@@Exit:
END;
{$ENDIF}
{$IFDEF ASM_FPC}                                      { FPC COMPATABLE ASM }
ASM
   MOVW %CX, %SI;                                     { Transfer x position }
   MOVB %BL, MOUSEBUTTONS;                            { Update mouse buttons }
   MOVW %SI, MOUSEWHERE;                              { Update x position }
   MOVW %DX, MOUSEWHERE+2;                            { Update y position }
   CMPW $16, EVENTCOUNT;                              { Check if queue full }
   JZ .L_QueueFull;                                   { Queue is full exit }
   PUSH %ES;                                          { Save segment }
   MOVW $0x40, %AX;                                   { Fetch DOS segment }
   MOVW %AX, %ES;                                     { Transfer to segment }
   MOVL $0x6C, %EDI;                                  { Address of ticks }
   MOVW %ES:(%EDI), %AX;                              { Fetch dos tick count }
   POP %ES;                                           { Recover segment }
   MOVL EVENTQTAIL, %EDI;                             { Queue tail address }
   CLD;                                               { Store forward }
   STOSW;                                             { Store tick count }
   XCHGW %BX, %AX;                                    { Transfer register }
   STOSW;                                             { Store button state }
   XCHGW %SI, %AX;                                    { Transfer register }
   STOSW;                                             { Store x position }
   XCHGW %DX, %AX;                                    { Transfer register }
   STOSW;                                             { Store y position }
   LEAL EVENTQLAST, %EAX;                             { Roll point address }
   CMPL %EAX, %EDI;                                   { Roll if at queue end }
   JNE .L_NoRollNeeded;                               { Not at queue end }
   LEAL EVENTQUEUE, %EDI;                             { Roll back to start }
.L_NoRollNeeded:
   MOVL %EDI, EVENTQTAIL;                             { Update queue tail }
   INCW EVENTCOUNT;                                   { One message added }
.L_QueueFull:
   MOVB $1, MOUSEINTFLAG;                             { Set interrupt flag }
   MOVL MOUSEMOVEPROC, %EAX;                          { Load proc address }
   CMPL $0, %EAX;                                     { Check for nil ptr }
   JZ .L_Exit;                                        { No move call so exit }
   PUSHL %EAX;                                        { Store EAX }
   PUSHL %EBX;                                        { Store EBX }
   PUSHL %ECX;                                        { Store ECX }
   PUSHL %EDX;                                        { Store EDX }
   PUSHL %ESI;                                        { Store ESI }
   PUSHL %EDI;                                        { Store EDI }
   PUSHL %EBP;                                        { Store EBP }
   PUSH %ES;                                          { Store ES }
   CALL %EAX;                                         { Standard procedure }
   POP %ES;                                           { Recover ES }
   POPL %EBP;                                         { Recover EBP }
   POPL %EDI;                                         { Recover EDI }
   POPL %ESI;                                         { Recover ESI }
   POPL %EDX;                                         { Recover EDX }
   POPL %ECX;                                         { Recover ECX }
   POPL %EBX;                                         { Recover EBX }
   POPL %EAX;                                         { Recover EAX }
.L_Exit:
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  HideMouseCursor -> Platforms DOS/DPMI - Updated 10Sep98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE HideMouseCursor; ASSEMBLER;
{$IFDEF ASM_BP}                                       { BP COMPATABLE ASM }
ASM
   CMP MouseEvents, 0;                                { Check mouse system }
   JZ @@Exit;                                         { Branch if not active }
   MOV AX, WORD PTR [HideMouseProc];                  { Fetch offset of addr }
   OR AX, WORD PTR [HideMouseProc+2];                 { Check for nil ptr }
   JZ @@UseMouseInt;                                  { Branch if nil }
   CALL FAR PTR [HideMouseProc];                      { Call hide mouse }
   JMP @@Exit;                                        { Now exit }
@@UseMouseInt:
   MOV AX, $2;                                        { Load function id }
   PUSH BP;                                           { Safety!! save reg }
   INT $33;                                           { Hide the mouse }
   POP BP;                                            { Restore register }
@@Exit:
END;
{$ENDIF}
{$IFDEF ASM_FPC}                                      { FPC COMPATABLE ASM }
ASM
   CMPB $0, MouseEvents;                              { Check mouse system }
   JZ .L_Exit;                                        { Branch if not active }
   MOVL HideMouseProc, %EAX;                          { Fetch address }
   ORL %EAX, %EAX;                                    { Check for nil ptr }
   JZ .L_UseMouseInt;                                 { Branch if nil }
   CALL HideMouseProc;                                { Call show mouse }
   JMP .L_Exit;                                       { Now exit }
.L_UseMouseInt:
   MOVW $2, %AX;                                      { Load function id }
   PUSHL %EBP;                                        { Save regigister }
   INT $0x33;                                         { Hide the mouse }
   POPL %EBP;                                         { Restore register }
.L_Exit:
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  ShowMouseCursor -> Platforms DOS/DPMI - Updated 10Sep98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE ShowMouseCursor; ASSEMBLER;
{$IFDEF ASM_BP}                                       { BP COMPATABLE ASM }
ASM
   CMP MouseEvents, 0;                                { Check mouse system }
   JZ @@Exit;                                         { Branch if not active }
   MOV AX, WORD PTR [ShowMouseProc];                  { Fetch offset of addr }
   OR AX, WORD PTR [ShowMouseProc+2];                 { Check for nil ptr }
   JZ @@UseMouseInt;                                  { Branch if nil }
   CALL FAR PTR [ShowMouseProc];                      { Call show mouse }
   JMP @@Exit;                                        { Now exit }
@@UseMouseInt:
   MOV AX, $1;                                        { Load function id }
   PUSH BP;                                           { Safety!! save reg }
   INT $33;                                           { Show the mouse }
   POP BP;                                            { Restore register }
@@Exit:
END;
{$ENDIF}
{$IFDEF ASM_FPC}                                      { FPC COMPATABLE ASM }
ASM
   CMPB $0, MouseEvents;                              { Check mouse system }
   JZ .L_Exit;                                        { Branch if not active }
   MOVL ShowMouseProc, %EAX;                          { Fetch address }
   ORL %EAX, %EAX;                                    { Check for nil ptr }
   JZ .L_UseMouseInt;                                 { Branch if nil }
   CALL ShowMouseProc;                                { Call show mouse }
   JMP .L_Exit;                                       { Now exit }
.L_UseMouseInt:
   MOVW $1, %AX;                                      { Load function id }
   PUSHL %EBP;                                        { Save regigister }
   INT $0x33;                                         { Hide the mouse }
   POPL %EBP;                                         { Restore register }
.L_Exit:
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  HookMouse -> Platforms DOS/DPMI - Updated 27Aug98 LdB                    }
{---------------------------------------------------------------------------}
PROCEDURE HookMouse;
BEGIN
   {$IFDEF ASM_BP}                                    { BP COMPTABABLE ASM }
   ASM
     MOV AX, $000C;                                   { Set user interrupt }
     MOV CX, $FFFF;                                   { For all event masks }
     MOV DX, OFFSET CS:MouseInt;                      { Mouse int is hook }
     PUSH CS;                                         { Push code segment }
     POP ES;                                          { ES:DX -> MouseInt }
     PUSH BP;                                         { Safety!! save reg }
     INT $33;                                         { Hook the routine }
     POP BP;                                          { Restore register }
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
     {$IFDEF GO32V2}                                  { GO32V2 CODE }
     Lock_Code(Pointer(@Mouse_Trap), 400);            { Lock trap code }
     Lock_Data(ActionRegs, SizeOf(ActionRegs));       { Lock registers }
     {$ENDIF}
   Mouse_Action(-1, @MouseInt);                       { Set masks/interrupt }
   {$ENDIF}
END;


{---------------------------------------------------------------------------}
{  UnHookMouse -> Platforms DOS/DPMI - Updated 27Aug98 LdB                  }
{---------------------------------------------------------------------------}
PROCEDURE UnHookMouse;
BEGIN
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASM
     MOV AX, $000C;                                   { Set user interrupt }
     XOR CX, CX;                                      { Clear all masks }
     XOR DX, DX;                                      { Clear register }
     MOV ES, CX;                                      { ES:DX -> Nil }
     PUSH BP;                                         { Safety!! save reg }
     INT $33;                                         { Release mouse hook }
     POP BP;                                          { Restore register }
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   Mouse_Action(0, Nil);                              { Clear mask/interrupt }
     {$IFDEF GO32V2}                                  { GO32V2 CODE }
     Unlock_Code(Pointer(@Mouse_Trap), 400);          { Release trap code }
     Unlock_Data(ActionRegs, SizeOf(TRealRegs));      { Release registers }
     {$ENDIF}
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  GetMousePosition -> Platforms DOS/DPMI - Updated 19May98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE GetMousePosition (Var X, Y: Integer); ASSEMBLER;
{$IFDEF ASM_BP}                                       { BP COMPATABLE ASM }
ASM
   MOV AX, $3;                                        { Set function id }
   PUSH BP;                                           { Safety!! save reg }
   INT $33;                                           { Get button data }
   POP BP;                                            { Restore register }
   LES DI, X;                                         { Adress of x }
   MOV ES:[DI], CX;                                   { Return x position }
   LES DI, Y;                                         { Adress of y }
   MOV ES:[DI], DX;                                   { Return y position }
END;
{$ENDIF}
{$IFDEF ASM_FPC}                                      { FPC COMPATABLE ASM }
ASM
   MOVW $3, %AX;                                      { Set function id }
   PUSHL %EBP;                                        { Save register }
   INT $0x33;                                         { Get button data }
   POPL %EBP;                                         { Restore register }
   MOVL X, %EDI;                                      { Adress of x }
   MOVW %CX, (%EDI);                                  { Return x position }
   MOVL Y, %EDI;                                      { Adress of y }
   MOVW %DX, (%EDI);                                  { Return y position }
END;
{$ENDIF}

{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{               DOS/DPMI/WIN/NT/OS2 PRIVATE INTERNAL ROUTINES               }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  ExitDrivers -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jun98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE ExitDrivers; FAR;
BEGIN
   DoneSysError;                                      { Relase error trap }
   DoneEvents;                                        { Close event driver }
   ExitProc := SaveExit;                              { Restore old exit }
END;

{---------------------------------------------------------------------------}
{  DetectVideo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE DetectVideo;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
ASSEMBLER;
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASM
     MOV AH, $0F;                                     { Set function id }
     PUSH BP;                                         { Safety!! save reg }
     INT $10;                                         { Get current crt mode }
     POP BP;                                          { Restore register }
     PUSH AX;                                         { Hold result }
     MOV AX, $1130;                                   { Set function id }
     MOV BH, 0;                                       { Zero register }
     MOV DL, 0;                                       { Zero register }
     PUSH BP;                                         { Safety!! save reg }
     INT $10;                                         { Get ext-video mode }
     POP BP;                                          { Restore register }
     POP AX;                                          { Recover held value }
     MOV DH, AH;                                      { Transfer high mode }
     CMP DL, 25;                                      { Check screen ht }
     SBB AH, AH;                                      { Subtract borrow }
     INC AH;                                          { Make #1 if in high }
     CMP AL, smMono;                                  { Is screen mono }
     JZ @@Exit1;                                      { Exit of mono }
     CMP AL, smBW80;                                  { Is screen B&W }
     JZ @@Exit1;                                      { Exit if B&W }
     MOV AX, smCO80;                                  { Else set to colour }
   @@Exit1:
     MOV ScreenMode, AX;                              { Hold screen mode }
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   ASM
     MOVB $0x0F, %AH;                                 { Set function id }
     PUSHL %EBP;                                      { Save register }
     INT $0x10;                                       { Get current crt mode }
     POPL %EBP;                                       { Restore register }
     PUSHL %EAX;                                      { Hold result }
     MOVW $0x1130, %AX;                               { Set function id }
     MOVB $0, %BH;                                    { Zero register }
     MOVB $0, %DL;                                    { Zero register }
     PUSHL %EBP;                                      { Safety!! save reg }
     INT $0x10;                                       { Get ext-video mode }
     POPL %EBP;                                       { Restore register }
     POPL %EAX;                                       { Recover held value }
     MOVB %AH, %DH;                                   { Transfer high mode }
     CMPB $25, %DL;                                   { Check screen ht }
     SBB %AH, %AH;                                    { Subtract borrow }
     INCB %AH;                                        { Make #1 if in high }
     CMPB $07, %AL;                                   { Is screen mono }
     JZ .L_Exit1;                                     { Exit of mono }
     CMPB $02, %AL;                                   { Is screen B&W }
     JZ .L_Exit1;                                     { Exit if B&W }
     MOVW $03, %AX;                                   { Else set to colour }
   .L_Exit1:
     MOVW %AX, SCREENMODE;                            { Hold screen mode }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
VAR Dc: HDC;
BEGIN
   Dc := GetDc(0);                                    { Get screen context }
   If ((GetDeviceCaps(Dc, BitsPixel) > 1) OR          { Colour capacity }
   (GetDeviceCaps(Dc, Planes) > 1)) Then              { Colour capacity }
     ScreenMode := smCO80 Else ScreenMode := smMono;  { Screen mode }
   ReleaseDc(0, Dc);                                  { Release context }
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
VAR Ps: Hps; Dc: Hdc; Colours: LongInt;
BEGIN
   Ps := WinGetPS(HWND_Desktop);                      { Get desktop PS }
   Dc := GpiQueryDevice(Ps);                          { Get gpi context }
   DevQueryCaps(Dc, Caps_Phys_Colors, 1, Colours);    { Colour capacity }
   If (Colours> 2) Then ScreenMode := smCO80          { Colour screen }
     Else ScreenMode := smMono;                       { Mono screen }
   WinReleasePS(Ps);                                  { Release desktop PS }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  DetectMouse -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB       }
{---------------------------------------------------------------------------}
FUNCTION DetectMouse: Byte;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV AX, $3533;                                   { Set function id }
     PUSH BP;                                         { Safety!! save reg }
     INT $21;                                         { Get mouse interrupt }
     POP BP;                                          { Restore register }
     MOV AX, ES;                                      { Transfer register }
     OR AX, BX;                                       { Check for nil ptr }
     JZ @@Exit2;                                      { Jump no mouse driver }
     XOR AX, AX;                                      { Set function id }
     PUSH BP;                                         { Safety!! save reg }
     INT $33;                                         { Reset mouse }
     POP BP;                                          { Restore register }
     OR AX, AX;                                       { Check for success }
     JZ @@Exit2;                                      { Reset mouse failed }
     MOV AX, BX;                                      { Return button count }
   @@Exit2:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOVW $0x200, %AX;                                { Get real mode int }
     MOVW $0x33, %BX;                                 { Vector 33H }
     PUSHL %EBP;                                      { Save register }
     INT $0x31;                                       { Get the address }
     POPL %EBP;                                       { Restore register }
     MOVW %CX, %AX;                                   { Transfer register }
     ORW %DX, %AX;                                    { Check for nil ptr }
     JZ .L_Exit2;                                     { Jump no mouse driver }
     XORW %AX, %AX;                                   { Set function id }
     PUSHL %EBP;                                      { Save register }
     INT $0x33;                                       { Reset mouse driver }
     POPL %EBP;                                       { Restore register }
     ORW %AX, %AX;                                    { Check for success }
     JZ .L_Exit2;                                     { Reset mouse failed }
     MOVW %BX, %AX;                                   { Return button count }
   .L_Exit2:
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
BEGIN
   If (GetSystemMetrics(sm_MousePresent) <> 0) Then
     DetectMouse := 2 Else DetectMouse := 0;          { Buttons present }
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   DetectMouse := WinQuerySysValue(HWND_Desktop,
     SV_CMouseButtons);                               { Buttons present }
END;
{$ENDIF}

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           BUFFER MOVE ROUTINES                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  CStrLen -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION CStrLen (Const S: String): Integer;
VAR I, J: Integer;
BEGIN
   J := 0;                                            { Set result to zero }
   For I := 1 To Length(S) Do
     If (S[I] <> '~') Then Inc(J);                    { Inc count if not ~ }
   CStrLen := J;                                      { Return length }
END;

{---------------------------------------------------------------------------}
{  MoveStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Jul99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE MoveStr (Var Dest; Const Str: String; Attr: Byte);
VAR I: Word; P: PWord;
BEGIN
   For I := 1 To Length(Str) Do Begin                 { For each character }
     P := @TWordArray(Dest)[I-1];                     { Pointer to word }
     If (Attr <> 0) Then WordRec(P^).Hi := Attr;      { Copy attribute }
     WordRec(P^).Lo := Byte(Str[I]);                  { Copy string char }
   End;
END;

{---------------------------------------------------------------------------}
{  MoveCStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Jul99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE MoveCStr (Var Dest; Const Str: String; Attrs: Word);
VAR B: Byte; I, J: Word; P: PWord;
BEGIN
   J := 0;                                            { Start position }
   For I := 1 To Length(Str) Do Begin                 { For each character }
     If (Str[I] <> '~') Then Begin                    { Not tilde character }
       P := @TWordArray(Dest)[J];                     { Pointer to word }
       If (Lo(Attrs) <> 0) Then
         WordRec(P^).Hi := Lo(Attrs);                 { Copy attribute }
       WordRec(P^).Lo := Byte(Str[I]);                { Copy string char }
       Inc(J);                                        { Next position }
     End Else Begin
       B := Hi(Attrs);                                { Hold attribute }
       WordRec(Attrs).Hi := Lo(Attrs);                { Copy low to high }
       WordRec(Attrs).Lo := B;                        { Complete exchange }
     End;
   End;
END;

{---------------------------------------------------------------------------}
{  MoveBuf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Jul99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE MoveBuf (Var Dest, Source; Attr: Byte; Count: Word);
VAR I: Word; P: PWord;
BEGIN
   For I := 1 To Count Do Begin
     P := @TWordArray(Dest)[I-1];                     { Pointer to word }
     If (Attr <> 0) Then WordRec(P^).Hi := Attr;      { Copy attribute }
     WordRec(P^).Lo := TByteArray(Source)[I-1];       { Copy source data }
   End;
END;

{---------------------------------------------------------------------------}
{  MoveChar -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Jul99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE MoveChar (Var Dest; C: Char; Attr: Byte; Count: Word);
VAR I: Word; P: PWord;
BEGIN
   For I := 1 To Count Do Begin
     P := @TWordArray(Dest)[I-1];                     { Pointer to word }
     If (Attr <> 0) Then WordRec(P^).Hi := Attr;      { Copy attribute }
     If (Ord(C) <> 0) Then WordRec(P^).Lo := Byte(C); { Copy character }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        KEYBOARD SUPPORT ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetAltCode -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB        }
{---------------------------------------------------------------------------}
FUNCTION GetAltCode (Ch: Char): Word;
BEGIN
   GetAltCode := 0;                                   { Preset zero return }
   Ch := UpCase(Ch);                                  { Convert upper case }
   If (Ch < #128) Then
     GetAltCode := AltCodes[Ord(Ch)] SHL 8            { Return code }
     Else If (Ch = #240) Then GetAltCode := $0200     { Return code }
       Else GetAltCode := 0;                          { Return zero }
END;

{---------------------------------------------------------------------------}
{  GetCtrlCode -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB       }
{---------------------------------------------------------------------------}
FUNCTION GetCtrlCode (Ch: Char): Word;
BEGIN
   GetCtrlCode := GetAltCode(Ch) OR (Ord(Ch) - $40);  { Ctrl+key code }
END;

{---------------------------------------------------------------------------}
{  GetAltChar -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB        }
{---------------------------------------------------------------------------}
FUNCTION GetAltChar (KeyCode: Word): Char;
VAR I: Integer;
BEGIN
   GetAltChar := #0;                                  { Preset fail return }
   If (Lo(KeyCode) = 0) Then Begin                    { Extended key }
     If (Hi(KeyCode) < 128) Then Begin                { Key between 0-127 }
       I := 0;                                        { Start at first }
       While (I < 128) AND (Hi(KeyCode) <> AltCodes[I])
         Do Inc(I);                                   { Search for match }
       If (I < 128) Then GetAltChar := Chr(I);        { Return character }
     End Else
       If (Hi(KeyCode)=$02) Then GetAltChar := #240;  { Return char }
   End;
END;

{---------------------------------------------------------------------------}
{  GetCtrlChar -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB       }
{---------------------------------------------------------------------------}
FUNCTION GetCtrlChar (KeyCode: Word): Char;
VAR C: Char;
BEGIN
  C := #0;                                            { Preset #0 return }
  If (Lo(KeyCode) > 0) AND (Lo(KeyCode) <= 26) Then   { Between 1-26 }
    C := Chr(Lo(KeyCode) + $40);                      { Return char A-Z }
  GetCtrlChar := C;                                   { Return result }
END;

{---------------------------------------------------------------------------}
{  CtrlToArrow -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25May96 LdB       }
{---------------------------------------------------------------------------}
FUNCTION CtrlToArrow (KeyCode: Word): Word;
CONST NumCodes = 11;
      CtrlCodes : Array [0..NumCodes-1] Of Char =
        (#19, #4, #5, #24, #1, #6, #7, #22, #18, #3, #8);
      ArrowCodes: Array [0..NumCodes-1] Of Word =
       (kbLeft, kbRight, kbUp, kbDown, kbHome, kbEnd, kbDel, kbIns,
        kbPgUp, kbPgDn, kbBack);
VAR I: Integer;
BEGIN
   CtrlToArrow := KeyCode;                            { Preset key return }
   For I := 0 To NumCodes - 1 Do
     If WordRec(KeyCode).Lo = Byte(CtrlCodes[I])      { Matches a code }
     Then Begin
       CtrlToArrow := ArrowCodes[I];                  { Return key stroke }
       Exit;                                          { Now exit }
     End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        KEYBOARD CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetShiftState -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul96 LdB     }
{---------------------------------------------------------------------------}
FUNCTION GetShiftState: Byte;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV ES, Seg0040;                                 { Load DOS segment }
     XOR AX, AX;
     MOV DX, AX;                                      { Clear registers }
     MOV AL, ES:[$0017];                              { Read shift state }
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   BEGIN
   ASM
     MOVW $0x0200, %AX;                               { Set function id }
     PUSHL %EBP;                                      { Save register }
     INT $0x16;                                       { Get shift status }
     POPL %EBP;                                       { Restore register }
   END;
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
CONST vk_Scroll = $91;                                { Borland forgot this! }
VAR B: Byte;
BEGIN
   B := 0;                                            { Clear all masks }
   If (GetKeyState(vk_Shift) AND $80 <> 0) Then
     B := B OR kbBothShifts;                          { Set both shifts }
   If (GetKeyState(vk_Control) AND $80 <> 0) Then
     B := B OR kbCtrlShift;                           { Set control mask }
   If (GetKeyState(vk_Menu) AND $80 <> 0) Then
     B := B OR kbAltShift;                            { Set alt mask }
   If (GetKeyState(vk_Scroll) AND $81 <> 0) Then
     B := B OR kbScrollState;                         { Set scroll lock mask }
   If (GetKeyState(vk_NumLock) AND $81 <> 0) Then
     B := B OR kbNumState;                            { Set number lock mask }
   If (GetKeyState(vk_Capital) AND $81 <> 0) Then
     B := B OR kbCapsState;                           { Set caps lock mask }
   If (GetKeyState(vk_Insert) AND $81 <> 0) Then
     B := B OR kbInsState;                            { Set insert mask }
   GetShiftState := B;                                { Return masks }
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
VAR Key: KbdInfo;
BEGIN
   Key.cb := SizeOf(Key);                             { Keyboard size }
   If KbdGetStatus(Key, 0) = 0 Then                   { Get key status }
     GetShiftState := Key.fsState Else                { Return shift state }
     GetShiftState := 0;                              { Failed so return 0 }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  GetKeyEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE GetKeyEvent (Var Event: TEvent);
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
ASSEMBLER;
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASM
     MOV AH, $1;                                      { Set function id }
     PUSH BP;                                         { Safety!! save reg }
     INT $16;                                         { Check for keypress }
     POP BP;                                          { Restore register }
     MOV AX, $0;                                      { Zero register AX }
     MOV BX, AX;                                      { Zero register BX }
     JZ @@Exit3;                                      { No keypress jump }
     MOV AH, $00;                                     { Set function id }
     PUSH BP;                                         { Safety!! save reg }
     INT $16;                                         { Read the key }
     POP BP;                                          { Restore register }
     XCHG AX, BX;                                     { Exchange registers }
     MOV AX, evKeyDown;                               { Set keydown event }
   @@Exit3:
     LES DI, Event;                                   { ES:DI -> Event }
     MOV ES:[DI].TEvent.What, AX;                     { Store event mask }
     MOV ES:[DI].TEvent.KeyCode, BX;                  { Store key code }
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   ASM
     MOVB $1, %AH;                                    { Set function id }
     PUSHL %EBP;                                      { Save register }
     INT $0x16;                                       { Check for keypress }
     POPL %EBP;                                       { Restore register }
     MOVW $0x0, %AX;                                  { Zero register AX }
     MOVW %AX, %BX;                                   { Zero register BX }
     JZ .L_Exit3;                                     { No keypress jump }
     MOVB $0, %AH;                                    { Set function id }
     PUSHL %EBP;                                      { Save register }
     INT $0x16;                                       { Read the key }
     POPL %EBP;                                       { Restore register }
     XCHGW %BX, %AX;                                  { Exchange registers }
     MOVW $0x10, %AX;                                 { Set keydown event }
   .L_Exit3:
     MOVL Event, %EDI;                                { EDI -> Event }
     CLD;
     STOSW;                                           { Store event mask }
     XCHGW %BX, %AX;                                  { Transfer key code }
     STOSW;                                           { Store key code }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
CONST NumPos: Byte = 0; Numeric: Byte = 0;
VAR Handled: Boolean; B: Byte; Msg: TMsg;
BEGIN
   Event.What := evNothing;                           { Preset no event }
   {$IFDEF PPC_FPC}                                   { FPC WINDOWS COMPILER }
   If (PeekMessage(@Msg, 0, 0, WM_MouseFirst-1, pm_Remove)
   OR PeekMessage(@Msg, 0, WM_MouseLast+1, $FFFF, pm_Remove))
   {$ELSE}                                            { OTHER COMPILERS }
   If (PeekMessage(Msg, 0, 0, WM_MouseFirst-1, pm_Remove)
   OR PeekMessage(Msg, 0, WM_MouseLast+1, $FFFF, pm_Remove))
   {$ENDIF}
   Then Begin                                         { Non mouse message }
     Handled := False;                                { Preset not handled }
     Case Msg.Message Of
       WM_Char: Begin                                 { CHARACTER KEY }
         NumPos := 0;                                 { Zero number position }
         Event.CharCode := Char(Msg.wParam);          { Transfer character }
         If (Event.CharCode > #127) Then
           Event.CharCode := Chr(WinAsciiToIntAscii[
             Ord(Event.CharCode)]);                   { Convert to ascii }
         Event.ScanCode := Lo(HiWord(Msg.lParam));    { Transfer scan code }
         If (Event.CharCode <> #0) Then Begin         { If valid key then }
           Event.What := evKeyDown;                   { Key down event }
           Handled := True;                           { Message was handled }
           If (Event.KeyCode = kbTab) AND             { Tab key is special }
           (GetShiftState AND kbBothShifts <> 0) Then { Check shift state }
             Event.KeyCode := kbShiftTab;             { If set make shifttab }
         End;
       End;
       WM_SysKeyDown: Begin                           { SYSTEM KEY DOWN }
         If (NumPos > 0) Then Begin                   { Numerics entry op }
           Case Msg.wParam Of
             VK_Insert: B := 0;                       { Key value = 0 }
             VK_End: B := 1;                          { Key value = 1 }
             VK_Down: B := 2;                         { Key value = 2 }
             VK_Next: B := 3;                         { Key value = 3 }
             VK_Left: B := 4;                         { Key value = 4 }
             VK_Clear: B := 5;                        { Key value = 5 }
             VK_Right: B := 6;                        { Key value = 6 }
             VK_Home: B := 7;                         { Key value = 7 }
             VK_Up: B := 8;                           { Key value = 8 }
             VK_Prior: B := 9;                        { Key value = 9 }
             VK_NumPad0..VK_NumPad9: B := Msg.wParam
               - $60;                                 { Numbic key pad }
             Else NumPos := 0;                        { Invalid key }
           End;
           If ((NumPos > 0) AND (NumPos < 4)) AND     { Valid position }
           ((B >= $0) AND (B <= $9)) Then Begin       { Valid key }
             Numeric := Numeric*10 + B;               { Adjust numeric }
             Inc(NumPos);                             { Next position }
             If (NumPos = 4) Then Begin               { We have three keys }
               Event.What := evKeyDown;               { Set keydown event }
               Event.CharCode := Chr(Numeric);        { Transfer code }
               NumPos := 0;                           { Zero number position }
             End;
             Handled := True;                         { Message was handled }
           End Else NumPos := 0;                      { Zero number position }
         End;
         If (Msg.WParam = vk_Menu) Then Begin         { ALT key down }
           Numeric := 0;                              { Zero numeric }
           NumPos := 1;                               { Set to start point }
           Handled := True;                           { Message was handled }
         End;
         If NOT Handled Then Begin                    { Key press not handled }
           If (Lo(Msg.wParam) < 128) Then Begin       { Ignore if above 128 }
             If (Msg.wParam = vk_F10) Then Begin      { F10 reports oddly }
               If (GetKeyState(vk_Shift) AND $80 <> 0)
                 Then Event.KeyCode := kbShiftF10 Else{ Shift F10 }
               If (GetKeyState(vk_Menu) AND $80 <> 0)
                 Then Event.KeyCode := kbAltF10 Else  { Alt F10 }
               If (GetKeyState(vk_Control) AND $80 <> 0)
                 Then Event.KeyCode := kbCtrlF10      { Ctrl F10 }
               Else Event.KeyCode := kbF10;           { Normal F10 }
             End Else Event.KeyCode :=
               AltVirtualToAscii[Lo(Msg.wParam)];     { Convert key code }
           End Else Event.KeyCode := 0;               { Clear Event.keycode }
           If (Event.KeyCode <> 0) Then Begin         { If valid key then }
             Event.What := evKeyDown;                 { Key down event }
             Handled := True;                         { Message was handled }
           End;
         End;
       End;
       WM_KeyDown: Begin                              { ARROWS/F1..F12 KEYS }
         If (((Msg.WParam >= Vk_F1) AND (Msg.WParam <= Vk_F12)) OR
        ((Msg.WParam >= Vk_Prior) AND (Msg.WParam <= Vk_Delete)))
         Then Begin                                   { Special key press }
           Event.CharCode := #0;                      { Clear char code }
           Event.ScanCode := Lo(HiWord(Msg.LParam));  { Create scan code }
           If (GetKeyState(vk_Shift) AND $80 <> 0)
           Then Begin                                 { Shift key down }
             Case Msg.wParam Of
               vk_F1..vk_F9: Event.KeyCode :=
                 Event.KeyCode + $1900;               { Shift F1..F9 keys }
               vk_F11: Event.KeyCode := kbShiftF11;   { Shift F11 key }
               vk_F12: Event.KeyCode := kbShiftF12;   { Shift F12 key }
             End;
           End Else If (GetKeyState(vk_Control) AND $80 <> 0)
           Then Begin                                 { Control key down }
             Case Msg.wParam Of
               vk_F1..vk_F9: Event.KeyCode :=
                 Event.KeyCode + $2300;               { Ctrl F1..F9 keys }
               vk_F11: Event.KeyCode := kbCtrlF11;    { Ctrl F11 key }
               vk_F12: Event.KeyCode := kbCtrlF12;    { Ctrl F12 key }
             End;
           End;
           If (Event.KeyCode <> 0) Then Begin         { If valid key then }
             Event.What := evKeyDown;                 { Key down event }
             Handled := True;                         { Message was handled }
           End;
         End;
         NumPos := 0;                                 { Zero number position }
       End;
     End;
     If NOT Handled Then Begin                        { Check we did not handle }
       TranslateMessage(Msg);                         { Translate message }
       DispatchMessage(Msg);                          { Dispatch message }
     End;
   End;
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
VAR Msg: QMsg;
BEGIN
   Event.What := evNothing;                           { Preset no event }
   If (WinPeekMsg(Anchor, Msg, 0, 0, WM_MouseFirst-1, pm_Remove)
   OR WinPeekMsg(Anchor, Msg, 0, WM_MouseLast+1, $FFFFFFFF, pm_Remove))
   Then Begin                                         { Check for message }
     If (Msg.Msg = WM_Char) AND                       { Character message }
     (Msg.Mp1 AND KC_KeyUp <> 0) AND                  { Key released }
     (Msg.Mp1 AND KC_Composite = 0)                   { Not composite key }
     Then Begin
       If (Short1FromMP(Msg.Mp1) AND KC_ScanCode <> 0 )
       Then Begin
         Event.ScanCode := Ord(Char4FromMP(Msg.Mp1)); { Return scan code }
         Event.CharCode := Char1FromMP(Msg.Mp2);      { Return char code }
         If (Event.CharCode = Chr($E0)) Then Begin
           Event.CharCode := #0;
           Event.ScanCode := Byte(Char2FromMP(Msg.Mp2));
         End;
         If (Event.KeyCode <> 0) Then
           Event.What := evKeyDown;                   { Key down event }
       End;
     End;
     If (Event.What = evNothing) Then                 { Event not handled }
       WinDispatchMsg(Anchor, Msg);                   { Disptach message }
   End;
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          MOUSE CONTROL ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  HideMouse -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE HideMouse;
BEGIN
   If (HideCount = 0) Then Begin                      { Is mouse hidden yet? }
     {$IFDEF OS_DOS}                                  { DOS/DPMI CODE }
     HideMouseCursor;                                 { Hide mouse cursor }
     {$ENDIF}
     {$IFDEF OS_WINDOWS}                              { WIN/NT CODE }
     ShowCursor(False);                               { Hide mouse cursor }
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 CODE }
     If (AppWindow <> 0) Then                         { Window valid }
       WinShowCursor(AppWindow, False);               { Hide mouse cursor }
     {$ENDIF}
   End;
   Inc(HideCount);                                    { Inc hide count }
END;

{---------------------------------------------------------------------------}
{  ShowMouse -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE ShowMouse;
BEGIN
   Dec(HideCount);                                    { Dec hide count }
   If (HideCount = 0) Then Begin                      { Is mouse visible? }
     {$IFDEF OS_DOS}                                  { DOS/DPMI CODE }
     ShowMouseCursor;                                 { Show mouse cursor }
     {$ENDIF}
     {$IFDEF OS_WINDOWS}                              { WIN/NT CODE }
     ShowCursor(True);                                { Show mouse cursor }
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 CODE }
     If (AppWindow <> 0) Then                         { Window valid }
       WinShowCursor(AppWindow, True);                { Show mouse cursor }
     {$ENDIF}
   End;
END;

{---------------------------------------------------------------------------}
{  GetMouseEvent -> Platforms DOS/DPMI/WINDOWS/OS2 - Updated 09Sep98 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE GetMouseEvent (Var Event: TEvent);
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
ASSEMBLER;
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
ASM
   CMP MouseEvents, 0;                                { Any mouse events }
   JNZ @@MouseOk;                                     { Check mouse active }
   JMP @@NoEventExit;                                 { Mouse not active }
@@MouseOk:
   CLI;                                               { Disable interrupts }
   CMP EventCount, 0;                                 { Check event count  }
   JNE @@MouseEventInQueue;                           { If > 0 event avail }
   MOV BL, MouseButtons;                              { Fetch mouse buttons }
   MOV CX, MouseWhere.Word[0];                        { Fetch mouse where.x }
   MOV DX, MouseWhere.Word[2];                        { Fetch mouse where.y }
   MOV ES, Seg0040;                                   { DOS DAT SEG }
   MOV DI, ES:Ticks;                                  { Fetch current time }
   JMP @@NextMsgReady;                                { Now process }
@@MouseEventInQueue:
   MOV SI, WORD PTR EventQHead;                       { Event queue head }
   CLD;                                               { Direction forward  }
   LODSW;                                             { Fetch word 1 }
   XCHG AX, DI;                                       { Set timer ticks }
   LODSW;                                             { Fetch word 2 }
   XCHG AX, BX;                                       { Set button masks }
   LODSW;                                             { Fetch word 3 }
   XCHG AX, CX;                                       { Set mouse x position }
   LODSW;                                             { Fetch word 4 }
   XCHG AX, DX;                                       { Set mouse y position }
   CMP SI, OFFSET EventQLast;                         { Check if roll needed }
   JNE @@NoRoll;
   MOV SI, OFFSET EventQueue;                         { Roll back to start }
@@NoRoll:
   MOV WORD PTR EventQHead, SI;                       { Update queue head }
   DEC EventCount;                                    { One event cleared }
@@NextMsgReady:
   STI;                                               { Enable interrupts }
   CMP MouseReverse, 0;                               { Check mouse reversed }
   JE @@MouseNormal;
   MOV BH, BL;                                        { Transfer button mask }
   AND BH, 3;                                         { Clear others masks }
   JE @@MouseNormal;                                  { Neither set exit }
   CMP BH, 3;                                         { Check not all set }
   JE @@MouseNormal;                                  { Both set exit }
   XOR BL, 3;                                         { Invert button masks }
@@MouseNormal:
   MOV BH, [LastDouble];                              { Load last double }
   MOV AL, [LastButtons];                             { Load last buttons }
   CMP AL, BL;                                        { Are buttons same? }
   JE @@SameButtonsDown;
   OR AL, AL;                                         { Any last buttons? }
   JE @@ButtonsDown;
   OR BL, BL;                                         { Any buttons down? }
   JE @@MouseUp;
   MOV BL, AL;                                        { Transfer new buttons }
@@SameButtonsDown:
   CMP CX, [LastWhereX];                              { Mouse moved from x }
   JNE @@MouseMove;
   CMP DX, [LastWhereY];                              { Mouse moved from y }
   JNE @@MouseMove;
   OR BL, BL;                                         { Any buttons pressed? }
   JE @@NoButtonsDown;
   MOV AX, DI;                                        { Current tick count }
   SUB AX, [AutoTicks];                               { Subtract last count }
   CMP AX, [AutoDelay];                               { Greater than delay? }
   JAE @@MouseAuto;                                   { Mouse auto event }
@@NoButtonsDown:
   JMP @@NoEventExit;                                 { No event exit }
@@ButtonsDown:
   MOV BH, 0;                                         { Preset no dbl click  }
   CMP BL, [DownButtons];                             { Check to last down }
   JNE @@MouseDown;
   CMP CX, [DownWhereX];                              { Check x position }
   JNE @@MouseDown;
   CMP DX, [DownWhereY];                              { Check y position }
   JNE @@MouseDown;
   MOV AX, DI;                                        { Transfer tick count }
   SUB AX, [DownTicks];                               { Sub last down count }
   CMP AX, [DoubleDelay];                             { Greater than delay? }
   JAE @@MouseDown;
   MOV BH, 1;                                         { Double click }
@@MouseDown:
   MOV [DownButtons], BL;                             { Hold down buttons }
   MOV [DownWhereX], CX;                              { Hold x down point }
   MOV [DownWhereY], DX;                              { Hold y down point }
   MOV [DownTicks], DI;                               { Hold tick value }
   MOV [AutoTicks], DI;                               { Hold tick value }
   MOV AX, [RepeatDelay];                             { Load delay count }
   MOV [AutoDelay], AX;                               { Set delay time }
   MOV AX, evMouseDown;                               { Mouse down event }
   JMP @@UpdateValues;                                { Update, svae & exit }
@@MouseUp:
   MOV AX, evMouseUp;                                 { Mouse button up }
   JMP @@UpdateValues;                                { Update, save & exit }
@@MouseMove:
   MOV AX, evMouseMove;                               { Mouse has moved }
   JMP @@UpdateValues;                                { Update, save & exit }
@@MouseAuto:
   MOV AX, evMouseAuto;                               { Mouse auto event }
   MOV [AutoTicks], DI;                               { Reset auto ticks }
   MOV [AutoDelay], 1;                                { Reset delay count }
@@UpdateValues:
   MOV [LastButtons], BL;                             { Save last buttons }
   MOV [LastDouble], BH;                              { Save double state }
   MOV [LastWhereX], CX;                              { Save x position }
   MOV [LastWhereY], DX;                              { Save y position }
   JMP @@StoreAndExit;                                { Now store and exit }
@@NoEventExit:
   XOR AX, AX;                                        { Clear register }
   MOV BX, AX;                                        { Clear register }
   MOV CX, AX;                                        { Clear register }
   MOV DX, AX;                                        { Clear register }
@@StoreAndExit:
   LES DI, Event;                                     { Address of event }
   CLD;                                               { Set direction fwd }
   STOSW;                                             { Save 1st word }
   XCHG AX, BX;                                       { Transfer register }
   STOSW;                                             { Save 2nd word }
   XCHG AX, CX;                                       { Transfer register }
   STOSW;                                             { Save 3rd word }
   XCHG AX, DX;                                       { Transfer register }
   STOSW;                                             { Save 4th word }
END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
ASM
   CMPB $0, MOUSEEVENTS;                              { Any mouse events }
   JNZ .L_MouseOk;                                    { Check mouse active }
   JMP .L_NoEventExit;                                { Mouse not active }
.L_MouseOk:
   CLI;
   CMPW $0, EVENTCOUNT;                               { Check event count  }
   JNE .L_MouseEventInQueue;                          { If > 0 event avail }
   MOVB MOUSEBUTTONS, %BL;                            { Fetch mouse buttons }
   MOVW MOUSEWHERE, %CX;                              { Fetch mouse where.x }
   MOVW MOUSEWHERE+2, %DX;                            { Fetch mouse where.y }
   PUSH %ES;                                          { Save segment }
   MOVW $0x40, %AX;                                   { Fetch DOS segment }
   MOVW %AX, %ES;                                     { Transfer to segment }
   MOVL $0x6C, %EDI;                                  { Tick address }
   MOVW %ES:(%EDI), %DI;                              { Fetch dos tick count }
   POP %ES;                                           { Recover segment }
   JMP .L_NextMsgReady;                               { Now process }
.L_MouseEventInQueue:
   MOVL EVENTQHEAD, %ESI;                             { Event queue head }
   CLD;                                               { Direction forward  }
   LODSW;                                             { Fetch word 1 }
   XCHGW %DI, %AX;                                    { Set timer ticks }
   LODSW;                                             { Fetch word 2 }
   XCHGW %BX, %AX;                                    { Set button masks }
   LODSW;                                             { Fetch word 3 }
   XCHGW %CX, %AX;                                    { Set mouse x position }
   LODSW;                                             { Fetch word 4 }
   XCHGW %DX, %AX;                                    { Set mouse y position }
   LEAL EVENTQLAST, %EAX;                             { Address of roll pt }
   CMPL %EAX, %ESI;                                   { Check if roll needed }
   JNE .L_NoHeadRoll;
   LEAL EVENTQUEUE, %ESI;                             { Roll back to start }
.L_NoHeadRoll:
   MOVL %ESI, EVENTQHEAD;                             { Update queue head }
   DECW EVENTCOUNT;                                   { One event cleared }
.L_NextMsgReady:
   STI;                                               { Enable interrupts }
   CMPB $0, MOUSEREVERSE;                             { Check mouse reversed }
   JE .L_MouseNormal;
   MOVB %BL, %BH;                                     { Transfer button mask }
   ANDB $3, %BH;                                      { Clear others masks }
   JE .L_MouseNormal;                                 { Neither set exit }
   CMPB $3, %BH;                                      { Check not all set }
   JE .L_MouseNormal;                                 { Both set exit }
   XORB $3, %BL;                                      { Invert button masks }
.L_MouseNormal:
   MOVB LASTDOUBLE, %BH;                              { Load last double }
   MOVB LASTBUTTONS, %AL;                             { Load last buttons }
   CMPB %BL, %AL;                                     { Are buttons same? }
   JE .L_SameButtonsDown;
   ORB %AL, %AL;                                      { Any last buttons? }
   JE .L_ButtonsDown;
   ORB %BL, %BL;                                      { Any buttons down? }
   JE .L_MouseUp;
   MOVB %AL, %BL;                                     { Transfer new buttons }
.L_SameButtonsDown:
   CMPW LASTWHEREX, %CX;                              { Mouse moved from x }
   JNE .L_MouseMove;
   CMPW LASTWHEREY, %DX;                              { Mouse moved from y }
   JNE .L_MouseMove;
   ORB %BL, %BL;                                      { Any buttons pressed? }
   JE .L_NoButtonsDown;
   MOVW %DI, %AX;                                     { Current tick count }
   SUBW AUTOTICKS, %AX;                               { Subtract last count }
   CMPW AUTODELAY, %AX;                               { Greater than delay? }
   JAE .L_MouseAuto;                                  { Mouse auto event }
.L_NoButtonsDown:
   JMP .L_NoEventExit;                                { No event exit }
.L_ButtonsDown:
   MOVB $0, %BH;                                      { Preset no dbl click  }
   CMPB DOWNBUTTONS, %BL;                             { Check to last down }
   JNE .L_MouseDown;
   CMPW DOWNWHEREX, %CX;                              { Check x position }
   JNE .L_MouseDown;
   CMPW DOWNWHEREY, %DX;                              { Check y position }
   JNE .L_MouseDown;
   MOVW %DI, %AX;                                     { Transfer tick count }
   SUBW DOWNTICKS, %AX;                               { Sub last down count }
   CMPW DOUBLEDELAY, %AX;                             { Greater than delay? }
   JAE .L_MouseDown;
   MOVB $1, %BH;                                      { Double click }
.L_MouseDown:
   MOVB %BL, DOWNBUTTONS;                             { Hold down buttons }
   MOVW %CX, DOWNWHEREX;                              { Hold x down point }
   MOVW %DX, DOWNWHEREY;                              { Hold y down point }
   MOVW %DI, DOWNTICKS;                               { Hold tick value }
   MOVW %DI, AUTOTICKS;                               { Hold tick value }
   MOVW REPEATDELAY, %AX;                             { Load delay count }
   MOVW %AX, AUTODELAY;                               { Set delay time }
   MOVW $1, %AX;                                      { Mouse down event }
   JMP .L_UpdateValues;                               { Update, svae & exit }
.L_MouseUp:
   MOVW $2, %AX;                                      { Mouse button up }
   JMP .L_UpdateValues;                               { Update, save & exit }
.L_MouseMove:
   MOVW $4, %AX;                                      { Mouse has moved }
   JMP .L_UpdateValues;                               { Update, save & exit }
.L_MouseAuto:
   MOVW $8, %AX;                                      { Mouse auto event }
   MOVW %DI, AUTOTICKS;                               { Reset auto ticks }
   MOVW $1, AUTODELAY;                                { Reset delay count }
.L_UpdateValues:
   MOVB %BL, LASTBUTTONS;                             { Save last buttons }
   MOVB %BH, LASTDOUBLE;                              { Save double state }
   MOVW %CX, LASTWHEREX;                              { Save x position }
   MOVW %DX, LASTWHEREY;                              { Save y position }
   JMP .L_StoreAndExit;                               { Now store and exit }
.L_NoEventExit:
   XORW %AX, %AX;                                     { Clear register }
   MOVW %AX, %BX;                                     { Clear register }
   MOVW %AX, %CX;                                     { Clear register }
   MOVW %AX, %DX;                                     { Clear register }
.L_StoreAndExit:
   MOVL Event, %EDI;                                  { Adress of event }
   CLD;                                               { Set direction fwd }
   STOSW;                                             { Save 1st word }
   XCHGW %BX, %AX;                                    { Transfer register }
   STOSW;                                             { Save 2nd word }
   XCHGW %CX, %AX;                                    { Transfer register }
   STOSW;                                             { Save 3rd word }
   XCHGW %DX, %AX;                                    { Transfer register }
   STOSW;                                             { Save 4th word }
END;
{$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
VAR Msg: TMsg;
BEGIN
   Event.What := evNothing;                           { Preset no event }
   {$IFDEF PPC_FPC}                                   { FPC WINDOWS COMPILER }
   If PeekMessage(@Msg, 0, WM_MouseFirst,
   WM_MouseLast, pm_Remove) Then Begin                { Fetch mouse message }
   {$ELSE}                                            { OTHER COMPILERS }
   If PeekMessage(Msg, 0, WM_MouseFirst,
   WM_MouseLast, pm_Remove) Then Begin                { Fetch mouse message }
   {$ENDIF}
     TranslateMessage(Msg);                           { Translate message }
     DispatchMessage(Msg);                            { Dispatch message }
   End;
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
VAR Msg: QMsg;
BEGIN
   Event.What := evNothing;                           { Preset no event }
   If WinPeekMsg(Anchor, Msg, 0, WM_MouseFirst,
   WM_MouseLast, pm_Remove) Then Begin                { Fetch mouse message }
     WinDispatchMsg(Anchor, Msg);                     { Dispatch message }
   End;
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                      EVENT HANDLER CONTROL ROUTINES                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  InitEvents -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 07Sep99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE InitEvents;
BEGIN
   If (ButtonCount <> 0) Then Begin                   { Mouse is available }
     {$IFDEF OS_DOS}                                  { DOS/DPMI CODE }
     EventQHead := @EventQueue;                       { Initialize head }
     EventQtail := @EventQueue;                       { Initialize tail }
     LastDouble  := False;                            { Clear last double }
     LastButtons := 0;                                { Clear last buttons }
     DownButtons := 0;                                { Clear down buttons }
     HookMouse;                                       { Hook the mouse }
     GetMousePosition(MouseWhere.X, MouseWhere.Y);    { Get mouse position }
     LastWhereX := MouseWhere.X;                      { Set last x position }
     LastWhereY := MouseWhere.Y;                      { Set last y position }
     MouseEvents := True;                             { Set initialized flag }
     ShowMouseCursor;                                 { Show the mouse }
     {$ENDIF}
     {$IFDEF OS_WINDOWS}                              { WIN/NT CODE }
     MouseEvents := True;                             { Set initialized flag }
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 CODE }
     If (Anchor=0) Then Anchor := WinInitialize(0);   { Create anchor block }
     If (MsgQue = 0) AND (Anchor <> 0) Then
       MsgQue := WinCreateMsgQueue(Anchor, 0);        { Initialize queue }
     If (MsgQue = 0) Then Halt(254);                  { Check queue created }
     MouseEvents := True;                             { Set initialized flag }
     {$ENDIF}
   End;
END;

{---------------------------------------------------------------------------}
{  DoneEvents -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jul99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE DoneEvents;
BEGIN
   If MouseEvents Then Begin                          { Initialized check }
     {$IFDEF OS_DOS}                                  { DOS/DPMI CODE }
     HideMouseCursor;                                 { Hide the mouse }
     MouseEvents := False;                            { Clear event flag }
     UnHookMouse;                                     { Unhook the mouse }
     {$ENDIF}
     {$IFDEF OS_WINDOWS}                              { WIN/NT CODE }
     MouseEvents := False;                            { Clr initialized flag }
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 CODE }
     If (MsgQue <> 0) Then WinDestroyMsgQueue(MsgQue);{ Destroy msg queue }
     If (Anchor <> 0) Then WinTerminate(Anchor);      { Destroy anchor block }
     MsgQue := 0;                                     { Zero msg queue handle }
     Anchor := 0;                                     { Zero anchor block }
     MouseEvents := False;                            { Clr initialized flag }
     {$ENDIF}
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           VIDEO CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
{$IFDEF PPC_FPC}                                      { FPC COMPILER ONLY }
{ ******************************* REMARK ****************************** }
{   This is purely temporary for FPC because the Graph is SuperVGA you  }
{  have no mouse pointer on screen because the mouse drivers don't go   }
{  up to supporting SVGA modes. This simply makes a cross hair so you   }
{  can see the mouse for now..will be fixed soon.                       }
{ ****************************** END REMARK *** Leon de Boer, 04Nov99 * }
VAR LastX, LastY: Integer;

PROCEDURE ShowTheMouse; FAR;
BEGIN
   If (MouseEvents = True) AND (HideCount = 0)        { Mouse visible }
   Then Begin
     SetWriteMode(XORPut);                            { XOR write mode }
     SetColor(15);                                    { Set color to white }
     Line(LastX-5, LastY, LastX+5, LastY);            { Remove horz line  }
     Line(LastX, LastY-5, LastX, LastY+5);            { Remove vert line }
     LastX := MouseWhere.X;                           { Update x position }
     LastY := MouseWHere.Y;                           { Update y position }
     Line(LastX-5, LastY, LastX+5, LastY);            { Draw horz line }
     Line(LastX, LastY-5, LastX, LastY+5);            { Draw vert line }
     SetWriteMode(NormalPut);                         { Write mode to normal }
   End;
END;
{$ENDIF}
{$ENDIF}

{---------------------------------------------------------------------------}
{  InitVideo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Nov99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE InitVideo;
VAR {$IFDEF OS_DOS} I, J: Integer; Ts: TextSettingsType; {$ENDIF}
    {$IFDEF OS_WINDOWS} Dc, Mem: HDc; TempFont: TLogFont; Tm: TTextmetric; {$ENDIF}
    {$IFDEF OS_OS2} Ts, Fs: Integer; Ps: HPs; Tm: FontMetrics; {$ENDIF}
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   I := Detect;                                       { Detect video card }
   J := 0;                                            { Zero select mode }
   InitGraph(I, J, '');                               { Initialize graphics }
   I := GetMaxX;                                      { Fetch max x size }
   J := GetMaxY;                                      { Fetch max y size }
   {$IFDEF PPC_FPC}                                   { FPC DOS COMPILER }
     ASM
       MOVW $7, %AX;                                  { Set function  id }
       MOVW $0, %CX;                                  { Clear register }
       MOVW I, %DX;                                   { Maximum x size }
       INT $0x33;                                     { Set mouse x movement }
       MOVW $8, %AX;                                  { Set function id }
       MOVW $0, %CX;                                  { Clear register }
       MOVW J, %DX;                                   { Maximum y size }
       INT $0x33;                                     { Set mouse y movement }
     END;
     Lock_Code(Pointer(@ShowTheMouse), 400);          { Lock cursor code }
     MouseMoveProc := ShowTheMouse;                   { Set move function }
     ShowMouseProc := ShowTheMouse;                   { Set show function }
     HideMouseProc := ShowTheMouse;                   { Set hide function }
   {$ELSE}                                            { OTHER DOS COMPILERS }
     ASM
       MOV AX, 7;                                     { Set function  id }
       XOR CX, CX;                                    { Clear register }
       MOV DX, I;                                     { Maximum x size }
       INT 33H;                                       { Set mouse x movement }
       MOV AX, 8;                                     { Set function id }
       XOR CX, CX;                                    { Clear register }
       MOV DX, J;                                     { Maximum y size }
       INT 33H;                                       { Set mouse y movement }
     END;
   {$ENDIF}
     SysScreenWidth := GetMaxX+1;                     { Max screen width }
     SysScreenHeight := GetMaxY+1;                    { Max screen height }

     If (DefFontHeight = 0) Then                      { Font height not set }
       J := SysScreenHeight DIV DefLineNum            { Approx font height }
       Else J := DefFontHeight;                       { Use set font height }
     I := J DIV (TextHeight('H')+4);                  { Approx magnification }
     If (I < 1) Then I := 1;                          { Must be 1 or above }
     GetTextSettings(Ts);                             { Get text style }
     SetTextStyle(Ts.Font, Ts.Direction, I);          { Set new font settings }
     SysFontWidth := TextWidth('H');                  { Transfer font width }
     SysFontHeight := TextHeight('H')+4;              { Transfer font height }
   {$ENDIF}
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     SysScreenWidth := GetSystemMetrics(
       SM_CXFullScreen)-GetSystemMetrics(SM_CXFrame); { Max screen width }
     SysScreenHeight := GetSystemMetrics(
       SM_CYFullScreen);                              { Max screen height }
     With TempFont Do Begin
       If (DefFontHeight = 0) Then Begin              { Font height not set }
         lfHeight := SysScreenHeight DIV DefLineNum;  { Best guess height }
       End Else lfHeight := -DefFontHeight;           { Specific font height }
       lfWidth         := 0;                          { No specific width }
       lfEscapement    := 0;                          { No specifics }
       lfOrientation   := 0;                          { Normal orientation }
       lfWeight        := DefFontWeight;              { Default font weight }
       lfItalic        := 0;                          { No italics }
       lfUnderline     := 0;                          { No underlines }
       lfStrikeOut     := 0;                          { No strikeouts }
       lfCharSet       := ANSI_CharSet;               { ANSI font set }
       lfOutPrecision  := Out_Default_Precis;         { Default precision out }
       lfClipPrecision := Clip_Default_Precis;        { Default clip precision }
       lfQuality       := Proof_Quality;              { Proof quality }
       lfPitchAndFamily:= Variable_Pitch OR
         Fixed_Pitch;                                 { Either fitch format }
       FillChar(lfFaceName, SizeOf(lfFaceName), #0);  { Clear memory area }
       Move(DefFontStyle[1], lfFacename,
         Length(DefFontStyle));                       { Transfer style name }
     End;
     DefGFVFont := CreateFontIndirect(TempFont);      { Create a default font }
     Dc := GetDc(0);                                  { Get screen context }
     Mem := CreateCompatibleDC(Dc);                   { Compatable context }
     SelectObject(Mem, DefGFVFont);                   { Select the font }
     {$IFDEF PPC_FPC}                                 { FPC WINDOWS COMPILER }
       GetTextMetrics(Mem, @Tm);                      { Get text metrics }
     {$ELSE}                                          { OTHER COMPILERS }
       GetTextMetrics(Mem, Tm);                       { Get text metrics }
     {$ENDIF}
     SysFontWidth := Tm.tmaveCharWidth+1;             { Ave char font width }
     SysFontHeight := Tm.tmHeight;                    { Ave char font height }
     DeleteDc(Mem);                                   { Destroy context }
     ReleaseDc(0, Dc);                                { Release context }
   {$ENDIF}
   {$IFDEF OS_OS2}                                    { OS2 CODE }
     Ts := WinQuerySysValue(HWND_Desktop,
       SV_CYTitleBar) + 2*WinQuerySysValue(HWND_Desktop,
       SV_CYSizeBorder);                              { Title size }
     Fs := 2*WinQuerySysValue(HWND_DeskTop,
       SV_CXSizeBorder);                              { Frame size }
     SysScreenWidth := WinQuerySysValue(HWND_Desktop,
       SV_CXFullScreen) - Fs;                         { Max screen width }
     SysScreenHeight := WinQuerySysValue(HWND_Desktop,
       SV_CYFullScreen) - Ts;                         { Max screen height }
     (*With DefGFVFont Do Begin
       usRecordLength := SizeOf(fAttrs);              { Structure size }
       fsSelection := $20;                              { Uses default selection }
       lMatch := 0;                                   { Does not force match }
       idRegistry := 0;                               { Uses default registry }
       usCodePage := 850;                             { Code-page 850 }
       If (DefFontHeight = 0) Then Begin              { Font height not set }
         lMaxBaselineExt := SysScreenHeight DIV DefLineNum;  { Best guess height }
       End Else lMaxBaselineExt := DefFontHeight;     { Specific font height }
       lAveCharWidth := 0;                            { Req font default width }
       fsType := 0;                                   { Uses default type }
       fsFontUse := fAttr_FontUse_Nomix;              { Doesn't mix with graphics }
       FillChar(szFaceName, SizeOf(szFaceName), #0);  { Clear memory area }
       Move(DefFontStyle[1], szFacename,
         Length(DefFontStyle));                       { Transfer style name }
     End;*)
     Ps := WinGetPS(HWND_Desktop);                    { Get desktop PS }
     (*GpiCreateLogFont(Ps, Nil, 1, DefGFVFont);*)        { Create the font }
     GpiQueryFontMetrics(Ps, SizeOf(Tm), Tm);         { Get text metrics }
     SysFontWidth := Tm.lAveCharWidth+1;              { Transfer font width }
     SysFontHeight := Tm.lMaxBaselineExt;             { Transfer font height }
    { SysFontheight := SysScreenheight DIV DefLineNum;}
     WinReleasePS(Ps);                                { Release desktop PS }
     DefPointer := WinQuerySysPointer(HWND_DESKTOP,
       SPTR_ARROW, False);                            { Hold default pointer }
   {$ENDIF}
   ScreenWidth := SysScreenWidth DIV SysFontWidth;    { Calc screen width }
   ScreenHeight := SysScreenHeight DIV SysFontHeight; { Calc screen height }
   SysScreenWidth := ScreenWidth * SysFontWidth;      { Actual width }
   SysScreenHeight := ScreenHeight * SysFontHeight;   { Actual height }
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
   Inc(SysScreenWidth, 2*GetSystemMetrics(SM_CXFrame)); { Max screen width }
   Inc(SysScreenHeight, GetSystemMetrics(SM_CYCaption)
     + GetSystemMetrics(SM_CYFrame));                 { Max screen height }
   {$ENDIF}
   {$IFDEF OS_OS2}                                    { OS2 CODE }
   Inc(SysScreenWidth, Fs);                           { Max screen width }
   Inc(SysScreenHeight, Ts);                          { Max screen height }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  DoneVideo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE DoneVideo;
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
     {$IFDEF PPC_FPC}
     MouseMoveProc := Nil;                            { Clr mouse move ptr }
     ShowMouseProc := Nil;                            { Clr show mouse ptr }
     HideMouseProc := Nil;                            { Clr hide mouse ptr }
     UnLock_Code(Pointer(@ShowTheMouse), 400);        { Unlock cursor code }
     {$ENDIF}
     CloseGraph;                                      { Close down graphics }
   {$ENDIF}
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     If (DefGFVFont <> 0) Then                        { Check font created }
       DeleteObject(DefGFVFont);                      { Delete the font }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  ClearScreen -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 04Jan97 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE ClearScreen;
BEGIN
END;

{---------------------------------------------------------------------------}
{  SetVideoMode -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Nov99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE SetVideoMode (Mode: Word);
BEGIN
   If (Mode > $100) Then DefLineNum := 50             { 50 line mode request }
     Else DefLineNum := 24;                           { Normal 24 line mode }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           ERROR CONTROL ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  InitSysError -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20May98 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE InitSysError;
BEGIN
   SysErrActive := True;                              { Set active flag }
END;

{---------------------------------------------------------------------------}
{  DoneSysError -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20May98 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE DoneSysError;
BEGIN
   SysErrActive := False;                             { Clear active flag }
END;

{---------------------------------------------------------------------------}
{  SystemError -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 20May98 LdB       }
{---------------------------------------------------------------------------}
FUNCTION SystemError (ErrorCode: Integer; Drive: Byte): Integer;
BEGIN
   If (FailSysErrors = False) Then Begin              { Check error ignore }

   End Else SystemError := 1;                         { Return 1 for ignored }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           STRING FORMAT ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  PrintStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Feb99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE PrintStr (CONST S: String);
{$IFNDEF OS_DOS} VAR Ts: String; {$ENDIF}
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   Write(S);                                          { Write to screen }
   {$ENDIF}
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
   Ts := S + #0;                                      { Make asciiz }
     {$IFNDEF PPC_SPEED}                              { NON SPEED COMPILER }
     MessageBox(0, @Ts[1], Nil, mb_Ok OR mb_IconStop);{ Display to screen }
     {$ELSE}                                          { SYBIL 2+ COMPILER }
     MessageBox(0, CString(@Ts[1]), Nil, mb_Ok OR
       mb_IconStop);                                  { Display to screen }
     {$ENDIF}
   {$ENDIF}
   {$IFDEF OS_OS2}                                    { OS2 CODE }
   Ts := S + #0;                                      { Make asciiz }
   WinMessageBox(0, 0, @Ts[1], Nil, mb_Ok OR
     0, mb_IconHand);                                 { Display to screen }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  FormatStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 13Jul99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE FormatStr (Var Result: String; CONST Format: String; Var Params);
TYPE TLongArray = Array[0..0] Of LongInt;
VAR ResultLength, FormatIndex, Justify, Wth: Byte; Fill: Char; S: String;

   FUNCTION LongToStr (L: Longint; Radix: Byte): String;
   CONST HexChars: Array[0..15] Of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7',
     '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   VAR I: LongInt; S: String; Sign: String[1];
   BEGIN
     LongToStr := '';                                 { Preset empty return }
     If (L < 0) Then Begin                            { If L is negative }
       Sign := '-';                                   { Sign is negative }
       L := Abs(L);                                   { Convert to positive }
     End Else Sign := '';                             { Sign is empty }
     S := '';                                         { Preset empty string }
     Repeat
       I := L MOD Radix;                              { Radix mod of value }
       S := HexChars[I] + S;                          { Add char to string }
       L := L DIV Radix;                              { Divid by radix }
     Until (L = 0);                                   { Until no remainder }
     LongToStr := Sign + S;                           { Return result }
   END;

   PROCEDURE HandleParameter (I : LongInt);
   BEGIN
     While (FormatIndex <= Length(Format)) Do Begin   { While length valid }
       While (Format[FormatIndex] <> '%') AND         { Param char not found }
       (FormatIndex <= Length(Format)) Do Begin       { Length still valid }
         Result[ResultLength+1] := Format[FormatIndex]; { Transfer character }
         Inc(ResultLength);                           { One character added }
         Inc(FormatIndex);                            { Next param char }
       End;
       If (FormatIndex < Length(Format)) AND          { Not last char and }
       (Format[FormatIndex] = '%') Then Begin         { '%' char found }
         Fill := ' ';                                 { Default fill char }
         Justify := 0;                                { Default justify }
         Wth := 0;                                    { Default 0=no width }
         Inc(FormatIndex);                            { Next character }
         If (Format[FormatIndex] = '0') Then
           Fill := '0';                               { Fill char to zero }
         If (Format[FormatIndex] = '-') Then Begin    { Optional just char }
           Justify := 1;                              { Right justify }
           Inc(FormatIndex);                          { Next character }
         End;
         While ((FormatIndex <= Length(Format)) AND   { Length still valid }
         (Format[FormatIndex] >= '0') AND
         (Format[FormatIndex] <= '9')) Do Begin       { Numeric character }
           Wth := Wth * 10;                           { Multiply x10 }
           Wth := Wth + Ord(Format[FormatIndex])-$30; { Add numeric value }
           Inc(FormatIndex);                          { Next character }
         End;
         If ((FormatIndex <= Length(Format)) AND      { Length still valid }
         (Format[FormatIndex] = '#')) Then Begin      { Parameter marker }
           Inc(FormatIndex);                          { Next character }
           HandleParameter(Wth);                      { Width is param idx }
         End;
         If (FormatIndex <= Length(Format)) Then Begin{ Length still valid }
           Case Format[FormatIndex] Of
             'c': S := Char(TLongArray(Params)[I]);  { Character parameter }
             'd': S := LongToStr(TLongArray(Params)[I],
               10);                                   { Decimal parameter }
             's': S := PString(TLongArray(Params)[I])^;{ String parameter }
             'x': S := LongToStr(TLongArray(Params)[I],
               16);                                   { Hex parameter }
             '%': Begin                               { Literal % }
               S := '%';                              { Set string }
               Inc(FormatIndex);                      { Next character }
               Move(S[1], Result[ResultLength+1], 1); { '%' char to result }
               Inc(ResultLength, Length(S));          { Inc result length }
               Continue;                              { Now continue }
             End;
           End;
           Inc(I);                                    { Next parameter }
           Inc(FormatIndex);                          { Next character }
           If (Wth > 0) Then Begin                    { Width control active }
             If (Length(S) > Wth) Then Begin          { We must shorten S }
               If (Justify=1) Then                    { Check right justify }
                 S := Copy(S, Length(S)-Wth+1, Wth)   { Take right side data }
                 Else S := Copy(S, 1, Wth);           { Take left side data }
             End Else Begin                           { We must pad out S }
               If (Justify=1) Then                    { Right justify }
                 While (Length(S) < Wth) Do
                   S := S+Fill Else                   { Right justify fill }
                 While (Length(S) < Wth) Do
                   S := Fill + S;                     { Left justify fill }
             End;
           End;
           Move(S[1], Result[ResultLength+1],
             Length(S));                              { Move data to result }
           ResultLength := ResultLength + Length(S);  { Adj result length }
         End;
       End;
     End;
   END;

BEGIN
   ResultLength := 0;                                 { Zero result length }
   FormatIndex := 1;                                  { Format index to 1 }
   HandleParameter(0);                                { Handle parameter }
   {$IFDEF PPC_DELPHI3}                               { DELPHI 3+ COMPILER }
   SetLength(Result, ResultLength);                   { Set string length }
   {$ELSE}                                            { OTHER COMPILERS }
   Result[0] := Chr(ResultLength);                    { Set string length }
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    NEW QUEUED EVENT HANDLER ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  PutEventInQueue -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Mar98 LdB   }
{---------------------------------------------------------------------------}
FUNCTION PutEventInQueue (Var Event: TEvent): Boolean;
BEGIN
   If (QueueCount < QueueMax) Then Begin              { Check room in queue }
     Queue[QueueHead] := Event;                       { Store event }
     Inc(QueueHead);                                  { Inc head position }
     If (QueueHead = QueueMax) Then QueueHead := 0;   { Roll to start check }
     Inc(QueueCount);                                 { Inc queue count }
     PutEventInQueue := True;                         { Return successful }
   End Else PutEventInQueue := False;                 { Return failure }
END;

{---------------------------------------------------------------------------}
{  NextQueuedEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17Mar98 LdB   }
{---------------------------------------------------------------------------}
PROCEDURE NextQueuedEvent(Var Event: TEvent);
BEGIN
   If (QueueCount > 0) Then Begin                     { Check queued event }
     Event := Queue[QueueTail];                       { Fetch next event }
     Inc(QueueTail);                                  { Inc tail position }
     If (QueueTail = QueueMax) Then QueueTail := 0;   { Roll to start check }
     Dec(QueueCount);                                 { Dec queue count }
   End Else Event.What := evNothing;                  { Return empty event }
END;

{***************************************************************************}
{                      UNIT INITIALIZATION ROUTINE                          }
{***************************************************************************}
BEGIN
   ButtonCount := DetectMouse;                        { Detect mouse }
   DetectVideo;                                       { Detect video }
   SaveExit := ExitProc;                              { Save old exit }
   ExitProc := @ExitDrivers;                          { Set new exit }
END.
