{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{          System independent GFV GRAPHICS UNIT            }
{                                                          }
{   Copyright (c) 1999, 2000 by Leon de Boer               }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@projectent.com.au - backup e-mail address      }
{                                                          }
{   This unit provides the interlink between the graphics  }
{   used in GFV and the graphics API for the different     }
{   operating systems.                                     }
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
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                 - Speed Pascal 1.0+       (32 Bit)       }
{                                                          }
{*****************[ REVISION HISTORY ]*********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------- }
{  1.00     26 Nov 99   Unit started from relocated code   }
{                       originally from views.pas          }
{  1.01     21 May 00   GetMaxX and GetMaxY added.         }
{  1.02     05 Dec 00   Fixed DOS/DPMI implementation.     }
{**********************************************************}

UNIT GFVGraph;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC} { FPC doesn't support these switches }
  {$F-} { Near far calls are okay }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$E+} { Emulation is on }
  {$N-} { No 80x87 code generation }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
USES Graph;                                           { Standard unit }
{$ENDIF}

{***************************************************************************}
{                              PUBLIC CONSTANTS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                          STANDARD COLOUR CONSTANTS                        }
{---------------------------------------------------------------------------}
CONST
   Black        = 0;                                  { Black }
   Blue         = 1;                                  { Blue }
   Green        = 2;                                  { Green }
   Cyan         = 3;                                  { Cyan }
   Red          = 4;                                  { Red }
   Magenta      = 5;                                  { Magenta }
   Brown        = 6;                                  { Brown }
   LightGray    = 7;                                  { Light grey }
   DarkGray     = 8;                                  { Dark grey }
   LightBlue    = 9;                                  { Light blue }
   LightGreen   = 10;                                 { Light green }
   LightCyan    = 11;                                 { Light cyan }
   LightRed     = 12;                                 { Light red }
   LightMagenta = 13;                                 { Light magenta }
   Yellow       = 14;                                 { Yellow }
   White        = 15;                                 { White }

{---------------------------------------------------------------------------}
{                            WRITE MODE CONSTANTS                           }
{---------------------------------------------------------------------------}
CONST
   NormalPut = 0;                                     { Normal overwrite }
   CopyPut   = 0;                                     { Normal put image }
   AndPut    = 1;                                     { AND colour write }
   OrPut     = 2;                                     { OR colour write }
   XorPut    = 3;                                     { XOR colour write }
   NotPut    = 4;                                     { NOT colour write }

{---------------------------------------------------------------------------}
{                          CLIP CONTROL CONSTANTS                           }
{---------------------------------------------------------------------------}
CONST
   ClipOn = True;                                     { Clipping on }
   ClipOff = False;                                   { Clipping off }

{---------------------------------------------------------------------------}
{                       VIDEO CARD DETECTION CONSTANTS                      }
{---------------------------------------------------------------------------}
CONST
   Detect = 0;                                        { Detect video }

{$IFDEF OS_DOS}                                       { DOS CODE ONLY }
{---------------------------------------------------------------------------}
{                 DOS GRAPHICS SOLID FILL BAR AREA CONSTANT                 }
{---------------------------------------------------------------------------}
CONST
   SolidFill = Graph.SolidFill;
{$ENDIF}

{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
{---------------------------------------------------------------------------}
{      WIN/NT STANDARD TColorRef CONSTANTS TO MATCH COLOUR CONSTANTS        }
{---------------------------------------------------------------------------}
CONST
   rgb_Black        = $00000000;                      { 0 = Black }
   rgb_Blue         = $007F0000;                      { 1 = Blue }
   rgb_Green        = $00007F00;                      { 2 = Green }
   rgb_Cyan         = $007F7F00;                      { 3 = Cyan }
   rgb_Red          = $0000007F;                      { 4 = Red }
   rgb_Magenta      = $007F7F00;                      { 5 = Magenta }
   rgb_Brown        = $00007F7F;                      { 6 = Brown }
   rgb_LightGray    = $00AFAFAF;                      { 7 = LightGray }
   rgb_DarkGray     = $004F4F4F;                      { 8 = DarkGray }
   rgb_LightBlue    = $00FF0000;                      { 9 = Light Blue }
   rgb_LightGreen   = $0000FF00;                      { 10 = Light Green }
   rgb_LightCyan    = $00FFFF00;                      { 11 = Light Cyan }
   rgb_LightRed     = $000000FF;                      { 12 = Light Red }
   rgb_LightMagenta = $00FFFF00;                      { 13 = Light Magenta }
   rgb_Yellow       = $0000FFFF;                      { 14 = Yellow }
   rgb_White        = $00FFFFFF;                      { 15 = White }
{$ENDIF}

{***************************************************************************}
{                          PUBLIC TYPE DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                        ViewPortType RECORD DEFINITION                     }
{---------------------------------------------------------------------------}
TYPE
   ViewPortType = PACKED RECORD
     X1, Y1, X2, Y2: Integer;                         { Corners of viewport }
     Clip          : Boolean;                         { Clip status }
   END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                     GRAPHICS MODE CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-SetWriteMode-------------------------------------------------------
Sets the current write mode constant all subsequent draws etc. are
then via the set mode.
26Nov99 LdB
---------------------------------------------------------------------}
PROCEDURE SetWriteMode (Mode: Byte; TextMode: Boolean);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         VIEWPORT CONTROL ROUTINES                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetViewSettings----------------------------------------------------
Returns the current viewport and clip parameters in the variable.
26Nov99 LdB
---------------------------------------------------------------------}
PROCEDURE GetViewSettings (Var CurrentViewPort: ViewPortType; TextMode: Boolean);

{-SetViewPort--------------------------------------------------------
Set the current viewport and clip parameters to that requested.
26Nov99 LdB
---------------------------------------------------------------------}
PROCEDURE SetViewPort (X1, Y1, X2, Y2: Integer; Clip, TextMode: Boolean);


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    GRAPHICS DEVICE CAPACITY ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetMaxX------------------------------------------------------------
Returns X coordinate of maximum value that can be entered in any
graphics routine, that is the actual screen width in pixels - 1.
21May2000 LdB
---------------------------------------------------------------------}
FUNCTION GetMaxX (TextMode: Boolean): Integer;

{-GetMaxY------------------------------------------------------------
Returns Y coordinate of maximum value that can be entered in any
graphics routine, that is the actual screen height in pixels - 1.
21May2000 LdB
---------------------------------------------------------------------}
FUNCTION GetMaxY (TextMode: Boolean): Integer;

{$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
PROCEDURE SetColor(Color: Word);
PROCEDURE SetFillStyle (Pattern: Word; Color: Word);
PROCEDURE Bar (X1, Y1, X2, Y2: Integer);
PROCEDURE Line(X1, Y1, X2, Y2: Integer);
PROCEDURE Rectangle(X1, Y1, X2, Y2: Integer);
PROCEDURE OutTextXY(X,Y: Integer; TextString: String);
{$ENDIF}

{***************************************************************************}
{                        INITIALIZED PUBLIC VARIABLES                       }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                INITIALIZED DOS/DPMI/WIN/NT/OS2 VARIABLES                  }
{---------------------------------------------------------------------------}
CONST
   WriteMode      : Byte = 0;                         { Current write mode }
   SysScreenWidth : Integer = 640;                    { Default screen width }
   SysScreenHeight: Integer = 480;                    { Default screen height}

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                               IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{***************************************************************************}
{                      PRIVATE INITIALIZED VARIABLES                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{               DOS/DPMI/WIN/NT/OS2 INITIALIZED VARIABLES                   }
{---------------------------------------------------------------------------}
CONST
   FillCol : Integer = 0;
   Cxp     : Integer = 0;                             { Current x position }
   Cyp     : Integer = 0;                             { Current y position }
   ViewPort: ViewPortType = (X1:0; Y1:0; X2: 639;
                             Y2: 479; Clip: True);    { Default viewport }

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                     GRAPHICS MODE CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  SetWriteMode -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE SetWriteMode (Mode: Byte; TextMode: Boolean);
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   If TextMode Then WriteMode := Mode                 { Hold write mode }
     Else Graph.SetWriteMode(Mode);                   { Call graph proc }
   {$ELSE}                                            { WIN/NT/OS2 CODE }
   WriteMode := Mode;                                 { Hold writemode value }
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         VIEW PORT CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetViewSettings -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB }
{---------------------------------------------------------------------------}
PROCEDURE GetViewSettings (Var CurrentViewPort: ViewPortType; TextMode: Boolean);
{$IFDEF OS_DOS} VAR Ts: Graph.ViewPortType;{$ENDIF}   { DOS/DPMI CODE }
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   If TextMode Then CurrentViewPort := ViewPort       { Textmode viewport }
     Else Begin
       Graph.GetViewSettings(Ts);                     { Get graph settings }
       CurrentViewPort.X1 := Ts.X1;                   { Transfer X1 }
       CurrentViewPort.Y1 := Ts.Y1;                   { Transfer Y1 }
       CurrentViewPort.X2 := Ts.X2;                   { Transfer X2 }
       CurrentViewPort.Y2 := Ts.Y2;                   { Transfer Y2 }
       CurrentViewPort.Clip := Ts.Clip;               { Transfer clip mask }
     End;
   {$ELSE}                                            { WIN/NT/OS2 CODE }
   CurrentViewPort := ViewPort;                       { Return view port }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  SetViewPort -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE SetViewPort (X1, Y1, X2, Y2: Integer; Clip, TextMode: Boolean);
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   If (TextMode = TRUE) Then Begin                    { TEXT MODE GFV }
   {$ENDIF}
     If (X1 < 0) Then X1 := 0;                        { X1 negative fix }
     If (X1 > SysScreenWidth) Then
       X1 := SysScreenWidth;                          { X1 off screen fix }
     If (Y1 < 0) Then Y1 := 0;                        { Y1 negative fix }
     If (Y1 > SysScreenHeight) Then
       Y1 := SysScreenHeight;                         { Y1 off screen fix }
     If (X2 < 0) Then X2 := 0;                        { X2 negative fix }
     If (X2 > SysScreenWidth) Then
       X2 := SysScreenWidth;                          { X2 off screen fix }
     If (Y2 < 0) Then Y2 := 0;                        { Y2 negative fix }
     If (Y2 > SysScreenHeight) Then
       Y2 := SysScreenHeight;                         { Y2 off screen fix }
     ViewPort.X1 := X1;                               { Set X1 port value }
     ViewPort.Y1 := Y1;                               { Set Y1 port value }
     ViewPort.X2 := X2;                               { Set X2 port value }
     ViewPort.Y2 := Y2;                               { Set Y2 port value }
     ViewPort.Clip := Clip;                           { Set port clip value }
     Cxp := X1;                                       { Set current x pos }
     Cyp := Y1;                                       { Set current y pos }
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   End Else Begin                                     { GRAPHICS MODE GFV }
     Graph.SetViewPort(X1, Y1, X2, Y2, Clip);         { Call graph proc }
   End;
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    GRAPHICS DEVICE CAPACITY ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetMaxX - Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB          }
{---------------------------------------------------------------------------}
FUNCTION GetMaxX (TextMode: Boolean): Integer;
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   If TextMode Then GetMaxX := SysScreenWidth-1       { Screen width }
     Else GetMaxX := Graph.GetMaxX;                   { Call graph func }
   {$ELSE}                                            { WIN/NT/OS2 CODE }
   GetMaxX := SysScreenWidth-1;                       { Screen width }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  GetMaxY - Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB          }
{---------------------------------------------------------------------------}
FUNCTION GetMaxY (TextMode: Boolean): Integer;
BEGIN
   {$IFDEF OS_DOS}                                    { DOS/DPMI CODE }
   If TextMode Then GetMaxY := SysScreenHeight-1      { Screen height }
     Else GetMaxY := Graph.GetMaxY;                   { Call graph func }
   {$ELSE}                                            { WIN/NT/OS2 CODE }
   GetMaxY := SysScreenHeight-1;                      { Screen height }
   {$ENDIF}
END;

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
PROCEDURE SetColor(Color: Word);
BEGIN
   Graph.SetColor(Color);                             { Call graph proc }
END;

PROCEDURE SetFillStyle (Pattern: Word; Color: Word);
BEGIN
   Graph.SetFillStyle(Pattern, Color);                { Call graph proc }
END;

PROCEDURE Bar (X1, Y1, X2, Y2: Integer);
BEGIN
   Graph.Bar(X1, Y1, X2, Y2);                         { Call graph proc }
END;

PROCEDURE Line(X1, Y1, X2, Y2: Integer);
BEGIN
   Graph.Line(X1, Y1, X2, Y2);                        { Call graph proc }
END;

PROCEDURE Rectangle(X1, Y1, X2, Y2: Integer);
BEGIN
   Graph.Rectangle(X1, Y1, X2, Y2);                  { Call graph proc }
END;

PROCEDURE OutTextXY(X,Y: Integer; TextString: string);
BEGIN
   Graph.OutTextXY(X, Y, TextString);                 { Call graph proc }
END;
{$ENDIF}

END.

{
 $Log$
 Revision 1.3  2001-04-10 21:29:55  pierre
  * import of Leon de Boer's files

 Revision 1.2  2000/08/24 12:00:21  marco
  * CVS log and ID tags


}
