{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{          System independent GFV GRAPHICS UNIT            }
{                                                          }
{   Copyright (c) 1999 by Leon de Boer                     }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@starwon.com.au - backup e-mail address         }
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
{*****************[ REVISION HISTORY ]*********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------- }
{  1.00     26 Nov 99   Unit started from relocated code   }
{                       originally from views.pas          }
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

{---------------------------------------------------------------------------}
{                        TEXT JUSTIFICATION CONSTANTS                       }
{---------------------------------------------------------------------------}
CONST
   LeftText   = 0;                                    { Left justify }
   CenterText = 1;                                    { Centre justify }
   RightText  = 2;                                    { Right justify }
   BottomText = 0;                                    { Bottom justify }
   TopText    = 2;                                    { Top justify }

{---------------------------------------------------------------------------}
{                           FILL PATTERN CONSTANTS                          }
{---------------------------------------------------------------------------}
CONST
   EmptyFill      = 0;                                { No fill pattern }
   SolidFill      = 1;                                { Solid colour }
   LineFill       = 2;                                { Line fill }
   LtSlashFill    = 3;                                { Fwd slash line type }
   SlashFill      = 4;                                { Fwd slash pattern }
   BkSlashFill    = 5;                                { Back slash pattern }
   LtBkSlashFill  = 6;                                { Back slash line type }
   HatchFill      = 7;                                { Hatch pattern }
   XHatchFill     = 8;                                { Cross hatch pattern }
   InterleaveFill = 9;                                { Interleaved pattern }
   WideDotFill    = 10;                               { Wide dot pattern }
   CloseDotFill   = 11;                               { Close dot pattern }
   UserFill       = 12;                               { User defined fill }

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
PROCEDURE SetWriteMode (Mode: Byte);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         VIEWPORT CONTROL ROUTINES                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetViewSettings----------------------------------------------------
Returns the current viewport and clip parameters in the variable.
26Nov99 LdB
---------------------------------------------------------------------}
PROCEDURE GetViewSettings (Var CurrentViewPort: ViewPortType);

{-SetViewPort--------------------------------------------------------
Set the current viewport and clip parameters to that requested.
26Nov99 LdB
---------------------------------------------------------------------}
PROCEDURE SetViewPort (X1, Y1, X2, Y2: Integer; Clip: Boolean);

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
{  SetWriteMode -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 09Aug99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE SetWriteMode (Mode: Byte);
BEGIN
   WriteMode := Mode;                                 { Hold writemode value }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         VIEW PORT CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetViewSettings -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 09Aug99 LdB   }
{---------------------------------------------------------------------------}
PROCEDURE GetViewSettings (Var CurrentViewPort: ViewPortType);
BEGIN
   CurrentViewPort := ViewPort;                       { Return view port }
END;

{---------------------------------------------------------------------------}
{  SetViewPort -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 09Aug99 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE SetViewPort (X1, Y1, X2, Y2: Integer; Clip: Boolean);
BEGIN
   If (X1 < 0) Then X1 := 0;                          { X1 negative fix }
   If (X1 > SysScreenWidth) Then
     X1 := SysScreenWidth;                            { X1 off screen fix }
   If (Y1 < 0) Then Y1 := 0;                          { Y1 negative fix }
   If (Y1 > SysScreenHeight) Then
     Y1 := SysScreenHeight;                           { Y1 off screen fix }
   If (X2 < 0) Then X2 := 0;                          { X2 negative fix }
   If (X2 > SysScreenWidth) Then X2 := SysScreenWidth;{ X2 off screen fix }
   If (Y2 < 0) Then Y2 := 0;                          { Y2 negative fix }
   If (Y2 > SysScreenHeight) Then
     Y2 := SysScreenHeight;                           { Y2 off screen fix }
   ViewPort.X1 := X1;                                 { Set X1 port value }
   ViewPort.Y1 := Y1;                                 { Set Y1 port value }
   ViewPort.X2 := X2;                                 { Set X2 port value }
   ViewPort.Y2 := Y2;                                 { Set Y2 port value }
   ViewPort.Clip := Clip;                             { Set port clip value }
   Cxp := X1;                                         { Set current x pos }
   Cyp := Y1;                                         { Set current y pos }
END;

END.

{
 $Log$
 Revision 1.2  2000-08-24 12:00:21  marco
  * CVS log and ID tags


}