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

{$IFDEF GRAPH_API}                                    { GRAPH CODE }
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

{$IFDEF GRAPH_API}                                    { DOS CODE ONLY }
{---------------------------------------------------------------------------}
{                 DOS GRAPHICS SOLID FILL BAR AREA CONSTANT                 }
{---------------------------------------------------------------------------}
CONST
   SolidFill = Graph.SolidFill;
   LowAscii : boolean = true;

{$ELSE not GRAPH_API }
CONST
   SolidFill = 0;
{$ENDIF not GRAPH_API}


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

PROCEDURE SetColor(Color: Word);
PROCEDURE SetFillStyle (Pattern: Word; Color: Word);
PROCEDURE Bar (X1, Y1, X2, Y2: Integer);
PROCEDURE Line(X1, Y1, X2, Y2: Integer);
PROCEDURE Rectangle(X1, Y1, X2, Y2: Integer);
PROCEDURE OutTextXY(X,Y: Integer; TextString: String);

procedure GraphUpdateScreen(Force: Boolean);

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
{$ifdef USE_VIDEO_API}
   SysFontWidth   : Integer = 8;                      { System font width }
   SysFontHeight  : Integer = 16;                     { System font height }
   TextScreenWidth : Integer = 80;
   TextScreenHeight : Integer = 25;
{$endif USE_VIDEO_API}

{$ifdef DEBUG}
const
  WriteDebugInfo : boolean = false;
{$endif DEBUG}

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                               IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{$ifdef USE_VIDEO_API}
USES video;                                           { Standard unit }
{$ENDIF}

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
{$IFDEF GRAPH_API}                                    { GRAPH CODE }
   If TextMode Then
     WriteMode := Mode                                { Hold write mode }
     Else Graph.SetWriteMode(Mode);                   { Call graph proc }
{$ELSE not GRAPH_API}
     WriteMode := Mode;                               { Hold write mode }
{$ENDIF not GRAPH_API}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         VIEW PORT CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetViewSettings -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB }
{---------------------------------------------------------------------------}
PROCEDURE GetViewSettings (Var CurrentViewPort: ViewPortType; TextMode: Boolean);
{$IFDEF GRAPH_API}
VAR Ts: Graph.ViewPortType;
{$ENDIF GRAPH_API}
BEGIN
{$IFNDEF GRAPH_API}
   CurrentViewPort := ViewPort;                       { Textmode viewport }
{$ELSE  GRAPH_API}
   If TextMode Then CurrentViewPort := ViewPort       { Textmode viewport }
     Else Begin
       Graph.GetViewSettings(Ts);                     { Get graph settings }
       CurrentViewPort.X1 := Ts.X1;                   { Transfer X1 }
       CurrentViewPort.Y1 := Ts.Y1;                   { Transfer Y1 }
       CurrentViewPort.X2 := Ts.X2;                   { Transfer X2 }
       CurrentViewPort.Y2 := Ts.Y2;                   { Transfer Y2 }
       CurrentViewPort.Clip := Ts.Clip;               { Transfer clip mask }
     End;
{$ENDIF GRAPH_API}
END;

{---------------------------------------------------------------------------}
{  SetViewPort -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE SetViewPort (X1, Y1, X2, Y2: Integer; Clip, TextMode: Boolean);
BEGIN
{$IFDEF GRAPH_API}
   If TextMode Then Begin                             { TEXT MODE GFV }
{$ENDIF GRAPH_API}
     If (X1 < 0) Then X1 := 0;                        { X1 negative fix }
     If (X1 >SysScreenWidth) Then
       X1 := SysScreenWidth;                             { X1 off screen fix }
     If (Y1 < 0) Then Y1 := 0;                        { Y1 negative fix }
     If (Y1 > SysScreenHeight) Then
       Y1 := SysScreenHeight;                            { Y1 off screen fix }
     If (X2 < 0) Then X2 := 0;                        { X2 negative fix }
     If (X2 > SysScreenWidth) Then
       X2 := SysScreenWidth;                             { X2 off screen fix }
     If (Y2 < 0) Then Y2 := 0;                        { Y2 negative fix }
     If (Y2 > SysScreenHeight) Then
       Y2 := SysScreenHeight;                            { Y2 off screen fix }
     ViewPort.X1 := X1;                               { Set X1 port value }
     ViewPort.Y1 := Y1;                               { Set Y1 port value }
     ViewPort.X2 := X2;                               { Set X2 port value }
     ViewPort.Y2 := Y2;                               { Set Y2 port value }
     ViewPort.Clip := Clip;                           { Set port clip value }
{$ifdef DEBUG}
     If WriteDebugInfo then
       Writeln(stderr,'New ViewPort(',X1,',',Y1,',',X2,',',Y2,')');
{$endif DEBUG}
     Cxp := X1;                                       { Set current x pos }
     Cyp := Y1;                                       { Set current y pos }
{$IFDEF GRAPH_API}
   End Else Begin                                     { GRAPHICS MODE GFV }
     Graph.SetViewPort(X1, Y1, X2, Y2, Clip);         { Call graph proc }
     X1:=X1 div SysFontWidth;
     X2:=X2 div SysFontWidth;
     Y1:=Y1 div SysFontHeight;
     Y2:=Y2 div SysFontHeight;
     If (X1 < 0) Then X1 := 0;                        { X1 negative fix }
     If (X1 >SysScreenWidth) Then
       X1 := SysScreenWidth;                             { X1 off screen fix }
     If (Y1 < 0) Then Y1 := 0;                        { Y1 negative fix }
     If (Y1 > SysScreenHeight) Then
       Y1 := SysScreenHeight;                            { Y1 off screen fix }
     If (X2 < 0) Then X2 := 0;                        { X2 negative fix }
     If (X2 > SysScreenWidth) Then
       X2 := SysScreenWidth;                             { X2 off screen fix }
     If (Y2 < 0) Then Y2 := 0;                        { Y2 negative fix }
     If (Y2 > SysScreenHeight) Then
       Y2 := SysScreenHeight;                            { Y2 off screen fix }
     ViewPort.X1 := X1;                               { Set X1 port value }
     ViewPort.Y1 := Y1;                               { Set Y1 port value }
     ViewPort.X2 := X2;                               { Set X2 port value }
     ViewPort.Y2 := Y2;                               { Set Y2 port value }
     ViewPort.Clip := Clip;                           { Set port clip value }
     Cxp := X1;                                       { Set current x pos }
     Cyp := Y1;                                       { Set current y pos }
   End;
{$ENDIF GRAPH_API}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    GRAPHICS DEVICE CAPACITY ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetMaxX - Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB          }
{---------------------------------------------------------------------------}
FUNCTION GetMaxX (TextMode: Boolean): Integer;
BEGIN
{$IFDEF GRAPH_API}
   If TextMode Then
{$ENDIF GRAPH_API}
     GetMaxX := SysScreenWidth-1                         { Screen width }
{$IFDEF GRAPH_API}
     Else GetMaxX := Graph.GetMaxX;                   { Call graph func }
{$ENDIF GRAPH_API}
END;

{---------------------------------------------------------------------------}
{  GetMaxY - Platforms DOS/DPMI/WIN/NT/OS2 - Updated 05Dec2000 LdB          }
{---------------------------------------------------------------------------}
FUNCTION GetMaxY (TextMode: Boolean): Integer;
BEGIN
{$IFDEF GRAPH_API}
   If TextMode Then
{$ENDIF GRAPH_API}
     GetMaxY := SysScreenHeight-1                     { Screen height }
{$IFDEF GRAPH_API}
     Else GetMaxY := Graph.GetMaxY;                   { Call graph func }
{$ENDIF GRAPH_API}
END;

PROCEDURE SetColor(Color: Word);
BEGIN
{$IFDEF GRAPH_API}
   Graph.SetColor(Color);                             { Call graph proc }
{$ENDIF GRAPH_API}
END;

PROCEDURE SetFillStyle (Pattern: Word; Color: Word);
BEGIN
{$IFDEF GRAPH_API}
   Graph.SetFillStyle(Pattern, Color);                { Call graph proc }
{$ENDIF GRAPH_API}
END;

PROCEDURE Bar (X1, Y1, X2, Y2: Integer);
BEGIN
{$IFDEF GRAPH_API}
   Graph.Bar(X1, Y1, X2, Y2);                         { Call graph proc }
{$ENDIF GRAPH_API}
END;

PROCEDURE Line(X1, Y1, X2, Y2: Integer);
BEGIN
{$IFDEF GRAPH_API}
   Graph.Line(X1, Y1, X2, Y2);                        { Call graph proc }
{$ENDIF GRAPH_API}
END;

PROCEDURE Rectangle(X1, Y1, X2, Y2: Integer);
BEGIN
{$IFDEF GRAPH_API}
   Graph.Rectangle(X1, Y1, X2, Y2);                  { Call graph proc }
{$ENDIF GRAPH_API}
END;

PROCEDURE OutTextXY(X,Y: Integer; TextString: string);
BEGIN
{$IFDEF GRAPH_API}
   Graph.OutTextXY(X, Y, TextString);                 { Call graph proc }
{$ENDIF GRAPH_API}
END;



procedure GraphUpdateScreen(Force: Boolean);
var
   smallforce  : boolean;
   i,x,y : longint;
   ch : char;
   attr : byte;
   SavedColor,SavedBkColor : longint;
   CurColor,CurBkColor : longint;
   NextColor,NextBkColor : longint;
   StoreFillSettings: FillSettingsType;


begin
{$IFDEF GRAPH_API}
{$ifdef USE_VIDEO_API}
  if force then
   smallforce:=true
  else
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        orl     %ecx,%ecx
        jz      .Lno_update
        movb    $1,smallforce
.Lno_update:
     end;
   end;
  if SmallForce then
    begin
      SavedColor:=Graph.GetColor;
      SavedBkColor:=Graph.GetBkColor;
      CurColor:=SavedColor;
      CurBkColor:=SavedBkColor;
      Graph.GetFillSettings(StoreFillSettings);
      Graph.SetFillStyle(EmptyFill,0);
      for y := 0 to TextScreenHeight - 1 do
        begin
           for x := 0  to TextScreenWidth - 1 do
             begin
               i:=y*TextScreenWidth+x;
               if OldVideoBuf^[i]<>VideoBuf^[i] then
                 begin
                   ch:=chr(VideoBuf^[i] and $ff);
                   if ch<>#0 then
                     begin
                       Attr:=VideoBuf^[i] shr 8;
                       NextColor:=Attr and $f;
                       NextBkColor:=(Attr and $70) shr 4;
                       if NextBkColor<>CurBkColor then
                         begin
                           Graph.SetBkColor(NextBkColor);
                           CurBkColor:=NextBkColor;
                         end;

                       Graph.Bar(x*SysFontWidth,y*SysFontHeight,(x+1)*SysFontWidth,(y+1)*SysFontHeight);
                       if NextColor<>CurColor then
                         begin
                           Graph.SetColor(NextColor);
                           CurColor:=NextColor;
                         end;
                       { SetBkColor does change the palette index 0 entry...
                         which leads to troubles if we want to write in dark }
                       if (CurColor=0) then
                         begin
                           Graph.SetBkColor(0);
                           CurBkColor:=0;
                         end;
                       Graph.OutTextXY(x*SysFontWidth,y*SysFontHeight,ch);
                     end;
                   OldVideoBuf^[i]:=VideoBuf^[i];
                 end;
             end;
        end;
      Graph.SetFillStyle(StoreFillSettings.pattern,StoreFillSettings.color);
      Graph.SetColor(SavedColor);
      Graph.SetBkColor(SavedBkColor);
    end;
{$else not USE_VIDEO_API}
  RunError(219);
{$endif USE_VIDEO_API}
{$ENDIF GRAPH_API}
end;


END.
{
 $Log$
 Revision 1.13  2002-05-29 19:35:31  pierre
  * fix GraphUpdateScreen procedure

 Revision 1.12  2002/05/28 19:42:32  pierre
  * fix non graphic mode compilation

 Revision 1.11  2002/05/28 19:13:44  pierre
  + GraphUpdateScreen function

 Revision 1.10  2001/10/02 16:35:51  pierre
  * fix several problems, try to get the graph version to compile

 Revision 1.9  2001/05/31 21:40:10  pierre
  * some debug stuff changed

 Revision 1.8  2001/05/10 16:46:28  pierre
  + some improovements made

 Revision 1.7  2001/05/07 23:36:35  pierre
  NO_WINDOW cond removed

 Revision 1.6  2001/05/07 22:22:03  pierre
  * removed NO_WINDOW cond, added GRAPH_API

 Revision 1.5  2001/05/04 15:43:45  pierre
  * several more fixes

 Revision 1.4  2001/04/10 21:57:55  pierre
  + first adds for Use_API define

 Revision 1.3  2001/04/10 21:29:55  pierre
  * import of Leon de Boer's files

 Revision 1.2  2000/08/24 12:00:21  marco
  * CVS log and ID tags

}
