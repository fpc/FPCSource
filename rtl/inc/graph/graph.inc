{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Graph unit implementation part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef logging}
var debuglog: text;

function strf(l: longint): string;
begin
  str(l, strf)
end;

Procedure Log(Const s: String);
Begin
  Append(debuglog);
  Write(debuglog, s);
  Close(debuglog);
End;

Procedure LogLn(Const s: string);
Begin
  Append(debuglog);
  Writeln(debuglog,s);
  Close(debuglog);
End;
{$endif logging}

const
   StdBufferSize = 4096;   { Buffer size for FloodFill }

type


  tinttable = array[0..16383] of smallint;
  pinttable = ^tinttable;

  WordArray = Array [0..StdbufferSize] Of word;
  PWordArray = ^WordArray;


const
   { Mask for each bit in byte used to determine pattern }
   BitArray: Array[0..7] of byte =
     ($01,$02,$04,$08,$10,$20,$40,$80);
   RevbitArray: Array[0..7] of byte =
     ($80,$40,$20,$10,$08,$04,$02,$01);

    { pre expanded line patterns }
    { 0 = LSB of byte pattern    }
    { 15 = MSB of byte pattern   }
    LinePatterns: Array[0..15] of BOOLEAN =
     (TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
      TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE);

const
  BGIPath : string = '.';


  { Default font 8x8 system from IBM PC }
  {$i fontdata.inc}



var
  CurrentColor:     Word;
  CurrentBkColor: Word;
  CurrentX : smallint;   { viewport relative }
  CurrentY : smallint;   { viewport relative }

  ClipPixels: Boolean;  { Should cliiping be enabled }


  CurrentWriteMode: smallint;


  _GraphResult : smallint;


  LineInfo : LineSettingsType;
  FillSettings: FillSettingsType;

  { information for Text Output routines }
  CurrentTextInfo : TextSettingsType;
  CurrentXRatio, CurrentYRatio: graph_float;
  installedfonts: longint;  { Number of installed fonts }


  StartXViewPort: smallint; { absolute }
  StartYViewPort: smallint; { absolute }
  ViewWidth : smallint;
  ViewHeight: smallint;


  IsGraphMode : Boolean; { Indicates if we are in graph mode or not }


  ArcCall: ArcCoordsType;   { Information on the last call to Arc or Ellipse }


var

  { ******************** HARDWARE INFORMATION ********************* }
  { Should be set in InitGraph once only.                           }
  IntCurrentMode : smallint;
  IntCurrentDriver : smallint;       { Currently loaded driver          }
  XAspect : word;
  YAspect : word;
  MaxX : smallint;       { Maximum resolution - ABSOLUTE }
  MaxY : smallint;       { Maximum resolution - ABSOLUTE }
  MaxColor : Longint;
  PaletteSize : longint; { Maximum palette entry we can set, usually equal}
                         { maxcolor.                                      }
  HardwarePages : byte;  { maximum number of hardware visual pages        }
  DriverName: String;
  DirectColor : Boolean ; { Is it a direct color mode? }
  ModeList : PModeInfo;
  DirectVideo : Boolean;  { Direct access to video memory? }




{--------------------------------------------------------------------------}
{                                                                          }
{                    LINE AND LINE RELATED ROUTINES                        }
{                                                                          }
{--------------------------------------------------------------------------}

  {$i clip.inc}

  procedure HLineDefault(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    xtmp: smallint;
   Begin

    { must we swap the values? }
    if x >= x2 then
      Begin
        xtmp := x2;
        x2 := x;
        x:= xtmp;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    X2  := X2 + StartXViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for x:= x to x2 do
      DirectPutPixel(X,Y);
   end;


  procedure VLineDefault(x,y,y2: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    ytmp: smallint;
  Begin
    { must we swap the values? }
    if y >= y2 then
     Begin
       ytmp := y2;
       y2 := y;
       y:= ytmp;
     end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    Y2  := Y2 + StartYViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for y := y to y2 do Directputpixel(x,y)
  End;

  Procedure DirectPutPixelClip(x,y: smallint);
  { for thickwidth lines, because they may call DirectPutPixel for coords }
  { outside the current viewport (bug found by CEC)                       }
  Begin
    If (Not ClipPixels) Or
       ((X >= StartXViewPort) And (X <= (StartXViewPort + ViewWidth)) And
        (Y >= StartYViewPort) And (Y <= (StartYViewPort + ViewHeight))) then
      Begin
        DirectPutPixel(x,y)
      End
  End;

  procedure LineDefault(X1, Y1, X2, Y2: smallint); {$ifndef fpc}far;{$endif fpc}

  var X, Y :           smallint;
      deltax, deltay : smallint;
      d, dinc1, dinc2: smallint;
      xinc1          : smallint;
      xinc2          : smallint;
      yinc1          : smallint;
      yinc2          : smallint;
      i              : smallint;
      Flag           : Boolean; { determines pixel direction in thick lines }
      NumPixels      : smallint;
      PixelCount     : smallint;
      OldCurrentColor: Word;
      swtmp          : smallint;
      TmpNumPixels   : smallint;
 begin
{******************************************}
{  SOLID LINES                             }
{******************************************}
  if lineinfo.LineStyle = SolidLn then
    Begin
       { we separate normal and thick width for speed }
       { and because it would not be 100% compatible  }
       { with the TP graph unit otherwise             }
       if y1 = y2 then
        Begin
     {******************************************}
     {  SOLID LINES HORIZONTAL                  }
     {******************************************}
          if lineinfo.Thickness=NormWidth then
            hline(x1,x2,y2)
          else
            begin
               { thick width }
               hline(x1,x2,y2-1);
               hline(x1,x2,y2);
               hline(x2,x2,y2+1);
            end;
        end
    else
    if x1 = x2 then
        Begin
     {******************************************}
     {  SOLID LINES VERTICAL                    }
     {******************************************}
          if lineinfo.Thickness=NormWidth then
            vline(x1,y1,y2)
          else
            begin
            { thick width }
              vline(x1-1,y1,y2);
              vline(x1,y1,y2);
              vline(x1+1,y1,y2);
            end;
        end
    else
    begin
     { Convert to global coordinates. }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y1 := y1 + StartYViewPort;
     y2 := y2 + StartYViewPort;
     { if fully clipped then exit... }
     if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;
     {******************************************}
     {  SLOPED SOLID LINES                      }
     {******************************************}
           oldCurrentColor :=
           CurrentColor;
           { Calculate deltax and deltay for initialisation }
           deltax := abs(x2 - x1);
           deltay := abs(y2 - y1);

          { Initialize all vars based on which is the independent variable }
          if deltax >= deltay then
            begin

             Flag := FALSE;
             { x is independent variable }
             numpixels := deltax + 1;
             d := (2 * deltay) - deltax;
             dinc1 := deltay Shl 1;
             dinc2 := (deltay - deltax) shl 1;
             xinc1 := 1;
             xinc2 := 1;
             yinc1 := 0;
             yinc2 := 1;
            end
          else
            begin

             Flag := TRUE;
             { y is independent variable }
             numpixels := deltay + 1;
             d := (2 * deltax) - deltay;
             dinc1 := deltax Shl 1;
             dinc2 := (deltax - deltay) shl 1;
             xinc1 := 0;
             xinc2 := 1;
             yinc1 := 1;
             yinc2 := 1;
            end;

         { Make sure x and y move in the right directions }
         if x1 > x2 then
           begin
            xinc1 := - xinc1;
            xinc2 := - xinc2;
           end;
         if y1 > y2 then
          begin
           yinc1 := - yinc1;
           yinc2 := - yinc2;
          end;

         { Start drawing at <x1, y1> }
         x := x1;
         y := y1;


         If LineInfo.Thickness=NormWidth then

          Begin

            { Draw the pixels }
            for i := 1 to numpixels do
              begin
                DirectPutPixel(x, y);
                if d < 0 then
                  begin
                   d := d + dinc1;
                   x := x + xinc1;
                   y := y + yinc1;
                  end
                else
                  begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                  end;
                  CurrentColor := OldCurrentColor;
             end;
          end
        else
         { Thick width lines }
          begin
            { Draw the pixels }
             for i := 1 to numpixels do
               begin
                { all depending on the slope, we can determine         }
                { in what direction the extra width pixels will be put }
                If Flag then
                  Begin
                    DirectPutPixelClip(x-1,y);
                    DirectPutPixelClip(x,y);
                    DirectPutPixelClip(x+1,y);
                  end
                else
                  Begin
                    DirectPutPixelClip(x, y-1);
                    DirectPutPixelClip(x, y);
                    DirectPutPixelClip(x, y+1);
                  end;
                if d < 0 then
                  begin
                    d := d + dinc1;
                    x := x + xinc1;
                    y := y + yinc1;
                  end
                else
                  begin
                    d := d + dinc2;
                    x := x + xinc2;
                    y := y + yinc2;
                  end;
                CurrentColor := OldCurrentColor;
               end;
          end;
        end;
  end
   else
{******************************************}
{  begin patterned lines                   }
{******************************************}
    Begin
      { Convert to global coordinates. }
      x1 := x1 + StartXViewPort;
      x2 := x2 + StartXViewPort;
      y1 := y1 + StartYViewPort;
      y2 := y2 + StartYViewPort;
      { if fully clipped then exit... }
      if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;

      OldCurrentColor := CurrentColor;
      PixelCount:=0;
      if y1 = y2 then
            Begin
             { Check if we must swap }
         if x1 >= x2 then
               Begin
                 swtmp := x1;
                 x1 := x2;
                 x2 := swtmp;
               end;
         if LineInfo.Thickness = NormWidth then
              Begin
               for PixelCount:=x1 to x2 do
                     { optimization: PixelCount mod 16 }
                     if LinePatterns[PixelCount and 15] = TRUE then
                      begin
                        DirectPutPixel(PixelCount,y2);
                      end;
              end
             else
              Begin
               for i:=-1 to 1 do
                     Begin
                       for PixelCount:=x1 to x2 do
                         { Optimization from Thomas - mod 16 = and 15 }
                         {this optimization has been performed by the compiler
                          for while as well (JM)}
                         if LinePatterns[PixelCount and 15] = TRUE then
                           begin
                                 DirectPutPixelClip(PixelCount,y2+i);
                           end;
                     end;
              end;
        end
      else
      if x1 = x2 then
           Begin
            { Check if we must swap }
            if y1 >= y2 then
              Begin
                swtmp := y1;
                y1 := y2;
                y2 := swtmp;
              end;
            if LineInfo.Thickness = NormWidth then
              Begin
                for PixelCount:=y1 to y2 do
                    { compare if we should plot a pixel here , compare }
                    { with predefined line patterns...                 }
                    if LinePatterns[PixelCount and 15] = TRUE then
                      begin
                    DirectPutPixel(x1,PixelCount);
                      end;
              end
            else
              Begin
                for i:=-1 to 1 do
                     Begin
                       for PixelCount:=y1 to y2 do
                       { compare if we should plot a pixel here , compare }
                       { with predefined line patterns...                 }
                         if LinePatterns[PixelCount and 15] = TRUE then
                           begin
                             DirectPutPixelClip(x1+i,PixelCount);
                           end;
                     end;
              end;
           end
      else
           Begin
             oldCurrentColor := CurrentColor;
             { Calculate deltax and deltay for initialisation }
             deltax := abs(x2 - x1);
             deltay := abs(y2 - y1);

             { Initialize all vars based on which is the independent variable }
             if deltax >= deltay then
               begin

                 Flag := FALSE;
                 { x is independent variable }
                 numpixels := deltax + 1;
                 d := (2 * deltay) - deltax;
                 dinc1 := deltay Shl 1;
                 dinc2 := (deltay - deltax) shl 1;
                 xinc1 := 1;
                 xinc2 := 1;
                 yinc1 := 0;
                 yinc2 := 1;
              end
            else
              begin

                Flag := TRUE;
                { y is independent variable }
                numpixels := deltay + 1;
                d := (2 * deltax) - deltay;
                dinc1 := deltax Shl 1;
                dinc2 := (deltax - deltay) shl 1;
                xinc1 := 0;
                xinc2 := 1;
                yinc1 := 1;
                yinc2 := 1;
              end;

            { Make sure x and y move in the right directions }
            if x1 > x2 then
              begin
                xinc1 := - xinc1;
                xinc2 := - xinc2;
              end;
            if y1 > y2 then
              begin
                yinc1 := - yinc1;
                yinc2 := - yinc2;
              end;

            { Start drawing at <x1, y1> }
            x := x1;
            y := y1;

            If LineInfo.Thickness=ThickWidth then

             Begin
               TmpNumPixels := NumPixels-1;
               { Draw the pixels }
               for i := 0 to TmpNumPixels do
                 begin
                     { all depending on the slope, we can determine         }
                     { in what direction the extra width pixels will be put }
                       If Flag then
                          Begin
                            { compare if we should plot a pixel here , compare }
                            { with predefined line patterns...                 }
                            if LinePatterns[i and 15] = TRUE then
                              begin
                                DirectPutPixelClip(x-1,y);
                                DirectPutPixelClip(x,y);
                                DirectPutPixelClip(x+1,y);
                              end;
                          end
                       else
                          Begin
                            { compare if we should plot a pixel here , compare }
                            { with predefined line patterns...                 }
                            if LinePatterns[i and 15] = TRUE then
                             begin
                               DirectPutPixelClip(x,y-1);
                               DirectPutPixelClip(x,y);
                               DirectPutPixelClip(x,y+1);
                             end;
                          end;
                   if d < 0 then
                         begin
                           d := d + dinc1;
                           x := x + xinc1;
                           y := y + yinc1;
                         end
                   else
                         begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                         end;
                end;
            end
           else
            Begin
             { instead of putting in loop , substract by one now }
             TmpNumPixels := NumPixels-1;
            { NormWidth }
             for i := 0 to TmpNumPixels do
             begin
                  if LinePatterns[i and 15] = TRUE then
                    begin
                          DirectPutPixel(x,y);
                    end;
             if d < 0 then
                 begin
                   d := d + dinc1;
                   x := x + xinc1;
                   y := y + yinc1;
                 end
             else
                 begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                 end;
             end;
            end
        end;
{******************************************}
{  end patterned lines                     }
{******************************************}
       { restore color }
       CurrentColor:=OldCurrentColor;
   end;
 end;  { Line }


  {********************************************************}
  { Procedure DummyPatternLine()                           }
  {--------------------------------------------------------}
  { This is suimply an procedure that does nothing which   }
  { can be passed as a patternlineproc for non-filled      }
  { ellipses                                               }
  {********************************************************}
  Procedure DummyPatternLine(x1, x2, y: smallint); {$ifdef tp} far; {$endif tp}
  begin
  end;


  {********************************************************}
  { Procedure InternalEllipse()                            }
  {--------------------------------------------------------}
  { This routine first calculates all points required to   }
  { draw a circle to the screen, and stores the points     }
  { to display in a buffer before plotting them. The       }
  { aspect ratio of the screen is taken into account when  }
  { calculating the values.                                }
  {--------------------------------------------------------}
  { INPUTS: X,Y : Center coordinates of Ellipse.           }
  {  XRadius - X-Axis radius of ellipse.                   }
  {  YRadius - Y-Axis radius of ellipse.                   }
  {  stAngle, EndAngle: Start angle and end angles of the  }
  {  ellipse (used for partial ellipses and circles)       }
  {  pl: procedure which either draws a patternline (for   }
  {      FillEllipse) or does nothing (arc etc)            }
  {--------------------------------------------------------}
  { NOTE: -                                                }
  {       -                                                }
  {********************************************************}

  Procedure InternalEllipseDefault(X,Y: smallint;XRadius: word;
    YRadius:word; stAngle,EndAngle: word; pl: PatternLineProc); {$ifndef fpc}far;{$endif fpc}
   Const ConvFac = Pi/180.0;

   var
    j, Delta, DeltaEnd: graph_float;
    NumOfPixels: longint;
    TempTerm: graph_float;
    xtemp, ytemp, xp, yp, xm, ym, xnext, ynext,
      plxpyp, plxmyp, plxpym, plxmym: smallint;
    BackupColor, TmpAngle, OldLineWidth: word;
  Begin
   If LineInfo.ThickNess = ThickWidth Then
    { first draw the two outer ellipses using normwidth and no filling (JM) }
     Begin
       OldLineWidth := LineInfo.Thickness;
       LineInfo.Thickness := NormWidth;
       InternalEllipseDefault(x,y,XRadius,YRadius,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       InternalEllipseDefault(x,y,XRadius+1,YRadius+1,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       If (XRadius > 0) and (YRadius > 0) Then
         { draw the smallest ellipse last, since that one will use the }
         { original pl, so it could possibly draw patternlines (JM)    }
         Begin
           Dec(XRadius);
           Dec(YRadius);
         End
       Else Exit;
       { restore line thickness }
       LineInfo.Thickness := OldLineWidth;
     End;
   { Adjust for screen aspect ratio }
   XRadius:=(longint(XRadius)*10000) div XAspect;
   YRadius:=(longint(YRadius)*10000) div YAspect;
   If xradius = 0 then inc(xradius);
   if yradius = 0 then inc(yradius);
   { check for an ellipse with negligable x and y radius }
   If (xradius <= 1) and (yradius <= 1) then
     begin
       putpixel(x,y,CurrentColor);
       ArcCall.X := X;
       ArcCall.Y := Y;
       ArcCall.XStart := X;
       ArcCall.YStart := Y;
       ArcCall.XEnd := X;
       ArcCall.YEnd := Y;
       exit;
     end;
   { check if valid angles }
   stangle := stAngle mod 361;
   EndAngle := EndAngle mod 361;
   { if impossible angles then swap them! }
   if Endangle < StAngle then
     Begin
       TmpAngle:=EndAngle;
       EndAngle:=StAngle;
       Stangle:=TmpAngle;
     end;
   { approximate the number of pixels required by using the circumference }
   { equation of an ellipse.                                              }
   { Changed this formula a it (trial and error), but the net result is that }
   { less pixels have to be calculated now                                   }
   NumOfPixels:=Round(Sqrt(3)*sqrt(sqr(XRadius)+sqr(YRadius)));
   { Calculate the angle precision required }
   Delta := 90.0 / NumOfPixels;
   { for restoring after PatternLine }
   BackupColor := CurrentColor;
   { removed from inner loop to make faster }
   { store some arccall info }
   ArcCall.X := X;
   ArcCall.Y := Y;
   TempTerm := (StAngle)*ConvFac;
   ArcCall.XStart := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YStart := round(YRadius*Sin(TempTerm+Pi)) + Y;
   TempTerm := (EndAngle)*ConvFac;
   ArcCall.XEnd := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YEnd := round(YRadius*Sin(TempTerm+Pi)) + Y;
   { Always just go over the first 90 degrees. Could be optimized a   }
   { bit if StAngle and EndAngle lie in the same quadrant, left as an }
   { exercise for the reader :) (JM)                                  }
   j := 0;
   { calculate stop position, go 1 further than 90 because otherwise }
   { 1 pixel is sometimes not drawn (JM)                             }
   DeltaEnd := 91;
   { Calculate points }
   xnext := XRadius;
   ynext := 0;
   Repeat
     xtemp := xnext;
     ytemp := ynext;
     { this is used by both sin and cos }
     TempTerm := (j+Delta)*ConvFac;
     { Calculate points }
     xnext := round(XRadius*Cos(TempTerm));
     ynext := round(YRadius*Sin(TempTerm+Pi));

     xp := x + xtemp;
     xm := x - xtemp;
     yp := y + ytemp;
     ym := y - ytemp;
     plxpyp := maxsmallint;
     plxmyp := -maxsmallint-1;
     plxpym := maxsmallint;
     plxmym := -maxsmallint-1;
     If (j >= StAngle) and (j <= EndAngle) then
       begin
         plxpyp := xp;
         PutPixel(xp,yp,CurrentColor);
       end;
     If ((180-j) >= StAngle) and ((180-j) <= EndAngle) then
       begin
         plxmyp := xm;
         PutPixel(xm,yp,CurrentColor);
       end;
     If ((j+180) >= StAngle) and ((j+180) <= EndAngle) then
       begin
         plxmym := xm;
         PutPixel(xm,ym,CurrentColor);
       end;
     If ((360-j) >= StAngle) and ((360-j) <= EndAngle) then
       begin
         plxpym := xp;
         PutPixel(xp,ym,CurrentColor);
       end;
     If (ynext <> ytemp) and
        (xp - xm >= 1) then
       begin
         CurrentColor := FillSettings.Color;
         pl(plxmyp+1,plxpyp-1,yp);
         pl(plxmym+1,plxpym-1,ym);
         CurrentColor := BackupColor;
       end;
     j:=j+Delta;
   Until j > (DeltaEnd);
  end;

  {********************************************************}
  { Procedure InternalEllipse()                            }
  {--------------------------------------------------------}
  { This routine first calculates all points required to   }
  { draw a circle to the screen, and stores the points     }
  { to display in a buffer before plotting them. The       }
  { aspect ratio of the screen is taken into account when  }
  { calculating the values.                                }
  {--------------------------------------------------------}
  { INPUTS: X,Y : Center coordinates of Ellipse.           }
  {  XRadius - X-Axis radius of ellipse.                   }
  {  YRadius - Y-Axis radius of ellipse.                   }
  {  stAngle, EndAngle: Start angle and end angles of the  }
  {  ellipse (used for partial ellipses and circles)       }
  {--------------------------------------------------------}
  { NOTE: - uses the current write mode.                   }
  {       - Angles must both be between 0 and 360          }
  {********************************************************}
(*
Procedure InternalEllipseDefault (x, y : smallint;
    xradius, yradius, stAngle, EndAngle : Word; pl: PatternLineProc); {$ifndef fpc} far; {$endif fpc}
{ Draw an ellipse arc. Crude but it works (anyone have a better one?) }
Var
  aSqr, bSqr, twoaSqr, twobSqr, xa, ya, twoXbSqr, twoYaSqr, error : LongInt;
  Alpha, TempTerm : graph_float;
  BackupColor: Word;
  plxpyp, plxmyp, plxpym, plxmym: smallint;
const
  RadToDeg = 180/Pi;


Procedure PlotPoints;

var
 i,j: smallint;
 xm, ym: smallint;
 xp, yp: smallint;
Begin
   ym := y-ya;
   yp := y+ya;
   xm := x-xa;
   xp := x+xa;
   plxpyp := maxsmallint;
   plxmyp := -maxsmallint-1;
   plxpym := maxsmallint;
   plxmym := -maxsmallint-1;
   if LineInfo.Thickness = Normwidth then
     Begin
       If (Alpha+270>=StAngle) And (Alpha+270<=EndAngle) then
          Begin
            plxmym := xm;
            PutPixel (xm,ym, CurrentColor);
          End;
       If ((180+270)-Alpha>=StAngle) And ((180+270)-Alpha<=EndAngle) then
          Begin
            plxmyp := xm;
            PutPixel (xm,yp, CurrentColor);
          End;
       If ((180+270)+Alpha>=StAngle) And ((180+270)+Alpha<=EndAngle) then
          Begin
            plxpyp := xp;
            PutPixel (xp,yp, CurrentColor);
          End;
       If ((360+270)-Alpha>=StAngle) And ((360+270)-Alpha<=EndAngle) then
          Begin
            plxpym := xp;
            PutPixel (xp,ym, CurrentColor);
          End;
     end
   else
     Begin
       If (Alpha+270>=StAngle) And (Alpha+270<=EndAngle) then
         Begin
           plxmym := xm + 1;
           for i:=-1 to 1 do
             for j:=-1 to 1 do
               PutPixel (xm+i,ym+j, CurrentColor);
         End;
       If ((180+270)-Alpha>=StAngle) And ((180+270)-Alpha<=EndAngle) then
         Begin
           plxmyp := xm + 1;
           for i:=-1 to 1 do
             for j:=-1 to 1 do
               PutPixel (xm+i,yp+j, CurrentColor);
         End;
       If ((180+270)+Alpha>=StAngle) And ((180+270)+Alpha<=EndAngle) then
         Begin
           plxpyp := xp - 1;
           for i:=-1 to 1 do
             for j:=-1 to 1 do
               PutPixel (xp+i,yp+j, CurrentColor);
         End;
       If ((360+270)-Alpha>=StAngle) And ((360+270)-Alpha<=EndAngle) then
         Begin
           plxpym := xp - 1;
           for i:=-1 to 1 do
             for j:=-1 to 1 do
               PutPixel (xp+i,ym+j, CurrentColor);
         End;
     end;
     If (xp <> xm) then
       begin
         CurrentColor := FillSettings.Color;
         pl(plxmyp+1,plxpyp-1,yp);
         pl(plxmym+1,plxpym-1,ym);
         CurrentColor := BackupColor;
       end;
End;

Begin
  { check for an ellipse with negligable x and y radius }
  If (xradius <= 1) and (yradius <= 1) then
    begin
      putpixel(x,y,CurrentColor);
      ArcCall.X := X;
      ArcCall.Y := Y;
      ArcCall.XStart := X;
      ArcCall.YStart := Y;
      ArcCall.XEnd := X;
      ArcCall.YEnd := Y;
      exit;
    end;
  { for restoring after PatternLine }
  BackupColor := CurrentColor;
  If xradius = 0 then inc(xradius);
  if yradius = 0 then inc(yradius);
  { store arccall info }
  ArcCall.x := x;
  ArcCall.y := y;
  TempTerm := StAngle*RadToDeg;
  ArcCall.XStart := round(XRadius*Cos(TempTerm)) + X;
  ArcCall.YStart := round(YRadius*Sin(TempTerm+Pi)) + Y;
  TempTerm := EndAngle*RadToDeg;
  ArcCall.XEnd := round(XRadius*Cos(TempTerm)) + X;
  ArcCall.YEnd := round(YRadius*Sin(TempTerm+Pi)) + Y;

  StAngle:=StAngle MOD 361;
  EndAngle:=EndAngle MOD 361;
  StAngle := StAngle + 270;
  EndAngle := EndAngle + 270;
  If StAngle>EndAngle then
  Begin
    StAngle:=StAngle Xor EndAngle; EndAngle:=EndAngle Xor StAngle; StAngle:=EndAngle Xor StAngle;
  End;
  { Adjust for screen aspect ratio }
  XRadius:=(longint(XRadius)*10000) div XAspect;
  YRadius:=(longint(YRadius)*10000) div YAspect;
  aSqr:=LongInt (xradius)*LongInt (xradius);
  bSqr:=LongInt (yradius)*LongInt (yradius);
  twoaSqr:=2*aSqr;
  twobSqr:=2*bSqr;
  xa:=0;
  ya:=yradius;
  twoXbSqr:=0;
  twoYaSqr:=ya*twoaSqr;
  error:=-ya*aSqr;
  While twoXbSqr<=twoYaSqr Do Begin
    If ya=0 then Alpha:=90 Else Alpha:=RadToDeg*Arctan (xa/ya); { Crude but it works }
    PlotPoints;
    Inc (xa);
    Inc (twoXbSqr,twobSqr);
    Inc (error,twoXbSqr-bSqr);
    If error>=0 then Begin
      Dec (ya);
      Dec (twoYaSqr,twoaSqr);
      Dec (error,twoYaSqr);
    End;
  End;
  xa:=xradius;
  ya:=0;
  twoXbSqr:=xa*twobSqr;
  twoYaSqr:=0;
  error:=-xa*bSqr;
  While twoXbSqr>twoYaSqr Do Begin
    If ya=0 then Alpha:=90 Else Alpha:=RadToDeg*Arctan (xa/ya);
    PlotPoints;
    Inc (ya);
    Inc (twoYaSqr,twoaSqr);
    Inc (error,twoYaSqr-aSqr);
    If error>=0 then Begin
      Dec (xa);
      Dec (twoXbSqr,twobSqr);
      Dec (error,twoXbSqr);
    End;
  End;
End;
*)
  procedure PatternLineDefault(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
  {********************************************************}
  { Draws a horizontal patterned line according to the     }
  { current Fill Settings.                                 }
  {********************************************************}
  { Important notes:                                       }
  {  - CurrentColor must be set correctly before entering  }
  {    this routine.                                       }
  {********************************************************}
   var
    NrIterations: smallint;
    i           : smallint;
    j           : smallint;
    TmpFillPattern : byte;
    OldWriteMode : word;
    OldCurrentColor : word;
   begin
     { convert to global coordinates ... }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y  := y + StartYViewPort;
     { if line was fully clipped then exit...}
     if LineClipped(x1,y,x2,y,StartXViewPort,StartYViewPort,
        StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
         exit;

     OldWriteMode := CurrentWriteMode;
     CurrentWriteMode := NormalPut;


     { Get the current pattern }
     TmpFillPattern := FillPatternTable
       [FillSettings.Pattern][(y and $7)+1];

     Case TmpFillPattern Of
       0:
         begin
           OldCurrentColor := CurrentColor;
           CurrentColor := CurrentBkColor;
  { hline converts the coordinates to global ones, but that has been done }
  { already here!!! Convert them back to local ones... (JM)                }
           HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
           CurrentColor := OldCurrentColor;
         end;
       $ff:
         begin
           HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
         end;
       else
         begin
           { number of times to go throuh the 8x8 pattern }
           NrIterations := abs(x2 - x1+8) div 8;
           For i:= 0 to NrIterations do
             Begin
               for j:=0 to 7 do
                    Begin
                            { x1 mod 8 }
                    if RevBitArray[x1 and 7] and TmpFillPattern <> 0 then
                       DirectPutpixel(x1,y)
                    else
                      begin
                            { According to the TP graph manual, we overwrite everything }
                            { which is filled up - checked against VGA and CGA drivers  }
                            { of TP.                                                    }
                            OldCurrentColor := CurrentColor;
                            CurrentColor := CurrentBkColor;
                            DirectPutPixel(x1,y);
                            CurrentColor := OldCurrentColor;
                      end;
                    Inc(x1);
                    if x1 > x2 then
                     begin
                           CurrentWriteMode := OldWriteMode;
                           exit;
                     end;
                   end;
             end;
          end;
     End;
     CurrentWriteMode := OldWriteMode;
   end;




  procedure LineRel(Dx, Dy: smallint);

   Begin
     Line(CurrentX, CurrentY, CurrentX + Dx, CurrentY + Dy);
     CurrentX := CurrentX + Dx;
     CurrentY := CurrentY + Dy;
   end;


  procedure LineTo(x,y : smallint);

   Begin
     Line(CurrentX, CurrentY, X, Y);
     CurrentX := X;
     CurrentY := Y;
   end;




  procedure Rectangle(x1,y1,x2,y2:smallint);

   begin
     { Do not draw the end points }
     Line(x1,y1,x2-1,y1);
     Line(x1,y1+1,x1,y2);
     Line(x2,y1,x2,y2-1);
     Line(x1+1,y2,x2,y2);
   end;


  procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);

   begin
    Activelineinfo:=Lineinfo;
   end;


  procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);

   var
    i: byte;
    j: byte;

   Begin
    if (LineStyle > UserBitLn) or ((Thickness <> Normwidth) and (Thickness <> ThickWidth)) then
      _GraphResult := grError
    else
      begin
       LineInfo.Thickness := Thickness;
       LineInfo.LineStyle := LineStyle;
       case LineStyle of
            UserBitLn: Lineinfo.Pattern := pattern;
            SolidLn:   Lineinfo.Pattern  := $ffff;  { ------- }
            DashedLn : Lineinfo.Pattern := $F8F8;   { -- -- --}
            DottedLn:  LineInfo.Pattern := $CCCC;   { - - - - }
            CenterLn: LineInfo.Pattern :=  $FC78;   { -- - -- }
       end; { end case }
       { setup pattern styles }
       j:=16;
       for i:=0 to 15 do
        Begin
         dec(j);
         { bitwise mask for each bit in the word }
         if (word($01 shl i) AND LineInfo.Pattern) <> 0 then
               LinePatterns[j]:=TRUE
             else
               LinePatterns[j]:=FALSE;
        end;
      end;
   end;




{--------------------------------------------------------------------------}
{                                                                          }
{                    VIEWPORT RELATED ROUTINES                             }
{                                                                          }
{--------------------------------------------------------------------------}


Procedure ClearViewPortDefault; {$ifndef fpc}far;{$endif fpc}
var
 j: smallint;
 OldWriteMode, OldCurColor: word;
 LineSets : LineSettingsType;
Begin
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;

  { Save all old settings }
  OldCurColor := CurrentColor;
  CurrentColor:=CurrentBkColor;
  OldWriteMode:=CurrentWriteMode;
  CurrentWriteMode:=NormalPut;
  GetLineSettings(LineSets);
  { reset to normal line style...}
  SetLineStyle(SolidLn, 0, NormWidth);

  { routines are relative here...}
  for J:=0 to ViewHeight - 1 do
       HLine(0, ViewWidth - 1, J);

  { restore old settings...}
  SetLineStyle(LineSets.LineStyle, LineSets.Pattern, LineSets.Thickness);
  CurrentColor := OldCurColor;
  CurrentWriteMode := OldWriteMode;
end;


Procedure SetViewPort(X1, Y1, X2, Y2: smallint; Clip: Boolean);
Begin
  if (X1 > GetMaxX) or (X2 > GetMaxX) or (X1 > X2) or (X1 < 0) then
  Begin
    _GraphResult := grError;
    exit;
  end;
  if (Y1 > GetMaxY) or (Y2 > GetMaxY) or (Y1 > Y2) or (Y1 < 0) then
  Begin
    _GraphResult := grError;
    exit;
  end;
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;
  StartXViewPort := X1;
  StartYViewPort := Y1;
  ViewWidth :=  X2-X1;
  ViewHeight:=  Y2-Y1;
  ClipPixels := Clip;
end;


procedure GetViewSettings(var viewport : ViewPortType);
begin
  ViewPort.X1 := StartXViewPort;
  ViewPort.Y1 := StartYViewPort;
  ViewPort.X2 := ViewWidth + StartXViewPort;
  ViewPort.Y2 := ViewHeight + StartYViewPort;
  ViewPort.Clip := ClipPixels;
end;

procedure ClearDevice;
var
  ViewPort: ViewPortType;
begin
  { Reset the CP }
  CurrentX := 0;
  CurrentY := 0;
  { save viewport }
  ViewPort.X1 :=  StartXviewPort;
  ViewPort.X2 :=  ViewWidth - StartXViewPort;
  ViewPort.Y1 :=  StartYViewPort;
  ViewPort.Y2 :=  ViewHeight - StartYViewPort;
  ViewPort.Clip := ClipPixels;
  { put viewport to full screen }
  StartXViewPort := 0;
  ViewHeight := MaxY;
  StartYViewPort := 0;
  ViewWidth := MaxX;
  ClipPixels := TRUE;
  ClearViewPort;
  { restore old viewport }
  StartXViewPort := ViewPort.X1;
  ViewWidth := ViewPort.X2-ViewPort.X1;
  StartYViewPort := ViewPort.Y1;
  ViewHeight := ViewPort.Y2-ViewPort.Y1;
  ClipPixels := ViewPort.Clip;
end;



{--------------------------------------------------------------------------}
{                                                                          }
{                      BITMAP PUT/GET ROUTINES                             }
{                                                                          }
{--------------------------------------------------------------------------}


  Procedure GetScanlineDefault (X1, X2, Y : smallint; Var Data); {$ifndef fpc}far;{$endif fpc}
  {**********************************************************}
  { Procedure GetScanLine()                                  }
  {----------------------------------------------------------}
  { Returns the full scanline of the video line of the Y     }
  { coordinate. The values are returned in a WORD array      }
  { each WORD representing a pixel of the specified scanline }
  { note: we only need the pixels inside the ViewPort! (JM)  }
  { note2: extended so you can specify start and end X coord }
  {   so it is usable for GetImage too (JM)                  }
  {**********************************************************}


  Var
    x : smallint;
  Begin
     For x:=X1 to X2 Do
       WordArray(Data)[x-x1]:=GetPixel(x, y);
  End;



Function DefaultImageSize(X1,Y1,X2,Y2: smallint): longint; {$ifndef fpc}far;{$endif fpc}
Begin
  { each pixel uses two bytes, to enable modes with colors up to 64K }
  { to work.                                                         }
  DefaultImageSize := 12 + (((X2-X1+1)*(Y2-Y1+1))*2);
end;

Procedure DefaultPutImage(X,Y: smallint; var Bitmap; BitBlt: Word); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  k: longint;
  oldCurrentColor: word;
  oldCurrentWriteMode, i, j, y1, x1, deltaX, deltaX1, deltaY: smallint;
Begin
{$ifdef logging}
  LogLn('putImage at ('+strf(x)+','+strf(y)+') with width '+strf(ptw(Bitmap)[0])+
    ' and height '+strf(ptw(Bitmap)[1]));
  deltaY := 0;
{$endif logging}
  inc(x,startXViewPort);
  inc(y,startYViewPort);
  x1 := ptw(Bitmap)[0]+x; { get width and adjust end coordinate accordingly }
  y1 := ptw(Bitmap)[1]+y; { get height and adjust end coordinate accordingly }

  deltaX := 0;
  deltaX1 := 0;
  k := 3 * sizeOf(Longint) div sizeOf(Word); { Three reserved longs at start of bitmap }
 { check which part of the image is in the viewport }
  if clipPixels then
    begin
      if y < startYViewPort then
        begin
          deltaY := startYViewPort - y;
          inc(k,(x1-x+1)*deltaY);
          y := startYViewPort;
         end;
      if y1 > startYViewPort+viewHeight then
        y1 := startYViewPort+viewHeight;
      if x < startXViewPort then
        begin
          deltaX := startXViewPort-x;
          x := startXViewPort;
        end;
      if x1 > startXViewPort + viewWidth then
        begin
          deltaX1 := x1 - (startXViewPort + viewWidth);
          x1 := startXViewPort + viewWidth;
        end;
    end;
{$ifdef logging}
  LogLn('deltax: '+strf(deltax)+', deltax1: '+strf(deltax1)+',deltay: '+strf(deltay));
{$endif logging}
  oldCurrentColor := currentColor;
  oldCurrentWriteMode := currentWriteMode;
  currentWriteMode := bitBlt;
  for j:=Y to Y1 do
   Begin
     inc(k,deltaX);
     for i:=X to X1 do
      begin
        currentColor := pt(bitmap)[k];
        directPutPixel(i,j);
        inc(k);
     end;
     inc(k,deltaX1);
   end;
  currentWriteMode := oldCurrentWriteMode;
  currentColor := oldCurrentColor;
end;

Procedure DefaultGetImage(X1,Y1,X2,Y2: smallint; Var Bitmap); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  i,j: smallint;
  k: longint;
Begin
  k:= 3 * Sizeof(longint) div sizeof(word); { Three reserved longs at start of bitmap }
  i := x2 - x1 + 1;
  for j:=Y1 to Y2 do
   Begin
     GetScanLine(x1,x2,j,pt(Bitmap)[k]);
     inc(k,i);
   end;
   ptw(Bitmap)[0] := X2-X1;   { First longint  is width  }
   ptw(Bitmap)[1] := Y2-Y1;   { Second longint is height }
   ptw(bitmap)[2] := 0;       { Third longint is reserved}
end;






  Procedure GetArcCoords(var ArcCoords: ArcCoordsType);
   Begin
     ArcCoords.X := ArcCall.X;
     ArcCoords.Y := ArcCall.Y;
     ArcCoords.XStart := ArcCall.XStart;
     ArcCoords.YStart := ArcCall.YStart;
     ArcCoords.XEnd := ArcCall.XEnd;
     ArcCoords.YEnd := ArcCall.YEnd;
   end;


  procedure SetVisualPageDefault(page : word); {$ifndef fpc}far;{$endif fpc}
   begin
   end;


  procedure SetActivePageDefault(page : word); {$ifndef fpc}far;{$endif fpc}
   begin
   end;

  procedure DirectPutPixelDefault(X,Y: smallint);
   begin
     RunError(218);
   end;

  function GetPixelDefault(X,Y: smallint): word;
   begin
     RunError(218);
     exit(0); { avoid warning }
   end;

  procedure PutPixelDefault(X,Y: smallint; Color: Word);
   begin
     RunError(218);
   end;

  procedure SetRGBPaletteDefault(ColorNum, RedValue, GreenValue, BlueValue: smallint);
   begin
     RunError(218);
   end;

  procedure GetRGBPaletteDefault(ColorNum: smallint; var
            RedValue, GreenValue, BlueValue: smallint);
   begin
     RunError(218);
   end;


  Procedure DefaultHooks;
  {********************************************************}
  { Procedure DefaultHooks()                               }
  {--------------------------------------------------------}
  { Resets all hookable routine either to nil for those    }
  { which need overrides, and others to defaults.          }
  { This is called each time SetGraphMode() is called.     }
  {********************************************************}
  Begin
    { All default hooks procedures }

    { required...}
    DirectPutPixel := {$ifdef fpc}@{$endif}DirectPutPixelDefault;
    PutPixel := {$ifdef fpc}@{$endif}PutPixelDefault;
    GetPixel := {$ifdef fpc}@{$endif}GetPixelDefault;
    SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteDefault;
    GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteDefault;

    { optional...}
    SetActivePage := {$ifdef fpc}@{$endif}SetActivePageDefault;
    SetVisualPage := {$ifdef fpc}@{$endif}SetVisualPageDefault;
    ClearViewPort := {$ifdef fpc}@{$endif}ClearViewportDefault;
    PutImage := {$ifdef fpc}@{$endif}DefaultPutImage;
    GetImage := {$ifdef fpc}@{$endif}DefaultGetImage;
    ImageSize := {$ifdef fpc}@{$endif}DefaultImageSize;
    GraphFreeMemPtr := nil;
    GraphGetMemPtr := nil;
    GetScanLine := {$ifdef fpc}@{$endif}GetScanLineDefault;
    Line := {$ifdef fpc}@{$endif}LineDefault;
    InternalEllipse := {$ifdef fpc}@{$endif}InternalEllipseDefault;
    PatternLine := {$ifdef fpc}@{$endif}PatternLineDefault;
    HLine := {$ifdef fpc}@{$endif}HLineDefault;
    VLine := {$ifdef fpc}@{$endif}VLineDefault;
  end;

  Procedure InitVars;
  {********************************************************}
  { Procedure InitVars()                                   }
  {--------------------------------------------------------}
  { Resets all internal variables, and resets all          }
  { overridable routines.                                  }
  {********************************************************}
   Begin
    DirectVideo := TRUE;  { By default use fastest access possible }
    ArcCall.X := 0;
    ArcCall.Y := 0;
    ArcCall.XStart := 0;
    ArcCall.YStart := 0;
    ArcCall.XEnd := 0;
    ArcCall.YEnd := 0;
    { Reset to default values }
    IntCurrentMode := 0;
    IntCurrentDriver := 0;
    XAspect := 0;
    YAspect := 0;
    MaxX := 0;
    MaxY := 0;
    MaxColor := 0;
    PaletteSize := 0;
    DirectColor := FALSE;
    HardwarePages := 0;
    if hardwarepages=0 then; { remove note }
    DefaultHooks;
  end;

{$i modes.inc}
{$i palette.inc}

  function InstallUserDriver(Name: string; AutoDetectPtr: Pointer): smallint;
   begin
     _graphResult := grError;
     InstallUserDriver:=grError;
   end;

  function RegisterBGIDriver(driver: pointer): smallint;

   begin
     _graphResult := grError;
     RegisterBGIDriver:=grError;
   end;



{ ----------------------------------------------------------------- }


  Procedure Arc(X,Y : smallint; StAngle,EndAngle,Radius: word);

{   var
    OldWriteMode: word;}

   Begin
     { Only if we are using thickwidths lines do we accept }
     { XORput write modes.                                 }
{     OldWriteMode := CurrentWriteMode;
     if (LineInfo.Thickness = NormWidth) then
       CurrentWriteMode := NormalPut;}
     InternalEllipse(X,Y,Radius,Radius,StAngle,Endangle,{$ifdef fpc}@{$endif}DummyPatternLine);
{     CurrentWriteMode := OldWriteMode;}
   end;


 procedure Ellipse(X,Y : smallint; stAngle, EndAngle: word; XRadius,YRadius: word);
  Begin
    InternalEllipse(X,Y,XRadius,YRadius,StAngle,Endangle,{$ifdef fpc}@{$endif}DummyPatternLine);
  end;


 procedure FillEllipse(X, Y: smallint; XRadius, YRadius: Word);
  {********************************************************}
  { Procedure FillEllipse()                                }
  {--------------------------------------------------------}
  { Draws a filled ellipse using (X,Y) as a center point   }
  { and XRadius and YRadius as the horizontal and vertical }
  { axes. The ellipse is filled with the current fill color}
  { and fill style, and is bordered with the current color.}
  {********************************************************}
  begin
    InternalEllipse(X,Y,XRadius,YRadius,0,360,PatternLine)
  end;



 procedure Circle(X, Y: smallint; Radius:Word);
  {********************************************************}
  { Draws a circle centered at X,Y with the given Radius.  }
  {********************************************************}
  { Important notes:                                       }
  {  - Thickwidth circles use the current write mode, while}
  {    normal width circles ALWAYS use CopyPut/NormalPut   }
  {    mode. (Tested against VGA BGI driver -CEC 13/Aug/99 }
  {********************************************************}
  var OriginalArcInfo: ArcCoordsType;
      OldWriteMode: word;

  begin
     if (Radius = 0) then
          Exit;

     if (Radius = 1) then
     begin
      { only normal put mode is supported by a call to PutPixel }
          PutPixel(X, Y, CurrentColor);
          Exit;
     end;

     { save state of arc information }
     { because it is not needed for  }
     { a circle call.                }
     move(ArcCall,OriginalArcInfo, sizeof(ArcCall));
     if LineInfo.Thickness = Normwidth then
       begin
             OldWriteMode := CurrentWriteMode;
             CurrentWriteMode := CopyPut;
       end;
     InternalEllipse(X,Y,Radius,Radius,0,360,{$ifdef fpc}@{$endif}DummyPatternLine);
     if LineInfo.Thickness = Normwidth then
         CurrentWriteMode := OldWriteMode;
     { restore arc information }
     move(OriginalArcInfo, ArcCall,sizeof(ArcCall));
 end;

 procedure SectorPL(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
 var plx1, plx2: smallint;
{$ifdef sectorpldebug}
     t : text;
{$endif sectorpldebug}
 begin
{$ifdef sectorpldebug}
   assign(t,'sector.log');
   append(t);
   writeln(t,'Got here for line ',y);
   close(t);
{$endif sectorpldebug}
   If (x1 = -maxsmallint) Then
     If (x2 = maxsmallint-1) Then
       { no ellipse points drawn on this line }
       If (((Y < ArcCall.Y) and (Y > ArcCall.YStart)) or
          ((Y > ArcCall.Y) and (Y < ArcCall.YStart))) Then
         { there is a part of the sector at this y coordinate, but no    }
         { ellips points are plotted on this line, so draw a patternline }
         { between the lines connecting (arccall.x,arccall.y) with       }
         { the start and the end of the arc (JM)                         }
         { use: y-y1=(y2-y1)/(x2-x1)*(x-x1) =>                           }
         { x = (y-y1)/(y2-y1)*(x2-x1)+x1                                 }
         Begin
{$ifdef sectorpldebug}
           If (ArcCall.YStart-ArcCall.Y) = 0 then
             begin
               append(t);
               writeln(t,'bug1');
               close(t);
               runerror(202);
             end;
{$endif sectorpldebug}
           plx1 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                   div (ArcCall.YStart-ArcCall.Y)+ArcCall.X;
{$ifdef sectorpldebug}
           If (ArcCall.YEnd-ArcCall.Y) = 0 then
             begin
               append(t);
               writeln(t,'bug2');
               close(t);
               runerror(202);
             end;
{$endif sectorpldebug}
           plx2 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                   div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X;
           If plx1 > plx2 then
             begin
               plx1 := plx1 xor plx2;
               plx2 := plx1 xor plx2;
               plx1 := plx1 xor plx2;
             end;
{$ifdef sectorpldebug}
           append(t);
           writeln(t,'lines: ',plx1,' - ',plx2);
           close(t);
{$endif sectorpldebug}
         End
       { otherwise two points which have nothing to do with the sector }
       Else exit
     Else
       { the arc is plotted at the right side, but not at the left side, }
       { fill till the line between (ArcCall.X,ArcCall.Y) and            }
       { (ArcCall.XStart,ArcCall.YStart)                                 }
       Begin
         If (y < ArcCall.Y) then
           begin
{$ifdef sectorpldebug}
             If (ArcCall.YEnd-ArcCall.Y) = 0 then
               begin
                 append(t);
                 writeln(t,'bug3');
                 close(t);
                 runerror(202);
               end;
{$endif sectorpldebug}
             plx1 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                     div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X
           end
         else if (y > ArcCall.Y) then
           begin
{$ifdef sectorpldebug}
             If (ArcCall.YStart-ArcCall.Y) = 0 then
               begin
                 append(t);
                 writeln(t,'bug4');
                 close(t);
                 runerror(202);
               end;
{$endif sectorpldebug}
             plx1 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                     div (ArcCall.YStart-ArcCall.Y)+ArcCall.X
             end
         else plx1 := ArcCall.X;
         plx2 := x2;
{$ifdef sectorpldebug}
         append(t);
         writeln(t,'right: ',plx1,' - ',plx2);
         close(t);
{$endif sectorpldebug}
       End
   Else
     If (x2 = maxsmallint-1) Then
       { the arc is plotted at the left side, but not at the rigth side.   }
       { the right limit can be either the first or second line. Just take }
       { the closest one, but watch out for division by zero!              }
       Begin
         If (y < ArcCall.Y) then
           begin
{$ifdef sectorpldebug}
             If (ArcCall.YStart-ArcCall.Y) = 0 then
               begin
                 append(t);
                 writeln(t,'bug5');
                 close(t);
                 runerror(202);
               end;
{$endif sectorpldebug}
             plx2 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                     div (ArcCall.YStart-ArcCall.Y)+ArcCall.X
           end
         else if (y > ArcCall.Y) then
           begin
{$ifdef sectorpldebug}
             If (ArcCall.YEnd-ArcCall.Y) = 0 then
               begin
                 append(t);
                 writeln(t,'bug6');
                 close(t);
                 runerror(202);
               end;
{$endif sectorpldebug}
             plx2 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                     div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X
           end
         else plx2 := ArcCall.X;
         plx1 := x1;
{$ifdef sectorpldebug}
         append(t);
         writeln(t,'left: ',plx1,' - ',plx2);
         close(t);
{$endif sectorpldebug}
       End
     Else
       { the arc is plotted at both sides }
       Begin
         plx1 := x1;
         plx2 := x2;
{$ifdef sectorpldebug}
         append(t);
         writeln(t,'normal: ',plx1,' - ',plx2);
         close(t);
{$endif sectorpldebug}
       End;
   If plx2 > plx1 then
     Begin
{$ifdef sectorpldebug}
       append(t);
       Writeln(t,'drawing...');
       close(t);
{$endif sectorpldebug}
       PatternLine(plx1,plx2,y);
     end;
 end;

 procedure Sector(x, y: smallint; StAngle,EndAngle, XRadius, YRadius: Word);
  begin
     internalellipse(x,y,XRadius, YRadius, StAngle, EndAngle, {$ifdef fpc}@{$endif}SectorPL);
     Line(ArcCall.XStart, ArcCall.YStart, x,y);
     Line(x,y,ArcCall.Xend,ArcCall.YEnd);
  end;



   procedure SetFillStyle(Pattern : word; Color: word);

   begin
     { on invalid input, the current fill setting will be }
     { unchanged.                                         }
     if (Pattern > UserFill) or (Color > GetMaxColor) then
      begin
           _GraphResult := grError;
           exit;
      end;
     FillSettings.Color := Color;
     FillSettings.Pattern := Pattern;
   end;


  procedure SetFillPattern(Pattern: FillPatternType; Color: word);
  {********************************************************}
  { Changes the Current FillPattern to a user defined      }
  { pattern and changes also the current fill color.       }
  { The FillPattern is saved in the FillPattern array so   }
  { it can still be used with SetFillStyle(UserFill,Color) }
  {********************************************************}
   var
    i: smallint;

   begin
     if Color > GetMaxColor then
       begin
            _GraphResult := grError;
            exit;
       end;

     FillSettings.Color := Color;
     FillSettings.Pattern := UserFill;

     { Save the pattern in the buffer }
     For i:=1 to 8 do
       FillPatternTable[UserFill][i] := Pattern[i];

   end;

  procedure Bar(x1,y1,x2,y2:smallint);
  {********************************************************}
  { Important notes for compatibility with BP:             }
  {     - WriteMode is always CopyPut                      }
  {     - No contour is drawn for the lines                }
  {********************************************************}
  var y               : smallint;
      origcolor       : longint;
      origlinesettings: Linesettingstype;
      origwritemode   : smallint;
   begin
     origlinesettings:=lineinfo;
     origcolor:=CurrentColor;
     if y1>y2 then
       begin
          y:=y1;
          y1:=y2;
          y2:=y;
       end;

     { Always copy mode for Bars }
     origwritemode := CurrentWriteMode;
     CurrentWriteMode := CopyPut;

     { All lines used are of this style }
     Lineinfo.linestyle:=solidln;
     Lineinfo.thickness:=normwidth;

     case Fillsettings.pattern of
     EmptyFill :
       begin
         Currentcolor:=CurrentBkColor;
         for y:=y1 to y2 do
           Hline(x1,x2,y);
       end;
     SolidFill :
       begin
         CurrentColor:=FillSettings.color;
           for y:=y1 to y2 do
              Hline(x1,x2,y);
       end;
     else
      Begin
        CurrentColor:=FillSettings.color;
        for y:=y1 to y2 do
          patternline(x1,x2,y);
      end;
    end;
    CurrentColor:= Origcolor;
    LineInfo := OrigLineSettings;
    CurrentWriteMode := OrigWritemode;
   end;




procedure bar3D(x1, y1, x2, y2 : smallint;depth : word;top : boolean);
var
 origwritemode : smallint;
 OldX, OldY : smallint;
begin
  origwritemode := CurrentWriteMode;
  CurrentWriteMode := CopyPut;
  Bar(x1,y1,x2,y2);
  Rectangle(x1,y1,x2,y2);

  { Current CP should not be updated in Bar3D }
  { therefore save it and then restore it on  }
  { exit.                                     }
  OldX := CurrentX;
  OldY := CurrentY;

  if top then begin
    Moveto(x1,y1);
    Lineto(x1+depth,y1-depth);
    Lineto(x2+depth,y1-depth);
    Lineto(x2,y1);
  end;
  if Depth <> 0 then
    Begin
      Moveto(x2+depth,y1-depth);
      Lineto(x2+depth,y2-depth);
      Lineto(x2,y2);
    end;
  { restore CP }
  CurrentX := OldX;
  CurrentY := OldY;
  CurrentWriteMode := origwritemode;
end;



{--------------------------------------------------------------------------}
{                                                                          }
{                       COLOR AND PALETTE ROUTINES                         }
{                                                                          }
{--------------------------------------------------------------------------}


  procedure SetColor(Color: Word);

   Begin
     CurrentColor := Color;
   end;


  function GetColor: Word;

   Begin
     GetColor := CurrentColor;
   end;

  function GetBkColor: Word;

   Begin
     GetBkColor := CurrentBkColor;
   end;


  procedure SetBkColor(ColorNum: Word);
  { Background color means background screen color in this case, and it is  }
  { INDEPENDANT of the viewport settings, so we must clear the whole screen }
  { with the color.                                                         }
   var
     ViewPort: ViewportType;
   Begin
     GetViewSettings(Viewport);
     SetViewPort(0,0,MaxX,MaxY,FALSE);
     CurrentBkColor := ColorNum;
     {ClearViewPort;}
     if not DirectColor and (ColorNum<256) then
      SetRGBPalette(0,
          DefaultColors[ColorNum].Red,
          DefaultColors[ColorNum].Green,
          DefaultColors[ColorNum].Blue);
     SetViewport(ViewPort.X1,Viewport.Y1,Viewport.X2,Viewport.Y2,Viewport.Clip);
   end;


  function GetMaxColor: word;
  { Checked against TP VGA driver - CEC }

   begin
      GetMaxColor:=MaxColor-1; { based on an index of zero so subtract one }
   end;






   Procedure MoveRel(Dx, Dy: smallint);
    Begin
     CurrentX := CurrentX + Dx;
     CurrentY := CurrentY + Dy;
   end;

   Procedure MoveTo(X,Y: smallint);
  {********************************************************}
  { Procedure MoveTo()                                     }
  {--------------------------------------------------------}
  { Moves the current pointer in VIEWPORT relative         }
  { coordinates to the specified X,Y coordinate.           }
  {********************************************************}
    Begin
     CurrentX := X;
     CurrentY := Y;
    end;


function GraphErrorMsg(ErrorCode: smallint): string;
Begin
 GraphErrorMsg:='';
 case ErrorCode of
  grOk,grFileNotFound,grInvalidDriver: exit;
  grNoInitGraph: GraphErrorMsg:='Graphics driver not installed';
  grNotDetected: GraphErrorMsg:='Graphics hardware not detected';
  grNoLoadMem,grNoScanMem,grNoFloodMem: GraphErrorMsg := 'Not enough memory for graphics';
  grNoFontMem: GraphErrorMsg := 'Not enough memory to load font';
  grFontNotFound: GraphErrorMsg:= 'Font file not found';
  grInvalidMode: GraphErrorMsg := 'Invalid graphics mode';
  grError: GraphErrorMsg:='Graphics error';
  grIoError: GraphErrorMsg:='Graphics I/O error';
  grInvalidFont,grInvalidFontNum: GraphErrorMsg := 'Invalid font';
  grInvalidVersion: GraphErrorMsg:='Invalid driver version';
 end;
end;




  Function GetMaxX: smallint;
  { Routine checked against VGA driver - CEC }
   Begin
     GetMaxX := MaxX;
   end;

  Function GetMaxY: smallint;
  { Routine checked against VGA driver - CEC }
   Begin
    GetMaxY := MaxY;
   end;




Function GraphResult: smallint;
Begin
  GraphResult := _GraphResult;
  _GraphResult := grOk;
end;


  Function GetX: smallint;
   Begin
     GetX := CurrentX;
   end;


  Function GetY: smallint;
   Begin
     GetY := CurrentY;
   end;

   Function GetDriverName: string;
    begin
      GetDriverName:=DriverName;
    end;


   procedure graphdefaults;
   { PS: GraphDefaults does not ZERO the ArcCall structure }
   { so a call to GetArcCoords will not change even the    }
   { returned values even if GraphDefaults is called in    }
   { between.                                              }
    var
     i: smallint;
   begin
     lineinfo.linestyle:=solidln;
     lineinfo.thickness:=normwidth;
     { reset line style pattern }
     for i:=0 to 15 do
       LinePatterns[i] := TRUE;

     { By default, according to the TP prog's reference }
     { the default pattern is solid, and the default    }
     { color is the maximum color in the palette.       }
     fillsettings.color:=GetMaxColor;
     fillsettings.pattern:=solidfill;
     { GraphDefaults resets the User Fill pattern to $ff }
     { checked with VGA BGI driver - CEC                 }
     for i:=1 to 8 do
       FillPatternTable[UserFill][i] := $ff;


     CurrentColor:=white;
     SetBkColor(Black);


     ClipPixels := TRUE;
     { Reset the viewport }
     StartXViewPort := 0;
     StartYViewPort := 0;
     ViewWidth := MaxX;
     ViewHeight := MaxY;
     { Reset CP }
     CurrentX := 0;
     CurrentY := 0;

     { normal write mode }
     CurrentWriteMode := CopyPut;

     { Schriftart einstellen }
     CurrentTextInfo.font := DefaultFont;
     CurrentTextInfo.direction:=HorizDir;
     CurrentTextInfo.charsize:=1;
     CurrentTextInfo.horiz:=LeftText;
     CurrentTextInfo.vert:=TopText;

     XAspect:=10000; YAspect:=10000;
   end;


  procedure GetAspectRatio(var Xasp,Yasp : word);
  begin
    XAsp:=XAspect;
    YAsp:=YAspect;
  end;

  procedure SetAspectRatio(Xasp, Yasp : word);
  begin
    Xaspect:= XAsp;
    YAspect:= YAsp;
  end;


  procedure SetWriteMode(WriteMode : smallint);
  { TP sets the writemodes according to the following scheme (JM) }
   begin
     Case writemode of
       xorput, andput: CurrentWriteMode := XorPut;
       notput, orput, copyput: CurrentWriteMode := CopyPut;
     End;
   end;


  procedure GetFillSettings(var Fillinfo:Fillsettingstype);
   begin
     Fillinfo:=Fillsettings;
   end;

  procedure GetFillPattern(var FillPattern:FillPatternType);
   begin
     FillPattern:=FillpatternTable[UserFill];
   end;






  procedure DrawPoly(numpoints : word;var polypoints);

      type
            ppointtype = ^pointtype;
        pt = array[0..16000] of pointtype;

      var
            i : longint;

    begin
         if numpoints < 2 then
           begin
             _GraphResult := grError;
             exit;
           end;
         for i:=0 to numpoints-2 do
           line(pt(polypoints)[i].x,
                pt(polypoints)[i].y,
                pt(polypoints)[i+1].x,
                pt(polypoints)[i+1].y);
    end;


  procedure PieSlice(X,Y,stangle,endAngle:smallint;Radius: Word);
  begin
    Sector(x,y,stangle,endangle,radius,radius);
  end;

{$i fills.inc}
{$i gtext.inc}


  procedure DetectGraph(var GraphDriver:smallint;var GraphMode:smallint);
  var LoMode, HiMode: smallint;
      CpyMode: smallint;
      CpyDriver: smallint;
  begin
    HiMode := -1;
    LoMode := -1;
    { We start at VGA }
    GraphDriver := VGA;
    CpyMode := 0;
    { search all possible graphic drivers in ascending order...}
    { usually the new driver numbers indicate newest hardware...}
    { Internal driver numbers start at VGA=9 }
    repeat
       GetModeRange(GraphDriver,LoMode,HiMode);
       { save the highest mode possible...}
       {$ifdef logging}
       logln('Found driver '+strf(graphdriver)+' with modes '+
              strf(lomode)+' - '+strf(himode));
       {$endif logging}
       if HiMode = -1 then break;
       CpyMode:=HiMode;
       CpyDriver:=GraphDriver;
       { go to next driver if it exists...}
       Inc(GraphDriver);
    until (CpyMode=-1);
    { If this is equal to -1 then no graph mode possible...}
    if CpyMode = -1 then
      begin
        _GraphResult := grNotDetected;
        exit;
      end;
    _GraphResult := grOK;
    GraphDriver := CpyDriver;
    GraphMode := CpyMode;
  end;

  procedure InitGraph(var GraphDriver:smallint;var GraphMode:smallint;
    const PathToDriver:String);
  const
    {$IFDEF Linux}
    dirchar = '/';
    {$ELSE}
    dirchar = '\';
    {$ENDIF}
  begin
    InitVars;
    { path to the fonts (where they will be searched)...}
    bgipath:=PathToDriver;
    if (Length(bgipath) > 0) and (bgipath[length(bgipath)]<>dirchar) then
      bgipath:=bgipath+dirchar;

    if not assigned(SaveVideoState) then
      RunError(216);
    DriverName:=InternalDriverName;   { DOS Graphics driver }

    if (Graphdriver=Detect) then
      begin
        DetectGraph(GraphDriver,GraphMode);
        If _GraphResult = grNotDetected then Exit;

        { _GraphResult is now already set to grOK by DetectGraph }
        IntCurrentDriver := GraphDriver;
        SaveVideoState;
{ Actually set the graph mode...}
        SetGraphMode(GraphMode);
      end
    else
      begin
        { Search if that graphics modec actually exists...}
        if SearchMode(GraphDriver,GraphMode) = nil then
          begin
            _GraphResult := grInvalidMode;
            exit;
         end
        else
         begin
           _GraphResult := grOK;
           IntCurrentDriver := GraphDriver;
           SaveVideoState;
{$ifdef logging}
           If _GraphResult <> grOK then
             LogLn('Mode setting failed after savevideostate');
{$endif logging}
           SetGraphMode(GraphMode);
         end;
      end;
  end;


 procedure SetDirectVideo(DirectAccess: boolean);
  begin
    DirectVideo := DirectAccess;
  end;

 function GetDirectVideo: boolean;
  begin
    GetDirectVideo := DirectVideo;
  end;

 procedure GraphExitProc; {$ifndef fpc} far; {$endif fpc}
 { deallocates all memory allocated by the graph unit }
  var
    list: PModeInfo;
    tmp : PModeInfo;
    c: graph_int;
  begin
   { restore old exitproc! }
   exitproc := exitsave;
   if IsGraphMode and ((errorcode<>0) or (erroraddr<>nil)) then
     CloseGraph;
{$ifdef testsave}
   restorevideostate;
{$endif testsave}
   { release memory allocated for fonts }
   for c := 1 to installedfonts do
     with fonts[c] Do
     If assigned(instr) Then
       Freemem(instr,instrlength);
   { release memory allocated for modelist }
   list := ModeList;
   while assigned(list) do
     begin
       tmp := list;
       list:=list^.next;
       dispose(tmp);
     end;
{$IFDEF DPMI}
  { We had copied the buffer of mode information }
  { and allocated it dynamically... now free it  }
  { Warning: if GetVESAInfo returned false, this buffer is not allocated! (JM)}
   If hasVesa then
     Dispose(VESAInfo.ModeList);
{$ENDIF}
  end;


procedure InitializeGraph;
begin
{$ifdef logging}
 assign(debuglog,'grlog.txt');
 rewrite(debuglog);
 close(debuglog);
{$endif logging}
 isgraphmode := false;
 ModeList := nil;
 SaveVideoState := nil;
 RestoreVideoState := nil;
{$ifdef oldfont}
{$ifdef go32v2}
 LoadFont8x8;
{$endif go32v2}
{$endif oldfont}
 { This must be called at startup... because GetGraphMode may }
 { be called even when not in graph mode.                     }
{$ifdef logging}
 LogLn('Calling QueryAdapterInfo...');
{$endif logging}
 QueryAdapterInfo;
 { Install standard fonts }
 { This is done BEFORE startup... }
 InstalledFonts := 0;
 InstallUserFont('TRIP');
 InstallUserFont('LITT');
 InstallUserFont('SANS');
 InstallUserFont('GOTH');
 InstallUserFont('SCRI');
 InstallUserFont('SIMP');
 InstallUserFont('TSCR');
 InstallUserFont('LCOM');
 InstallUserFont('EURO');
 InstallUserFont('BOLD');
 { This installs an exit procedure which cleans up the mode list...}
 ExitSave := ExitProc;
 ExitProc := @GraphExitProc;
{$ifdef testsave}
 savevideostate;
{$endif testsave}
{$ifdef win32}
 charmessagehandler:=nil;
{$endif win32}
end;
{
  $Log$
  Revision 1.28  2000-03-19 11:20:13  peter
    * graph unit include is now independent and the dependent part
      is now in graph.pp
    * ggigraph unit for linux added

  Revision 1.60  2000/03/18 10:45:07  sg
  * Fix for ClearViewportDefault: The width and the height of the rectangle
    it filled has been one pixel too high.

  Revision 1.59  2000/03/17 13:28:54  sg
  * Use linux unit under Linux

  Revision 1.58  2000/03/08 14:20:14  jonas
    * writemode was not set to normalput during clearviewport (and it uses hline)

  Revision 1.57  2000/02/27 14:41:25  peter
    * removed warnings/notes

  Revision 1.56  2000/02/06 01:47:15  sg
  * For Linux, "/" is added to the bgipath instead of "\" if this character
    isn't already there.

  Revision 1.55  2000/01/07 16:41:37  daniel
    * copyright 2000

  Revision 1.54  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.53  2000/01/02 19:02:39  jonas
    * removed/commented out (inited but) unused vars and unused types

  Revision 1.52  1999/12/29 17:26:00  jonas
    + by default, also attempt to install the fonts that come with TP7

  Revision 1.51  1999/12/26 10:33:06  jonas
    * XAspect and YAspect are now words instead of smallints, they
      overflowed for resolutions > 640x480 otherwise
    * the number of pixels required for an ellipse in internalellipsedef
      is now calculated after the aspectratios have been taken into
      account

  Revision 1.50  1999/12/21 17:42:17  jonas
    * changed vesa.inc do it doesn't try to use linear modes anymore (doesn't work
      yet!!)
    * fixed mode detection so the low modenumber of a driver doesn't have to be zero
      anymore (so VESA autodetection now works)

  Revision 1.49  1999/12/21 09:16:48  pierre
   + CloseGraph if errors

  Revision 1.48  1999/12/20 11:22:36  peter
    * integer -> smallint to overcome -S2 switch needed for ggi version

  Revision 1.47  1999/12/12 13:34:20  jonas
    * putimage now performs the lipping itself and uses directputpixel
      (note: this REQUIRES or/and/notput support in directputpixel,
      this is not yet the case in the assembler versions!)
    * YOffset addition moved in hlinevesa256 and vlinevesa256
      because it uses still putpixel afterwards

  Revision 1.46  1999/12/11 23:41:38  jonas
    * changed definition of getscanlineproc to "getscanline(x1,x2,y:
      smallint; var data);" so it can be used by getimage too
    * changed getimage so it uses getscanline
    * changed floodfill, getscanline16 and definitions in Linux
      include files so they use this new format
    + getscanlineVESA256 for 256 color VESA modes (banked)

  Revision 1.45  1999/12/10 12:47:41  pierre
   * SetBkColor like BP by changing Palette entry zero

  Revision 1.44  1999/11/30 08:57:46  michael
  + Removed charmessagehandler declaration, it is in graphh.inc

  Revision 1.43  1999/11/28 16:13:55  jonas
    * corrected misplacement of call to initvars in initgraph
    + some extra debugging commands (for -dlogging) in the mode functions

  Revision 1.42  1999/11/28 12:19:59  jonas
    * _GraphResult is now properly set to grOK by DetectGraph and
      InitGraph if there are no errors

  Revision 1.41  1999/11/27 21:48:01  jonas
    * fixed VlineVESA256 and re-enabled it in graph.inc
    * added procedure detectgraph to interface of graph unit

  Revision 1.40  1999/11/25 17:44:14  pierre
   * memory corruption within GetImage removed

  Revision 1.39  1999/11/24 23:42:31  pierre
    * PutImage used an smallint index that became negative !!!!
    * Default needed procedure now genrate a RTE 218 instead of a
      GPF by call to nil pointer !

  Revision 1.38  1999/11/11 17:55:07  florian
    * the size was calculated wrong by imagesize

  Revision 1.37  1999/11/11 14:07:14  florian
    * better looking font

  Revision 1.36  1999/11/08 15:01:38  peter
    * fpcmake support

  Revision 1.35  1999/11/08 11:15:22  peter
    * move graph.inc to the target dir

  Revision 1.34  1999/11/03 20:23:01  florian
    + first release of win32 gui support

  Revision 1.33  1999/10/17 10:20:13  jonas
    * fixed clipping for thickwidth lines (bug 659)
    * fixed the faster internalellipsedefault, but it doesn't plot
      all pixels (there are gaps in the ellipses)

  Revision 1.32  1999/09/28 15:07:46  jonas
    * fix for disposing font data because it can contain #0 chars

  Revision 1.31  1999/09/28 13:56:25  jonas
    * reordered some local variables (first 4 byte vars, then 2 byte vars
      etc)
    * font data is now disposed in exitproc, exitproc is now called
      GraphExitProc (was CleanModes) and resides in graph.pp instead of in
      modes.inc

  Revision 1.30  1999/09/27 23:34:41  peter
    * new graph unit is default for go32v2
    * removed warnings/notes

  Revision 1.29  1999/09/26 13:31:06  jonas
    * changed name of modeinfo variable to vesamodeinfo and fixed
      associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
      of sizeof(TVesamodeinfo) etc)
    * changed several sizeof(type) to sizeof(varname) to avoid similar
      errors in the future

  Revision 1.28  1999/09/25 11:48:43  jonas
    + detectgraph
    * small change to internalellipsedefault so less pixels are
      calculated twice
    * some small corrections to graph.tex

  Revision 1.27  1999/09/24 22:52:38  jonas
    * optimized patternline a bit (always use hline when possible)
    * isgraphmode stuff cleanup
    * vesainfo.modelist now gets disposed in cleanmode instead of in
      closegraph (required moving of some declarations from vesa.inc to
      new vesah.inc)
    * queryadapter gets no longer called from initgraph (is called from
      initialization of graph unit)
    * bugfix for notput in 32k and 64k vesa modes
    * a div replaced by / in fillpoly

  Revision 1.26  1999/09/22 13:13:35  jonas
    * renamed text.inc -> gtext.inc to avoid conflict with system unit
    * fixed textwidth
    * isgraphmode now gets properly updated, so mode restoring works
      again

  Revision 1.25  1999/09/18 22:21:10  jonas
    + hlinevesa256 and vlinevesa256
    + support for not/xor/or/andput in vesamodes with 32k/64k colors
    * lots of changes to avoid warnings under FPC

  Revision 1.24  1999/09/18 16:03:37  jonas
    * graph.pp: removed pieslice and sector from ToDo list
    * closegraph: exits now immidiately if isgraphmode = false (caused
      RTE 204 with VESA enabled if you set exitproc to call closegraph
      and also called closegraph explicitely before exit, like bgidemo)

  Revision 1.23  1999/09/17 13:58:31  jonas
  * another fix for a case where internalellipsedefault went haywire
  * sector() and pieslice() fully implemented!
  * small change to prevent buffer overflow with floodfill

  Revision 1.22  1999/09/15 13:37:50  jonas
    * small change to internalellipsedef to be TP compatible
    * fixed directputpixel for vga 320*200*256

  Revision 1.21  1999/09/13 12:49:08  jonas
    * fixed Arc: internallellipse went into an endless loop if StAngle =
      EndAngle
    * FillEllipse is now much faster: no more floodfill,
      InternalEllipseDefault now draws the patternlines immediatety!

  Revision 1.20  1999/09/12 17:29:00  jonas
    * several changes to internalellipse to make it faster
      and to make sure it updates the ArcCall correctly
      (not yet done for width = 3)
    * Arc mostly works now, only sometimes an endless loop, don't know
      why

  Revision 1.19  1999/09/11 19:43:01  jonas
    * FloodFill: did not take into account current viewport settings
    * GetScanLine: only get line inside viewport, data outside of it
      is not used anyway
    * InternalEllipseDefault: fix for when xradius or yradius = 0 and
      increase xradius and yradius always by one (TP does this too)
    * fixed conlict in vesa.inc from last update
    * some conditionals to avoid range check and overflow errors in
      places where it doesn't matter

  Revision 1.18  1999/07/26 09:38:41  florian
    * bar: y2 can be less y1, fixed
    * settextstyle: charsize can be 0, must be changed into 1

  Revision 1.17  1999/07/18 15:07:20  jonas
    + xor-, and and- orput support for VESA256 modes
    * compile with -dlogging if you wnt some info to be logged to grlog.txt

  Revision 1.16  1999/07/14 18:18:04  florian
    * cosmetic changes

}
