{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by Thomas Schatzl and Carl Eric Codere

    This include implements polygon filling and flood filling.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}   { No range checking here, because we do some special typecasts }

type

        pedge = ^edge;
        edge = packed record    { an edge structure }
                x,                                      { current x-coordinate on the edge }
                dx : graph_float;       { deltax of the edge }
                i : graph_int;          { index to which points this edge belongs to
                                                          always [i] and [i+1] }
        end;

        { used for typecasting because TP/BP is more strict here than FPC }
        pedgearray = ^edgearray;
    { 0..0 }
        edgearray = array[0..0] of edge;

        pint = ^graph_int;

        pintarray = ^intarray;
    { 0..0 }
        intarray = array[0..0] of graph_int;

        ppointtype = ^pointtype;
        ppointarray = ^pointarray;
        pointarray = array[0..0] of pointtype;

{ definition of the called compare routine for the sort process. Returns -1 if
 the two parameters should be swapped }
type
        compareproc = function (a, b : pointer) : graph_int;

{ simple bubblesort, since it is expected that the edges themselves are not
  too mixed, it is fastest (?). Rather than sorting the active edge table
  this way, it is recommened to implement this using a linked list (not
  nearly as much memory is transfered then) }
   procedure bsort(p : pointer; number : smallint; sizeelem :
      smallint; c : compareproc);
   var    i : graph_int;
           swap : boolean;
       temp : pointer;

           curp, nextp : pointer;
   begin
     getmem(temp, sizeelem);
         repeat
            curp := p;
                nextp := pointer(longint(p) + sizeelem);
                swap := false;
                for i := 0 to (number-2) do begin
                        if (c(curp, nextp)=1) then begin
                                { swap elements, you can't do it slower ;( }
                                move(curp^, temp^, sizeelem);
                                move(nextp^, curp^, sizeelem);
                                move(temp^, nextp^, sizeelem);
                                swap := true;
                        end;
                        inc(longint(curp), sizeelem);
                        inc(longint(nextp), sizeelem);
                end;
         until swap = false;
         freemem(temp, sizeelem);
   end;

  { guess what this does }
  function ceil(x : graph_float) : graph_int;
    var t : graph_int;
  begin
        t:=Trunc(x);
        If (frac(x)>0) then inc(t);
        ceil := t;
  end;

  { guess what this does too }
  function floor(x : graph_float) : graph_int;
   var t : graph_int;
  begin
        t:=Trunc(x);
        If (frac(x)<0) then dec(t);
        floor := t;
  end;

  { simple descriptive name }
  function max(a, b : graph_int) : graph_int;
  begin
         if (a >= b) then max := a
          else max := b;
  end;

  { here too }
  function min(a, b : graph_int) : graph_int;
  begin
        if (a <= b) then min := a
        else min := b;
  end;

  { needed for the compare functions; should NOT be used for anything else }
var
        ptable : ppointarray; { pointer to points list }

function compare_ind(u, v : pointer) : graph_int; {$ifndef fpc} far; {$endif fpc}
begin
        if (ptable^[pint(u)^].y <= ptable^[pint(v)^].y) then compare_ind := -1
        else compare_ind := 1;
end;

function compare_active(u, v : pointer) : graph_int; {$ifndef fpc} far; {$endif fpc}
begin
        if (pedge(u)^.x <= pedge(v)^.x) then compare_active := -1
        else compare_active := 1;
end;

procedure fillpoly(numpoints : word; var PolyPoints);
{ variables needed within the helper procedures too }
var
        activetable : pedgearray; { active edge table, e.g. edges crossing current scanline }
        activepoints : graph_int; { number of points in active edge table }

{ remove edge i from active edge table }
procedure cdelete(index : graph_int);
var
        j : graph_int;
begin
        j := 0;
        while (j < activepoints) and (pedgearray(activetable)^[j].i <> index) do inc(j);
        if (j >= activepoints) then exit;
        dec(activepoints);
        move(pedgearray(activetable)^[j+1], pedgearray(activetable)^[j],
                (activepoints-j) * sizeof(edge));
end;

{ insert edge index into active edge table (at the last position) }
procedure cinsert(index, y : graph_int);
var
        j : graph_int;
        deltax : graph_float;
        p, q : ppointtype;
begin
        if (index < (numpoints-1)) then j := index + 1 else j := 0;

        if (ptable^[index].y < ptable^[j].y) then begin
                p := @ptable^[index];
                q := @ptable^[j];
        end else begin
                p := @ptable^[j];
                q := @ptable^[index];
        end;
        deltax := (q^.x-p^.x) / (q^.y-p^.y);
        with activetable^[activepoints] do begin
                dx := deltax;
                x := dx * (y { + 0.5} - p^.y) + p^.x;
                i := index;
        end;
        inc(activepoints);
end;

{ variables for the main procedure }
var
        k, i, j : graph_int;
        starty, endy, y, xl, xr : graph_int;
    oldcolor : word;
var
        indextable : pintarray; { list of vertex indices, sorted by y }

begin
    oldcolor := CurrentColor;
    CurrentColor := FillSettings.Color;
        ptable := @PolyPoints;
        if (numpoints<=0) then exit;

        getmem(indextable, sizeof(graph_int) * numpoints);
        getmem(activetable, sizeof(edge) * numpoints);
    if (not assigned(activetable)) or (not assigned(indextable)) then
      begin
        _GraphResult := grNoScanMem;
        exit;
      end;
{$R-}
        { create y-sorted array of indices indextable[k] into vertex list }
        for k := 0 to (numpoints-1) do
                indextable^[k] := k;
        { sort the indextable by points[indextable[k]].y }
{$ifndef fpc}
        bsort(indextable, numpoints, sizeof(graph_int), compare_ind);
{$else fpc}
        bsort(indextable, numpoints, sizeof(graph_int), @compare_ind);
{$endif fpc}
        { start with empty active edge table }
        activepoints := 0;
        { indextable[k] is the next vertex to process }
        k := 0;
        { ymin of polygon }
        starty := ceil(pointarray(polypoints)[indextable^[0]].y-0.5);
        { ymax of polygon }
        endy := floor(pointarray(polypoints)[indextable^[numpoints-1]].y-0.5);

        { step through scanlines }
        for y := starty to endy do begin
                { check vertices between previous scanline and current one, if any }
                while (k < numpoints) and
           (pointarray(polypoints)[indextable^[k]].y<=(y+0.5)) do begin
                        i := indextable^[k];
                        { insert or delete edges before and after points[i] ((i-1) to i and
                          i to (i+1)) from active edge table if they cross scanline y }
                        { point previous to i }
                        if (i > 0) then j := i-1 else j := numpoints-1;
                        { old edge, remove from list }
                        if (pointarray(polypoints)[j].y <= (y-0.5)) then cdelete(j)
                        { new edge, add to active edges }
                        else if (pointarray(polypoints)[j].y > (y + 0.5)) then cinsert(j, y);

                        { point next after i }
                        if (i < (numpoints-1)) then j := i+1 else j := 0;
                        { old edge, remove from active edge table }
                        if (pointarray(polypoints)[j].y <= (y - 0.5)) then cdelete(i)
                        { new edge, add to active edges }
                        else if (pointarray(polypoints)[j].y > (y + 0.5)) then cinsert(i, y);
                        inc(k);
                end;
                { sort active edges list by active[j].x }
{$ifndef fpc}
                bsort(activetable, activepoints, sizeof(edge), compare_active);
{$else fpc}
                bsort(activetable, activepoints, sizeof(edge),@compare_active);
{$endif fpc}
                j := 0;
                { draw horizontal segments for scanline y }
                while (j < activepoints) do begin
                        {xl := ceil(activetable^[j].x-0.5);}
                        xl := trunc(activetable^[j].x-0.5);
                        if frac(activetable^[j].x-0.5)>0 then inc(xl);

                        xr := trunc(activetable^[j+1].x-0.5);
                        if frac(activetable^[j+1].x-0.5)<0 then dec(xr);

                        if (xl < xr) then
                          PatternLine(xl,xr,y);
{                               line(xl, y, xr+1, y);}
                        { increment both edges' coordinates }
                        with activetable^[j] do begin
                                x := x + dx;
                        end;
                        with activetable^[j+1] do begin
                                x := x + dx;
                        end;
                        inc(j, 2);
                end;
        end;
{$ifdef debug}
{$R+,Q+}
{$endif debug}
        freemem(activetable, sizeof(edge) * numpoints);
        freemem(indextable, sizeof(graph_int) * numpoints);
    { restore the old color }
    CurrentColor := OldColor;
    { now let's draw the outline of this polygon }
    DrawPoly(NumPoints, PolyPoints);
end;



type
  TFloodLine = record
    x1 : smallint;
    x2 : smallint;
    y  : smallint;
  end;

  TDrawnList  = Array[0..StdBuffersize] of TFloodLine;

var
   DrawnIndex : Word;
   DrawnList : TDrawnList;
   Buffer : Record                         { Union for byte and word addressing of buffer }
     ByteIndex : Word;
     WordIndex : Word;
     Case Boolean Of
        False : (Bytes : Array [0..StdBufferSize-1] Of Byte);
        True  : (Words : Array [0..(StdBufferSize DIV 2)-1] Of Word);
     End;

  s1, s2, s3 : PWordArray;                { Three buffers for scanlines                 }


  Procedure PushPoint (x, y : smallint);
  {********************************************************}
  { Adds a  point to the list of points to check if we     }
  { need to draw. Doesn't add the point if there is a      }
  { buffer overflow.                                       }
  {********************************************************}
  Begin
    If Buffer.WordIndex<(StdBufferSize DIV 2)-3 then
     Begin
       Buffer.Words[Buffer.WordIndex]:=x;
       Buffer.Words[Buffer.WordIndex+1]:=y;
       Inc (Buffer.WordIndex,2);
     End
  End;

  Procedure PopPoint (Var x, y : smallint);
  {********************************************************}
  { Removes a point from the list of points to check, if   }
  { we try to access an illegal point, then the routine    }
  { returns -1,-1 as a coordinate pair.                    }
  {********************************************************}
  Begin
   If Buffer.WordIndex>1 then
    Begin
      x:=Buffer.Words[Buffer.WordIndex-2];
      y:=Buffer.Words[Buffer.WordIndex-1];
      Dec (Buffer.WordIndex,2);
    End
   Else
    Begin
      x:=-1;
      y:=-1;
    End;
  End;






  {********************************************************}
  { Procedure AddLinePoints()                              }
  {--------------------------------------------------------}
  { Adds a line segment to the list of lines which will be }
  { drawn to the screen. The line added is on the specified}
  { Y axis, from the x1 to x2 coordinates.                 }
  {********************************************************}
  Procedure AddLinePoints(x1,x2,y: smallint);
   begin
     DrawnList[DrawnIndex].x1 := x1;
     DrawnList[DrawnIndex].x2 := x2;
     DrawnList[DrawnIndex].y := y;
     Inc(DrawnIndex);
   end;

  {********************************************************}
  { Procedure AlreadyDrawn()                               }
  {--------------------------------------------------------}
  { This routine searches through the list of segments     }
  { which will be drawn to the screen, and determines  if  }
  { the specified point (x,y) will already be drawn.       }
  { i.e : Checks if the x,y point lies within a known      }
  { segment which will be drawn to the screen. This makes  }
  { sure that we don't draw some segments two times.       }
  { Return TRUE if the point is already in the segment list}
  { to draw, otherwise returns FALSE.                      }
  {********************************************************}
  Function AlreadyDrawn(x, y: smallint): boolean;
   var
    LocalIndex : smallint;
   begin
    AlreadyDrawn := FALSE;
    LocalIndex := 0;
    while LocalIndex < DrawnIndex do
     Begin
       { if vertical val is equal to our y point ... }
       if DrawnList[LocalIndex].y = y then
         Begin
           { then check if x >< ... }
           if (x >= DrawnList[LocalIndex].x1) and
               (x <= DrawnList[LocalIndex].x2) then
                 Begin
                   AlreadyDrawn := TRUE;
                   exit;
                 end;
         end;
       Inc(LocalIndex);
     end;
   end;


  Procedure FloodFill (x, y : smallint; Border: word);
  {********************************************************}
  { Procedure FloodFill()                                  }
  {--------------------------------------------------------}
  { This routine fills a region of the screen bounded by   }
  { the <Border> color. It uses the current fillsettings   }
  { for the flood filling. Clipping is supported, and      }
  { coordinates are local/viewport relative.               }
  {********************************************************}
  Var
   stemp: PWordArray;
   Beginx : smallint;
   d, e : Byte;
   Cont : Boolean;
   BackupColor : Word;
   x1, x2, prevy: smallint;
   Index : smallint;
  Begin
    { init prevy }
    prevy := 32767;
    { Save current drawing color }
    BackupColor := CurrentColor;
    CurrentColor := FillSettings.Color;
    { MaxX is based on zero index }
    GetMem (s1,(ViewWidth+1)*2);  { A pixel color represents a word }
    GetMem (s2,(ViewWidth+1)*2);  { A pixel color represents a word }
    GetMem (s3,(ViewWidth+1)*2);  { A pixel color represents a word }
    if (not assigned(s1)) or (not assigned(s2)) or (not assigned(s3)) then
      begin
        _GraphResult := grNoFloodMem;
        exit;
      end;
    If (x<0) Or (y<0) Or
       (x>ViewWidth) Or (y>ViewHeight) then Exit;
    { Some internal variables }
    Index := 0;
    { Index of segments to draw }
    DrawnIndex := 0;
    { Index of points to check  }
    Buffer.WordIndex:=0;
    PushPoint (x,y);
    While Buffer.WordIndex>0 Do
     Begin
       PopPoint (x,y);
       { Get the complete lines for the following }
       If (prevy - y = 1) then
         { previous line was one below the new one, so the previous s2 }
         { = new s1                                                    }
         Begin
           stemp := s1;
           s1 := s2;
           s2 := stemp;
         End
       Else If (y - prevy = 1) then
         { previous line was one above the new one, so the previous s3 }
         { = new s1                                                    }
         Begin
           stemp := s1;
           s1 := s3;
           s3 := stemp;
         End
       Else GetScanline(0,ViewWidth,y,s1^);
       GetScanline(0,ViewWidth,y-1,s2^);
       GetScanline(0,ViewWidth,y+1,s3^);
       prevy := y;
       { check the current scan line }
       While (s1^[x]<>Border) And (x<=ViewWidth) Do Inc (x);
       d:=0;
       e:=0;
       dec(x);
       Beginx:=x;
       REPEAT
         { check the above line }
         If y<ViewHeight then
           Begin
              Cont:=(s3^[x]<>Border) and (not AlreadyDrawn(x,y+1));
              If (e=0) And Cont then
                Begin
                  PushPoint (x,y+1);
                  e:=1;
                End
              Else
                If (e=1) And Not Cont then e:=0;
           End;
        { check the line below }
        If (y>0) then
          Begin
            Cont:=(s2^[x]<>Border) and (not AlreadyDrawn(x,y-1));
            If (d=0) And Cont then
              Begin
                PushPoint (x,y-1);
                d:=1;
              End
            Else
              If (d=1) And Not Cont then d:=0;
          End;
        Dec (x);
       Until (x<0) Or (s1^[x]=Border);
       { swap the values }
       x1:=x+1;
       x2:=BeginX;
       if x1 > x2 then
         Begin
           x:=x1;
           x1:=x2;
           x2:=x;
         end;
       { Add to the list of drawn lines }
       AddLinePoints(x1,x2,y);
       PatternLine (x1,x2,y);
     End; { end while }

    FreeMem (s1,(ViewWidth+1)*2);
    FreeMem (s2,(ViewWidth+1)*2);
    FreeMem (s3,(ViewWidth+1)*2);
    CurrentColor := BackUpColor;
  End;

{
$Log$
Revision 1.13  1999-12-20 11:22:36  peter
  * integer -> smallint to overcome -S2 switch needed for ggi version

Revision 1.12  1999/12/11 23:41:38  jonas
  * changed definition of getscanlineproc to "getscanline(x1,x2,y:
    smallint; var data);" so it can be used by getimage too
  * changed getimage so it uses getscanline
  * changed floodfill, getscanline16 and definitions in Linux
    include files so they use this new format
  + getscanlineVESA256 for 256 color VESA modes (banked)

Revision 1.11  1999/09/27 23:34:40  peter
  * new graph unit is default for go32v2
  * removed warnings/notes

Revision 1.10  1999/09/24 22:52:38  jonas
  * optimized patternline a bit (always use hline when possible)
  * isgraphmode stuff cleanup
  * vesainfo.modelist now gets disposed in cleanmode instead of in
    closegraph (required moving of some declarations from vesa.inc to
    new vesah.inc)
  * queryadapter gets no longer called from initgraph (is called from
    initialization of graph unit)
  * bugfix for notput in 32k and 64k vesa modes
  * a div replaced by / in fillpoly

Revision 1.9  1999/09/24 14:23:08  jonas
  * floodfill uses scanline data from previous loop if line is adjacent

Revision 1.8  1999/09/18 22:21:09  jonas
  + hlinevesa256 and vlinevesa256
  + support for not/xor/or/andput in vesamodes with 32k/64k colors
  * lots of changes to avoid warnings under FPC

Revision 1.7  1999/09/17 13:58:31  jonas
* another fix for a case where internalellipsedefault went haywire
* sector() and pieslice() fully implemented!
* small change to prevent buffer overflow with floodfill

Revision 1.6  1999/09/12 17:28:59  jonas
  * several changes to internalellipse to make it faster
    and to make sure it updates the ArcCall correctly
    (not yet done for width = 3)
  * Arc mostly works now, only sometimes an endless loop, don't know
    why

Revision 1.5  1999/09/11 19:43:00  jonas
  * FloodFill: did not take into account current viewport settings
  * GetScanLine: only get line inside viewport, data outside of it
    is not used anyway
  * InternalEllipseDefault: fix for when xradius or yradius = 0 and
    increase xradius and yradius always by one (TP does this too)
  * fixed conlict in vesa.inc from last update
  * some conditionals to avoid range check and overflow errors in
    places where it doesn't matter

Revision 1.4  1999/07/12 14:52:52  jonas
  * fixed procvar syntax error and ceil and floor functions

Revision 1.3  1999/07/12 13:27:11  jonas
  + added Log and Id tags
  * added first FPC support, only VGA works to some extend for now
  * use -dasmgraph to use assembler routines, otherwise Pascal
    equivalents are used
  * use -dsupportVESA to support VESA (crashes under FPC for now)
  * only dispose vesainfo at closegrph if a vesa card was detected
  * changed int32 to longint (int32 is not declared under FPC)
  * changed the declaration of almost every procedure in graph.inc to
    "far;" becquse otherwise you can't assign them to procvars under TP
    real mode (but unexplainable "data segnment too large" errors prevent
    it from working under real mode anyway)

}
