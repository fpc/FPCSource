{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by the Free Pascal development team

    This include implements the different clipping algorithms

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const
  LEFT   = 1;    { Left window   }
  RIGHT  = 2;    { Right window  }
  BOTTOM = 4;    { Bottom window }
  TOP    = 8;    { Top window    }
                 { 0 = in window }







  function LineClipped(var x1, y1,x2,y2: smallint; xmin, ymin,
      xmax, ymax:smallint): boolean;
  {********************************************************}
  { Function LineClipped()                                 }
  {--------------------------------------------------------}
  { This routine clips the line coordinates to the         }
  { min. and max. values of the window. Returns TRUE if    }
  { the ENTIRE line was clipped.  Updated                  }
  { clipped line endpoints are also returned.              }
  { This algorithm is the classic Cohen-Sutherland line    }
  { clipping algorithm.                                    }
  {--------------------------------------------------------}
  var
   code1, code2: longint;
   done:boolean;
   code: longint;
   newx,newy: word;


    function outcode(x,y:smallint): longint;
    {********************************************************}
    { Function OutCode()                                     }
    {--------------------------------------------------------}
    { This routine determines if the specified end point     }
    { of a line lies within the visible window, if not it    }
    { determines in which window the point is.               }
    {--------------------------------------------------------}

    var
     code: longint;
    begin
      code := 0;
      if (x<xmin) then
        code:=code or LEFT
      else if (x>xmax) then
        code:=code or RIGHT;
      if (y>ymax) then
        code:=code or BOTTOM
      else if (y<ymin) then
        code:=code or TOP;

      outcode:=code;
    end;

  begin
    done:=false;
    code1:= OutCode(x1,y1);
    code2:= OutCode(x2,y2);

    while not done do
     begin
       { Accept trivially }
       { both points are in window }
       if ((code1=0) and (code2=0)) then
         begin
           done:=TRUE;
           LineClipped:=FALSE;
               exit;
             end
       else
       { Reject trivially }
       { Neither points are in window }
       if (code1 and code2) <> 0 then
         begin
           done:=true;
           LineClipped:=TRUE;
           exit;
         end
       else
          begin
            { Some points are partially out of the window }
            { find the new end point of the lines...      }
            if code1 = 0 then
             code:=code2
            else
             code:=code1;
            if (code and LEFT) <> 0 then
              begin
                newy:=y1+((y2-y1)*(xmin-x1)) div (x2-x1);
                newx:=xmin;
              end
            else
            if (code and RIGHT) <> 0 then
              begin
                newy:=y1+((y2-y1)*(xmax-x1)) div (x2-x1);
                newx:=xmax;
              end
            else
            if (code and BOTTOM) <> 0 then
              begin
                newx:=x1+((x2-x1)*(ymax-y1)) div (y2-y1);
                newy:=ymax;
              end
            else
            if (code and TOP) <> 0 then
              begin
                newx:=x1+((x2-x1)*(ymin-y1)) div (y2-y1);
                newy:=ymin;
              end;
           if (code1 = code) then
            begin
              x1 := newx;  y1:= newy;
              code1:=outcode(x1,y1)
            end
               else
            begin
              x2:= newx; y2:= newy;
              code2:=outcode(x2,y2);
            end
         end;
      end;
  LineClipped:=FALSE;
end;

{
$Log$
Revision 1.7  1999-12-20 11:22:35  peter
  * integer -> smallint to overcome -S2 switch needed for ggi version

Revision 1.6  1999/09/27 12:35:27  jonas
  * execute multiplications before divisions in lineclipped to avoid rounding errors

Revision 1.5  1999/09/18 22:21:09  jonas
  + hlinevesa256 and vlinevesa256
  + support for not/xor/or/andput in vesamodes with 32k/64k colors
  * lots of changes to avoid warnings under FPC

Revision 1.4  1999/09/12 17:28:59  jonas
  * several changes to internalellipse to make it faster
    and to make sure it updates the ArcCall correctly
    (not yet done for width = 3)
  * Arc mostly works now, only sometimes an endless loop, don't know
    why

Revision 1.3  1999/07/12 13:27:09  jonas
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
