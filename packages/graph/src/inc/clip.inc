{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

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
   code: longint;
   newx,newy: smallint;
   done:boolean;


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

