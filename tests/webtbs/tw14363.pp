program bug;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, math;
  { you can add units after this }

type tpointarray=array of tpoint;
procedure smallestCircel(var x,y,rsqr: float; const p1i,p2i,p3i: longint; const points: tpointarray);
var //p1i,p2i,p3i: longint;
    //hull: TLongintArray;
    f1,f2,besti,temp:longint;
    nx,ny,nrsqr:float;
    p0,p1,p2: ^tpoint;
begin
  //(x-x0)^2+(y-y0)^2=r^2=x^2-2xx0+x0^2+y^2-2yy0+y0^2
  //(x-x1)^2+(y-y1)^2=r^2=x^2-2xx1+x1^2+y^2-2yy1+y1^2
  //(x-x2)^2+(y-y2)^2=r^2=x^2-2xx2+x2^2+y^2-2yy2+y2^2
  //=> r^2 - r^2=x^2-2xx0+x0^2+y^2-2yy0+y0^2 - x^2+2xx1-x1^2-y^2+2yy1-y1^2
  //-2xx0+x0^2-2yy0+y0^2 +2xx1-x1^2 + 2yy1-y1^2 = 0
  //2x(x1-x0) + 2y(y1-y0) = x1^2 + y1^2 - x0^2 - y0^2
  //2x(x2-x1) + 2y(y2-y1) = x2^2 + y2^2 - x1^2 - y1^2
  //p1i:=hull[i];
  p0:=@points[p1i];
  //p2i:=hull[j];
  p1:=@points[p2i];
  //p3i:=hull[k];
  p2:=@points[p3i];
  nrsqr:=-1;
  if p0^.y=p1^.y then begin
    if (p1^.y=p2^.y) or (p0^.x=p1^.x) then begin
      //Fall 0: points on a line
      ny:=p0^.y;
      if (p0^.x<=p1^.x) and (p1^.x<=p2^.x) then begin
        nx:=(p0^.x + p2^.x) / 2;
        nrsqr:=sqr(p0^.x - p2^.x) /4;
      end else if (p1^.x<=p0^.x) and (p0^.x<=p2^.x) then begin
        nx:=(p1^.x + p2^.x) / 2;
        nrsqr:=sqr(p1^.x - p2^.x) /4;
      end else begin//if (p0^.x<p2^.x) and (p2^.x<p1^.x) then begin
        nx:=(p0^.x + p1^.x) / 2;
        nrsqr:=sqr(p0^.x - p1^.x) /4;
      end;
    end else begin
      //=> Fall 1: y0=y1
      // 2x(x1-x0) = x1^2 + x0^2 => x = x1^2 + x0^2 / (2(x1-x0))
      nx:=(sqr(p1^.x) - sqr(p0^.x)) / (2*(p1^.x-p0^.x));
      // 2y(y2-y1) = x2^2 + y2^2 - x1^2 - y1^2 - 2x(x2-x1)
      ny := (sqr(p2^.x)+sqr(p2^.y)-sqr(p1^.x)-sqr(p1^.y) - 2*nx*(p2^.x-p1^.x))/(2*(p2^.y-p1^.y));
    end;
  end else begin
    //=> Fall 3: y0<>y1
    //2x(x1-x0)*(x2-x1) + 2y(y1-y0)*(x2-x1) = (x1^2 + y1^2 - x0^2 - y0^2) * (x2-x1)
    //2x(x1-x0)*(x2-x1) + 2y(y2-y1)*(x1-x0) = (x2^2 + y2^2 - x1^2 - y1^2) * (x1-x0)
    //=> 2y(y1-y0)*(x2-x1)-2y(y2-y1)*(x1-x0) = (x1^2 + y1^2 - x0^2 - y0^2) * (x2-x1)-(x2^2 + y2^2 - x1^2 - y1^2) * (x1-x0)
    //=> y= ((x1^2 + y1^2 - x0^2 - y0^2) * (x2-x1)-(x2^2 + y2^2 - x1^2 - y1^2) * (x1-x0)) /
    // (2(y1-y0)*(x2-x1)-2(y2-y1)*(x1-x0))
    temp:=(p1^.y-p0^.y)*(p2^.x-p1^.x)-(p2^.y-p1^.y)*(p1^.x-p0^.x);
    if temp=0 then begin
      //=>(p1^.y-p0^.y)/(p1^.x-p0^.x) = (p2^.y-p1^.y)/(p2^.x-p1^.x)
      //=> p0->p1 parallel to p1->p2
      //=> all points lie on a single line
      //where is p2 on the line from p0 to p1?
      //Eu: (u-p0^.x) * (p1^.y-p0^.y)/(p1^.x-p0^.x) + p0^.y = p2^.y
      //=> u = (p2^.y - p0^.y) * (p1^.x-p0^.x) / (p1^.y-p0^.y) + -p0^.x
      nx:=(p2^.y - p0^.y) * (p1^.x-p0^.x) / (p1^.y-p0^.y) -p0^.x; //exists, y checked above
      if nx > 1 then begin
        nx:=(p0^.x + p2^.x) / 2;
        ny:=(p0^.y + p2^.y) / 2;
        nrsqr:=sqr(p0^.x - p2^.x) /4;
      end else if nx<1 then begin
        nx:=(p1^.x + p2^.x) / 2;
        ny:=(p1^.y + p2^.y) / 2;
        nrsqr:=sqr(p1^.x - p2^.x) /4;
      end else begin
        nx:=(p0^.x + p1^.x) / 2;
        ny:=(p0^.y + p1^.y) / 2;
        nrsqr:=sqr(p0^.x - p1^.x) /4;
      end;
    end else begin
      ny:=((sqr(p1^.x) + sqr(p1^.y) - sqr(p0^.x) - sqr(p0^.y)) * (p2^.x-p1^.x) - (sqr(p2^.x) + sqr(p2^.y) - sqr(p1^.x) - sqr(p1^.y)) * (p1^.x-p0^.x)) / (2*temp);
      //2x(x1-x0) = x1^2 + y1^2 - x0^2 - y0^2 - 2y(y1-y0)
      //2x(x2-x1) = x2^2 + y2^2 - x1^2 - y1^2 - 2y(y2-y1)
      if p0^.x <> p1^.x then
        nx:= (sqr(p1^.x) + sqr(p1^.y) - sqr(p0^.x) - sqr(p0^.y) - 2*ny*(p1^.y-p0^.y)) / (2*(p1^.x-p0^.x))
      else
        nx:= (sqr(p2^.x) + sqr(p2^.y) - sqr(p1^.x) - sqr(p1^.y) - 2*ny*(p2^.y-p1^.y)) / (2*(p2^.x-p1^.x))
    end;
  end;
  if nrsqr < 0 then
    nrsqr := sqr(nx-p0^.x) + sqr(ny-p0^.y);

  x:=nx;
  y:=ny;
  rsqr:=nrsqr;
end;

var x,y,rsqr: float;
    points: tpointarray;
    s: string;
begin
  setlength(points,4);
  points[0].x:=55; points[0].y:=14;
  points[1].x:=63; points[1].y:=61;
  points[2].x:=10; points[2].y:=64;
  points[3].x:=20; points[3].y:=44;
  smallestCircel(x,y,rsqr,2,1,0,points);
  writestr(s,x:4:2,' ',y:4:2,' ',rsqr:4:2);
  if (s<>'35.31 41.53 1145.57') then
    halt(1);
end.
