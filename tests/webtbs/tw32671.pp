{ %CPU=i386 }
{ %OPT=-Cfsse2 }
program test;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses math,sysutils;

var
  e : exception;

procedure initLut();
const
      width = 640;
      height = 480;
var
    Lut : array[0..width*height-1] of longword;
     i,j : longint;
    x,y,w,r,a,u,v,s : single;
    iu,iv,iw : longint;
begin
    for j:=height div 2 to height div 2+1 do
    for i:=width div 2 to width div 2+1 do
    begin
        x := -1.0 + i*(2.0/width);
        y := 1.0 - j*(2.0/height);
        r := sqrt( x*x+y*y );
        a := arctan2( y, x );

        writeln(r);

        u := 1.0/r;
        v := a*(3.0/3.14159);
        w := r*r;
        if( w>1.0 ) then w := 1.0;

        iu := round(u*255.0);
        iv := round(v*255.0);
        iw := round(w*255.0);

        Lut[width*j+i] := ((iw and 255)<<16) or ((iv and 255)<<8) or (iu and 255);
    end;
end;

begin
  try
    initLut();
  except
    on e : EZeroDivide do
      begin
        writeln('ok');
        halt(0);
      end;
  end;
  { no exception is also ok, if the exception occurs, depends on rounding during expression evaluation }
  writeln('ok');
end.
