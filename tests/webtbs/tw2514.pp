{ %opt=-Or }

{ Source provided for Free Pascal Bug Report 2514 }
{ Submitted by "Andreas Horst" on  2003-05-28 }
{ e-mail: AndyHorst@web.de }
{$mode objfpc}
type TRGB=record
 TRGB2HSL: record
 r,g,b : extended;
 l,h,s : extended;
 end;
 end;

 THSL=record end;

function TRGB2HSL(rgb:TRGB):THSL;
var maxcolor,mincolor:Extended;
begin
  with rgb,TRGB2HSL do begin
    if r<0 then r:=0;
    if r>1 then r:=1;
    if g<0 then g:=0;
    if g>1 then g:=1;
    if b<0 then b:=0;
    if b>1 then b:=1;
    if r<g then begin
      mincolor:=r;
      maxcolor:=g;
    end else begin
      mincolor:=g;
      maxcolor:=r;
    end;
    if b<mincolor then
      mincolor:=b;
    if b>maxcolor then
      maxcolor:=b;
    if maxcolor=mincolor then begin
      l:=maxcolor;
      s:=0;
      h:=0;
      Exit;
    end;
    l:=(maxcolor+mincolor)/2;
    if l<0.5 then
      s:=(maxcolor-mincolor)/(maxcolor+mincolor)
    else
      s:=(maxcolor-mincolor)/(2-maxcolor-mincolor);
    if r=maxcolor then h:=(g-b)/(maxcolor-mincolor);
    if g=maxcolor then h:=2+(b-r)/(maxcolor-mincolor);
    if b=maxcolor then h:=4+(r-g)/(maxcolor-mincolor);
  end
end;

var
  rgb : TRGB;
  i : longint;
begin
  fillchar(rgb,sizeof(rgb),0);
  for i:=0 to 100 do
    TRGB2HSL(rgb);
end.
