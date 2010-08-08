{ Author   : Mike Bradbery
  Copyright: 2000 Mike Bradbery and others, see file "forum.txt" }

program gdtestcgi;

uses gd;

var
  f:pFile;
  black,white:integer;
  red,green,blue:integer;
  im:gdImagePtr;
  s1,s2:string;
  points:array[0..2] of gdpoint;
  x : longint;
  styleDotted: array[0..1] of longint;
  styleDashed: array[0..5] of longint;
  top,bottom,left,right : longint;

// Reference to the libc fdopen function, needed to open standard output.

Function fdopen(FD: longint; Mode : Pchar) : Pointer;cdecl;external 'c';

Begin
  left := 60;
  top := 30;
  right := 580;
  bottom := 300;
  im:=gdImageCreate(600,390);
  black:=gdImageColorAllocate(im,0,0,0);
  white:=gdImageColorAllocate(im,255,255,255);
  red:=gdImageColorAllocate(im,255,0,0);
  green:=gdImageColorAllocate(im,0,255,0);
  blue:=gdImageColorAllocate(im,0,0,255);
  styleDotted[0] := red;
  styleDotted[1] := gdTransparent;

  styleDashed[0] := white;
  styleDashed[1] := white;
  styleDashed[2] := white;
  styleDashed[3] := gdTransparent;
  styleDashed[4] := gdTransparent;
  styleDashed[5] := gdTransparent;
  gdImageSetStyle(im,@styleDashed[0],6 );

  {box around the lot}
  gdImageRectangle(im,0,0,599,389,white);

  {main title}
  s1:='The Test Graph Title.';
  gdImageString(im, gdFontLarge,{im^.w}600  div 2 - ((length(s1)-1)*gdFontLarge^.w div 2),2{gdFontLarge^.h}, s1, white);
  gdImageLine(im,600  div 2 - ((length(s1)-1)*gdFontLarge^.w div 2),gdFontLarge^.h+3,600  div 2 + ((length(s1)-1)*gdFontLarge^.w div 2),gdFontLarge^.h+3,white);

  {box around the legend.}
  gdImageRectangle(im,100,350,500,370,white);
  s1:='The Legend.';
  gdImageString(im, gdFontLarge, 100+2, 350+2, s1, white);

  s1:='The Y axis Title.';
  gdImageStringUp(im, gdFontLarge, Left-5-gdFontLarge^.h-gdFontLarge^.w*2, top+(bottom-top) div 2+((length(s1)-1) * gdFontLarge^.w div 2),s1, white);
  // gdImageStringUp(im, gdFontLarge, 2, 50, @s1[1], white);
  s1:='The X axis Title.';
  gdImageString(im, gdFontLarge, left+(right-left) div 2-((length(s1)-1)*gdFontLarge^.w div 2),Bottom + 5 +gdFontLarge^.h,s1,white);

  {axis}
  gdImageLine(im,left,top,left,bottom,white);
  gdImageLine(im,left,bottom,right,bottom,white);

  { the origin is 30,360}
  for x:=0 to 10 do
  begin
    str(x,s1);
    gdImageLine(im,left+(x*(right-left) div 10) ,bottom,left+(x*(right-left) div 10) ,bottom+3,white);
    gdImageLine(im, left+(x*(right-left) div 10), bottom, left+(x*(right-left) div 10), top, gdStyled);
    gdImageString(im, gdFontLarge, left+(x*(right-left) div 10) - ((length(s1)-1)*gdFontLarge^.w div 2),bottom+5, s1, white);
    gdImageLine(im,left,bottom-(x*(bottom-top) div 10),left-3,bottom-(x*(bottom-top) div 10),white);
    gdImageLine(im, left, bottom-(x*(bottom-top) div 10), right, bottom-(x*(bottom-top) div 10), gdStyled);
    gdImageString(im, gdFontLarge,left-5-((length(s1)-1)*gdFontLarge^.w),bottom-(x*(bottom-top) div 10)-gdFontLarge^.h div 2, s1, white);
  end;
  // open standard output as C file.
  f:=fdopen(1,'wb');
  // Write header
  Writeln('Content-type: image/png');
  Writeln;
  gdImagePng(im,f);
  fclose(f);
  gdImageDestroy(im);
End.
