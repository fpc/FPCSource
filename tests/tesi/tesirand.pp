{
  $Id$

  This program test the random function
  It gets 10M random values
  that are placed in 10000 windows
  and print the number of occurence for each window
  and the profile of the distribution
  of the counts

  - this gave very bad value due to a modulo problem
    but after this solved
    it still shows strange wings !!
}
program test_random;

uses
{$ifdef go32v2}
  dpmiexcp,
{$endif go32v2}
   graph;


const max = 1000;
      maxint = 10000*max;


var x : array[0..max-1] of longint;
    y : array[-100..100] of longint;

    mean,level,i : longint;
    maxcount,delta,maximum,minimum : longint;
    st,st2 : string;
    gm,gd : integer;
    color : longint;

begin

{$ifdef go32v2}
   gm:=m640x400x256;
   gd:=vesa;
{$else }
   gd:=detect;
{$endif }
   InitGraph(gd,gm,'\tp\bgi');
{$ifdef FPC}
   SetWriteMode(NormalPut);
{$endif FPC}
   SetColor(red);
   color:=blue;

   mean:=maxint div max;

   setfillstyle(solidfill,blue);
   for level:=0 to 10 do
     begin

        for i:=0 to max-1 do
          x[i]:=0;
        for i:=-100 to 100 do
          y[i]:=0;
        for i:=0 to maxint-1 do
          begin
             if level=0 then
               inc(x[trunc(random*max)])
             else
               inc(x[random(max*level) div (level)]);
             if i mod (maxint div 10) = 0 then
               begin
                  bar(20+textwidth('iteration '),17,
                    20+textwidth('iteration 0000000'),26);
                  st:='';
                  str(i,st);
                  st:='iteration '+st;
                  OutTextXY(20,20,st);
                  {Writeln(stderr,st);}
               end;
          end;
        maximum:=0;
        minimum:=$7FFFFFFF;
        maxcount:=0;
        for i:=0 to max-1 do
          begin
             if x[i]>maximum then
               maximum:=x[i];
             if x[i]<minimum then
               minimum:=x[i];
             if abs(x[i]-mean)<100 then
               inc(y[x[i]-mean]);
          end;
        if maximum=0 then
          inc(maximum);

        for i:=-100 to 100 do
          if y[i]>maxcount then
            maxcount:=y[i];
        if maxcount=0 then
          inc(maxcount);

        OutTextXY(GetMaxX div 2,GetMaxY-30,'Random Test Program');

        str(level,st);
        st:='Level '+st;
        bar(30,GetMaxY-65,
         30+textwidth(st),getMaxY-52);
        OutTextXY(30,GetMaxY-59,st);
        str(maximum,st);
        str(minimum,st2);
        st:='Maximum = '+st+' Minimum ='+st2;
        bar(30,GetMaxY-35,
         30+Textwidth(st),getMaxY-22);
        OutTextXY(30,GetMaxY-29,st);

        for i:=0 to max-1 do
          putpixel( (i*getmaxX) div max,
            GetMaxY-(x[i]*getMaxY) div (2*mean), color);
        inc(color);
        setColor(color);
        delta:=maximum-minimum+1;
        for i:=-100 to 100 do
          begin
            if i=minimum then
              moveto( ((i+100)*getMaxX) div 201,
                GetMaxY-(y[i]*getMaxY) div maxcount)
            else
              lineto( ((i+100)*getMaxX) div 201,
                GetMaxY-(y[i]*getMaxY) div maxcount);
            if y[i]>0 then
              circle( ((i+100)*getMaxX) div 201,
                GetMaxY-(y[i]*getMaxY) div maxcount,5);
          end;
        readln;
        inc(color);
     end;
   CloseGraph;
end.

{
  $Log$
  Revision 1.1  2000-07-13 09:22:04  michael
  + Initial import

  Revision 1.2  2000/03/25 13:45:35  jonas
    * works with new graph unit

  Revision 1.1  1999/12/02 17:37:44  peter
    * moved *.pp into subdirs
    * fpcmaked

  Revision 1.3  1999/01/25 20:23:13  peter
    * linux updates

  Revision 1.2  1998/11/23 23:44:52  pierre
   + several bugs converted

}

