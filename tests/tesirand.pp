program test_random;

uses
{$ifdef go32v2}
  dpmiexcp,
{$endif go32v2}
   graph;
   

const max = 1000;
      maxint = 1000*max;
      

var x : array[0..max-1] of longint;
    mean,level,i,maximum,minimum : longint;
    st,st2 : string;
    gm,gd : word;
    color : longint;
    
begin

   gm:=G640x400x256;
   gd:=$ff;
   InitGraph(gd,gm,'');
   SetWriteMode(NormalPut or BackPut);
   SetColor(red);
   color:=blue;

   mean:=maxint div max;
   
   for level:=1 to 10 do
     begin
        for i:=0 to max-1 do
          x[i]:=0;
        for i:=0 to maxint-1 do
          begin
             inc(x[random(max*level*100) div (level*100)]);
             if i mod 1000 = 0 then
               begin
                  st:='';
                  str(i,st);
                  st:='iteration '+st;
                  OutTextXY(20,20,st);
                  {Writeln(stderr,st);}
               end;
          end;
        maximum:=0;
        minimum:=$7FFFFFFF;
        for i:=0 to max-1 do
          begin
             if x[i]>maximum then
               maximum:=x[i];
             if x[i]<minimum then
               minimum:=x[i];
          end;
        if maximum=0 then
          inc(maximum);
     
        OutTextXY(GetMaxX div 2,GetMaxY-30,'Random Test Program');
        
        str(level,st);
        st:='Level '+st;
        OutTextXY(30,GetMaxY-60,st);
        str(maximum,st);
        str(minimum,st2);
        st:='Maximum = '+st+' Minimum ='+st2;
        OutTextXY(30,GetMaxY-30,st);
        
        
        for i:=0 to max-1 do
          putpixel( (i*getmaxX) div max, GetMaxY-(x[i]*getMaxY) div (2*mean), color);
        readln;
        inc(color);
     end;
   CloseGraph;
end.
        

