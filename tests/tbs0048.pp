uses
   graph,crt;

var
   gd,gm : integer;
   i,size : longint;
   p : pointer;

begin
   gd:=detect;
   initgraph(gd,gm,'');
   setcolor(brown);
   line(0,0,getmaxx,0);
   readkey;
   size:=imagesize(0,0,getmaxx,0);
   getmem(p,size);
   getimage(0,0,getmaxx,0,p^);
   cleardevice;
   for i:=0 to getmaxy do
     begin
        putimage(0,i,p^,xorput);
     end;
   readkey;
   for i:=0 to getmaxy do
     begin
        putimage(0,i,p^,xorput);
     end;
   readkey;
   closegraph;
end.
   
