{ %GRAPH }

{ Old file: tbs0048.pp }
{  shows a problem with putimage on some computers       OK 0.99.13 (JM) }

{$ifdef go32v2}
{$define OK}
{$endif}
{$ifdef Unix}
{$define OK}
{$endif}
{$ifdef win32}
{$define OK}
{$endif}

{$ifdef OK}
uses
   graph,crt;

var
   gd,gm : integer;
   i,size : longint;
   p : pointer;
{$endif OK}

begin
{$ifdef OK}
   gd:=detect;
   initgraph(gd,gm,'');
   setcolor(brown);
   line(0,0,getmaxx,0);
   {readkey;}delay(1000);
   size:=imagesize(0,0,getmaxx,0);
   getmem(p,size);
   getimage(0,0,getmaxx,0,p^);
   cleardevice;
   for i:=0 to getmaxy do
     begin
        putimage(0,i,p^,xorput);
     end;
   {readkey;}delay(1000);
   for i:=0 to getmaxy do
     begin
        putimage(0,i,p^,xorput);
     end;
   {readkey;}delay(1000);
   closegraph;
{$endif OK}
end.
