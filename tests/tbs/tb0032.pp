{ %GRAPH }
{ %TARGET=go32v2,win32,linux }

{ Old file: tbs0037.pp }
{  tests missing graph.setgraphmode                    OK RTL (FK) }

uses
   graph,
   crt;

var
   gd,gm,res : integer;

begin
   gd:=detect;
   initgraph(gd,gm,'');
   res := graphresult;
   if res <> grOk then
     begin
       graphErrorMsg(res);
       halt(1);
     end;
   setviewport(0,0,getmaxx,getmaxy,clipon);
   line(1,1,100,100);
   {readkey;}
   setgraphmode(m1024x768);
   setviewport(0,0,getmaxx,getmaxy,clipon);
   res := graphresult;
   if res <> grOk then
     begin
       closegraph;
       graphErrorMsg(res);
       { no error, graph mode is simply not supported }
       halt(0);
     end;
   line(100,100,1024,800);
   {readkey;}
   delay(1000);
   closegraph;
end.
