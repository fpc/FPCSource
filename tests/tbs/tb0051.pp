{ %GRAPH }
{ %TARGET=go32v2,win32,linux }

{ Old file: tbs0057.pp }
{  Graph, shows a crash with switch graph/text/graph    OK 0.99.9 (PM) }

uses
   graph,crt;

var
   gd,gm : integer;

begin
   gd:=detect;
   gm:=$103;
   initgraph(gd,gm,'');
   setcolor(white);
   line(1,1,100,100);
   {readkey;}delay(1000);
   closegraph;
   initgraph(gd,gm,'');
   line(100,100,1,100);
   {readkey;}delay(1000);
   closegraph;
   writeln('OK');
end.
