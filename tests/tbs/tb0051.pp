{ %GRAPH }

{ Old file: tbs0057.pp }
{  Graph, shows a crash with switch graph/text/graph    OK 0.99.9 (PM) }

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

{$endif OK}
begin
{$ifdef OK}
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
{$endif OK}
   writeln('OK');
end.
